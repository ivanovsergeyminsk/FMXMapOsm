unit Import.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.DApt, FMX.ListBox, FMX.Controls.Presentation,
  FMX.StdCtrls, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Text, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite,

  System.Threading,
  System.SyncObjs
  ;

type
  TViewMain = class(TForm)
    ButtonImport: TButton;
    ComboBoxZoom: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    EditPath: TEdit;
    EllipsesEditButton1: TEllipsesEditButton;
    Label3: TLabel;
    EditDB: TEdit;
    EllipsesEditButton2: TEllipsesEditButton;
    MemoLog: TMemo;
    SaveDialog: TSaveDialog;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    ProgressBar1: TProgressBar;
    procedure EllipsesEditButton1Click(Sender: TObject);
    procedure EllipsesEditButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonImportClick(Sender: TObject);
  private const
    DEF_SQLITE = 'DEF_OSM_SQLITE_POOLED';
  private
    { Private declarations }
    FZoom: Cardinal;
    FMapPath: string;
    FDBPath: string;
    FDirs: TArray<string>;

    FFileCount: int64;

    FTask: ITask;
    FIsBreak: boolean;

    procedure InitPoolConnection;
    procedure DoImport;
    procedure IterateImport(Idx: int64; State: TParallel.TLoopState);

    procedure Import;
    procedure BreakImport;

    procedure EnableControls(Value: boolean);
  public
    { Public declarations }
    procedure Log(Value: string);
    procedure ResetProgress(AMaxValue: Single);
    procedure IncProgress;
  end;

var
  ViewMain: TViewMain;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  System.StrUtils,
  System.DateUtils,
  Winapi.Windows
  ;

procedure TViewMain.Import;
begin
  Log('Start');
  InitPoolConnection;
  FIsBreak := false;
  FTask := TTask.Run(procedure begin
    try
      DoImport;
    except
      on E: Exception do begin
        Log(E.ClassName+': '+E.Message);
      end;
    end;

    TThread.Synchronize(nil, procedure begin
      FDManager.CloseConnectionDef(DEF_SQLITE);
      EnableControls(true);
      Log('Finish');
    end);
  end)
end;

procedure TViewMain.EllipsesEditButton1Click(Sender: TObject);
var
  MapDirectory: string;
begin
  if SelectDirectory('Select Map folder', TDirectory.GetCurrentDirectory, MapDirectory) then
    EditPath.Text := MapDirectory;
end;

procedure TViewMain.EllipsesEditButton2Click(Sender: TObject);
begin
  SaveDialog.InitialDir := TDirectory.GetCurrentDirectory;
  SaveDialog.Title      := 'Select DB SQLite file for save';
  if SaveDialog.Execute then
    EditDB.Text := SaveDialog.FileName;
end;

procedure TViewMain.EnableControls(Value: boolean);
begin
  EditPath.Enabled      := Value;
  EditDB.Enabled        := Value;
  ComboBoxZoom.Enabled  := Value;

  ButtonImport.Tag := Value.ToInteger;
  ButtonImport.Text := ifthen(Value, 'Import', 'BREAK');
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  EnableControls(true);
end;

procedure TViewMain.BreakImport;
begin
  FIsBreak := true;
  Log('Break');
end;

procedure TViewMain.ButtonImportClick(Sender: TObject);
begin
  if ButtonImport.Tag.ToBoolean then begin
    EnableControls(false);

    FMapPath := ifthen(EditPath.Text.EndsWith('\'), EditPath.Text, EditPath.Text+'\');
    FDBPath  := EditDB.Text;
    FZoom    :=  ComboBoxZoom.ItemIndex;

    Import;
  end else begin
    BreakImport;
    EnableControls(true);
  end;
end;

procedure TViewMain.DoImport;
begin
  FDirs := TDirectory.GetDirectories(FMapPath+FZoom.ToString);
  if Length(FDirs) = 0 then exit;

  ResetProgress(Length(FDirs));

  TParallel.For(0, Length(FDirs)-1, IterateImport);

  Log(format('Zoom: %d | Files: %d', [FZoom, FFileCount]));
end;

procedure TViewMain.IncProgress;
begin
  TThread.Queue(nil, procedure begin
    ProgressBar1.Value := ProgressBar1.Value + 1;
  end);
end;

procedure TViewMain.InitPoolConnection;
begin
  if FDManager.IsConnectionDef(DEF_SQLITE) then begin
    FDManager.CloseConnectionDef(DEF_SQLITE);
    FDManager.DeleteConnectionDef(DEF_SQLITE);
  end;

  var oParams := TStringList.Create;
  oParams.Add(format('Database=%s', [FDBPath]));
  oParams.Add('Pooled=True');
  oParams.Add('LockingMode=Normal');
  FDManager.AddConnectionDef(DEF_SQLITE, 'SQLite', oParams);
end;

procedure TViewMain.IterateImport(Idx: int64; State: TParallel.TLoopState);
var
  ParameterX, ParameterY: Cardinal;
  ArrFiles: TArray<string>;
  DirName: string;
  FileName: string;
begin
 if FIsBreak then begin
    State.Stop;
    exit;
  end;

  var Connection  := TFDConnection.Create(nil);
  var Query       := TFDQuery.Create(nil);
  try
    Connection.ConnectionDefName := DEF_SQLITE;
    Query.Connection := Connection;
    Query.SQL.Text := 'insert into Tiles (Zoom, X, Y, Value) values (:pZoom, :pX, :pY, :pValue)';

    with Query do begin
      Params[0].DataType := ftInteger;
      Params[1].DataType := ftInteger;
      Params[2].DataType := ftInteger;
      Params[3].DataType := ftBlob;
    end;

    DirName     := FDirs[Idx];
    ParameterX  := TPath.GetFileNameWithoutExtension(DirName).ToInteger;

    ArrFiles := TDirectory.GetFiles(DirName);
    Query.Params.ArraySize := Length(ArrFiles);
    TInterlocked.Add(FFileCount, Query.Params.ArraySize);

    for var I := 0 to Query.Params.ArraySize-1 do begin
      FileName := ArrFiles[I];
      ParameterY := TPath.GetFileNameWithoutExtension(FileName).ToInteger;

      with Query do begin
        Query.Params[0].AsIntegers[I] := FZoom;
        Query.Params[1].AsIntegers[I] := ParameterX;
        Query.Params[2].AsIntegers[I] := ParameterY;
        Query.Params[3].LoadFromFile(FileName, TFieldType.ftBlob, I);
      end;

    end;

    Query.Execute(Query.Params.ArraySize, 0);
  finally
    Query.Free;
    Connection.Free;
    IncProgress;
  end;
end;

procedure TViewMain.Log(Value: string);
begin
  TThread.Synchronize(nil, procedure begin
    MemoLog.BeginUpdate;
    try
      if MemoLog.Lines.Count = 100 then
        MemoLog.Lines.Delete(MemoLog.Lines.Count-1);

      MemoLog.Lines.Insert(0, Now.Format('[yyyy.mm.dd hh:nn:ss:zzz] ')+Value);
      MemoLog.CaretPosition := TCaretPosition.Create(0, 0);
    finally
      MemoLog.EndUpdate;
    end;
  end);
end;

procedure TViewMain.ResetProgress(AMaxValue: single);
begin
  TThread.Synchronize(nil, procedure begin
    ProgressBar1.Value := 0;
    ProgressBar1.Max   := AMaxValue;
  end);
end;

end.

unit Loader.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.ListBox, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo,

  OSM.FMX.Utils,
  OSM.FMX.Utils.Loader;

type
  TVIewMain = class(TForm)
    EditPath: TEdit;
    EllipsesEditButton2: TEllipsesEditButton;
    Label3: TLabel;
    ButtonImport: TButton;
    ComboBoxZoom: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    MemoLog: TMemo;
    EditHost: TEdit;
    SaveDialog: TSaveDialog;
    ProgressBar2: TProgressBar;
    procedure EllipsesEditButton2Click(Sender: TObject);
    procedure ButtonImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FZoom: Cardinal;
    FMapPath: string;
    FURI: string;

    FLoader: TOSMLoader;
    procedure Import;
    procedure BreakImport;
    procedure EnableControls(Value: boolean);

    procedure DoProgressGenerate(Sender: TObject; const CurProgress, MaxProgress: uint64);
    procedure DoProgressLoad(Sender: TObject; const CurProgress, MaxProgress: uint64);
    procedure DoFinish(Sender: TObject);
  public
    procedure Log(Value: string);
  end;

var
  VIewMain: TVIewMain;

implementation
uses
  System.IOUtils,
  System.StrUtils,
  System.DateUtils,
  FMX.Text,
  Winapi.Windows
  ;

{$R *.fmx}

procedure TVIewMain.BreakImport;
begin
  FLoader.Stop;
  FreeAndNil(FLoader);
end;

procedure TVIewMain.ButtonImportClick(Sender: TObject);
begin
  if ButtonImport.Tag.ToBoolean then begin
    EnableControls(false);

    FMapPath  := ifthen(EditPath.Text.EndsWith('\'), EditPath.Text, EditPath.Text+'\');
    FURI      := EditHost.Text;
    FZoom     := ComboBoxZoom.ItemIndex;

    Import;
  end else begin
    BreakImport;
    EnableControls(true);
  end;
end;

procedure TVIewMain.DoFinish(Sender: TObject);
begin
  Log('Finish');
  Log('==============================');
  Log('All   : '+ FLoader.AllTiles.ToString);
  Log('Loaded: '+ FLoader.LoadedTiles.ToString);
  Log('Errors: '+ FLoader.ErrorTiles.ToString);
end;

procedure TVIewMain.DoProgressGenerate(Sender: TObject; const CurProgress,
  MaxProgress: uint64);
begin
  TThread.Queue(nil, procedure begin
    ProgressBar1.Max    := MaxProgress;
    ProgressBar1.Value  := CurProgress;
  end);
end;

procedure TVIewMain.DoProgressLoad(Sender: TObject; const CurProgress,
  MaxProgress: uint64);
begin
  TThread.Queue(nil, procedure begin
    ProgressBar2.Max    := MaxProgress;
    ProgressBar2.Value  := CurProgress;
  end);
end;

procedure TVIewMain.EllipsesEditButton2Click(Sender: TObject);
var
  MapDirectory: string;
begin
  if SelectDirectory('Select Map folder', TDirectory.GetCurrentDirectory, MapDirectory) then
    EditPath.Text := MapDirectory;
end;

procedure TVIewMain.EnableControls(Value: boolean);
begin
  EditPath.Enabled      := false;
  EditHost.Enabled      := false;
  ComboBoxZoom.Enabled  := Value;

  ButtonImport.Tag := Value.ToInteger;
  ButtonImport.Text := ifthen(Value, 'Import', 'BREAK');
end;

procedure TVIewMain.FormCreate(Sender: TObject);
begin
  EnableControls(true);
  FLoader := nil;
end;

procedure TVIewMain.FormDestroy(Sender: TObject);
begin
  FreeAndNIl(FLoader);
end;

procedure TVIewMain.Import;
var
  GeoRegionBel: TGeoRect;
begin
  GeoRegionBel := TGeoRect.Create(
    TGeoPoint.Create(23.17832, 56.17219),
    TGeoPoint.Create(32.76278, 51.26268)
    );


  Log('Start');
  EnableControls(False);

  FreeAndNil(FLoader);

  FLoader := TOSMLoader.Create;
  FLoader.OnProgressGenerateTiles := DoProgressGenerate;
  FLoader.OnProgressLoadTiles     := DoProgressLoad;
  FLoader.OnFinish                := DoFinish;
  FLoader.Start(GeoRegionBel, FZoom, FZoom);
end;

procedure TVIewMain.Log(Value: string);
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

end.

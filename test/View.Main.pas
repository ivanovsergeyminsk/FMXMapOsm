unit View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, 

  OSM.FMX.TileStorage,
  OSM.FMX.TileStorage.Files,
  OSM.FMX.TileStorage.SQL,
  OSM.FMX.TileStorage.Server,
  OSM.FMX.MapControl,
  OSM.FMX.Utils;

type
  TViewMain = class(TForm)
    ButtonZoomInc: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    ButtonZoomDec: TButton;
    CheckBoxAnimateZoom: TCheckBox;
    GroupBoxStorage: TGroupBox;
    RadioButtonTileNone: TRadioButton;
    RadioButtonFile: TRadioButton;
    RadioButtonSQLite: TRadioButton;
    GroupBoxLayer: TGroupBox;
    CheckBoxLayer1: TCheckBox;
    CheckBoxLayer2: TCheckBox;
    CheckBoxLayer3: TCheckBox;
    CheckBoxLayer4: TCheckBox;
    CheckBoxLayer5: TCheckBox;
    RadioButtonServer: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonZoomIncClick(Sender: TObject);
    procedure ButtonZoomDecClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBoxAnimateZoomChange(Sender: TObject);
    procedure RadioButtonTileNoneChange(Sender: TObject);
    procedure CheckBoxLayer1Change(Sender: TObject);
  private
    Map: TMapControl;
    StorageFile: ITileStorage;
    StorageSQLite: ITileStorage;
    StorageServer: ITileStorage;

//    procedure CreateConnectionDef(const ASQLiteFile: string);

    procedure DrawMapLayer(Sender: TObject; Canvas: TCanvas; const ARect: TRectF; const Layer: Cardinal);
  public
    { Public declarations }
  end;

var
  ViewMain: TViewMain;

const
  DEF_MAPSTORAGE_SQLITE = 'defMapStorageSQLite';
  DEF_MAPSTORAGE_SERVER = 'https://tile.openstreetmap.org/';

implementation

{$R *.fmx}

uses
  System.Math,
  FireDAC.Comp.Client,
  FireDAC.Phys.SQLite,
  Winapi.Windows
  ;

procedure TViewMain.ButtonZoomDecClick(Sender: TObject);
begin
  Map.Zoom := Map.Zoom - 1;
end;

procedure TViewMain.ButtonZoomIncClick(Sender: TObject);
begin
  Map.Zoom := Map.Zoom + 1;
end;

procedure TViewMain.CheckBoxAnimateZoomChange(Sender: TObject);
begin
  var Props := Map.Properties;
  if CheckBoxAnimateZoom.IsChecked
    then include(Props, TMapControlProperty.IsZoomAnimate)
    else exclude(Props, TMapControlProperty.IsZoomAnimate);

  Map.Properties := Props;
end;

procedure TViewMain.CheckBoxLayer1Change(Sender: TObject);
begin
  Map.LayerDisplay[0] := CheckBoxLayer1.IsChecked;
  Map.LayerDisplay[1] := CheckBoxLayer2.IsChecked;
  Map.LayerDisplay[2] := CheckBoxLayer3.IsChecked;
  Map.LayerDisplay[3] := CheckBoxLayer4.IsChecked;
  Map.LayerDisplay[4] := CheckBoxLayer5.IsChecked;
end;

//procedure TViewMain.CreateConnectionDef(const ASQLiteFile: string);
//var
//  Params: TStrings;
//begin
//  Params := TStringList.Create;
//  try
//    Params.Add('Pooled=True');
//    Params.Add(format('Database=%s', [ASQLiteFile]));
//  //  Params.Add('User_Name=***');
//  //  Params.Add('Password=*****');
//    Params.Add('LockingMode=Normal');
//    Params.Add('DriverID=SQLite');
//
//    FDManager.AddConnectionDef(DEF_MAPSTORAGE_SQLITE, 'SQLite', Params);
//  finally
//    FreeAndNil(Params);
//  end;
//end;

procedure TViewMain.DrawMapLayer(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF; const Layer: Cardinal);
begin
  var LRect := ARect;
  case Layer of
    1:  begin
          LRect.TopLeft := TPointF.Create(Layer*20, Layer*20);
          Canvas.Fill.Color := TAlphaColors.Red;
          Canvas.Stroke.Color := TAlphaColors.Green;
          Canvas.DrawRect(LRect, 1);
          Canvas.FillText(LRect, 'Слой 1', false, 1, [], TTextAlign.Leading, TTextAlign.Leading);
        end;
    2:  begin
          LRect.TopLeft := TPointF.Create(Layer*30, Layer*30);
          Canvas.Fill.Color := TAlphaColors.Gold;
          Canvas.DrawRect(LRect, 1);
          Canvas.FillText(LRect, 'Слой 2', false, 1, [], TTextAlign.Leading, TTextAlign.Leading);
        end;
    3:  begin
          LRect.TopLeft := TPointF.Create(Layer*40, Layer*40);
          Canvas.Fill.Color := TAlphaColors.Brown;
          Canvas.DrawRect(LRect, 1);
          Canvas.FillText(LRect, 'Слой 3', false, 1, [], TTextAlign.Leading, TTextAlign.Leading);
        end;
    4:  begin
          LRect.TopLeft := TPointF.Create(Layer*50, Layer*50);
          Canvas.Fill.Color := TAlphaColors.Coral;
          Canvas.DrawRect(LRect, 1);
          Canvas.FillText(LRect, 'Слой 4', false, 1, [], TTextAlign.Leading, TTextAlign.Leading);
        end;
    5:  begin
          LRect.TopLeft := TPointF.Create(Layer*60, Layer*60);
          Canvas.Fill.Color := TAlphaColors.Firebrick;
          Canvas.FillText(LRect, 'Слой 5', false, 1, [], TTextAlign.Leading, TTextAlign.Leading);
        end;
  end;
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  var GeoBEL := TGeoRect.Create(TGeoPoint.Create(23.17832, 56.17219),
                                TGeoPoint.Create(32.76278, 51.26268));


//  CreateConnectionDef('D:\Work\PROJECTS\Delphi\MapGis\Win32\Debug\bin\OSMTile_Belarus.db');
//  StorageFile   := TTileStorageSimpleFile.Create(TDirectory.GetCurrentDirectory);
//  StorageSQLite := TTileStorageSimpleSQL.Create(DEF_MAPSTORAGE_SQLITE);
  StorageServer := TTileStorageSimpleServer.Create(DEF_MAPSTORAGE_SERVER);

//  StorageFile.CacheInflate    := TSize.Create(5,5);
//  StorageSQLite.CacheInflate  := TSize.Create(5,5);
  StorageServer.CacheInflate  := TSize.Create(5,5);

  Map := TMapControl.Create(Self);
  Map.Name        := 'MapControl';
  Map.Align       := TAlignLayout.Client;
  Map.Parent      := Layout1;
//  Map.Properties  := [TMapControlProperty.IsDrawCopyright];
  Map.ConstrainedMapArea  := GeoBEL;
  Map.MinZoom     := 7;
  Map.MaxZoom     := 20;
  Map.LayerCount  := 5;
  Map.OnDrawLayer := DrawMapLayer;

  Map.SetZoom(8);
  Map.CenterGeoPoint := TGeoPoint.Create(27.55, 53.90);
end;

procedure TViewMain.FormDestroy(Sender: TObject);
begin
  Map.Free;
  StorageFile   := nil;
  StorageSQLite := nil;
  StorageServer := nil;
end;

procedure TViewMain.RadioButtonTileNoneChange(Sender: TObject);
begin
  var TileStorage: ITileStorage := nil;
  if RadioButtonTileNone.IsChecked then
    TileStorage := nil;
  if RadioButtonFile.IsChecked then
    TileStorage := StorageFile;
  if RadioButtonSQLite.IsChecked then
    TileStorage := StorageSQLite;
  if RadioButtonServer.IsChecked then
    TileStorage := StorageServer;


  Map.TileStorage := TileStorage;
end;

initialization
  ReportMemoryLeaksOnShutdown := true;

end.

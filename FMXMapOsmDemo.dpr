program FMXMapOsmDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  View.Main in 'test\View.Main.pas' {ViewMain},
  OSM.FMX.Utils in 'source\OSM.FMX.Utils.pas',
  OSM.FMX.MapControl in 'source\OSM.FMX.MapControl.pas',
  OSM.FMX.TileStorage in 'source\OSM.FMX.TileStorage.pas',
  OSM.FMX.TileStorage.Files in 'source\OSM.FMX.TileStorage.Files.pas',
  OSM.FMX.TileStorage.SQL in 'source\OSM.FMX.TileStorage.SQL.pas',
  OSM.FMX.TileStorage.Server in 'source\OSM.FMX.TileStorage.Server.pas',
  System.Messaging in 'System.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.

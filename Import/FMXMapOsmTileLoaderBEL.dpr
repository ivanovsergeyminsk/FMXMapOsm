program FMXMapOsmTileLoaderBEL;

uses
  System.StartUpCopy,
  FMX.Forms,
  Loader.View.Main in 'source\Loader.View.Main.pas' {VIewMain},
  OSM.FMX.Utils.Loader in '..\source\OSM.FMX.Utils.Loader.pas',
  OSM.FMX.Utils in '..\source\OSM.FMX.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TVIewMain, VIewMain);
  Application.Run;
end.

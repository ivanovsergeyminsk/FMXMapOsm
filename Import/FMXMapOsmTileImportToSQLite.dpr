program FMXMapOsmTileImportToSQLite;

uses
  System.StartUpCopy,
  FMX.Forms,
  Import.View.Main in 'source\Import.View.Main.pas' {ViewMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.

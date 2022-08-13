unit OSM.FMX.TileStorage.Files;

interface
uses
    FMX.Graphics
  , OSM.FMX.Utils
  , OSM.FMX.TileStorage
  ;

type
  ///Пример источника тайлов из файловой системе
  ///по отностительному пути path\map \Z\X\Y.png
  TTileStorageSimpleFile = class(TTileStorage)
  strict private
    FPath: string;
  protected
    function DoGetTile(Tile: TTile; var Bitmap: TBitmap): boolean; override;
  public
    constructor Create(APathMap: string);
  end;

implementation
uses
    System.IOUtils
  , System.SysUtils
  , System.Math
  ;

{$REGION 'TSimpleTileFileStorage'}

constructor TTileStorageSimpleFile.Create(APathMap: string);
begin
  inherited Create;
  FPath := APathMap;
end;

function TTileStorageSimpleFile.DoGetTile(Tile: TTile;
  var Bitmap: TBitmap): boolean;
var
  PathTile: string;
begin
  try
    PathTile := TPath.Combine(FPath, format('map\%d\%d\%d.png', [Tile.Zoom, Tile.ParameterX, Tile.ParameterY]));

    if not TFile.Exists(PathTile) then begin
      Bitmap := nil;
      exit(false);
    end;

    Bitmap := TBitmap.CreateFromFile(PathTile);
    result := true;
  except
    FreeAndNil(Bitmap);
    exit(False);
  end;
end;

{$ENDREGION}

end.

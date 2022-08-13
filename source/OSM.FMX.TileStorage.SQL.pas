unit OSM.FMX.TileStorage.SQL;

interface
uses
    FMX.Graphics
  , OSM.FMX.Utils
  , OSM.FMX.TileStorage
  , FireDAC.Comp.Client
  , FireDAC.Comp.UI
  , FireDAC.Stan.Def
  , FireDAC.Stan.Pool
  , FireDAC.Stan.Async
  , FireDAC.Stan.Intf
  , FireDAC.UI.Intf
  , FireDAC.FMXUI.Wait
  , FireDAC.FMXUI.Async
  , FireDAC.DApt
  , System.Classes
  , System.Generics.Collections
  ;

type
  TTileStorageSimpleSQL = class(TTileStorage)
  strict private const
    SQL_SELECT_TILE   = 'select distinct * from Tiles where Zoom = %s and X = %s and Y = %s';
    SQL_SELECT_TILES  = 'select * from Tiles where Zoom in (%s) and X in (%s) and Y in (%s)';
  strict private
    FConnectionName: string;
    FConnection: TFDConnection;

    function GetPreparedSQLTiles(const [ref] NeedTiles: TArray<TTile>): string;
    function LoadBitmapFormDataSet(ADataSet: TFDQuery; out ABitmap: TBitmap; out ATile: TTile): boolean;
  protected
    function DoGetTile(Tile: TTile; var Bitmap: TBitmap): boolean; override;
  public
    constructor Create(AConnectionName: string); overload;
    destructor Destroy; override;
  end;

implementation
uses
    System.SysUtils
  , System.Variants
  , Data.DB
  , FireDAC.Stan.Param
  ;

{ TTileStorageSimpleSQL }

constructor TTileStorageSimpleSQL.Create(AConnectionName: string);
begin
  Assert(FDManager.IsConnectionDef(AConnectionName), 'AConnectionName not found');
  FConnectionName := AConnectionName;
  FConnection     := TFDConnection.Create(nil);
  FConnection.ConnectionDefName := AConnectionName;
  FConnection.ResourceOptions.SilentMode := true;
  inherited Create;
end;

destructor TTileStorageSimpleSQL.Destroy;
begin
  FConnection.Free;
  inherited;
end;

function TTileStorageSimpleSQL.DoGetTile(Tile: TTile;
  var Bitmap: TBitmap): boolean;
begin
  try
    var ResultSet: TFDQuery := nil;
    try
      FConnection.ExecSQL(GetPreparedSQLTiles([Tile]), TDataSet(ResultSet));

      if ResultSet.IsEmpty then begin
        Bitmap := nil;
        exit(false);
      end;

      var Dummyile: TTile;
      result := LoadBitmapFormDataSet(ResultSet, Bitmap, Dummyile);
    finally
      FreeAndNil(ResultSet);
    end;
  except
    on E: Exception do begin
      FreeAndNil(Bitmap);
      exit(false);
    end;
  end;
end;

function TTileStorageSimpleSQL.GetPreparedSQLTiles(
  const [ref] NeedTiles: TArray<TTile>): string;
var
  ArrZoom, ArrX, ArrY: TArray<string>;
  sZoom, sX, sY: string;
begin
  result := string.Empty;
  var ArrSize := Length(NeedTiles);

  SetLength(ArrZoom,  ArrSize);
  SetLength(ArrX,     ArrSize);
  SetLength(ArrY,     ArrSize);

  for var I := 0 to Length(NeedTiles)-1 do begin
    if IsRelease then exit;
    ArrZoom[I] := Integer(NeedTiles[I].Zoom).ToString;
    ArrX[I]    := NeedTiles[I].ParameterX.ToString;
    ArrY[I]    := NeedTiles[I].ParameterY.ToString;
  end;

  sZoom := string.Join(',', ArrZoom);
  sX    := string.Join(',', ArrX);
  sY    := string.Join(',', ArrY);

  if ArrSize = 1
    then result := format(SQL_SELECT_TILE,  [sZoom, sX, sY])
    else result := format(SQL_SELECT_TILES, [sZoom, sX, sY]);
end;

function TTileStorageSimpleSQL.LoadBitmapFormDataSet(ADataSet: TFDQuery;
  out ABitmap: TBitmap; out ATile: TTile): boolean;
begin
  ABitmap := nil;

  if ADataSet.FieldByName('Value').IsNull then exit(false);

  ABitmap := TBitmap.Create;
  try
    var MemStream := ADataSet.CreateBlobStream(ADataSet.FieldByName('Value'), TBlobStreamMode.bmRead);
    try
      MemStream.Position := 0;
      ABitmap.LoadFromStream(MemStream);
    finally
      FreeAndNil(MemStream);
    end;
  except
    on E: Exception do begin
      FreeAndNil(ABitmap);
      exit(false);
    end;
  end;

  ATile := TTile.Create(ADataSet.FieldByName('Zoom').AsLongWord,
                        ADataSet.FieldByName('X').AsLongWord,
                        ADataSet.FieldByName('Y').AsLongWord);

  result := true;
end;

end.

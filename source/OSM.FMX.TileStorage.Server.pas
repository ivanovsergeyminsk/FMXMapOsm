unit OSM.FMX.TileStorage.Server;

interface
uses
    FMX.Graphics
  , OSM.FMX.Utils
  , OSM.FMX.TileStorage
  , System.Generics.Collections
  , System.Types
  , System.Net.HttpClient
  , System.Net.URLClient
  ;

type
  TTileStorageSimpleServer = class(TTileStorage)
  private const
    TileURLPatt = '%d/%d/%d.png';
  private
    FURLServer: string;
    FHttpClient: THTTPClient;

    function TileToFullMapFileURL(const Tile: TTile): string;
  protected
    function DoGetTile(Tile: TTile; var Bitmap: TBitmap): boolean; override;
  public
    constructor Create(const URLTileServer: string); overload;
    destructor Destroy; override;
  end;

implementation
uses
    System.SysUtils
  , System.Classes
  {$IFDEF MSWINDOWS}
  , Winapi.ActiveX
  {$ENDIF}
  ;

{ TTileStorageSimpleServer }

constructor TTileStorageSimpleServer.Create(const URLTileServer: string);
begin
  FURLServer  := URLTileServer;
  FHttpClient := THTTPClient.Create;
  inherited Create;
end;

destructor TTileStorageSimpleServer.Destroy;
begin
  inherited;
  FHttpClient.Free;
end;

function TTileStorageSimpleServer.DoGetTile(Tile: TTile;
  var Bitmap: TBitmap): boolean;
begin
  Bitmap := nil;
  var HttpClient := THttpClient.Create;
  var MemStream  := TMemoryStream.Create;
  try
    try

      var Response := HttpClient.Get(TileToFullMapFileURL(Tile), MemStream,
                      [ TNetHeader.Create('User-Agent', 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0'),
                        TNetHeader.Create('Accept', 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8'),
                        TNetHeader.Create('Accept-Encoding', 'gzip, deflate, br'),
                        TNetHeader.Create('Accept-Language', 'ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3'),
                        TNetHeader.Create('Cache-Control','max-age=0'),
                        TNetHeader.Create('Connection', 'keep-alive')
                      ]);

      if Response.StatusCode = 200 then begin
        Bitmap := TBitmap.Create;
        MemStream.Position := 0;

        //Анонимный метод не может захватит Bitmap, поэтому прибегаем к локальной переменной
        var LBitmap := Bitmap;
        {$IFDEF MSWINDOWS}
        CoInitialize(nil);
        try
          LBitmap.LoadFromStream(MemStream);
        finally
          CoUninitialize;
        end;
        {$ELSE}
        LBitmap.LoadFromStream(MemStream);
        {$ENDIF}

        result := true;
      end else begin
        Bitmap := nil;
        result := false;
      end;
    finally
      FreeAndNil(HttpClient);
      FreeAndNil(MemStream);
    end;
  except
    on E: Exception do begin
      FreeAndNil(Bitmap);
      exit(false);
    end;
  end;
end;

function TTileStorageSimpleServer.TileToFullMapFileURL(
  const Tile: TTile): string;
begin
  result := FURLServer+format(TileURLPatt, [Tile.Zoom, Tile.ParameterX, Tile.ParameterY])
end;

end.

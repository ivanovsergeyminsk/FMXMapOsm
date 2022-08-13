unit OSM.FMX.Utils.Loader;

interface
uses
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Threading,
  System.SyncObjs,
  OSM.FMX.Utils
  ;

type
  TNotifyProgress = procedure(Sender: TObject; const CurProgress, MaxProgress: uint64) of object;
  TOnError = procedure(Sender: TObject; Error: string) of object;
  TOSMLoader = class
  private const
    MAX_PACK = 250;
    POP_TIMEOUT_MS = 10000;
  private
    FOnProgressGenerateTiles: TNotifyProgress;
    FOnProgressLoadTiles: TNotifyProgress;
    FOnFinish: TNotifyEvent;
    FOnError: TOnError;
    procedure SetOnError(const Value: TOnError);
    procedure SetOnFinish(const Value: TNotifyEvent);
  private
    FTask1Event,
    FTask2Event: TEvent;
    FPackQueue: TThreadedQueue<TArray<TTile>>;

    FSourceURI: string;
    FMapPath: string;

    FLoadedCount: int64;
    FErrorCount:  int64;
    FAllCount:    int64;

//    function TileToFullMapFileURL(const Tile: TTile): string;

    procedure GenerateTilesForLoad(Region: TGeoRect; MinZoomLevel, MaxZoomLevel: TMapZoomLevel);
    procedure LoadPackTiles(APackTiles: TArray<TTile>);

    procedure TaskLoad;
    procedure DoTaskComplete;
  protected
    procedure DoProgressGenerateTiles(CurProgress, MaxProgress: UInt64);
    procedure DoProgressLoadTiles(CurProgress, MaxProgress: UInt64);
    procedure DoFinish;
    procedure DoError(Error: string);
  public
    constructor Create; overload;
    constructor Create(ASourceURI: string; AMapPath: string); overload;
    destructor Destroy; override;

    procedure Start(Region: TGeoRect; MinZoomLevel, MaxZoomLevel: TMapZoomLevel);
    procedure Stop;

    property LoadedTiles: int64 read FLoadedCount;
    property ErrorTiles:  int64 read FErrorCount;
    property AllTiles:    int64 read FAllCount;

    property OnProgressGenerateTIles: TNotifyProgress read FOnProgressGenerateTiles write FOnProgressGenerateTiles;
    property OnProgressLoadTiles: TNotifyProgress read FOnProgressLoadTiles write FOnProgressLoadTiles;
    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish;
    property OnError:  TOnError read FOnError write SetOnError;
  end;

implementation
uses
  System.Math,
  system.IOUtils,
  Winapi.Windows
  ;

{ TOSMLoader }

constructor TOSMLoader.Create;
begin
  Create(MapURLPrefix, TDirectory.GetCurrentDirectory);
end;

constructor TOSMLoader.Create(ASourceURI: string; AMapPath: string);
begin
  FSourceURI := ASourceURI;
  FMapPath   := AMapPath;

  FPackQueue := TThreadedQueue<TArray<TTile>>.Create(250, INFINITE, POP_TIMEOUT_MS);
  FTask1Event := TSimpleEvent.Create;
  FTask2Event := TSimpleEvent.Create;
end;

destructor TOSMLoader.Destroy;
begin
  Stop;
  FPackQueue.Free;
  FTask1Event.Free;
  FTask2Event.Free;
  inherited;
end;

procedure TOSMLoader.DoError(Error: string);
begin
  if assigned(FOnError) then
    FOnError(self, Error);
end;

procedure TOSMLoader.DoFinish;
begin
  if assigned(FOnFinish) then
    FOnFinish(self);
end;

procedure TOSMLoader.DoProgressGenerateTiles(CurProgress, MaxProgress: UInt64);
begin
  if assigned(FOnProgressGenerateTiles) then
    FOnProgressGenerateTiles(self, CurProgress, MaxProgress);
end;

procedure TOSMLoader.DoProgressLoadTiles(CurProgress, MaxProgress: UInt64);
begin
  if assigned(FOnProgressLoadTiles) then
    FOnProgressLoadTiles(self, CurProgress, MaxProgress);
end;

procedure TOSMLoader.DoTaskComplete;
begin
  var WR1 := FTask1Event.WaitFor(INFINITE);
  var WR2 := FTask2Event.WaitFor(INFINITE);

  if (WR1 = wrSignaled) and (WR2 = wrSignaled) then begin
    DoFinish;
  end;
end;

procedure TOSMLoader.GenerateTilesForLoad(Region: TGeoRect; MinZoomLevel,
  MaxZoomLevel: TMapZoomLevel);
var
  SavePath: string;
begin
  var CountInPack: Cardinal := 0;
  var TilePack: TArray<TTile> := [];
  for var ZoomLevel := MinZoomLevel to MaxZoomLevel do begin
    var MapRegionBel := EnsureInMap(ZoomLevel, GeoCoordsToMap(ZoomLevel, Region));

    var HorzStartNum  := Floor(MapRegionBel.Left   / TILE_IMAGE_WIDTH);
    var VertStartNum  := Floor(MapRegionBel.Top    / TILE_IMAGE_HEIGHT);

    var HorzEndNum    := Floor(MapRegionBel.Right  / TILE_IMAGE_WIDTH);
    var VertEndNum    := Floor(MapRegionBel.Bottom / TILE_IMAGE_HEIGHT);

    var CPro: integer := 0;
    var MPro: integer := (HorzEndNum - HorzStartNum)+(VertEndNum-VertStartNum);

    DoProgressGenerateTiles(CPro, MPro);

    for var Horz := HorzStartNum to HorzEndNum do
      for var Vert := VertStartNum to VertEndNum do begin
        inc(CPro);
        DoProgressGenerateTiles(CPro, MPro);
        SavePath := format('%s\map\%d\%d\%d.png', [TDirectory.GetCurrentDirectory, ZoomLevel, Horz, Vert]);
        if TFile.Exists(SavePath) then continue;

        TilePack := TilePack + [TTile.Create(ZoomLevel, Horz, Vert)];

        inc(CountInPack);
        TInterlocked.Increment(FAllCount);
        if CountInPack = MAX_PACK then begin
          FPackQueue.PushItem(TilePack);
          TilePack    := [];
          CountInPack := 0;
        end;
      end;
  end;

  if Length(TilePack) > 0 then
    FPackQueue.PushItem(TilePack);
end;

procedure TOSMLoader.LoadPackTiles(APackTiles: TArray<TTile>);
var
  Response: IHTTPResponse;
  SavePath: string;
begin
  var Client := THTTPClient.Create;
  try
    for var Tile in APackTiles do begin
      var MemStream := TMemoryStream.Create;
      try
        Response :=
          Client.Get(TileToFullSlippyMapFileURL(Tile), MemStream,
                    [ TNetHeader.Create('User-Agent', 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0'),
                      TNetHeader.Create('Accept', 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8'),
                      TNetHeader.Create('Accept-Encoding', 'gzip, deflate, br'),
                      TNetHeader.Create('Accept-Language', 'ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3'),
                      TNetHeader.Create('Cache-Control','max-age=0'),
                      TNetHeader.Create('Connection', 'keep-alive')
                    ]);

        if Response.StatusCode = 200 then begin
          SavePath := format('%s\map\%d\%d\', [TDirectory.GetCurrentDirectory, Tile.Zoom, Tile.ParameterX]);
          if not TDirectory.Exists(SavePath) then
            TDirectory.CreateDirectory(SavePath);

          MemStream.SaveToFile(format(SavePath+'%d.png', [Tile.ParameterY]));
          TInterlocked.Increment(FLoadedCount);
        end else begin
          TInterlocked.Increment(FErrorCount);
          DoError(format('Error request (%d): %s.'#13#10'Tile: [%d, %d, %d]'#13#10'%s',
            [Response.StatusCode, Response.StatusText,
             Tile.Zoom, Tile.ParameterX, Tile.ParameterY,
             Response.ContentAsString]));
        end;
      finally
        MemStream.Free;

        DoProgressLoadTiles(TInterlocked.Read(FLoadedCount)+TInterlocked.Read(FErrorCount),  TInterlocked.Read(FAllCount));
      end;
    end;
  finally
    Response := nil;
    Client.Free;
  end;
end;

procedure TOSMLoader.Start(Region: TGeoRect; MinZoomLevel,
  MaxZoomLevel: TMapZoomLevel);
begin
  FLoadedCount := 0;
  FErrorCount  := 0;
  FAllCount    := 0;

  TTask.Run(procedure begin
    GenerateTilesForLoad(Region, MinZoomLevel, MaxZoomLevel);
  end);


  TTask.Run(
    procedure begin
      FTask1Event.ResetEvent;
      try
        TaskLoad
      finally
        FTask1Event.SetEvent;
      end;
    end);

  TTask.Run(
    procedure begin
      FTask2Event.ResetEvent;
      try
        TaskLoad
      finally
        FTask2Event.SetEvent;
      end;
    end);


  TTask.Run(DoTaskComplete);
end;

procedure TOSMLoader.Stop;
begin
  FPackQueue.DoShutDown;
end;

procedure TOSMLoader.TaskLoad;
var
  PackTiles: TArray<TTile>;
begin
  try
    while FPackQueue.PopItem(PackTiles) = wrSignaled do begin
      if FPackQueue.ShutDown then
        break;

      LoadPackTiles(PackTiles);
      sleep(500);
    end;
  except
    on E: Exception do
      DoError(E.ClassName+': '+E.Message);
  end;
end;

//function TOSMLoader.TileToFullMapFileURL(const Tile: TTile): string;
//begin
//  Result :=
//    FSourceURI+
//    Format(TileURLPatt, [Tile.Zoom, Tile.ParameterX, Tile.ParameterY]) +
//    MapURLPostfix;
//end;

procedure TOSMLoader.SetOnError(const Value: TOnError);
begin
  FOnError := Value;
end;

procedure TOSMLoader.SetOnFinish(const Value: TNotifyEvent);
begin
  FOnFinish := Value;
end;

end.

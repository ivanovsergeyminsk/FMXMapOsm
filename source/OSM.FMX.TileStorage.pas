unit OSM.FMX.TileStorage;

interface
uses
    System.SysUtils
  , System.Classes
  , System.Types
  , System.Generics.Collections
  , System.Generics.Defaults
  , System.Threading
  , System.SyncObjs
  , FMX.Graphics
  , OSM.FMX.Utils
  ;

type
  ETileStorageException = class(Exception);
  ETileStorageMapNotFoundException = class(ETileStorageException);
  ETileStorageZoomNotFoundException = class(ETileStorageException);

  TOnCacheEvent = procedure(const Tile: TTile; const Bitmap: TBitmap) of object;

  ITileStorage = interface
  ['{0D15AEEF-E4CA-4A5E-AFDB-D51DD8B553D5}']
    function GetCacheInflate: TSize;
    procedure SetCacheInflate(const Value: TSize);

    function GetOnCacheEvent: TOnCacheEvent;
    procedure SetOnCacheEvent(const Value: TOnCacheEvent);

    function GetParallelCount: Byte;
    procedure SetParallelCount(const Value: Byte);

    ///Поставить в очередь задачу на кэширование тайлов для отображаемой части карты
    procedure UpdateCache(AZoom: TMapZoomLevel; ViewMapRect: TRectF);
    //Завешена ли задача на обновление кэша
    function IsUpdatedCache: boolean;
    ///Расширить кэш относительно отображаемой части карты (значения в кол-ве тайлов)
    property CacheInflate: TSize read GetCacheInflate write SetCacheInflate;
    ///Количество палаллельных задач для подгрузки кэша
    property ParallelCount: Byte read GetParallelCount write SetParallelCount;

    ///Событие о завершнении попытки подгрузки тайла в кэш
    property OnCacheEvent: TOnCacheEvent read GetOnCacheEvent write SetOnCacheEvent;

    ///Получить тайл
    function GetTile(Tile: TTile; var Bitmap: TBitmap): boolean;
  end;

  TTileStorage = class abstract(TInterfacedObject, ITileStorage)
  strict private const
    DefaultParallelCount = 4;
  strict private type
    TRequestUpdateCache = record
      Zoom: TMapZoomLevel;
      ViewMapRect: TRectF;
      CacheInflate: TSize;
      constructor New(AZoom: TMapZoomLevel; AViewMapRect: TRectF; ACacheInflate: TSize);
    end;

  strict private
    FCacheComparer: IEqualityComparer<TTile>;
    FCache: TObjectDictionary<TTile, TBitmap>;
    FCacheInflate: TSize;
    FOnCacheEvent: TOnCacheEvent;

    FTaskCache: ITask;
    FCacheMREW: TLightweightMREW;
    FParallelCount: Byte;
    FIsUpdating: boolean;

    FReleaseEvent: TEvent;

    FQueueRequestUpdateCache: TThreadedQueue<TRequestUpdateCache>;
  strict private

    procedure DoCachingTile(const Tile: TTile);
    procedure DoCachingTilesSingleThreaded(const [ref] ANeedTiles: TArray<TTile>);
    procedure DoCachingTilesMultiThreaded(const [ref] ANeedTiles: TArray<TTile>);

    procedure DoCachingTiles(var ANeedTiles: TArray<TTile>);
    procedure DoUpdateNeedTiles(var ANeedTiles, ACurrentTiles: TArray<TTile>);
    procedure DoRemoveTiles(const [ref] ATiles: TArray<TTile>; var ACurrentTiles: TArray<TTile>);

    procedure GetRemoveTiles(var ACurrentTiles, ANeedTiles, AResult: TArray<TTile>);
    procedure GetNeedTiles(AReq: TRequestUpdateCache; var AResult: TArray<TTile>);
    procedure GetCurrentTiles(var AResult: TArray<TTile>);
    function GetComparisonTile: IComparer<TTile>;
    function GetComparisonTileDraw: IComparer<TTile>;

    procedure TaskUpdateCache;

    function GetCacheInflate: TSize;
    procedure SetCacheInflate(const Value: TSize);

    function GetOnCacheEvent: TOnCacheEvent;
    procedure SetOnCacheEvent(const Value: TOnCacheEvent);

    function GetParallelCount: Byte;
    procedure SetParallelCount(const Value: Byte);
  protected
    function  IsRelease: boolean;
    ///Получить тайл из базы, файла, сайта и т.п.
    ///Если не удалось: Bitmap := nil, Result := false
    function DoGetTile(Tile: TTile; var Bitmap: TBitmap): boolean; virtual; abstract;
    procedure DoOnCacheEvent(const Tile: TTile; const Bitmap: TBitmap);
  public
    constructor Create; overload;
    destructor Destroy; override;

    function GetTile(Tile: TTile; var Bitmap: TBitmap): boolean;

    procedure UpdateCache(AZoom: TMapZoomLevel; ViewMapRect: TRectF);
    function IsUpdatedCache: boolean;

    property CacheInflate: TSize read GetCacheInflate write SetCacheInflate;
    property ParallelCount: Byte read GetParallelCount write SetParallelCount;

    property OnCacheEvent: TOnCacheEvent read GetOnCacheEvent write SetOnCacheEvent;
  end;


  ///Для отладки
  TTileSotrageMock = class sealed(TInterfacedObject, ITileStorage)
  protected
    function GetCacheInflate: TSize;
    procedure SetCacheInflate(const Value: TSize);

    function GetOnCacheEvent: TOnCacheEvent;
    procedure SetOnCacheEvent(const Value: TOnCacheEvent);

    function GetParallelCount: Byte;
    procedure SetParallelCount(const Value: Byte);

    ///Поставить в очередь задачу на кэширование тайлов для отображаемой части карты
    procedure UpdateCache(AZoom: TMapZoomLevel; ViewMapRect: TRectF);
    //Завешена ли задача на обновление кэша
    function IsUpdatedCache: boolean;
    ///Расширить кэш относительно отображаемой части карты (значения в кол-ве тайлов)
    property CacheInflate: TSize read GetCacheInflate write SetCacheInflate;
    ///Количество палаллельных задач для подгрузки кэша
    property ParallelCount: Byte read GetParallelCount write SetParallelCount;

    ///Событие о завершнении попытки подгрузки тайла в кэш
    property OnCacheEvent: TOnCacheEvent read GetOnCacheEvent write SetOnCacheEvent;

    ///Получить тайл
    function GetTile(Tile: TTile; var Bitmap: TBitmap): boolean;
  end;

  function GetCacheEqualityComparer: IEqualityComparer<TTile>;

implementation
uses
    System.Math
  , System.Hash
  , System.Diagnostics
  , System.RTLConsts
  ;

{ TTileStorage }

constructor TTileStorage.Create;
begin
  FCacheComparer := GetCacheEqualityComparer;

  FCache := TObjectDictionary<TTile, TBitmap>.Create([doOwnsValues], FCacheComparer);
  FQueueRequestUpdateCache := TThreadedQueue<TRequestUpdateCache>.Create(1000, 1, INFINITE);

  FParallelCount  := DefaultParallelCount;
  FCacheInflate   := TSize.Create(1, 1);
  FReleaseEvent   := TSimpleEvent.Create;
  FIsUpdating     := false;

  FTaskCache      := TTask.Run(TaskUpdateCache);
end;

destructor TTileStorage.Destroy;
begin
  FReleaseEvent.SetEvent;
  FQueueRequestUpdateCache.DoShutDown;

  TTask.WaitForAll([FTaskCache]);

  FQueueRequestUpdateCache.Free;
  FCache.Free;
  FReleaseEvent.Free;
  inherited;
end;

procedure TTileStorage.DoCachingTile(const Tile: TTile);
begin
  var Bitmap: TBitmap := nil;
  if DoGetTile(Tile, Bitmap) then begin
    FCacheMREW.BeginWrite;
    try
      FCache.AddOrSetValue(Tile, Bitmap);
    finally
      FCacheMREW.EndWrite;
    end;
  end;

  DoOnCacheEvent(Tile, Bitmap);
end;

procedure TTileStorage.DoCachingTiles(var ANeedTiles: TArray<TTile>);
begin
  if Length(ANeedTiles) = 0 then exit;

  var Comparer := GetComparisonTileDraw;
  TArray.Sort<TTile>(ANeedTiles, Comparer);

  if FParallelCount = 1
    then DoCachingTilesSingleThreaded(ANeedTiles)
    else DoCachingTilesMultiThreaded(ANeedTiles);
end;

procedure TTileStorage.DoCachingTilesMultiThreaded(
  const [ref] ANeedTiles: TArray<TTile>);
begin
  var LNeedTiles := ANeedTiles;

  var Stride  := Ceil(Length(LNeedTiles) / FParallelCount);
  var MinIdx  := 0;
  var MaxIdx  := Length(LNeedTiles) - 1;

  TParallel.For(Stride, MinIdx, MaxIdx,
    procedure (Idx: Int64; LoopState: TParallel.TLoopState)
    begin
      if (IsRelease) or (FQueueRequestUpdateCache.QueueSize > 0) then begin
        LoopState.Break;
        exit;
      end;

      DoCachingTile(LNeedTiles[Idx]);
    end);
end;

procedure TTileStorage.DoCachingTilesSingleThreaded(
  const [ref] ANeedTiles: TArray<TTile>);
begin
  for var Tile in ANeedTiles do begin
    if IsRelease then break;
    if FQueueRequestUpdateCache.QueueSize > 0 then break;

    DoCachingTile(Tile);
  end;
end;

procedure TTileStorage.DoOnCacheEvent(const Tile: TTile; const Bitmap: TBitmap);
begin
  if assigned(FOnCacheEvent) then
    FOnCacheEvent(Tile, Bitmap);
end;

procedure TTileStorage.DoRemoveTiles(const [ref] ATiles: TArray<TTile>; var ACurrentTiles: TArray<TTile>);
begin
  for var Tile in ATiles do begin
    if IsRelease then break;

    FCacheMREW.BeginWrite;
    try
      FCache.Remove(Tile);
    finally
      FCacheMREW.EndWrite;
    end;
  end;

  FCacheMREW.BeginRead;
  try
    ACurrentTiles := FCache.Keys.ToArray;
  finally
    FCacheMREW.EndRead;
  end;
end;

procedure TTileStorage.DoUpdateNeedTiles(var ANeedTiles,
  ACurrentTiles: TArray<TTile>);
begin
  var Comparer := GetComparisonTile;

  TArray.Sort<TTile>(ACurrentTiles, Comparer);
  TArray.Sort<TTile>(ANeedTiles, Comparer);

  var Dummy: integer;
  var LResult: TArray<TTile>;
  for var Tile in ANeedTiles do begin
    if IsRelease then break;
    if not TArray.BinarySearch<TTile>(ACurrentTiles, Tile, Dummy, Comparer) then
      LResult := LResult+[Tile];
  end;

  ANeedTiles := LResult;
end;


function TTileStorage.GetCacheInflate: TSize;
begin
  result := FCacheInflate;
end;

function TTileStorage.GetComparisonTile: IComparer<TTile>;
begin
  result := TComparer<TTile>.Construct(
    function(const Left, Right: TTile): Integer
    begin
      result := CompareValue(Integer(Left.Zoom), Integer(Right.Zoom));
      if result <> EqualsValue then exit;
      result := CompareValue(Integer(Left.ParameterX), Integer(Right.ParameterX));
      if result <> EqualsValue then exit;
      result := CompareValue(Integer(Left.ParameterY), Integer(Right.ParameterY));

    end);
end;

function TTileStorage.GetComparisonTileDraw: IComparer<TTile>;
begin
  //Сортировка тайлов по горизонтали
  result := TComparer<TTile>.Construct(
    function(const Left, Right: TTile): Integer
    begin
      result := CompareValue(Integer(Left.Zoom), Integer(Right.Zoom));
      if result <> EqualsValue then exit;
      result := CompareValue(Integer(Left.ParameterY), Integer(Right.ParameterY));
      if result <> EqualsValue then exit;
      result := CompareValue(Integer(Left.ParameterX), Integer(Right.ParameterX));
    end);
end;

procedure TTileStorage.GetCurrentTiles(var AResult: TArray<TTile>);
begin
  FCacheMREW.BeginRead;
  try
    AResult := FCache.Keys.ToArray;
  finally
    FCacheMREW.EndRead;
  end;
end;

procedure TTileStorage.GetNeedTiles(AReq: TRequestUpdateCache; var AResult: TArray<TTile>);
begin
  var CacheRect := AReq.ViewMapRect;
  CacheRect.Width   := ToTileWidthGreater(CacheRect.Width);
  CacheRect.Height  := ToTileHeightGreater(CacheRect.Height);

  CacheRect := EnsureInMap(AReq.Zoom, CacheRect);
  var HorzStartNum  := Max(0, Floor(CacheRect.Left / TILE_IMAGE_WIDTH)    - AReq.CacheInflate.cx);
  var VertStartNum  := Max(0, Floor(CacheRect.Top  / TILE_IMAGE_HEIGHT)   - AReq.CacheInflate.cy);

  var HorzEndNum    := Max(0, Floor(CacheRect.Right  / TILE_IMAGE_WIDTH)  + AReq.CacheInflate.cx);
  var VertEndNum    := Max(0, Floor(CacheRect.Bottom / TILE_IMAGE_HEIGHT) + AReq.CacheInflate.cy);

  AResult := [];
  for var Horz := HorzStartNum to HorzEndNum do begin
    if IsRelease then break;
    for var Vert := VertStartNum to VertEndNum do begin
      if IsRelease then break;
      if TileValid(TTile.Create(AReq.Zoom, Horz, Vert)) then
        AResult := AResult + [TTile.Create(AReq.Zoom, Horz, Vert)];
    end;
  end;
end;

function TTileStorage.GetOnCacheEvent: TOnCacheEvent;
begin
  result := FOnCacheEvent;
end;

function TTileStorage.GetParallelCount: Byte;
begin
  result := FParallelCount;
end;

procedure TTileStorage.GetRemoveTiles(var ACurrentTiles, ANeedTiles, AResult: TArray<TTile>);
begin
  var Comparer := GetComparisonTile;

  TArray.Sort<TTile>(ACurrentTiles, Comparer);
  TArray.Sort<TTile>(ANeedTiles, Comparer);

  AResult := [];
  var Dummy: integer;
  for var Tile in ACurrentTiles do begin
    if IsRelease then break;
    if not TArray.BinarySearch<TTile>(ANeedTiles, Tile, Dummy, Comparer) then
      AResult := AResult+[Tile];
  end;
end;

function TTileStorage.GetTile(Tile: TTile; var Bitmap: TBitmap): boolean;
begin
  FCacheMREW.BeginRead;
  try
    if FCache.TryGetValue(Tile, Bitmap)
      then result := true
      else result := false;
  finally
    FCacheMREW.EndRead;
  end;
end;

function TTileStorage.IsRelease: boolean;
begin
  result := FReleaseEvent.WaitFor(0) = wrSignaled;
end;

function TTileStorage.IsUpdatedCache: boolean;
begin
  result := not FIsUpdating;
end;

procedure TTileStorage.SetCacheInflate(const Value: TSize);
begin
  FCacheInflate := Value;
end;

procedure TTileStorage.SetOnCacheEvent(const Value: TOnCacheEvent);
begin
  FOnCacheEvent := Value;
end;

procedure TTileStorage.SetParallelCount(const Value: Byte);
begin
  FParallelCount := max(1, Value);
end;

procedure TTileStorage.TaskUpdateCache;
begin
  repeat
    var Req: TRequestUpdateCache;
    while FQueueRequestUpdateCache.PopItem(Req) = wrSignaled do begin
      if FReleaseEvent.WaitFor(0) = wrSignaled then Break;
      FIsUpdating := true;
      try
        var CurrentTiles, NeedTiles, RemoveTiles: TArray<TTile>;

        GetCurrentTiles(CurrentTiles);
        GetNeedTiles(Req, NeedTiles);
        GetRemoveTiles(CurrentTiles, NeedTiles, RemoveTiles);

        DoRemoveTiles(RemoveTiles, CurrentTiles);
        DoUpdateNeedTiles(NeedTiles, CurrentTiles);
        DoCachingTiles(NeedTiles);
      finally
        FIsUpdating := false;
      end;
    end;

  until FReleaseEvent.WaitFor(1) = wrSignaled;
end;

procedure TTileStorage.UpdateCache(AZoom: TMapZoomLevel; ViewMapRect: TRectF);
begin
  FQueueRequestUpdateCache.PushItem(TRequestUpdateCache.New(AZoom, ViewMapRect, FCacheInflate));
end;

{ TTileStorage.TRequestUpdateCache }

constructor TTileStorage.TRequestUpdateCache.New(AZoom: TMapZoomLevel;
  AViewMapRect: TRectF; ACacheInflate: TSize);
begin
  Zoom          := AZoom;
  ViewMapRect   := AViewMapRect;
  CacheInflate  := ACacheInflate;
end;

function GetCacheEqualityComparer: IEqualityComparer<TTile>;
begin
  var LResult: IEqualityComparer<TTile> :=
  TEqualityComparer<TTile>.Construct(
    function(const Left, Right: TTile): Boolean
    begin
      result := Left = Right;
    end,
    function(const Value: TTile): Integer
    begin
      var Hasher := THashBobJenkins.Create;
      Hasher.Update(Value.Zoom, SizeOf(Value.Zoom));
      Hasher.Update(Value.ParameterX, SizeOf(Value.ParameterX));
      Hasher.Update(Value.ParameterY, SizeOf(Value.ParameterX));

      Result := hasher.HashAsInteger;
    end
  );

  result := LResult;
end;


{$REGION ' TTileSotrageMock '}

function TTileSotrageMock.GetCacheInflate: TSize;
begin
  result := TSize.Create(0,0);
end;

function TTileSotrageMock.GetOnCacheEvent: TOnCacheEvent;
begin
  result := nil;
end;

function TTileSotrageMock.GetParallelCount: Byte;
begin
  result := 0;
end;

function TTileSotrageMock.GetTile(Tile: TTile; var Bitmap: TBitmap): boolean;
begin
  Bitmap := nil;
  result := false;
end;

function TTileSotrageMock.IsUpdatedCache: boolean;
begin
  result := true;
end;

procedure TTileSotrageMock.SetCacheInflate(const Value: TSize);
begin
// nothing
end;

procedure TTileSotrageMock.SetOnCacheEvent(const Value: TOnCacheEvent);
begin
// nothing
end;

procedure TTileSotrageMock.SetParallelCount(const Value: Byte);
begin
// nothing
end;

procedure TTileSotrageMock.UpdateCache(AZoom: TMapZoomLevel;
  ViewMapRect: TRectF);
begin
// nothing
end;

{$ENDREGION}

end.

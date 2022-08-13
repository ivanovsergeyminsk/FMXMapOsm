unit OSM.FMX.MapControl;

interface
uses
    System.Types
  , System.Classes
  , System.UITypes
  , System.Generics.Collections
  , System.Threading
  , System.SyncObjs
  , FMX.Objects
  , FMX.Controls
  , FMX.Graphics
  , FMX.TextLayout
  , FMX.Types
  , FMX.Forms
  , OSM.FMX.Utils
  , OSM.FMX.TileStorage
  ;

type
  TMapControl     = class;
  TMapZoomAnimate = class;

  TMapControlProperty = (IsZoomAnimate, IsZoomCache, IsDrawScalebar, IsDrawBackgroundTextLoading, IsDrawCopyright);
  TMapControlProperties = set of TMapControlProperty;

  TMapControlState = (ZoomAnimate);
  TMapControlStates = set of TMapControlState;

  TOnDrawTile = procedure (Sender: TMapControl; TileHorzNum, TileVertNum: Cardinal;
    const TopLeft: TPointF; Canvas: TCanvas; var Handled: Boolean) of object;

  TOnDrawLayerEvent = procedure(Sender: TObject; Canvas: TCanvas; const ARect: TRectF; const Layer: Cardinal) of object;

  TMapControl = class (TControl)
  private const
    DefaultBackgroudColor       = TAlphaColors.Gray; //$FFCCCAC4;//$FFF2EFE9;
    DefaultBackgroundTextColor  = TAlphaColors.White;
    DefaultLinesColor           = TAlphaColors.White;
    DefaultZoom                 = 9;
    DefaultTileMargins          = 2;
    LayerMapIdx                 = 0;
    CS_LOADING                  = 'Loading [%d | %d : %d]...';
    CS_NO_STORAGE_SPECIFIED     = 'No storage specified';
    CS_COPYRIGHT                = '© OpenStreetMap';
  private
    ///Паямоугольник карты в пикселях для текущего значения Zoom.
    FMapRect: TRectF;
    ///Масшатибирование карты
    FZoom: integer;
    FMinZoom: TMapZoomLevel;
    FMaxZoom: TMapZoomLevel;

    FConstrainedMapArea: TGeoRect;
    ///Минимальный и максимальный тайлы для текущего уровня масштабирования
    FMinTile: TTile;
    FMaxTile: TTile;

    ///Центр карты
    FMapCenter: TPointF;
    ///Смещение контрола отностиельно FMapRect.
    FLocalOffset: TPointF;

    ///Источник тайлов
    FTileStorage: ITileStorage;
  private
    FControlState: TMapControlStates;
    FCacheImage: TBitmap;
    ///Положение буфферизированного изображения на карте в координатах карты
    FCacheImageRect: TRect;

    FBackgroundColor: TAlphaColor;
    FBackgroundTextColor: TAlphaColor;
    FLinesColor: TAlphaColor;
    FPressedMousePos: TPointF;

    FLayerCount: Cardinal;
    FLayerDisplay: TArray<boolean>;

    FProperties: TMapControlProperties;

    FZoomAnimate: TMapZoomAnimate;

    FLoadingText: string;

    //Events
    FOnChangedZoom: TNotifyEvent;
    FOnDrawTileLoading: TOnDrawTile;
    FOnDrawTile: TOnDrawTile;
    FOnDrawLayer: TOnDrawLayerEvent;
  protected
    //Overrides
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Resize; override;

    //Main methods
    function CalcTileRectInCacheImage(X, Y: Cardinal): TRectF;
    procedure CalcCacheImageTileSize(var StartTile, EndTile: TSize);

    procedure DrawTile(Tile: TTile; const TileRect: TRectF; Bitmap: TBitmap; ACanvas: TCanvas);
    procedure DrawTileLoading(Tile: TTile; const TileRect: TRectF; ACanvas: TCanvas);
    procedure DrawStorageNoSpecified(ACanvas: TCanvas);
    procedure DrawCacheImageTiles(StartTile, EndTile: TSize; ACanvas: TCanvas);
    procedure DrawCacheImage;

    procedure DrawTiles(ACanvas: TCanvas);
    procedure DrawLayers(ACanvas: TCanvas);
    procedure DrawZoomAnimate(ACanvas: TCanvas);
    procedure DrawCopyright(ACanvas: TCanvas);
    procedure DrawScalebar(ACanvas: TCanvas);

    procedure DoAnimateStartEvent(Sender: TObject);
    procedure DoAnimateEvent(Sender: TObject);
    procedure DoAnimateStopEvent(Sender: TObject);

    procedure DoCacheEvent(const Tile: TTile; const Bitmap: TBitmap);

    procedure SetLocalOffset(ALocalOffset: TPointF);
    function ToInnerCoords(const StartPt: TPointF; const Rect: TRectF): TRectF;
    procedure GetCacheImageDCRects(const CacheImageLocation: TPointF; var ViewRect, DCRect: TRectF);

    function ViewInCache: Boolean; inline;
    function SetCacheDimensions: Boolean;
    procedure CalcCacheCoords;
    procedure CalcConstranedMapArea;
    procedure UpdateCache;

    //Call Events
    procedure DoOnChangedZoom;
    procedure DoOnDrawTileLoading(TileHorzNum, TileVertNum: Cardinal; const TopLeft: TPointF; Canvas: TCanvas; var Handled: Boolean);
    procedure DoOnDrawTile(TileHorzNum, TileVertNum: Cardinal; const TopLeft: TPointF; Canvas: TCanvas; var Handled: Boolean);
    procedure DoOnDrawLayer(Canvas: TCanvas; const ARect: TRectF; const Layer: Cardinal);

    ///Перевести Tile в координаты карты
    function TileToMap(AZoom: Integer; ATile: TTile): TRectF;
    ///Перевести координаты карты в Tiles
    function MapToTiles(AZoom: Integer; AMap: TRectF): TArray<TTile>;
    ///Видимые тайлы которые должны отображаться при текущем состоянии контрола.
    function GetViewTiles: TArray<TTile>;

    //Getters/Setters
    function GetCenterGeoPoint: TGeoPoint;
    function GetZoom: Integer;
    function GetMapSize: TSizeF;
    function GetLayerCount: Cardinal;
    function GetLayerDisplay(Idx: Cardinal): boolean;

    procedure SetCenterGeoPoint(const Value: TGeoPoint);
    procedure SetCenterMapPoint(const Value: TPointF);
    procedure SetTileStorage(const Value: ITileStorage);
    procedure SetConstrainedMapArea(const Value: TGeoRect);
    procedure SetMaxZoom(const Value: TMapZoomLevel);
    procedure SetMinZoom(const Value: TMapZoomLevel);
    procedure SetLayerCount(const Value: Cardinal);
    procedure SetLayerDisplay(Idx: Cardinal; const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    ///Перевести локальные координаты в координаты карты
    function LocalToMap(ALocal: TRectF): TRectF; overload;
    ///Перевести локальные координаты в координаты карты
    function LocalToMap(ALocal: TPointF): TPointF; overload;

    ///Перевести координаты карты в локальные координаты
    function MapToLocal(AMap: TRectF): TRectF; overload;
    ///Перевести координаты карты в локальные координаты
    function MapToLocal(AMap: TPointf): TPointF; overload;

    ///Перевести локальные координаты в геокоординаты
    function LocalToGeo(const ALocal: TRectF): TGeoRect; overload;
    ///Перевести локальные координаты в геокоординаты
    function LocalToGeo(const ALocal: TPointF): TGeoPoint; overload;

    ///Перевести геокоординаты в локальные координаты
    function GeoToLocal(const AGeo: TGeoRect): TRectF; overload;
    ///Перевести геокоординаты в локальные координаты
    function GeoToLocal(const AGeo: TGeoPoint): TPointF; overload;

    ///Масштабировать
    procedure SetZoom(Level: Integer); overload;
    procedure SetZoom(Level: Integer; const BindPoint: TPointF); overload;
    procedure SetZoom(Level: Integer; const BindPoint: TGeoPoint); overload;
    //Масштабировать до отображения области GeoRect
    procedure ZoomToArea(const GeoRect: TGeoRect);
    //Масштабировать, максимально заполнив область отображения
    procedure ZoomToFit;

    property Properties: TMapControlProperties read FProperties write FProperties;
    ///Размер карты для текущего уровня масштабирования
    property MapSize: TSizeF read GetMapSize;
    ///Уровень масштабирования
    property Zoom: Integer read GetZoom write SetZoom;

    ///Минимально допустимый уровень масштабирования
    property MinZoom: TMapZoomLevel read FMinZoom write SetMinZoom;
    ///Максимально допустимый уровень масштабирования
    property MaxZoom: TMapZoomLevel read FMaxZoom write SetMaxZoom;
    ///Область карты, которая доступна для просмотра
    property ConstrainedMapArea: TGeoRect read FConstrainedMapArea write SetConstrainedMapArea;

    ///Количество слоев
    property LayerCount: Cardinal read GetLayerCount write SetLayerCount;
    ///Видимость слоя
    property LayerDisplay[Idx: Cardinal]: boolean read GetLayerDisplay write SetLayerDisplay;

    ///Центрировать по кординатам карты
    property CenterMapPoint: TPointF    read FMapCenter write SetCenterMapPoint;
    ///Центрировать по геокоординатам
    property CenterGeoPoint: TGeoPoint  read GetCenterGeoPoint write SetCenterGeoPoint;

    ///Текст загрузки, который отображается когда тайл в процессе загрузки
    property LoadingText: string read FLoadingText write FLoadingText;
    ///Цвет фона
    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor;
    property BackgroundTextColor: TAlphaColor read FBackgroundTextColor write FBackgroundTextColor;
    ///Цвет линий сетки
    property LinesColor: TAlphaColor read FLinesColor write FLinesColor;

    ///Источник тайлов
    property TileStorage: ITileStorage read FTileStorage write SetTileStorage;

    property OnChangedZoom: TNotifyEvent read FOnChangedZoom write FOnChangedZoom;
    property OnDrawTileLoading: TOnDrawTile read FOnDrawTileLoading write FOnDrawTileLoading;
    property OnDrawTile: TOnDrawTile read FOnDrawTile write FOnDrawTile;
    property OnDrawLayer: TOnDrawLayerEvent read FOnDrawLayer write FOnDrawLayer;
  end;

  TMapZoomAnimate = class
  strict private const
    DefaultTimerInterval        = 10;
    DefaultZoomAnimateFactor    = 0.05;
    MinZoomAnimateFactor        = 0.45;
  strict private
    FOwner: TMapControl;

    FZoomTimer: TTimer;
    FCacheFrameTiles: TDictionary<TTile, TBitmap>;

    FSourceFrame: TBitmap;
    FAnimateFrame: TBitmap;
    FSourceFrameRect: TRect;
    FLocalRect: TRectF;
    FScaleFactor: single;
    FBindPoint: TPointF;
    FOffset: TPointF;
    FZoomPrev: integer;
    FZoomNext: integer;
    FOffsetNext: TPointF;

    FOnStart: TNotifyEvent;
    FOnAnimate: TNotifyEvent;
    FOnStop: TNotifyEvent;
  strict private
    procedure DoZoomAnimate(Sender: TObject);
    procedure GetAnimateDCRects(const ScaleFactor: single; var ViewRect, DCClipViewRect: TRectF);
    procedure GenerateAnimateFrame(const ViewRect, DCClipViewRect: TRectF);
    procedure GenerateCacheFrameTiles;

    function ToInnerCoords(const StartPt: TPointF; const Rect: TRectF): TRectF;

    procedure DoOnStart;
    procedure DoOnAnimate;
    procedure DoOnStop;
  public
    constructor Create(AOwner: TMapControl);
    destructor Destroy; override;

    procedure PrepareAnimate(const BindPoint: TPointF);
    procedure RunAnimate;
    procedure PrepareCacheFrameTiles;

    property CacheFrame: TDictionary<TTile, TBitmap> read FCacheFrameTiles;
    function TryGetAnimateFrame(var Bitmap: TBitmap): boolean;

    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnAnimate: TNotifyEvent read FOnAnimate write FOnAnimate;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation
uses
    System.Math
  , System.Math.Vectors
  , System.SysUtils
  , System.IOUtils
  ;

function CalcTextSize(Text: string; Font: TFont; FontSize: single = 0): TSizeF;
var
  TextLayout: TTextLayout;
begin
  TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    TextLayout.BeginUpdate;
    try
      TextLayout.text := text;
      TextLayout.MaxSize := TPointF.Create(9999, 9999);
      TextLayout.Font.Assign(Font);
      if not SameValue(0, FontSize) then
      begin
        TextLayout.Font.Size := FontSize;
      end;
      TextLayout.WordWrap := true;
      TextLayout.Trimming := TTextTrimming.None;
      TextLayout.HorizontalAlign := TTextAlign.Leading;
      TextLayout.VerticalAlign := TTextAlign.Leading;
    finally
      TextLayout.EndUpdate;
    end;

    Result.Width := TextLayout.Width;
    Result.Height := TextLayout.Height;
  finally
    TextLayout.DisposeOf;
  end;
end;

{$REGION 'TMapControl'}

function TMapControl.GetViewTiles: TArray<TTile>;
begin
  result := MapToTiles(FZoom, LocalToMap(LocalRect));
end;

// Calc new cache coords to cover current view area
procedure TMapControl.CalcCacheCoords;
var
  ViewRect: TRectF;
  MarginH, MarginV: Cardinal;
begin
  ViewRect := ToTileBoundary(LocalToMap(LocalRect));

  // reposition new cache rect to cover tile-aligned view area
  // calc margins
  MarginH := FCacheImageRect.Width - ViewRect.Round.Width;
  MarginV := FCacheImageRect.Height - ViewRect.Round.Height;
  // margins on the both sides
  if MarginH > TILE_IMAGE_WIDTH then
    MarginH := MarginH div 2;
  if MarginV > TILE_IMAGE_HEIGHT then
    MarginV := MarginV div 2;

  FCacheImageRect.SetLocation(ViewRect.Round.TopLeft);
  FCacheImageRect.TopLeft.Subtract(Point(MarginH, MarginV));
end;

function TMapControl.CalcTileRectInCacheImage(X, Y: Cardinal): TRectF;
begin
  var TileOffset := TPointF.Create(TILE_IMAGE_WIDTH  * X, TILE_IMAGE_HEIGHT * Y);
  Result := TRectF.Create(0.0, 0.0, TILE_IMAGE_WIDTH, TILE_IMAGE_HEIGHT);
  Result.Offset(TileOffset);
end;

function TMapControl.GeoToLocal(const AGeo: TGeoRect): TRectF;
begin
  result.TopLeft      := GeoToLocal(AGeo.TopLeft);
  result.BottomRight  := GeoToLocal(AGeo.BottomRight)
end;

constructor TMapControl.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundColor      := DefaultBackgroudColor;
  FBackgroundTextColor  := DefaultBackgroundTextColor;
  FLinesColor           := DefaultLinesColor;

  FTileStorage := nil;

  FZoom           := DefaultZoom;
  FMinZoom        := Low(TMapZoomLevel);
  FMaxZoom        := High(TMapZoomLevel);

  FMinTile            := TTile.Create(0,0,0);
  FMaxTile            := TTile.Create(0,0,0);
  FConstrainedMapArea := TGeoRect.Create(MapToGeoCoords(FZoom, TPointF.Zero), MapToGeoCoords(FZoom, TPointF.Create(MapWidth(FZoom), MapHeight(FZoom))));
  FLayerCount         := 1;
  FLayerDisplay       := [true];

  FMapRect        := TRectF.Create(TPoint.Zero, TSizeF.Create(MapWidth(FZoom), MapHeight(FZoom)));
  FMapCenter      := FMapRect.CenterPoint;
  FLocalOffset    := TPointF.Zero;

  FCacheImage     := TBitmap.Create;
  FProperties     := [TMapControlProperty.IsZoomAnimate, TMapControlProperty.IsDrawCopyright, TMapControlProperty.IsDrawScalebar];

  FLoadingText    := CS_LOADING;

  FZoomAnimate    := TMapZoomAnimate.Create(self);
  FZoomAnimate.OnStart    := DoAnimateStartEvent;
  FZoomAnimate.OnAnimate  := DoAnimateEvent;
  FZoomAnimate.OnStop     := DoAnimateStopEvent;
end;

destructor TMapControl.Destroy;
begin
  FTileStorage := nil;
  FCacheImage.Free;
  FZoomAnimate.Free;
  inherited;
end;

procedure TMapControl.DrawZoomAnimate(ACanvas: TCanvas);
begin
  var Bitmap: TBitmap := nil;
  if FZoomAnimate.TryGetAnimateFrame(Bitmap) then
    Canvas.DrawBitmap(Bitmap, LocalRect, LocalRect, 1);
end;

procedure TMapControl.DrawCacheImage;
begin
  var StartTile, EndTile: TSize;
  CalcCacheImageTileSize(StartTile, EndTile);

  DrawCacheImageTiles(StartTile, EndTile, FCacheImage.Canvas);
end;

procedure TMapControl.DrawCacheImageTiles(StartTile, EndTile: TSize;
  ACanvas: TCanvas);
begin
  if ACanvas.BeginScene then
    try
      ACanvas.Clear(FBackgroundColor);

      for var Horz := StartTile.cx to EndTile.cx do
        for var Vert := StartTile.cy to EndTile.cy do begin
          if not TileValid(TTile.Create(FZoom, Horz, Vert)) then continue;

          var TileRect := CalcTileRectInCacheImage(Horz - StartTile.cx, Vert - StartTile.cy);
          var Tile     := TTile.Create(FZoom, Horz, Vert);

          //Экземпляр TileBitmap принадлежит TileStorage. Освобождать не нужно!
          var TileBitmap: TBitmap := nil;

          if FTileStorage.GetTile(Tile, TileBitmap)
            then begin
              DrawTile(Tile, TileRect, TileBitmap, ACanvas);
            end else begin
              if FTileStorage.IsUpdatedCache then
                FZoomAnimate.CacheFrame.Clear;

              if FZoomAnimate.CacheFrame.TryGetValue(Tile, TileBitmap)
                then DrawTile(Tile, TileRect, TileBitmap, ACanvas)
                else DrawTileLoading(Tile, TileRect, ACanvas);
            end;
        end;
    finally
      ACanvas.EndScene;
    end;
end;

procedure TMapControl.DrawCopyright(ACanvas: TCanvas);
begin
  var TextSize := CalcTextSize(CS_COPYRIGHT, ACanvas.Font);
  var RectCopyright := TRectF.Create(TPointF.Zero, TextSize.Width, TextSize.Height);
  RectCopyright.Inflate(5,5);

  var CopyrightLocaltion := LocalRect.BottomRight;
  CopyrightLocaltion.X  := LocalRect.Right  - (RectCopyright.Width  + 5);
  CopyrightLocaltion.Y  := LocalRect.Bottom - (RectCopyright.Height + 5);
  RectCopyright.Location := CopyrightLocaltion;

  ACanvas.Fill.Color := TAlphaColors.White;
  ACanvas.FillRect(RectCopyright, 0.5);
  ACanvas.Fill.Color := TAlphaColors.Black;
  ACanvas.FillText(RectCopyright, CS_COPYRIGHT, false, 1, [], TTextAlign.Center, TTextAlign.Center);
end;

procedure TMapControl.DrawScalebar(ACanvas: TCanvas);
begin
  var ScalebarWidthInPixel: Cardinal;
  var ScalebarWidthInMeter: Cardinal;
  var ScalebarText: string;
  GetScaleBarParams(FZoom, ScalebarWidthInPixel, ScalebarWidthInMeter, ScalebarText);

  var TextSize := CalcTextSize(ScalebarText, ACanvas.Font);
  var RectScalebar := TRectF.Create(TPointF.Zero, ScalebarWidthInPixel, TextSize.Height + 5);

  var ScalebarLocaltion := TPointF.Create(LocalRect.Left + 5, LocalRect.Bottom - (RectScalebar.Height + 5));
  RectScalebar.Location := ScalebarLocaltion;  

  ACanvas.Fill.Color := TAlphaColors.White;
  ACanvas.FillRect(RectScalebar, 0.5);
  
  ACanvas.Stroke.Color      := TAlphaColors.Black;
  ACanvas.Stroke.Thickness  := 1;
  ACanvas.Stroke.Kind       := TBrushKind.Solid;
  ACanvas.DrawLine(RectScalebar.TopLeft, TPointF.Create(RectScalebar.Left, RectScalebar.Bottom), 1);
  ACanvas.DrawLine(TPointF.Create(RectScalebar.Left, RectScalebar.Bottom), RectScalebar.BottomRight, 1);
  ACanvas.DrawLine(RectScalebar.BottomRight, TPointF.Create(RectScalebar.Right, RectScalebar.Top), 1);
    
  RectScalebar.Inflate(-3,-3);
  ACanvas.Fill.Color := TAlphaColors.Black;
  ACanvas.FillText(RectScalebar, ScalebarText, false, 1, [], TTextAlign.Leading, TTextAlign.Center);  
end;

procedure TMapControl.DrawStorageNoSpecified(ACanvas: TCanvas);
begin
  ACanvas.Fill.Color := FBackgroundTextColor;
  ACanvas.FillText(LocalRect, CS_NO_STORAGE_SPECIFIED, false, 1, [], TTextAlign.Center, TTextAlign.Center);
end;

procedure TMapControl.DrawTile(Tile: TTile; const TileRect: TRectF; Bitmap: TBitmap;
  ACanvas: TCanvas);
begin
  var BitmapRect := Bitmap.BoundsF;
  ACanvas.DrawBitmap(Bitmap, BitmapRect, TileRect, 1, false);

  var Handled := false;
  DoOnDrawTile(Tile.ParameterX, Tile.ParameterY, TileRect.TopLeft, ACanvas, Handled);
end;

procedure TMapControl.DoAnimateEvent(Sender: TObject);
begin
  InvalidateRect(LocalRect);
end;

procedure TMapControl.DoAnimateStartEvent(Sender: TObject);
begin
  Include(FControlState, ZoomAnimate);
end;

procedure TMapControl.DoAnimateStopEvent(Sender: TObject);
begin
  Exclude(FControlState, ZoomAnimate);
  invalidateRect(LocalRect);
end;

procedure TMapControl.DoCacheEvent(const Tile: TTile; const Bitmap: TBitmap);
begin
  var LState := ComponentState; // Snag a local copy
  if csDestroying in LState then exit;

  BeginInvoke(procedure begin
    InvalidateRect(LocalRect);
  end);
end;

procedure TMapControl.DoOnChangedZoom;
begin
  if assigned(FOnChangedZoom) then
    FOnChangedZoom(self);
end;

procedure TMapControl.DoOnDrawLayer(Canvas: TCanvas;
  const ARect: TRectF; const Layer: Cardinal);
begin
  if assigned(FOnDrawLayer) then
    FOnDrawLayer(self, Canvas, ARect, Layer);
end;

procedure TMapControl.DoOnDrawTile(TileHorzNum, TileVertNum: Cardinal;
  const TopLeft: TPointF; Canvas: TCanvas; var Handled: Boolean);
begin
  if assigned(FOnDrawTile) then
    FOnDrawTile(self, TileHorzNum, TileVertNum, TopLeft, Canvas, Handled);
end;

procedure TMapControl.DoOnDrawTileLoading(TileHorzNum, TileVertNum: Cardinal;
  const TopLeft: TPointF; Canvas: TCanvas; var Handled: Boolean);
begin
  if assigned(FOnDrawTileLoading) then
    FOnDrawTileLoading(self, TileHorzNum, TileVertNum, TopLeft, Canvas, Handled);
end;

procedure TMapControl.DrawTileLoading(Tile: TTile; const TileRect: TRectF; ACanvas: TCanvas);
  procedure DrawBorder(ATileRect: TRectF);
  begin
    ACanvas.Stroke.Color      := FLinesColor;
    ACanvas.Stroke.Thickness  := 0.5;

    var LTileRect := ATileRect;
    LTileRect.Size := TSizeF.Create(LTileRect.Size.cx / 2, LTileRect.Size.cy / 2);
    ACanvas.DrawRect(LTileRect, 0.5);

    LTileRect.SetLocation(LTileRect.TopLeft + TPointF.Create(LTileRect.Size.cx, LTileRect.Size.cy));
    ACanvas.DrawRect(LTileRect, 0.5);

    ACanvas.DrawRect(ATileRect, 1);
  end;

  procedure DrawTileText(ATileRect: TRectF);
  var
    Text: string;
  begin
    if not (IsDrawBackgroundTextLoading in FProperties) then exit;
    if FLoadingText.IsEmpty then
      Text := Format(CS_LOADING, [Tile.Zoom, Tile.ParameterX, Tile.ParameterY])
    else
      Text := FLoadingText;

    ACanvas.Fill.Color := FBackgroundTextColor;
    ACanvas.FillText(ATileRect, Text, false, 1, [], TTextAlign.Center, TTextAlign.Center);
  end;

begin
  if (not InRange(Tile.ParameterX, FMinTile.ParameterX, FMaxTile.ParameterX)) or
     (not InRange(Tile.ParameterY, FMinTile.ParameterY, FMaxTile.ParameterY)) then
    exit;

  var Handled := false;
  DoOnDrawTileLoading(Tile.ParameterX, Tile.ParameterY, TileRect.TopLeft, ACanvas, Handled);
  if Handled then exit;
  

  DrawBorder(TileRect);
  DrawTileText(TileRect);
end;

procedure TMapControl.DrawTiles(ACanvas: TCanvas);
begin
begin
  var RectNormalized := LocalToMap(LocalRect); //Переходим в координаты карты

  //Расширяем до тайлов
  RectNormalized.TopLeft.X := ToTileWidthLesser(RectNormalized.TopLeft.X);
  RectNormalized.TopLeft.Y := ToTileHeightLesser(RectNormalized.TopLeft.Y);
  RectNormalized.BottomRight.X := ToTileWidthGreater(RectNormalized.BottomRight.X);
  RectNormalized.BottomRight.Y := ToTileHeightGreater(RectNormalized.BottomRight.Y);

  //Вычисляем стартовый и конечный тайл

  var StartX := trunc(RectNormalized.TopLeft.X / TILE_IMAGE_WIDTH);
  var StartY := trunc(RectNormalized.TopLeft.Y / TILE_IMAGE_HEIGHT);
  var EndX   := trunc(RectNormalized.BottomRight.X / TILE_IMAGE_WIDTH);
  var EndY   := trunc(RectNormalized.BottomRight.Y / TILE_IMAGE_WIDTH);

  RectNormalized := MapToLocal(RectNormalized); //Переходим в координаты контрола


  var iX := StartX;
  var pX := RectNormalized.TopLeft.X;
  while iX <= EndX do begin
    var iY := StartY;
    var pY := RectNormalized.TopLeft.Y;
    while iY <= EndY do begin
      var TileRect  := TRectF.Create(TPointF.Create(pX, pY), TILE_IMAGE_WIDTH, TILE_IMAGE_HEIGHT);
      var Tile      := TTile.Create(FZoom, iX, iY);

      //Экземпляр TileBitmap принадлежит TileStorage. Освобождать не нужно!
      var TileBitmap: TBitmap := nil;

      if FTileStorage.GetTile(Tile, TileBitmap)
        then begin
          DrawTile(Tile, TileRect, TileBitmap, ACanvas);
        end else begin
          if FTileStorage.IsUpdatedCache then
            FZoomAnimate.CacheFrame.Clear;

          if FZoomAnimate.CacheFrame.TryGetValue(Tile, TileBitmap)
            then DrawTile(Tile, TileRect, TileBitmap, ACanvas)
            else DrawTileLoading(Tile, TileRect, ACanvas);
        end;


      inc(iY);
      pY := pY + TILE_IMAGE_HEIGHT;
    end;
    inc(iX);
    pX := pX + TILE_IMAGE_WIDTH;
  end;

end;
end;

procedure TMapControl.DrawLayers(ACanvas: TCanvas);

begin
  for var Layer := 0 to Length(FLayerDisplay)-1 do begin
    if (Layer = LayerMapIdx) and FLayerDisplay[LayerMapIdx] then begin
      DrawCacheImage;

      var ViewRect, DCClipViewRect: TRectF;
      GetCacheImageDCRects(FCacheImageRect.TopLeft, ViewRect, DCClipViewRect);
      ACanvas.DrawBitmap(FCacheImage, ViewRect, DCClipViewRect, 1);
//      DrawTiles(ACanvas);
      continue;
    end;

    if FLayerDisplay[Layer] then
      DoOnDrawLayer(ACanvas, LocalRect, Layer);
  end;
end;

procedure TMapControl.GetCacheImageDCRects(const CacheImageLocation: TPointF; var ViewRect, DCRect: TRectF);
begin
  // ViewRect is current view area in CacheImage coords
  ViewRect := ToInnerCoords(CacheImageLocation, LocalToMap(LocalRect));
  // View rect in DC coords
  DCRect := TRectF.Create(0, 0, ViewRect.Width, ViewRect.Height);
end;

procedure TMapControl.CalcCacheImageTileSize(var StartTile, EndTile: TSize);
begin
    StartTile   := TSize.Create(Max(0, FCacheImageRect.Left div TILE_IMAGE_WIDTH),
                                Max(0, FCacheImageRect.Top  div TILE_IMAGE_HEIGHT)
                                );

    EndTile     := TSize.Create(Max(0, FCacheImageRect.Right  div TILE_IMAGE_WIDTH),
                                Max(0, FCacheImageRect.Bottom div TILE_IMAGE_HEIGHT)
                               );
end;

procedure TMapControl.CalcConstranedMapArea;
begin
  var ConstrMap := TRectF.Empty;
  ConstrMap.TopLeft      := GeoCoordsToMap(FZoom, FConstrainedMapArea.TopLeft);
  ConstrMap.BottomRight  := GeoCoordsToMap(fZoom, FConstrainedMapArea.BottomRight);

  ConstrMap := EnsureInMap(FZoom, ConstrMap);
  var HorzStartNum  := Max(0, Floor(ConstrMap.Left   / TILE_IMAGE_WIDTH));
  var VertStartNum  := Max(0, Floor(ConstrMap.Top    / TILE_IMAGE_HEIGHT));

  var HorzEndNum    := Max(0, Floor(ConstrMap.Right  / TILE_IMAGE_WIDTH));
  var VertEndNum    := Max(0, Floor(ConstrMap.Bottom / TILE_IMAGE_HEIGHT));

  FMinTile  := TTile.Create(FZoom, HorzStartNum, VertStartNum);
  FMaxTile  := TTile.Create(FZoom, HorzEndNum, VertEndNum);
end;

function TMapControl.GetCenterGeoPoint: TGeoPoint;
begin
  result := MapToGeoCoords(FZoom, FMapCenter);
end;

function TMapControl.GetLayerCount: Cardinal;
begin
  result := FLayerCount - 1;
end;

function TMapControl.GetLayerDisplay(Idx: Cardinal): boolean;
begin
  if Idx > FLayerCount-1 then exit(false);
  result := FLayerDisplay[Idx];
end;

function TMapControl.GetMapSize: TSizeF;
begin
  result := FMapRect.Size;
end;

function TMapControl.GeoToLocal(const AGeo: TGeoPoint): TPointF;
begin
  result := MapToLocal(GeoCoordsToMap(FZoom, AGeo));
end;

function TMapControl.GetZoom: Integer;
begin
  result := FZoom;
end;

function TMapControl.LocalToMap(ALocal: TRectF): TRectF;
begin
  result := ALocal;
  result.Offset(FLocalOffset);
end;

function TMapControl.LocalToGeo(const ALocal: TRectF): TGeoRect;
begin
  result.TopLeft      := LocalToGeo(ALocal.TopLeft);
  result.BottomRight  := LocalToGeo(Alocal.BottomRight);
end;

function TMapControl.LocalToGeo(const ALocal: TPointF): TGeoPoint;
begin
  result := MapToGeoCoords(FZoom, LocalToMap(ALocal));
end;

function TMapControl.LocalToMap(ALocal: TPointF): TPointF;
begin
  result := ALocal;
  result.Offset(FLocalOffset);
end;

function TMapControl.MapToLocal(AMap: TRectF): TRectF;
begin
  result := AMap;
  result.Offset(-FLocalOffset);
end;

function TMapControl.MapToLocal(AMap: TPointf): TPointF;
begin
  result := AMap;
  result.Offset(-FLocalOffset);
end;

function TMapControl.MapToTiles(AZoom: Integer; AMap: TRectF): TArray<TTile>;
begin
  AMap := EnsureInMap(AZoom, AMap);
  var HorzStartNum  := Max(0, Floor(AMap.Left   / TILE_IMAGE_WIDTH));
  var VertStartNum  := Max(0, Floor(AMap.Top    / TILE_IMAGE_HEIGHT));

  var HorzEndNum    := Max(0, Floor(AMap.Right  / TILE_IMAGE_WIDTH));
  var VertEndNum    := Max(0, Floor(AMap.Bottom / TILE_IMAGE_HEIGHT));

  result := [];
  for var Horz := HorzStartNum to HorzEndNum do
    for var Vert := VertStartNum to VertEndNum do begin
      if TileValid(TTile.Create(AZoom, Horz, Vert)) then
        result := result + [TTile.Create(AZoom, Horz, Vert)];
    end;
end;

procedure TMapControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then begin
    Capture;
    FPressedMousePos :=  LocalToMap(TPointF.Create(X, Y));
  end;
end;

procedure TMapControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Pressed and (not PressedPosition.IsZero) then begin
    var MapPresed   := FPressedMousePos;
    var MapCurrent  := LocalToMap(TPointF.Create(X, Y));
    var Delta       := MapPresed-MapCurrent;

    SetLocalOffset(FLocalOffset + Delta);
    UpdateCache;
    InvalidateRect(LocalRect);
  end;
end;

procedure TMapControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then begin
    Pressed := false;
    FPressedMousePos := TPointF.Zero;
    ReleaseCapture;
  end;
end;

procedure TMapControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;
  var MousePos := ScreenToLocal(Screen.MousePos);

  var NewZoom: integer := FZoom + Sign(WheelDelta);
  if not InRange(NewZoom, Low(TMapZoomLevel), High(TMapZoomLevel)) then exit;

  if not (ZoomAnimate in FControlState) then
    SetZoom(NewZoom, LocalToMap(MousePos));

  Handled := True;
end;

procedure TMapControl.Paint;
begin
  Canvas.ClearRect(LocalRect, FBackgroundColor);

  CalcCacheCoords;
  if assigned(FTileStorage) then begin
    if (TMapControlProperty.IsZoomAnimate in FProperties) and (ZoomAnimate in FControlState)
      then DrawZoomAnimate(Canvas)
      else DrawLayers(Canvas);

  end else begin
    DrawStorageNoSpecified(Canvas);
  end;

  if TMapControlProperty.IsDrawCopyright in FProperties then
    DrawCopyright(Canvas);

  if TMapControlProperty.IsDrawScalebar in FProperties then
    DrawScalebar(Canvas);
end;

procedure TMapControl.Resize;
begin
  SetCacheDimensions;
  UpdateCache;
  inherited;
end;

procedure TMapControl.SetZoom(Level: Integer);
begin
  SetZoom(Level, LocalToMap(LocalRect.CenterPoint));
end;

procedure TMapControl.SetZoom(Level: Integer; const BindPoint: TPointF);
begin
  if not (Level in [FMinZoom..FMaxZoom]) then Exit;
  if Level = FZoom then Exit;

  var BindPt := BindPoint;
  if not InMap(FZoom, BindPoint)
    then BindPt := TPointF.Create(0,0);

  // save bind point if zoom is valid (zoom value is used to calc geo coords)
  var SavedBindCoords      := MapToGeoCoords(FZoom, BindPt);
  // save bind point in view coords, we'll reposition to it after zoom
  var SavedLocalBindPoint  := MapToLocal(BindPt);

  if (TMapControlProperty.IsZoomAnimate in FProperties) or
     (TMapControlProperty.IsZoomCache in FProperties)
  then begin
    FZoomAnimate.PrepareAnimate(MapToLocal(BindPt));
  end;

  FZoom := Level;
  FMapRect := TRectF.Create(TPointF.Zero, TSizeF.Create(MapWidth(FZoom), MapHeight(FZoom)));
  CalcConstranedMapArea;

  // move viewport
  var CurrBindPoint := GeoCoordsToMap(FZoom, SavedBindCoords); // bind point in new map coords
  var NewViewNW := CurrBindPoint - SavedLocalBindPoint; // view's top-left corner in new map coords
  SetLocalOffset(NewViewNW);

  SetCacheDimensions;
  UpdateCache;
  if TMapControlProperty.IsZoomAnimate in FProperties then begin
    include(FControlState, ZoomAnimate);
    FZoomAnimate.RunAnimate;
  end;

  if TMapControlProperty.IsZoomCache in FProperties then
    FZoomAnimate.PrepareCacheFrameTiles;

  InvalidateRect(LocalRect);

  DoOnChangedZoom;
end;

function TMapControl.SetCacheDimensions: Boolean;
var
  CtrlSize, CacheSize: TSizeF;
begin
  // dims of view area in pixels rounded to full tiles
  CtrlSize.cx := ToTileWidthGreater(LocalRect.Width);
  CtrlSize.cy := ToTileHeightGreater(LocalRect.Height);

  // cache dims = Max(control+margins, Min(map, default+margins))
  CacheSize.cx := Min(FMapRect.Size.cx, DefaultTileMargins*TILE_IMAGE_WIDTH);
  CacheSize.cy := Min(FMapRect.Size.cy, DefaultTileMargins*TILE_IMAGE_HEIGHT);

  // Cast to signed to get rid of warning
  CacheSize.cx := Max(CacheSize.cx, CtrlSize.cx + DefaultTileMargins*TILE_IMAGE_WIDTH);
  CacheSize.cy := Max(CacheSize.cy, CtrlSize.cy + DefaultTileMargins*TILE_IMAGE_HEIGHT);

  Result := (FCacheImageRect.Width <> CacheSize.cx) or (FCacheImageRect.Height <> CacheSize.cy);
  if not Result then Exit;
  FCacheImageRect.Size := CacheSize.Round;
  FCacheImage.SetSize(CacheSize.Round.cx, CacheSize.Round.cy);
end;

procedure TMapControl.SetCenterGeoPoint(const Value: TGeoPoint);
begin
  SetCenterMapPoint(GeoCoordsToMap(FZoom, Value));
end;

procedure TMapControl.SetCenterMapPoint(const Value: TPointF);
begin
  FMapCenter := Value;

  var FCenteredRect  := TRectF.Create(TPointF.Zero, FMapCenter*2);
  var OffsetToCenter := FCenteredRect.CenterPoint - LocalToMap(LocalRect.CenterPoint);

  SetLocalOffset(FLocalOffset + OffsetToCenter);
end;

procedure TMapControl.SetConstrainedMapArea(const Value: TGeoRect);
begin
  FConstrainedMapArea := Value;
  CalcConstranedMapArea;
end;

procedure TMapControl.SetLayerCount(const Value: Cardinal);
begin
  if FLayerCount = (Value+1) then exit;
  FLayerCount := Max(1, Value+1);
  SetLength(FLayerDisplay, FLayerCount);
  InvalidateRect(LocalRect);
end;

procedure TMapControl.SetLayerDisplay(Idx: Cardinal; const Value: boolean);
begin
  if Idx > FLayerCount-1 then exit;
  if FLayerDisplay[Idx] = Value then exit;

  FLayerDisplay[Idx] := Value;
  InvalidateRect(LocalRect);
end;

procedure TMapControl.SetLocalOffset(ALocalOffset: TPointF);
begin
  var MinPoint := TileToMap(FZoom, FMinTile).TopLeft;
  var MaxPoint := TileToMap(FZoom, FMaxTile).BottomRight;

  FLocalOffset.X := Max(MinPoint.X, (Min(ALocalOffset.X, MaxPoint.X-LocalRect.Width)));
  FLocalOffset.Y := Max(MinPoint.Y, (Min(ALocalOffset.Y, MaxPoint.Y-LocalRect.Height)));
end;

procedure TMapControl.SetMaxZoom(const Value: TMapZoomLevel);
begin
  FMaxZoom := Value;

  if FZoom > FMaxZoom then
    SetZoom(FMaxZoom);
end;

procedure TMapControl.SetMinZoom(const Value: TMapZoomLevel);
begin
  FMinZoom := Value;

  if FZoom < FMinZoom then
    SetZoom(FMinZoom);
end;

procedure TMapControl.SetTileStorage(const Value: ITileStorage);
begin
  FTileStorage := Value;
  if assigned(FTileStorage) then
    FTileStorage.OnCacheEvent := DoCacheEvent;

  SetZoom(FZoom);
  UpdateCache;
  InvalidateRect(LocalRect);
end;

procedure TMapControl.SetZoom(Level: Integer; const BindPoint: TGeoPoint);
begin
  SetZoom(Level, GeoCoordsToMap(Level, BindPoint));
end;

function TMapControl.TileToMap(AZoom: Integer; ATile: TTile): TRectF;
begin
  Result := TRectF.Empty;

  Result.SetLocation(ATile.ParameterX * TILE_IMAGE_WIDTH, ATile.ParameterY * TILE_IMAGE_HEIGHT);
  Result.Size := TSizeF.Create(TILE_IMAGE_WIDTH, TILE_IMAGE_HEIGHT);

  Result := EnsureInMap(AZoom, Result);
end;

function TMapControl.ToInnerCoords(const StartPt: TPointF;
  const Rect: TRectF): TRectF;
begin
  Result := TRectF.Create(Rect.TopLeft - StartPt, Rect.BottomRight - StartPt);
end;

procedure TMapControl.UpdateCache;
begin
  if assigned(FTileStorage) then
    FTileStorage.UpdateCache(FZoom, LocalToMap(LocalRect));
end;

function TMapControl.ViewInCache: Boolean;
begin
  Result := FCacheImageRect.Contains(LocalToMap(LocalRect).Round);
end;

procedure TMapControl.ZoomToArea(const GeoRect: TGeoRect);

  function CalculateZoom(AGeoRect: TGeoRect): TMapZoomLevel;
  begin
    var ViewRect := LocalToMap(LocalRect);

    var NewZoomH := FMaxZoom;
    for var zoom := FZoom to FMaxZoom do
      if GeoCoordsToMap(zoom, AGeoRect).Width > ViewRect.Width then begin
        NewZoomH := zoom;
        Break;
      end;

    var NewZoomV := FMaxZoom;
    for var zoom := FZoom to FMaxZoom do
      if GeoCoordsToMap(zoom, AGeoRect).Height > ViewRect.Height then begin
        NewZoomV := zoom;
        Break;
      end;

     result := Min(NewZoomH, NewZoomV);
  end;

begin
  var NewZoom := CalculateZoom(GeoRect);
  SetZoom(NewZoom, GeoCoordsToMap(NewZoom, GeoRect).CenterPoint);
end;

procedure TMapControl.ZoomToFit;
begin
  ZoomToArea(MapToGeoCoords(FZoom, FMapRect));
end;

{$ENDREGION}

{$REGION 'TMapZoomAnimate'}

constructor TMapZoomAnimate.Create(AOwner: TMapControl);
begin
  FOwner            := AOwner;
  FSourceFrame      := TBitmap.Create;
  FAnimateFrame     := TBitmap.Create;
  FSourceFrameRect  := TRect.Empty;
  FLocalRect        := TRectF.Empty;
  FScaleFactor      := 1;
  FBindPoint        := TPointF.Zero;
  FOffset           := TPointF.Zero;
  FZoomPrev         := 0;
  FZoomNext         := 0;

  FZoomTimer := TTimer.Create(nil);
  FZoomTimer.Enabled  := false;
  FZoomTimer.Interval := DefaultTimerInterval;
  FZoomTimer.OnTimer  := DoZoomAnimate;


  FCacheFrameTiles := TObjectDictionary<TTile, TBitmap>.Create([doOwnsValues], GetCacheEqualityComparer);
end;

destructor TMapZoomAnimate.Destroy;
begin
  FSourceFrame.Free;
  FAnimateFrame.Free;
  FCacheFrameTiles.Free;
  FZoomTimer.Free;
  inherited;
end;

procedure TMapZoomAnimate.DoOnAnimate;
begin
  if assigned(FOnAnimate) then
    FOnAnimate(self);
end;

procedure TMapZoomAnimate.DoOnStart;
begin
  if assigned(FOnStart) then
    FOnStart(self);
end;

procedure TMapZoomAnimate.DoOnStop;
begin
  if assigned(FOnStop) then
    FOnStop(self);
end;

procedure TMapZoomAnimate.DoZoomAnimate(Sender: TObject);
begin
  FScaleFactor := FScaleFactor - DefaultZoomAnimateFactor;

  var ViewRect, DCRect: TRectF;
  GetAnimateDCRects(FScaleFactor, ViewRect, DCRect);

  var IsLimitScaleFactor := not InRange(FScaleFactor, MinZoomAnimateFactor, 1);

  if IsLimitScaleFactor then begin
    FZoomTimer.Enabled := false;
    FScaleFactor       := 1;

    GenerateCacheFrameTiles;

    DoOnStop;
  end else begin
    if FScaleFactor < MinZoomAnimateFactor + DefaultZoomAnimateFactor then
      FScaleFactor := 0.50; //корректируем точность масштабирования
    GenerateAnimateFrame(ViewRect, DCRect);
    DoOnAnimate;
  end;
end;

procedure TMapZoomAnimate.GenerateAnimateFrame(const ViewRect,
  DCClipViewRect: TRectF);
begin
  FAnimateFrame.SetSize(Ceil(FOwner.LocalRect.Width), Ceil(FOwner.LocalRect.Height));
  FAnimateFrame.Clear(FOwner.BackgroundColor);

  with FAnimateFrame.Canvas do begin
    if BeginScene then
      try
        DrawBitmap(FSourceFrame, ViewRect, DCClipViewRect, 1);
      finally
        EndScene;
      end;
  end;
end;

procedure TMapZoomAnimate.GenerateCacheFrameTiles;

  function GetNormalizeAnimateFrameRectInMapCoords: TRectF;
  begin
    result := FLocalRect;
    result.Offset(FOffsetNext);
    //Выравниваем координаты до целых плиток
    result.TopLeft.X     := ToTileWidthLesser(result.TopLeft.X);
    result.TopLeft.Y     := ToTileHeightLesser(result.TopLeft.Y);
    result.BottomRight.X := ToTileWidthLesser(result.BottomRight.X);
    result.BottomRight.Y := ToTileHeightLesser(result.BottomRight.Y);
  end;

  function CalcTileRectInCacheImage(StartPoint: TPointF; X, Y: Cardinal): TRectF;
  begin
    var TileOffset := TPointF.Create(TILE_IMAGE_WIDTH  * X, TILE_IMAGE_HEIGHT * Y);
    Result := TRectF.Create(StartPoint.X, StartPoint.Y, StartPoint.X+TILE_IMAGE_WIDTH, StartPoint.Y+TILE_IMAGE_HEIGHT);
    Result.Offset(TileOffset);
  end;

  procedure GetSourceDestRects(const TileRect: TRectF; var SourceDC, DestDC: TRectF);
  begin
    SourceDC := TRectF.Intersect(FLocalRect, TileRect);
    DestDC   := TRectF.Create(TPointF.Zero, TPointF.Create(TILE_IMAGE_WIDTH, TILE_IMAGE_HEIGHT));

    var TileOffset := TileRect.TopLeft - TPointF.Zero;
    TileOffset.X := abs(ifthen(TileOffset.X < 0, TileOffset.X, 0));
    TileOffset.Y := abs(ifthen(TileOffset.Y < 0, TileOffset.Y, 0));

    DestDC.Offset(TileOffset);
    DestDC.Size := SourceDC.Size;
  end;

begin
  FCacheFrameTiles.Clear;

  var ViewAnimateFrameRect := GetNormalizeAnimateFrameRectInMapCoords;

  var HorzStartNum  := Max(0, Floor(ViewAnimateFrameRect.Left   / TILE_IMAGE_WIDTH));
  var VertStartNum  := Max(0, Floor(ViewAnimateFrameRect.Top    / TILE_IMAGE_HEIGHT));

  var HorzEndNum    := Max(0, Floor(ViewAnimateFrameRect.Right  / TILE_IMAGE_WIDTH));
  var VertEndNum    := Max(0, Floor(ViewAnimateFrameRect.Bottom / TILE_IMAGE_HEIGHT));

  ViewAnimateFrameRect.Offset(-FOffsetNext); //Вернулись в локальные координаты

  for var Horz := HorzStartNum to HorzEndNum do
    for var Vert := VertStartNum to VertEndNum do begin

      var Tile     := TTile.Create(FZoomNext, Horz, Vert);
      var TileRect := CalcTileRectInCacheImage(ViewAnimateFrameRect.TopLeft, Horz - HorzStartNum, Vert - VertStartNum);

      var SourceDC, DestDC: TRectF;
      GetSourceDestRects(TileRect, SourceDC, DestDC);

      var Bitmap := TBitmap.Create(TILE_IMAGE_WIDTH, TILE_IMAGE_HEIGHT);
      with Bitmap.Canvas do begin
        if BeginScene then
          try
            Clear(FOwner.FBackgroundColor);
            DrawBitmap(FAnimateFrame, SourceDC, DestDC, 1)
          finally
            EndScene;
          end;
      end;

      FCacheFrameTiles.AddOrSetValue(Tile, Bitmap);
    end;
end;

procedure TMapZoomAnimate.GetAnimateDCRects(const ScaleFactor: single; var ViewRect,
  DCClipViewRect: TRectF);
begin
  if FZoomPrev < FZoomNext then begin
    var ScaledRect: TRectF;

    ScaledRect.TopLeft      := ScaleFactor * (FLocalRect.TopLeft     - FBindPoint) + FBindPoint;
    ScaledRect.BottomRight  := ScaleFactor * (FLocalRect.BottomRight - FBindPoint) + FBindPoint;
    ScaledRect.Offset(FOffset);

    ViewRect        := ToInnerCoords(FSourceFrameRect.TopLeft, ScaledRect);
    DCClipViewRect  := TRectF.Create(0, 0, FLocalRect.Width, FLocalRect.Height);
  end else begin
    var ScaledRect: TRectF := FSourceFrameRect;

    ScaledRect.Offset(-FOffset);
    ScaledRect.TopLeft      := ScaleFactor * (ScaledRect.TopLeft - FBindPoint) + FBindPoint;
    ScaledRect.BottomRight  := ScaleFactor * (ScaledRect.BottomRight - FBindPoint) + FBindPoint;

    ViewRect        := FSourceFrame.BoundsF;
    DCClipViewRect  := ScaledRect;
  end;

end;

procedure TMapZoomAnimate.PrepareAnimate(const BindPoint: TPointF);
begin
  FBindPoint        := BindPoint;
  FScaleFactor      := 1;
  FZoomPrev         := FOwner.FZoom;
  FOffset           := FOwner.FLocalOffset;
  FLocalRect        := FOwner.LocalRect;
  FSourceFrameRect  := FOwner.FCacheImageRect;
  FSourceFrame.Assign(FOwner.FCacheImage);


  var ViewRect, DCClipViewRect: TRectF;
  GetAnimateDCRects(FScaleFactor, ViewRect, DCClipViewRect);
  GenerateAnimateFrame(ViewRect, DCClipViewRect);
end;

procedure TMapZoomAnimate.PrepareCacheFrameTiles;
begin
  FZoomNext   := FOwner.FZoom;
  FOffsetNext := FOwner.FLocalOffset;

  FScaleFactor := 0.5;

  var ViewRect, DCRect: TRectF;
  GetAnimateDCRects(FScaleFactor, ViewRect, DCRect);
  GenerateAnimateFrame(ViewRect, DCRect);
  GenerateCacheFrameTiles;

  FScaleFactor := 1;
end;

procedure TMapZoomAnimate.RunAnimate;
begin
  FZoomNext   := FOwner.FZoom;
  FOffsetNext := FOwner.FLocalOffset;
  FZoomTimer.Enabled := true;
  DoOnStart;
end;

function TMapZoomAnimate.ToInnerCoords(const StartPt: TPointF;
  const Rect: TRectF): TRectF;
begin
  Result := TRectF.Create(Rect.TopLeft - StartPt, Rect.BottomRight - StartPt);
end;

function TMapZoomAnimate.TryGetAnimateFrame(var Bitmap: TBitmap): boolean;
begin
  if FAnimateFrame.IsEmpty then exit(false);

  Bitmap := FAnimateFrame;
  result := true;
end;

{$ENDREGION}

end.

unit TerrainEngine;

interface

uses
  Winapi.OpenGL,
  System.Math,
  Vcl.Graphics,
  //GLS
  GLVectorTypes,
  GLVectorGeometry;

type
  TTerrainVertex = array[0..2] of Single;

  THeightData = class
  private
    FHeights: array of array of Single;
    FNormals: array of array of TAffineVector;
    FXDim: integer;
    FYDim: integer;
    procedure SetHeights(x, y: integer; const Value: single);
    function GetHeights(x, y: integer): single;
    function GetNormals(x, y: integer): TAffineVector;
  public
    constructor Create(XDim, YDim: integer);

    procedure CalculateNormals;

    property XDim: integer read FXDim;
    property YDim: integer read FYDim;
    property Heights[x, y: integer]: single read GetHeights write SetHeights;
    property Normals[x, y: integer]: TAffineVector read GetNormals;
  end;

  TTerrainBrush = class
  protected
    function GetWeight(x, y: integer): single; virtual; abstract;
    function GetHalfWidth: integer; virtual; abstract;
    function GetHalfHeight: integer; virtual; abstract;
  public
    property HalfWidth: integer read GetHalfWidth;
    property HalfHeight: integer read GetHalfHeight;
    // weights are defined on the intervals
    // -HalfWidth..HalfWidth and -HalfHeight..HalfHeight
    property Weight[x, y: integer]: single read GetWeight;
  end;

  TSmoothCircularBrush = class(TTerrainBrush)
  private
    FRadius: integer;
  protected
    function GetWeight(x, y: integer): single; override;
    function GetHalfWidth: integer; override;
    function GetHalfHeight: integer; override;
  public
    constructor Create(Radius: integer = 10);
    property Radius: integer read FRadius write FRadius;
  end;

  TTerrainHeightModifier = class
  private
    FData: THeightData;
  protected
    property Data: THeightData read FData;
  public
    constructor Create(Data: THeightData);

    procedure Modify(OriginX, OriginY: integer; Brush: TTerrainBrush;
      Strength: single); virtual; abstract;
  end;

  TElevateModifier = class(TTerrainHeightModifier)
  public
    procedure Modify(OriginX, OriginY: integer; Brush: TTerrainBrush;
      Strength: single); override;
  end;

  { .: TTerrain :. }
  TTerrain = class(TObject)
  private
    { Private declarations }
    FData: THeightData;

  protected
    VScale, TScale, GridSize: Single;
    DisplayList: GLuint;
    GotDisplayList: Boolean;

    procedure DrawManually();
    procedure LoadFromBitmap(Bitmap: TBitmap);
  public
    { Public declarations }
    constructor Create(const FileName: String; GridScale: Single = 1.0;
      VerticalScale: Single = 100.0; TextureScale: Single = 1.0);
    destructor Destroy(); override;

    function GridPointPosition(x, y: integer): TAffineVector;

    procedure Render;

    property Data: THeightData read FData;
  end;

implementation

{ THeightData }

procedure THeightData.CalculateNormals;
var
  X, Y, XX, YY, targetY, targetX: Integer;
  V: TAffineVector;
  H: Single;
begin
  for Y := 0 to YDim -1 do
  begin
    for X := 0 to XDim -1 do
    begin
      V:= AffineVectorMake(0, 0, 0);

      for YY := -1 to 1 do
      begin
        targetY:= min(max(Y + YY, 0), YDim - 1);

        for XX := -1 to 1 do
        begin
          if (XX = 0) and (YY = 0) then
            continue;

          targetX:= min(max(X + XX, 0), XDim - 1);

          H := Heights[TargetX, TargetY];

          V.X := V.X - XX * H;
          V.Z := V.Z - YY * H;
        end;
      end;
      V.Y := 1 / 3;
      // length can't possibly be zero because
      // V[1] is set to 1/3
      FNormals[Y][X] := VectorNormalize(V);
    end;
  end;
end;

constructor THeightData.Create(XDim, YDim: integer);
begin
  inherited Create;

  FXDim:= XDim;
  FYDim:= YDim;

  SetLength(FHeights, YDim, XDim);
  SetLength(FNormals, YDim, XDim);
end;

function THeightData.GetHeights(x, y: integer): single;
begin
  result:= FHeights[y][x];
end;

function THeightData.GetNormals(x, y: integer): TAffineVector;
begin
  result:= FNormals[y][x];
end;

procedure THeightData.SetHeights(x, y: integer; const Value: single);
begin
  FHeights[y][x]:= Value;
end;

{ TTerrainHeightModifier }

constructor TTerrainHeightModifier.Create(Data: THeightData);
begin
  inherited Create;

  FData:= Data;
end;

{ TTerrain }
constructor TTerrain.Create(const FileName: String; GridScale, VerticalScale,
  TextureScale: Single);
var
  BMP: TBitmap;
begin
  inherited Create();

  VScale := VerticalScale;
  TScale := TextureScale;
  GotDisplayList := False;
  GridSize := GridScale;

  BMP:= TBitmap.Create();
  try
    BMP.LoadFromFile(FileName);

    LoadFromBitmap(BMP);
  finally
    BMP.Free();
  end;
end;

destructor TTerrain.Destroy;
begin
  if GotDisplayList then
    glDeleteLists(DisplayList, 1);

  FData.Free;

  inherited Destroy();
end;

procedure TTerrain.DrawManually;
var
  X, Y: Integer;
  Hw, Hh: Single;
  N: TAffineVector;
begin
  Hw := (Data.XDim / 2) * GridSize;
  Hh := (Data.YDim / 2) * GridSize;

  for Y := 0 to Data.YDim -2 do
  begin
    glBegin(GL_TRIANGLE_STRIP);
      for X := 0 to Data.XDim -1 do
      begin
        glTexCoord2f(X / Data.XDim * TScale, Y / Data.XDim * TScale);
        N:= Data.Normals[X, Y];
        glNormal3fv(@N);
        glVertex3f(X * GridSize - Hw, Data.Heights[X, Y] * VScale, Y * GridSize - Hh);

        glTexCoord2f(X / Data.XDim * TScale, (Y + 1) / Data.XDim * TScale);
        N:= Data.Normals[X, Y+1];
        glNormal3fv(@N);
        glVertex3f(X * GridSize - Hw, Data.Heights[X, Y + 1] * VScale,
          (Y + 1) * GridSize - Hh);
      end;
    glEnd();
  end;
end;

function TTerrain.GridPointPosition(x, y: integer): TAffineVector;
var
  Hw, Hh: Single;
begin
  x:= min(max(x, 0), Data.XDim-1);
  y:= min(max(y, 0), Data.YDim-1);

  Hw := (Data.XDim / 2) * GridSize;
  Hh := (Data.YDim / 2) * GridSize;

  result:= AffineVectorMake(X * GridSize - Hw, Data.Heights[X, Y] * VScale, Y * GridSize - Hh);
end;

procedure TTerrain.LoadFromBitmap(Bitmap: TBitmap);
var
  X, Y: Integer;
  PB: PByteArray;
begin
  Bitmap.PixelFormat := pf24bit;

  // clear old data
  FData.Free;
  FData:= THeightData.Create(Bitmap.Width, Bitmap.Height);
  for Y := Bitmap.Height - 1 downto 0 do
  begin
    PB := Bitmap.ScanLine[Y];
    for X := 0 to Bitmap.Width-1 do
      Data.Heights[X, Bitmap.Height -1 - Y] := (PB[X * 3 + 2] +
        PB[X * 3 + 1] + PB[X * 3]) / 765;
  end;
  Data.CalculateNormals();
end;

procedure TTerrain.Render;
begin
  // can't use display lists when we constantly modify
  // the terrain data
  DrawManually();

//  if not GotDisplayList then
//  begin
//    DisplayList := glGenLists(1);
//
//    glNewList(DisplayList, GL_COMPILE);
//      DrawManually();
//    glEndList();
//
//    GotDisplayList := True;
//  end;
//
//  glCallList(DisplayList)
end;

{ TSmoothCircularBrush }

constructor TSmoothCircularBrush.Create(Radius: integer);
begin
  inherited Create;

  FRadius:= Radius;
end;

function TSmoothCircularBrush.GetHalfHeight: integer;
begin
  result:= Radius;
end;

function TSmoothCircularBrush.GetHalfWidth: integer;
begin
  result:= Radius;
end;

function TSmoothCircularBrush.GetWeight(x, y: integer): single;
var
  s: single;
begin
  result:= 0;
  if (abs(x) > HalfWidth) or (abs(y) > HalfHeight) then
    exit;

  s:= Radius / 3;
  // gaussian function with sigma = s and my = 0
  // gaussian is almost 0 if distance > 3*sigma, hence
  // the division of radius by 3 to get sigma
  result:= exp(-(x*x+y*y) / (2 * s*s)) / (s * sqrt(2*pi));   
end;

{ TElevateModifier }

procedure TElevateModifier.Modify(OriginX, OriginY: integer; Brush: TTerrainBrush; Strength: single);
var
  top, bottom, left, right: integer;
  x, y, bx, by: integer;
  oldHeight: single;
begin
  // make sure we don't go outside the data 
  top:= max(OriginY-Brush.HalfHeight, 0);
  left:= max(OriginX-Brush.HalfWidth, 0);

  bottom:= min(OriginY+Brush.HalfHeight, Data.YDim-1);
  right:= min(OriginX+Brush.HalfWidth, Data.XDim-1);

  for y:= top to bottom do
  begin
    by:= y - OriginY;
    for x:= left to right do
    begin
      bx:= x - OriginX;
      oldHeight:= Data.Heights[x, y];

      // add the brush weight to the height, modified by
      // the strength, and making sure we stay above 0
      Data.Heights[x, y]:=
        max(oldHeight + Strength * Brush.Weight[bx, by], 0);
    end;
  end;
end;

end.

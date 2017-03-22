unit uSpeedIndicator;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,

  GLScene,
  GLOBjects,
  GLHUDOBjects,
  GLMaterial,
  GLWindowsFont,
  GLTexture,
  GLVectorGeometry,
  GLVectorTypes,
  GLCrossPlatform;

const
  C_HUD_PATH = 'data\hud\';

  C_INDICATOR_OFFSET_X = -10;
  C_INDICATOR_OFFSET_Y = -10;
  C_INDICATOR_FULLANLGE = 90;

  C_MATNAME_ACTIVE = 'cell_active';
  C_MATNAME_INACTIVE = 'cell_inactive';

  C_CELL_COUNT = 10;
  C_CELL_RADIUS = 120;

  C_CELL_INACTIVE_R = 1.0;
  C_CELL_INACTIVE_G = 0.0;
  C_CELL_INACTIVE_B = 0.0;
  C_CELL_INACTIVE_A = 0.5;

  C_CELL_ACTIVE_R = 0.0;
  C_CELL_ACTIVE_G = 1.0;
  C_CELL_ACTIVE_B = 0.0;
  C_CELL_ACTIVE_A = 1.0;

type
  TdfSpeedIndicator = class (TGLDummyCube)
  private
    FActiveCount: Integer;

    FMaxSpeed, FSpeed: Single;
    FSpeedText: TGLHUDText;
    FCells: array of TGLHUDSprite;
    FCellCount: Integer;
    function GetCell(Index: Integer): TGLHUDSprite;
    function GetInactiveColor(): TVector;
    function GetActiveColor(): TVector;
    procedure SetCell(Index: Integer; const Value: TGLHUDSprite);
    procedure SetSpeed(const Value: Single);
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;
    property SpeedText: TGLHUDText read FSpeedText write FSpeedText;
    property Cells[Index: Integer]: TGLHUDSprite read GetCell write SetCell;
    property CellCount: Integer read FCellCount default C_CELL_COUNT;
    property MaxSpeed: Single read FMaxSpeed write FMaxSpeed;
    property Speed: Single read FSpeed write SetSpeed;
    procedure AddMaterialToCells(aTextureName: String);
    procedure AddFontToText(aFont: TGLWindowsBitmapFont);
  end;

//=======================================================================
implementation
//=======================================================================

uses
  uGLSceneObjects;

{ TdfSpeedIndicator }

procedure TdfSpeedIndicator.AddFontToText(aFont: TGLWindowsBitmapFont);
begin
  FSpeedText.BitmapFont := aFont;
  aFont.EnsureChars('0', '9');
end;

procedure TdfSpeedIndicator.AddMaterialToCells(aTextureName: String);
var
  w, h: Single;
  i: Integer;
begin
  if not Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(C_MATNAME_ACTIVE)) then
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := C_MATNAME_ACTIVE;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_HUD_PATH + aTextureName);
        w := Texture.Image.Width;
        h := Texture.Image.Height;
        Texture.Enabled := True;
        Texture.TextureMode := tmModulate;
        Texture.TextureWrap := twNone;
        BlendingMode := bmTransparency;
        MaterialOptions := [moIgnoreFog, moNoLighting];
        FrontProperties.Diffuse.Color := GetActiveColor();
      end;
    end
  else
    with dfGLSceneObjects.MatLibrary.LibMaterialByName(aTextureName).Material.Texture.Image do
    begin
      w := Width;
      h := Height;
    end;
  if not Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(C_MATNAME_INACTIVE)) then
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := C_MATNAME_INACTIVE;
      Material.Assign(dfGLSceneObjects.MatLibrary.LibMaterialByName(C_MATNAME_ACTIVE).Material);
      Material.FrontProperties.Diffuse.Color := GetInactiveColor;
    end;

  for i := 0 to FCellCount - 1 do
  begin
    FCells[i].Material.MaterialLibrary := dfGLSceneObjects.MatLibrary;
    FCells[i].Material.LibMaterialName := C_MATNAME_INACTIVE;
    FCells[i].Width := w;
    FCells[i].Height := h;
  end;
end;

constructor TdfSpeedIndicator.CreateAsChild(aParentOwner: TGLBaseSceneObject);
var
  i: Integer;
  stepAngle: Single;
  origin: TVector2f;
  cellPos: TAffineVector;
begin
  inherited;
  FCellCount := C_CELL_COUNT;
  FActiveCount := 0;

  SetLength(FCells, FCellCount);

  stepAngle := C_INDICATOR_FULLANLGE / (FCellCount - 1);
  origin.X := dfGLSceneObjects.Viewer.Width + C_INDICATOR_OFFSET_X;
  origin.Y := dfGLSceneObjects.Viewer.Height + C_INDICATOR_OFFSET_Y;

  for i := 0 to FCellCount - 1 do
  begin
    FCells[i] := TGLHUDSprite.CreateAsChild(Self);
    cellPos.X := - C_CELL_RADIUS * cos((pi / 180) * stepAngle * i);
    cellPos.Y := - C_CELL_RADIUS * sin((pi / 180) * stepAngle * i);
    cellPos.Z := 0.5;
    FCells[i].Rotation := (180 / pi) * ArcCos(VectorAngleCosine(YVector, cellPos));
    if cellPos.X < origin.X then
      FCells[i].Rotation := - FCells[i].Rotation;

    cellPos.X := cellPos.X + origin.X;
    cellPos.Y := cellPos.Y + origin.Y;
    FCells[i].Position.SetPoint(cellPos);
  end;

  FSpeedText := TGLHUDText.CreateAsChild(Self);
  FSpeedText.Alignment := taRightJustify;
  FSpeedText.Layout := tlBottom;
  FSpeedText.Position.SetPoint(origin.X, origin.Y, 0.5);
  FSpeedText.ModulateColor.SetColor(0, 1, 0, 0.8);
end;

function TdfSpeedIndicator.GetActiveColor: TVector;
begin
  Result.X := C_CELL_ACTIVE_R;
  Result.Y := C_CELL_ACTIVE_G;
  Result.Z := C_CELL_ACTIVE_B;
  Result.W := C_CELL_ACTIVE_A;
end;

function TdfSpeedIndicator.GetCell(Index: Integer): TGLHUDSprite;
begin
  Result := FCells[Index];
end;

function TdfSpeedIndicator.GetInactiveColor: TVector;
begin
  Result.X := C_CELL_INACTIVE_R;
  Result.Y := C_CELL_INACTIVE_G;
  Result.Z := C_CELL_INACTIVE_B;
  Result.W := C_CELL_INACTIVE_A;
end;

procedure TdfSpeedIndicator.SetCell(Index: Integer; const Value: TGLHUDSprite);
begin
  FCells[Index] := Value;
end;

procedure TdfSpeedIndicator.SetSpeed(const Value: Single);
var
  i, NewActiveCount, roundSpeed: Integer;
begin
  FSpeed := Value;
  roundSpeed := Round(FSpeed);
  FSpeedText.Text := IntToStr(roundSpeed);
  if roundSpeed = 0 then
    FSpeedText.ModulateColor.SetColor(1, 0, 0)
  else
    FSpeedText.ModulateColor.SetColor(0, 1, 0);

  NewActiveCount := Round(FSpeed / FMaxSpeed * FCellCount);
  if NewActiveCount <> FActiveCount then
  begin
    FActiveCount := NewActiveCount;
    for i := 0 to FActiveCount - 1 do
      FCells[i].Material.LibMaterialName := C_MATNAME_ACTIVE;

    for i := FActiveCount to FCellCount - 1 do
      FCells[i].Material.LibMaterialName := C_MATNAME_INACTIVE;
  end;
end;

end.

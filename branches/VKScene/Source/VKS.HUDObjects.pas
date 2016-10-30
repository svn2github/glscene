//
// VKScene project, http://glscene.sourceforge.net 
//
{
  GLScene objects that get rendered in 2D coordinates 
}
unit VKS.HUDObjects;

interface

{$I VKScene.inc}

uses
  System.Classes,

  VKS.Scene,
  VKS.VectorGeometry,
  VKS.Objects,
  VKS.BitmapFont,
  VKS.CrossPlatform,
  VKS.Color,
  VKS.RenderContextInfo,
  Winapi.OpenGL, Winapi.OpenGLext, 
  VKS.Context,
  VKS.State,
  VKS.XOpenGL;

type

  // TVKHUDSprite
  //
  { A rectangular area, NOT perspective projected. 
    (x, y) coordinates map directly to the viewport (in pixels) and refer
    the center of the area. 
    The coordinate system is that of an equivalent TCanvas, ie. top-left
    point is the origin (0, 0). 
    The z component is ignored and Z-Buffer is disabled when rendering. 
     Using TVKHUDSprite in 2D only scenes :  
    The most convenient way to use a TVKHUDSprite as a simple 2D sprite with
    blending capabilities (transparency or additive), is to set the texture
    mode to tmModulate, in FrontProperties, to use the Emission color to
    control coloring/intensity, and finally use the Diffuse color's alpha
    to control transparency (while setting the other RGB components to 0). 
    You can also control aplha-blending by defining a <1 value in the sprite's
    AlphaChannel field. This provides you with hardware accelerated,
    alpha-blended blitting. 
    Note : since TVKHUDSprite works in absolute coordinates, TVKProxyObject
    can't be used to duplicate an hud sprite. }
  TVKHUDSprite = class(TVKSprite)
  private
    { Private Declarations }
    FXTiles, FYTiles: Integer;
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
  protected
    { Protected Declarations }
    procedure SetXTiles(const val: Integer);
    procedure SetYTiles(const val: Integer);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure DoRender(var rci: TVKRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;

  published
    { Published Declarations }
    property XTiles: Integer read FXTiles write SetXTiles default 1;
    property YTiles: Integer read FYTiles write SetYTiles default 1;
    // Redeclare them with new default values.
    property Width stored StoreWidth;
    property Height stored StoreHeight;
  end;

  // TVKHUDText
  //
  { A 2D text displayed and positionned in 2D coordinates. 
    The HUDText uses a character font defined and stored by a TVKBitmapFont
    component. The text can be scaled and rotated (2D), the layout and
    alignment can also be controled. }
  TVKHUDText = class(TVKImmaterialSceneObject)
  private
    { Private Declarations }
    FBitmapFont: TVKCustomBitmapFont;
    FText: UnicodeString;
    FRotation: Single;
    FAlignment: TAlignment;
    FLayout: TVKTextLayout;
    FModulateColor: TVKColor;

  protected
    { Protected Declarations }
    procedure SetBitmapFont(const val: TVKCustomBitmapFont);
    procedure SetText(const val: UnicodeString);
    procedure SetRotation(const val: Single);
    procedure SetAlignment(const val: TAlignment);
    procedure SetLayout(const val: TVKTextLayout);
    procedure SetModulateColor(const val: TVKColor);

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure RenderTextAtPosition(const X, Y, Z: Single;
      var rci: TVKRenderContextInfo);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var rci: TVKRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;

  published
    { Published Declarations }
    { Refers the bitmap font to use. 
      The referred bitmap font component stores and allows access to
      individual character bitmaps. }
    property BitmapFont: TVKCustomBitmapFont read FBitmapFont
      write SetBitmapFont;
    { Text to render. 
      Be aware that only the characters available in the bitmap font will
      be rendered. CR LF sequences are allowed. }
    property Text: UnicodeString read FText write SetText;
    { Rotation angle in degrees (2d). }
    property Rotation: Single read FRotation write SetRotation;
    { Controls the text alignment (horizontal). 
      Possible values : taLeftJustify, taRightJustify, taCenter }
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    { Controls the text layout (vertical). 
      Possible values : tlTop, tlCenter, tlBottom }
    property Layout: TVKTextLayout read FLayout write SetLayout default tlTop;
    { Color modulation, can be used for fade in/out too. }
    property ModulateColor: TVKColor read FModulateColor write SetModulateColor;
  end;

  { Position (X, Y and X) is in absolute coordinates. This component converts
    them to screen coordinates and renderes text there. }
  TVKAbsoluteHUDText = class(TVKHUDText)
  public
    procedure DoRender(var rci: TVKRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  end;

  { Position (X and Y) is expected in a [0..1] range (from Screen size)
    This component converts this position to the actual screen position and
    renders the text there. This way a HUD text always appears to be in the
    the same place, regardless of the currect screen resolution.
    Note: this still does not solve the font scaling problem. }
  TVKResolutionIndependantHUDText = class(TVKHUDText)
  public
    procedure DoRender(var rci: TVKRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    constructor Create(AOwner: TComponent); override;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKHUDSprite ------------------
// ------------------

// Create
//

constructor TVKHUDSprite.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  Width := 16;
  Height := 16;
  FXTiles := 1;
  FYTiles := 1;
end;

// SetXTiles
//

procedure TVKHUDSprite.SetXTiles(const val: Integer);
begin
  if val <> FXTiles then
  begin
    FXTiles := val;
    StructureChanged;
  end;
end;

// SetYTiles
//

procedure TVKHUDSprite.SetYTiles(const val: Integer);
begin
  if val <> FYTiles then
  begin
    FYTiles := val;
    StructureChanged;
  end;
end;

// DoRender
//

procedure TVKHUDSprite.DoRender(var rci: TVKRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  vx, vy, vx1, vy1, f: Single;
  u0, v0, u1, v1: Integer;
begin
  if rci.ignoreMaterials then
    Exit;
  Material.Apply(rci);
  repeat
    if AlphaChannel <> 1 then
    begin
      if stLighting in rci.VKStates.States then
        rci.VKStates.SetMaterialAlphaChannel(GL_FRONT, AlphaChannel)
      else
        with Material.GetActualPrimaryMaterial.FrontProperties.Diffuse do
          glColor4f(Red, Green, Blue, AlphaChannel);
    end;
    // Prepare matrices
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadMatrixf(@TVKSceneBuffer(rci.buffer).BaseProjectionMatrix);
    if rci.renderDPI = 96 then
      f := 1
    else
      f := rci.renderDPI / 96;
    glScalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    glTranslatef(f * Position.X - rci.viewPortSize.cx * 0.5,
      rci.viewPortSize.cy * 0.5 - f * Position.Y, Position.Z);
    if Rotation <> 0 then
      glRotatef(Rotation, 0, 0, 1);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    rci.VKStates.Disable(stDepthTest);
    rci.VKStates.DepthWriteMask := GLboolean(False);

    // precalc coordinates
    vx := -Width * 0.5 * f;
    vx1 := vx + Width * f;
    vy := +Height * 0.5 * f;
    vy1 := vy - Height * f;

    // Texture coordinates
    if MirrorU then
    begin
      u0 := FXTiles;
      u1 := 0;
    end
    else
    begin
      u0 := 0;
      u1 := FXTiles;
    end;

    if MirrorV then
    begin
      v0 := FYTiles;
      v1 := 0;
    end
    else
    begin
      v0 := 0;
      v1 := FYTiles;
    end;

    // issue quad
    glBegin(GL_QUADS);
    glNormal3fv(@YVector);
    glTexCoord2f(u0, v0);
    glVertex2f(vx, vy1);
    glTexCoord2f(u1, v0);
    glVertex2f(vx1, vy1);
    glTexCoord2f(u1, v1);
    glVertex2f(vx1, vy);
    glTexCoord2f(u0, v1);
    glVertex2f(vx, vy);
    glEnd;

    // restore state
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  until not Material.UnApply(rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// StoreHeight
//

function TVKHUDSprite.StoreHeight: Boolean;
begin
  Result := Abs(Height - 16) > 0.001;
end;

// StoreWidth
//

function TVKHUDSprite.StoreWidth: Boolean;
begin
  Result := Abs(Height - 16) > 0.001;
end;

// ------------------
// ------------------ TVKHUDText ------------------
// ------------------

// Create
//

constructor TVKHUDText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FModulateColor := TVKColor.CreateInitialized(Self, clrWhite);
end;

// Destroy
//

destructor TVKHUDText.Destroy;
begin
  FModulateColor.Free;
  BitmapFont := nil;
  inherited;
end;

// Notification
//

procedure TVKHUDText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
    BitmapFont := nil;
  inherited;
end;

// SetBitmapFont
//

procedure TVKHUDText.SetBitmapFont(const val: TVKCustomBitmapFont);
begin
  if val <> FBitmapFont then
  begin
    if Assigned(FBitmapFont) then
      FBitmapFont.UnRegisterUser(Self);
    FBitmapFont := val;
    if Assigned(FBitmapFont) then
    begin
      FBitmapFont.RegisterUser(Self);
      FBitmapFont.FreeNotification(Self);
    end;
    StructureChanged;
  end;
end;

// SetText
//

procedure TVKHUDText.SetText(const val: UnicodeString);
begin
  FText := val;
  StructureChanged;
end;

// SetRotation
//

procedure TVKHUDText.SetRotation(const val: Single);
begin
  FRotation := val;
  StructureChanged;
end;

// SetAlignment
//

procedure TVKHUDText.SetAlignment(const val: TAlignment);
begin
  FAlignment := val;
  StructureChanged;
end;

// SetLayout
//

procedure TVKHUDText.SetLayout(const val: TVKTextLayout);
begin
  FLayout := val;
  StructureChanged;
end;

// SetModulateColor
//

procedure TVKHUDText.SetModulateColor(const val: TVKColor);
begin
  FModulateColor.Assign(val);
end;

// RenderTextAtPosition
//

procedure TVKHUDText.RenderTextAtPosition(const X, Y, Z: Single;
  var rci: TVKRenderContextInfo);
var
  f: Single;
begin
  if Assigned(FBitmapFont) and (Text <> '') then
  begin
    rci.VKStates.PolygonMode := pmFill;
    // Prepare matrices
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadMatrixf(@TVKSceneBuffer(rci.buffer).BaseProjectionMatrix);
    f := rci.renderDPI / 96;
    glScalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    glTranslatef(X * f - rci.viewPortSize.cx / 2, rci.viewPortSize.cy / 2 -
      Y * f, Z);
    if FRotation <> 0 then
      glRotatef(FRotation, 0, 0, 1);
    glScalef(Scale.DirectX * f, Scale.DirectY * f, 1);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    rci.VKStates.Disable(stDepthTest);
    // render text
    FBitmapFont.RenderString(rci, Text, FAlignment, FLayout,
      FModulateColor.Color);
    // restore state
    rci.VKStates.Enable(stDepthTest);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  end;
end;

// DoRender
//

procedure TVKHUDText.DoRender(var rci: TVKRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  RenderTextAtPosition(Position.X, Position.Y, Position.Z, rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------
// ------------------ TVKResolutionIndependantHUDText ------------------
// ------------------

// Create
//

constructor TVKResolutionIndependantHUDText.Create(AOwner: TComponent);
begin
  inherited;
  Position.X := 0.5;
  Position.Y := 0.5;
end;

// DoRender
//

procedure TVKResolutionIndependantHUDText.DoRender(var rci: TVKRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  RenderTextAtPosition(Position.X * rci.viewPortSize.cx,
    Position.Y * rci.viewPortSize.cy, Position.Z, rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------
// ------------------ TVKAbsoluteHUDText ------------------
// ------------------

// DoRender
//

procedure TVKAbsoluteHUDText.DoRender(var rci: TVKRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  Temp: TAffineVector;
begin
  Temp := TVKSceneBuffer(rci.buffer).WorldToScreen(Self.AbsoluteAffinePosition);
  Temp.Y := rci.viewPortSize.cy - Temp.Y;
  RenderTextAtPosition(Temp.X, Temp.Y, Temp.Z, rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// class registrations
RegisterClasses([TVKHUDText, TVKHUDSprite, TVKResolutionIndependantHUDText,
  TVKAbsoluteHUDText]);

end.

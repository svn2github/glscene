//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Texture-based Lens flare object.

}
unit VXS.TexLensFlare;

interface

{$I VXScene.inc}

uses
  System.Classes,

  VXS.OpenGL,
  VXS.Scene,
  VXS.VectorGeometry,
  VXS.PersistentClasses,
  VXS.Objects,
  VXS.Texture,
  VXS.Context,
  VXS.RenderContextInfo,
  VXS.BaseClasses,
  VXS.State,
  VXS.VectorTypes;

type

  TVXTextureLensFlare = class(TVXBaseSceneObject)
  private
    FSize: integer;
    FCurrSize: Single;
    FNumSecs: integer;
    FAutoZTest: boolean;
    //used for internal calculation
    FDeltaTime: Double;
    FImgSecondaries: TVXTexture;
    FImgRays: TVXTexture;
    FImgRing: TVXTexture;
    FImgGlow: TVXTexture;
    FSeed: Integer;
    procedure SetImgGlow(const Value: TVXTexture);
    procedure SetImgRays(const Value: TVXTexture);
    procedure SetImgRing(const Value: TVXTexture);
    procedure SetImgSecondaries(const Value: TVXTexture);
    procedure SetSeed(const Value: Integer);
  protected
    procedure SetSize(aValue: integer);
    procedure SetNumSecs(aValue: integer);
    procedure SetAutoZTest(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
  published
    // MaxRadius of the flare.
    property Size: integer read FSize write SetSize default 50;
    // Random seed
    property Seed: Integer read FSeed write SetSeed;
    // Number of secondary flares.
    property NumSecs: integer read FNumSecs write SetNumSecs default 8;
    // Number of segments used when rendering circles.
    //property Resolution: integer read FResolution write SetResolution default 64;
    property AutoZTest: boolean read FAutoZTest write SetAutoZTest default True;
    // The Textures
    property ImgGlow: TVXTexture read FImgGlow write SetImgGlow;
    property ImgRays: TVXTexture read FImgRays write SetImgRays;
    property ImgRing: TVXTexture read FImgRing write SetImgRing;
    property ImgSecondaries: TVXTexture read FImgSecondaries write SetImgSecondaries;
    property ObjectsSorting;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

//------------------------------------------------------------------
implementation
//------------------------------------------------------------------

// ------------------
// ------------------ TVXTextureLensFlare ------------------
// ------------------

constructor TVXTextureLensFlare.Create(AOwner: TComponent);
begin
  inherited;
  Randomize;
  FSeed := Random(2000) + 465;

  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FCurrSize := FSize;
  FNumSecs := 8;
  FAutoZTest := True;

  FImgRays := TVXTexture.Create(Self);
  FImgSecondaries := TVXTexture.Create(Self);
  FImgRing := TVXTexture.Create(Self);
  FImgGlow := TVXTexture.Create(Self);
end;

procedure TVXTextureLensFlare.SetSize(aValue: integer);
begin
  if FSize <> aValue then
  begin
    FSize := aValue;
    FCurrSize := FSize;
    StructureChanged;
  end;
end;

procedure TVXTextureLensFlare.SetNumSecs(aValue: integer);
begin
  if FNumSecs <> aValue then
  begin
    FNumSecs := aValue;
    StructureChanged;
  end;
end;

procedure TVXTextureLensFlare.SetAutoZTest(aValue: boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

procedure TVXTextureLensFlare.BuildList(var rci: TVXRenderContextInfo);
var
  v, rv, screenPos, posVector: TAffineVector;
  depth, rnd: Single;
  flag: Boolean;
  i: Integer;
  CurrentBuffer: TVXSceneBuffer;
begin
  CurrentBuffer := TVXSceneBuffer(rci.buffer);
  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := CurrentBuffer.WorldToScreen(v);
    if (screenPos.X < rci.viewPortSize.cx) and (screenPos.X >= 0)
      and (screenPos.Y < rci.viewPortSize.cy) and (screenPos.Y >= 0) then
    begin
      if FAutoZTest then
      begin
        depth := CurrentBuffer.GetPixelDepth(Round(ScreenPos.X),
          Round(rci.viewPortSize.cy - ScreenPos.Y));
        // but is it behind something?
        if screenPos.Z >= 1 then
          flag := (depth >= 1)
        else
          flag := (depth >= screenPos.Z);
      end
      else
        flag := True;
    end
    else
      flag := False;
  end
  else
    flag := False;

  MakeVector(posVector,
    screenPos.X - rci.viewPortSize.cx / 2,
    screenPos.Y - rci.viewPortSize.cy / 2, 0);

  // make the glow appear/disappear progressively

  if Flag then
    if FCurrSize < FSize then
      FCurrSize := FCurrSize + FDeltaTime * 200 {FSize * 4};
  if not Flag then
    if FCurrSize > 0 then
      FCurrSize := FCurrSize - FDeltaTime * 200 {FSize * 4};
  if FCurrSize <= 0 then
    Exit;

  // Prepare matrices
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glScalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);

  rci.VXStates.Disable(stLighting);
  rci.VXStates.Disable(stDepthTest);
  rci.VXStates.Enable(stBlend);
  rci.VXStates.SetBlendFunc(bfOne, bfOne);

  //Rays and Glow on Same Position
  glPushMatrix;
  glTranslatef(posVector.X, posVector.Y, posVector.Z);

  if not ImgGlow.Disabled and Assigned(ImgGlow.Image) then
  begin
    ImgGlow.Apply(rci);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex3f(-FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 0);
    glVertex3f(FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 1);
    glVertex3f(FCurrSize, FCurrSize, 0);
    glTexCoord2f(0, 1);
    glVertex3f(-FCurrSize, FCurrSize, 0);
    glEnd;
    ImgGlow.UnApply(rci);
  end;

  if not ImgRays.Disabled and Assigned(ImgRays.Image) then
  begin
    ImgRays.Apply(rci);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex3f(-FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 0);
    glVertex3f(FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 1);
    glVertex3f(FCurrSize, FCurrSize, 0);
    glTexCoord2f(0, 1);
    glVertex3f(-FCurrSize, FCurrSize, 0);
    glEnd;
    ImgRays.UnApply(rci);
  end;
  glPopMatrix;

  if not ImgRing.Disabled and Assigned(ImgRing.Image) then
  begin
    glPushMatrix;
    glTranslatef(posVector.X * 1.1, posVector.Y * 1.1, posVector.Z);
    ImgRing.Apply(rci);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex3f(-FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 0);
    glVertex3f(FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 1);
    glVertex3f(FCurrSize, FCurrSize, 0);
    glTexCoord2f(0, 1);
    glVertex3f(-FCurrSize, FCurrSize, 0);
    glEnd;
    ImgRing.UnApply(rci);
    glPopMatrix;
  end;

  if not ImgSecondaries.Disabled and Assigned(ImgSecondaries.Image) then
  begin
    RandSeed := FSeed;
    glPushMatrix;
    ImgSecondaries.Apply(rci);
    for i := 1 to FNumSecs do
    begin
      rnd := 2 * Random - 1;
      v := PosVector;
      if rnd < 0 then
        ScaleVector(V, rnd)
      else
        ScaleVector(V, 0.8 * rnd);
      glPushMatrix;
      glTranslatef(v.X, v.Y, v.Z);

      rnd := random * 0.5 + 0.1;
      glBegin(GL_QUADS);
      glTexCoord2f(0, 0);
      glVertex3f(-FCurrSize * rnd, -FCurrSize * rnd, 0);
      glTexCoord2f(1, 0);
      glVertex3f(FCurrSize * rnd, -FCurrSize * rnd, 0);
      glTexCoord2f(1, 1);
      glVertex3f(FCurrSize * rnd, FCurrSize * rnd, 0);
      glTexCoord2f(0, 1);
      glVertex3f(-FCurrSize * rnd, FCurrSize * rnd, 0);
      glEnd;
      glPopMatrix
    end;
    ImgSecondaries.UnApply(rci);
    glPopMatrix;
  end;

  // restore state

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TVXTextureLensFlare.DoProgress(const progressTime: TVXProgressTimes);
begin
  FDeltaTime := progressTime.deltaTime;
  inherited;
end;

procedure TVXTextureLensFlare.SetImgGlow(const Value: TVXTexture);
begin
  FImgGlow.Assign(Value);
  StructureChanged;
end;

procedure TVXTextureLensFlare.SetImgRays(const Value: TVXTexture);
begin
  FImgRays.Assign(Value);
  StructureChanged;
end;

procedure TVXTextureLensFlare.SetImgRing(const Value: TVXTexture);
begin
  FImgRing.Assign(Value);
  StructureChanged;
end;

procedure TVXTextureLensFlare.SetImgSecondaries(const Value: TVXTexture);
begin
  FImgSecondaries.Assign(Value);
  StructureChanged;
end;

destructor TVXTextureLensFlare.Destroy;
begin
  FImgRays.Free;
  FImgSecondaries.Free;
  FImgRing.Free;
  FImgGlow.Free;
  inherited;
end;

procedure TVXTextureLensFlare.SetSeed(const Value: Integer);
begin
  FSeed := Value;
  StructureChanged;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TVXTextureLensFlare]);

end.


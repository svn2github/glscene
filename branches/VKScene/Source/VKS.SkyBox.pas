//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   A TVKImmaterialSceneObject drawing 6 quads (plus another quad as "Cloud" plane)
   for use as a skybox always centered on the camera. 
    
}
unit VKS.SkyBox;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,

  VKS.XOpenGL,
  VKS.Scene,
  VKS.Material,
  VKS.VectorGeometry,
  VKS.RenderContextInfo,
  VKS.VectorTypes;

type

  TVKSkyBoxStyle = (sbsFull, sbsTopHalf, sbsBottomHalf, sbTopTwoThirds,
    sbsTopHalfClamped);

  TVKSkyBox = class(TVKCameraInvariantObject, IGLMaterialLibrarySupported)
  private
    FMatNameTop: string;
    FMatNameRight: string;
    FMatNameFront: string;
    FMatNameLeft: string;
    FMatNameBack: string;
    FMatNameBottom: string;
    FMatNameClouds: string;
    FMaterialLibrary: TVKMaterialLibrary;
    FCloudsPlaneOffset: Single;
    FCloudsPlaneSize: Single;
    FStyle: TVKSkyBoxStyle;
    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
  protected
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary);
    procedure SetMatNameBack(const Value: string);
    procedure SetMatNameBottom(const Value: string);
    procedure SetMatNameFront(const Value: string);
    procedure SetMatNameLeft(const Value: string);
    procedure SetMatNameRight(const Value: string);
    procedure SetMatNameTop(const Value: string);
    procedure SetMatNameClouds(const Value: string);
    procedure SetCloudsPlaneOffset(const Value: single);
    procedure SetCloudsPlaneSize(const Value: single);
    procedure SetStyle(const value: TVKSkyBoxStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var ARci: TVKRenderContextInfo); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  published
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    property MatNameTop: TVKLibMaterialName read FMatNameTop write
      SetMatNameTop;
    property MatNameBottom: TVKLibMaterialName read FMatNameBottom write
      SetMatNameBottom;
    property MatNameLeft: TVKLibMaterialName read FMatNameLeft write
      SetMatNameLeft;
    property MatNameRight: TVKLibMaterialName read FMatNameRight write
      SetMatNameRight;
    property MatNameFront: TVKLibMaterialName read FMatNameFront write
      SetMatNameFront;
    property MatNameBack: TVKLibMaterialName read FMatNameBack write
      SetMatNameBack;
    property MatNameClouds: TVKLibMaterialName read FMatNameClouds write
      SetMatNameClouds;
    property CloudsPlaneOffset: Single read FCloudsPlaneOffset write
      SetCloudsPlaneOffset;
    property CloudsPlaneSize: Single read FCloudsPlaneSize write
      SetCloudsPlaneSize;
    property Style: TVKSkyBoxStyle read FStyle write FStyle default sbsFull;
  end;

//===================================================================
implementation
//===================================================================

uses
  VKS.Context,
  VKS.State;

// ------------------
// ------------------ TVKSkyBox ------------------
// ------------------

constructor TVKSkyBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CamInvarianceMode := cimPosition;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FCloudsPlaneOffset := 0.2;
    // this should be set far enough to avoid near plane clipping
  FCloudsPlaneSize := 32;
    // the bigger, the more this extends the clouds cap to the horizon
end;

destructor TVKSkyBox.Destroy;
begin
  inherited;
end;

function TVKSkyBox.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVKSkyBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMaterialLibrary) then
    MaterialLibrary := nil;
  inherited;
end;

// DoRender
//

procedure TVKSkyBox.DoRender(var ARci: TVKRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
begin
  // We want children of the sky box to appear far away too
  // (note: simply not writing to depth buffer may not make this not work,
  //  child objects may need the depth buffer to render themselves properly,
  //  this may require depth buffer cleared after that. - DanB)
  Arci.VKStates.DepthWriteMask := 0;
  Arci.ignoreDepthRequests := true;
  inherited;
  Arci.ignoreDepthRequests := False;
end;
// DoRender
//

procedure TVKSkyBox.BuildList(var ARci: TVKRenderContextInfo);
var
  f, cps, cof1: Single;
  oldStates: TVKStates;
  libMat: TVKLibMaterial;
begin
  if FMaterialLibrary = nil then
    Exit;

  with ARci.VKStates do
  begin
    oldStates := States;
    Disable(stDepthTest);
    Disable(stLighting);
    Disable(stFog);
  end;

  glPushMatrix;
  f := ARci.rcci.farClippingDistance * 0.5;
  glScalef(f, f, f);

  try
    case Style of
      sbsFull: ;
      sbsTopHalf, sbsTopHalfClamped:
        begin
          glTranslatef(0, 0.5, 0);
          glScalef(1, 0.5, 1);
        end;
      sbsBottomHalf:
        begin
          glTranslatef(0, -0.5, 0);
          glScalef(1, 0.5, 1);
        end;
      sbTopTwoThirds:
        begin
          glTranslatef(0, 1 / 3, 0);
          glScalef(1, 2 / 3, 1);
        end;
    end;

    // FRONT
    libMat := MaterialLibrary.LibMaterialByName(FMatNameFront);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(-1, 1, -1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(-1, -1, -1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(1, -1, -1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(1, 1, -1);
        if Style = sbsTopHalfClamped then
        begin
          glTexCoord2f(0.002, 0.002);
          glVertex3f(-1, -1, -1);
          glTexCoord2f(0.002, 0.002);
          glVertex3f(-1, -3, -1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(1, -3, -1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(1, -1, -1);
        end;
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // BACK
    libMat := MaterialLibrary.LibMaterialByName(FMatNameBack);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(1, 1, 1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(1, -1, 1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(-1, -1, 1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(-1, 1, 1);
        if Style = sbsTopHalfClamped then
        begin
          glTexCoord2f(0.002, 0.002);
          glVertex3f(1, -1, 1);
          glTexCoord2f(0.002, 0.002);
          glVertex3f(1, -3, 1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(-1, -3, 1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(-1, -1, 1);
        end;
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // TOP
    libMat := MaterialLibrary.LibMaterialByName(FMatNameTop);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(-1, 1, 1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(-1, 1, -1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(1, 1, -1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(1, 1, 1);
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // BOTTOM
    libMat := MaterialLibrary.LibMaterialByName(FMatNameBottom);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(-1, -1, -1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(-1, -1, 1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(1, -1, 1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(1, -1, -1);
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // LEFT
    libMat := MaterialLibrary.LibMaterialByName(FMatNameLeft);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(-1, 1, 1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(-1, -1, 1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(-1, -1, -1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(-1, 1, -1);
        if Style = sbsTopHalfClamped then
        begin
          glTexCoord2f(0.002, 0.002);
          glVertex3f(-1, -1, 1);
          glTexCoord2f(0.002, 0.002);
          glVertex3f(-1, -3, 1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(-1, -3, -1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(-1, -1, -1);
        end;
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // RIGHT
    libMat := MaterialLibrary.LibMaterialByName(FMatNameRight);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(1, 1, -1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(1, -1, -1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(1, -1, 1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(1, 1, 1);
        if Style = sbsTopHalfClamped then
        begin
          glTexCoord2f(0.002, 0.002);
          glVertex3f(1, -1, -1);
          glTexCoord2f(0.002, 0.002);
          glVertex3f(1, -3, -1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(1, -3, 1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(1, -1, 1);
        end;
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // CLOUDS CAP PLANE
    libMat := MaterialLibrary.LibMaterialByName(FMatNameClouds);
    if libMat <> nil then
    begin
      // pre-calculate possible values to speed up
      cps := FCloudsPlaneSize * 0.5;
      cof1 := FCloudsPlaneOffset;

      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0, 1);
        glVertex3f(-cps, cof1, cps);
        glTexCoord2f(0, 0);
        glVertex3f(-cps, cof1, -cps);
        glTexCoord2f(1, 0);
        glVertex3f(cps, cof1, -cps);
        glTexCoord2f(1, 1);
        glVertex3f(cps, cof1, cps);
        glEnd;
      until not libMat.UnApply(ARci);
    end;

    glPopMatrix;

    if stLighting in oldStates then
      ARci.VKStates.Enable(stLighting);
    if stFog in oldStates then
      ARci.VKStates.Enable(stFog);
    if stDepthTest in oldStates then
      ARci.VKStates.Enable(stDepthTest);

  finally
  end;
end;

procedure TVKSkyBox.SetCloudsPlaneOffset(const Value: single);
begin
  FCloudsPlaneOffset := Value;
  StructureChanged;
end;

procedure TVKSkyBox.SetCloudsPlaneSize(const Value: single);
begin
  FCloudsPlaneSize := Value;
  StructureChanged;
end;

// SetStyle
//

procedure TVKSkyBox.SetStyle(const value: TVKSkyBoxStyle);
begin
  FStyle := value;
  StructureChanged;
end;

// SetMaterialLibrary
//

procedure TVKSkyBox.SetMaterialLibrary(const value: TVKMaterialLibrary);
begin
  FMaterialLibrary := value;
  StructureChanged;
end;

procedure TVKSkyBox.SetMatNameBack(const Value: string);
begin
  FMatNameBack := Value;
  StructureChanged;
end;

procedure TVKSkyBox.SetMatNameBottom(const Value: string);
begin
  FMatNameBottom := Value;
  StructureChanged;
end;

procedure TVKSkyBox.SetMatNameClouds(const Value: string);
begin
  FMatNameClouds := Value;
  StructureChanged;
end;

procedure TVKSkyBox.SetMatNameFront(const Value: string);
begin
  FMatNameFront := Value;
  StructureChanged;
end;

procedure TVKSkyBox.SetMatNameLeft(const Value: string);
begin
  FMatNameLeft := Value;
  StructureChanged;
end;

procedure TVKSkyBox.SetMatNameRight(const Value: string);
begin
  FMatNameRight := Value;
  StructureChanged;
end;

procedure TVKSkyBox.SetMatNameTop(const Value: string);
begin
  FMatNameTop := Value;
  StructureChanged;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClass(TVKSkyBox);

end.


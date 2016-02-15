//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   Implements a basic, stencil-based mirror (as in Mark Kilgard's demo). 

   It is strongly recommended to read and understand the explanations in the
   materials/mirror demo before using this component. 
   
}
unit GLS.Mirror;

interface

{$I GLScene.inc}

uses
  System.Classes,
  GLS.Scene, GLS.VectorGeometry, GLS.OpenGLAdapter, GLS.OpenGLTokens,
  GLS.Context, GLS.Material, GLS.Color, GLS.RenderContextInfo,
  GLS.State, GLS.VectorTypes;


type

  // TMirrorOptions
  //
  TMirrorOption = (moUseStencil, moOpaque, moMirrorPlaneClip, moClearZBuffer);
  TMirrorOptions = set of TMirrorOption;

const
  cDefaultMirrorOptions = [moUseStencil];

type

  // TMirrorShapes  ORL
  TMirrorShapes = (msRect, msDisk);

  // TVKMirror
  //
  { A simple plane mirror. 
     This mirror requires a stencil buffer for optimal rendering! 
     The object is a mix between a plane and a proxy object, in that the plane
     defines the mirror's surface, while the proxy part is used to reference
     the objects that should be mirrored (it is legal to self-mirror, but no
     self-mirror visuals will be rendered). 
     It is strongly recommended to read and understand the explanations in the
     materials/mirror demo before using this component. }
  TVKMirror = class(TVKSceneObject)
  private
    { Private Declarations }
    FRendering: Boolean;
    FMirrorObject: TVKBaseSceneObject;
    FWidth, FHeight: GLfloat;
    FMirrorOptions: TMirrorOptions;
    FOnBeginRenderingMirrors, FOnEndRenderingMirrors: TNotifyEvent;

    FShape: TMirrorShapes; //ORL
    FRadius: GLfloat; //ORL
    FSlices: GLint; //ORL

  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetMirrorObject(const val: TVKBaseSceneObject);
    procedure SetMirrorOptions(const val: TMirrorOptions);
    procedure ClearZBufferArea(aBuffer: TVKSceneBuffer);

    procedure SetHeight(AValue: GLfloat);
    procedure SetWidth(AValue: GLfloat);

    procedure SetRadius(const aValue: Single); //ORL
    procedure SetSlices(const aValue: GLint); //ORL
    procedure SetShape(aValue: TMirrorShapes); //ORL
    function GetRadius: single; //ORL
    function GetSlices: GLint; //ORL

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var ARci: TVKRenderContextInfo); override;

    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;

  published
    { Public Declarations }
          { Selects the object to mirror. 
             If nil, the whole scene is mirrored. }
    property MirrorObject: TVKBaseSceneObject read FMirrorObject write
      SetMirrorObject;
    { Controls rendering options. 
        
        moUseStencil: mirror area is stenciled, prevents reflected
          objects to be visible on the sides of the mirror (stencil buffer
          must be active in the viewer)
        moOpaque: mirror is opaque (ie. painted with background color)
        moMirrorPlaneClip: a ClipPlane is defined to prevent reflections
          from popping out of the mirror (for objects behind or halfway through)
        moClearZBuffer: mirror area's ZBuffer is cleared so that background
          objects don't interfere with reflected objects (reflected objects
          must be rendered AFTER the mirror in the hierarchy). Works only
          along with stenciling.
        
    }
    property MirrorOptions: TMirrorOptions read FMirrorOptions write
      SetMirrorOptions default cDefaultMirrorOptions;

    property Height: GLfloat read FHeight write SetHeight;
    property Width: GLfloat read FWidth write SetWidth;

    { Fired before the object's mirror images are rendered. }
    property OnBeginRenderingMirrors: TNotifyEvent read FOnBeginRenderingMirrors
      write FOnBeginRenderingMirrors;
    { Fired after the object's mirror images are rendered. }
    property OnEndRenderingMirrors: TNotifyEvent read FOnEndRenderingMirrors
      write FOnEndRenderingMirrors;

    property Radius: GLfloat read FRadius write SetRadius; //ORL
    property Slices: GLint read FSlices write SetSlices default 16; //ORL
    property Shape: TMirrorShapes read FShape write SetShape default msRect;
    //ORL
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKMirror ------------------
// ------------------

// Create
//

constructor TVKMirror.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FWidth := 1;
  FHeight := 1;
  FMirrorOptions := cDefaultMirrorOptions;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  Material.FrontProperties.Diffuse.Initialize(VectorMake(1, 1, 1, 0.1));
  Material.BlendingMode := bmTransparency;

  FRadius := 1; //ORL
  FSlices := 16; //ORL
  Shape := msRect; //ORL
end;

// DoRender
//

procedure TVKMirror.DoRender(var ARci: TVKRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  oldProxySubObject: Boolean;
  refMat, curMat, ModelMat: TMatrix;
  clipPlane: TDoubleHmgPlane;
  bgColor: TColorVector;
  cameraPosBackup, cameraDirectionBackup: TVector;
  CurrentBuffer: TVKSceneBuffer;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    oldProxySubObject := ARci.proxySubObject;
    ARci.proxySubObject := True;
    CurrentBuffer := TVKSceneBuffer(ARci.buffer);

    if VectorDotProduct(VectorSubtract(ARci.cameraPosition, AbsolutePosition),
      AbsoluteDirection) > 0 then
      with ARci.GLStates do
      begin

        // "Render" stencil mask
        if MirrorOptions <> [] then
        begin
          if (moUseStencil in MirrorOptions) then
          begin
            Enable(stStencilTest);
            ARci.GLStates.StencilClearValue := 0;
            glClear(GL_STENCIL_BUFFER_BIT);
            SetStencilFunc(cfAlways, 1, 1);
            SetStencilOp(soReplace, soZero, soReplace);
          end;
          if (moOpaque in MirrorOptions) then
          begin
            bgColor := ConvertWinColor(CurrentBuffer.BackgroundColor);
            ARci.GLStates.SetGLMaterialColors(cmFront, bgColor, clrBlack,
              clrBlack, clrBlack, 0);
          end
          else
            SetGLColorWriting(False);

          Enable(stDepthTest);
          DepthWriteMask := False;

          BuildList(ARci);

          DepthWriteMask := True;
          if (moUseStencil in MirrorOptions) then
          begin
            SetStencilFunc(cfEqual, 1, 1);
            SetStencilOp(soKeep, soKeep, soKeep);
          end;

          if (moClearZBuffer in MirrorOptions) then
            ClearZBufferArea(CurrentBuffer);

          if not (moOpaque in MirrorOptions) then
            SetGLColorWriting(True);
        end;

        ARci.PipelineTransformation.Push;
        ARci.PipelineTransformation.ModelMatrix := IdentityHmgMatrix;

        Disable(stCullFace);
        Enable(stNormalize);

        if moMirrorPlaneClip in MirrorOptions then
        begin
          glEnable(GL_CLIP_PLANE0);
          SetPlane(clipPlane, PlaneMake(AffineVectorMake(AbsolutePosition),
            VectorNegate(AffineVectorMake(AbsoluteDirection))));
          GL.ClipPlane(GL_CLIP_PLANE0, @clipPlane);
        end;

        // Mirror lights
        refMat := MakeReflectionMatrix(
          AffineVectorMake(AbsolutePosition),
          AffineVectorMake(AbsoluteDirection));
        curMat := MatrixMultiply(refMat, ARci.PipelineTransformation.ViewMatrix);
        ARci.PipelineTransformation.ViewMatrix := curMat;
        Scene.SetupLights(CurrentBuffer.LimitOf[limLights]);

        // mirror geometry and render master
        cameraPosBackup := ARci.cameraPosition;
        cameraDirectionBackup := ARci.cameraDirection;
        ARci.cameraPosition := VectorTransform(ARci.cameraPosition, refMat);
        ARci.cameraDirection := VectorTransform(ARci.cameraDirection, refMat);

        // temporary fix? (some objects don't respect culling options, or ?)
        CullFaceMode := cmFront;
        if Assigned(FOnBeginRenderingMirrors) then
          FOnBeginRenderingMirrors(Self);
        if Assigned(FMirrorObject) then
        begin
          ModelMat := IdentityHmgMatrix;
          if FMirrorObject.Parent <> nil then
            MatrixMultiply(ModelMat, FMirrorObject.Parent.AbsoluteMatrix, ModelMat);
          MatrixMultiply(ModelMat, FMirrorObject.LocalMatrix^, ModelMat);
          ARci.PipelineTransformation.ModelMatrix := ModelMat;
          FMirrorObject.DoRender(ARci, ARenderSelf, FMirrorObject.Count > 0);
        end
        else
        begin
          Scene.Objects.DoRender(ARci, ARenderSelf, True);
        end;
        if Assigned(FOnEndRenderingMirrors) then
          FOnEndRenderingMirrors(Self);

        // Restore to "normal"
        ARci.cameraPosition := cameraPosBackup;
        ARci.cameraDirection := cameraDirectionBackup;
        ARci.GLStates.CullFaceMode := cmBack;
        ARci.PipelineTransformation.ReplaceFromStack;
        Scene.SetupLights(CurrentBuffer.LimitOf[limLights]);
        ARci.PipelineTransformation.Pop;
        if moMirrorPlaneClip in MirrorOptions then
          glDisable(GL_CLIP_PLANE0);
        ARci.GLStates.Disable(stStencilTest);

        ARci.proxySubObject := oldProxySubObject;

        // start rendering self
        if ARenderSelf then
        begin
          Material.Apply(ARci);
          repeat
            BuildList(ARci);
          until not Material.UnApply(ARci);
        end;

      end;

    if ARenderChildren then
      Self.RenderChildren(0, Count - 1, ARci);

    if Assigned(FMirrorObject) then
      FMirrorObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
end;

// BuildList
//

procedure TVKMirror.BuildList(var ARci: TVKRenderContextInfo);
var
  hw, hh: GLfloat;
  quadric: PGLUquadricObj;
begin
  if msRect = FShape then
  begin
    hw := FWidth * 0.5;
    hh := FHeight * 0.5;
    GL.Normal3fv(@ZVector);
    glBegin(GL_QUADS);
    glVertex3f(hw, hh, 0);
    glVertex3f(-hw, hh, 0);
    glVertex3f(-hw, -hh, 0);
    glVertex3f(hw, -hh, 0);
    glEnd;
  end
  else
  begin
    quadric := gluNewQuadric;
    gluDisk(Quadric, 0, FRadius, FSlices, 1); //radius. slices, loops
  end;
end;

// BuildList
//

procedure TVKMirror.ClearZBufferArea(aBuffer: TVKSceneBuffer);
var
  worldMat: TMatrix;
  p: TAffineVector;
begin
  with aBuffer do
  begin
    glPushMatrix;
    worldMat := Self.AbsoluteMatrix;
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    GL.Ortho(0, Width, 0, Height, 1, -1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;

    with aBuffer.RenderingContext.GLStates do
    begin
      DepthFunc := cfAlways;
      SetGLColorWriting(False);
    end;

    glBegin(GL_QUADS);
    p := WorldToScreen(VectorTransform(AffineVectorMake(Self.Width * 0.5,
      Self.Height * 0.5, 0), worldMat));
    glVertex3f(p.V[0], p.V[1], 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width * 0.5,
      Self.Height * 0.5, 0), worldMat));
    glVertex3f(p.V[0], p.V[1], 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width * 0.5,
      -Self.Height * 0.5, 0), worldMat));
    glVertex3f(p.V[0], p.V[1], 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(Self.Width * 0.5,
      -Self.Height * 0.5, 0), worldMat));
    glVertex3f(p.V[0], p.V[1], 0.999);
    glEnd;

    with aBuffer.RenderingContext.GLStates do
    begin
      DepthFunc := cfLess;
      SetGLColorWriting(True);
    end;

    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  end;
end;

// Notification
//

procedure TVKMirror.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMirrorObject) then
    MirrorObject := nil;
  inherited;
end;

// SetMirrorObject
//

procedure TVKMirror.SetMirrorObject(const val: TVKBaseSceneObject);
begin
  if FMirrorObject <> val then
  begin
    if Assigned(FMirrorObject) then
      FMirrorObject.RemoveFreeNotification(Self);
    FMirrorObject := val;
    if Assigned(FMirrorObject) then
      FMirrorObject.FreeNotification(Self);
    NotifyChange(Self);
  end;
end;

// SetWidth
//

procedure TVKMirror.SetWidth(AValue: GLfloat);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    NotifyChange(Self);
  end;
end;

// SetHeight
//

procedure TVKMirror.SetHeight(AValue: GLfloat);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    NotifyChange(Self);
  end;
end;

// Assign
//

procedure TVKMirror.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKMirror) then
  begin
    FWidth := TVKMirror(Source).FWidth;
    FHeight := TVKMirror(Source).FHeight;
    FMirrorOptions := TVKMirror(Source).FMirrorOptions;
    MirrorObject := TVKMirror(Source).MirrorObject;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TVKMirror.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result := VectorMake(0.5 * Abs(FWidth),
    0.5 * Abs(FHeight), 0);
end;

// SetMirrorOptions
//

procedure TVKMirror.SetMirrorOptions(const val: TMirrorOptions);
begin
  if FMirrorOptions <> val then
  begin
    FMirrorOptions := val;
    NotifyChange(Self);
  end;
end;

//ORL add-ons

// SetRadius
//

procedure TVKMirror.SetRadius(const aValue: Single);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// GetRadius
//

function TVKMirror.GetRadius: single;
begin
  result := FRadius;
end;

// SetSlices
//

procedure TVKMirror.SetSlices(const aValue: GLint);
begin
  if aValue <> FSlices then
  begin
    if aValue > 2 then
      FSlices := aValue;
    StructureChanged;
  end
  else
  begin
  end;
end;

// GetSlices
//

function TVKMirror.GetSlices: GLint;
begin
  result := FSlices;
end;

// SetShape
//

procedure TVKMirror.SetShape(aValue: TMirrorShapes);
begin
  if aValue <> FShape then
  begin
    FShape := aValue;
    StructureChanged;
  end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TVKMirror]);

end.


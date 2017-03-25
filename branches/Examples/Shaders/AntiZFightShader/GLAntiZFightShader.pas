unit GLAntiZFightShader;

{    TGLAntiZFightShader

  Jason Bell, with lots of help from Stuart Gooding
  August 10, 2003

  Adjusts the projection matrix prior to rendering to minimize/eliminate
  ZBuffer fighting.  The user must provide a value to the modifier
  property; this adjusts the calculated camera distance to compensate for
  the scale/dimensions of the scene.  It's a crude fix for now :)
  Basically, you'll want to use a large value for a large scene and vice
  versa.

}

interface

uses
  System.Classes,

  OpenGl1x,
  OpenGLTokens,
  GLScene,
  GLTexture,
  GLVectorGeometry,
  GLRenderContextInfo,
  GLMaterial;

type

  TGLAntiZFightShader = class(TGLShader)
  private
    REnabled: boolean;
    RModifier: double;
  public
    property Enabled: boolean read REnabled write REnabled;
    property Modifier: double read RModifier write RModifier;
    constructor Create(aOwner: TComponent); override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): boolean; override;
  end;

implementation

constructor TGLAntiZFightShader.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  REnabled := true;
  RModifier := 1;
end;

procedure TGLAntiZFightShader.DoApply(var rci: TGLRenderContextInfo;
  Sender: TObject);
var
  Left, Right, Top, Bottom, zFar, MaxDim, Ratio, f, nearplane, farplane: double;

  CameraDistance: double;

  NewSceneScale, NewFocalLength: single;
  mat: TMatrix;
begin
  if REnabled then
  begin
    glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;

    with rci.viewPortSize do
    begin
      CameraDistance := vectordistance(mat.W, TGLScene(rci.scene)
        .CurrentGLCamera.AbsolutePosition);

      CameraDistance := CameraDistance * (CameraDistance / RModifier);

      if CameraDistance > 0 then
      begin
        NewFocalLength := TGLScene(rci.scene).CurrentGLCamera.focallength *
          CameraDistance;
        NewSceneScale := TGLScene(rci.scene).CurrentGLCamera.scenescale /
          CameraDistance;
      end
      else
      begin
        NewFocalLength := TGLScene(rci.scene).CurrentGLCamera.focallength;
        NewSceneScale := TGLScene(rci.scene).CurrentGLCamera.scenescale;
        CameraDistance := 0;
      end;

      MaxDim := cx;
      if cy > MaxDim then
        MaxDim := cy;
      f := TGLScene(rci.scene).CurrentGLCamera.NearPlaneBias /
        (cx * NewSceneScale);

      Ratio := (2 * cx + 2 * 0 - cx) * f;
      Right := Ratio * cx / (2 * MaxDim);
      Ratio := (cx - 2 * 0) * f;
      Left := -Ratio * cx / (2 * MaxDim);
      f := 1 / (cy * NewSceneScale);
      Ratio := (2 * cy + 2 * 0 - cy) * f;
      Top := Ratio * cy / (2 * MaxDim);
      Ratio := (cy - 2 * 0) * f;
      Bottom := -Ratio * cy / (2 * MaxDim);
      nearplane := (NewFocalLength * 2 * rci.renderdpi / (25.4 * MaxDim) *
        TGLScene(rci.scene).CurrentGLCamera.NearPlaneBias);

      farplane := nearplane + TGLScene(rci.scene).CurrentGLCamera.DepthofView;
      glLoadIdentity();
      glFrustum(Left, Right, Bottom, Top, nearplane, farplane);
      glMatrixMode(GL_MODELVIEW);
    end;
  end;
end;

function TGLAntiZFightShader.DoUnApply(var rci: TGLRenderContextInfo): boolean;
begin
  if REnabled then
  begin
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
  end;
  result := false;
end;

end.

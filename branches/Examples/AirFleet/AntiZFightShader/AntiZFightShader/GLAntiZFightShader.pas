unit GLAntiZFightShader;

{
        TGLAntiZFightShader

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
  Classes,
   
  GLScene, GLTexture, OpenGl1x, GLVectorGeometry,
  GLRenderContextInfo, GLMaterial, OpenGLTokens;

type

        TGLAntiZFightShader = class(TGLShader)
                private
                     REnabled : boolean;
                     RModifier : double;
                public
                     Property Enabled : boolean read REnabled write REnabled;
                     Property Modifier : double read RModifier write RModifier;

                     constructor Create(aOwner : TComponent); override;

                     Procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
                     Function DoUnApply(var rci : TGLRenderContextInfo) : Boolean; override;
        end;

implementation

constructor TGLAntiZFightShader.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

   REnabled := true;
   RModifier := 1;
end;

procedure TGLAntiZFightShader.DoApply(var rci : TGLRenderContextInfo; Sender : TObject);
var
   Left, Right, Top, Bottom, zFar, MaxDim, Ratio, f, nearplane,farplane: Double;

   CameraDistance : double;

   NewSceneScale, NewFocalLength : single;
   mat : TMatrix;
begin
        if REnabled then
        begin

        glGetFloatv(GL_MODELVIEW_MATRIX,@mat);

        glMatrixMode(GL_PROJECTION);
        glPushMatrix;

        with rci.viewPortSize do
        begin
                CameraDistance := vectordistance(mat.W,
                        TGLScene(rci.scene).CurrentGLCamera.AbsolutePosition);

                CameraDistance := CameraDistance*(CameraDistance/RModifier);

                if cameradistance > 0 then
                begin
                        NewFocalLength := TGLScene(rci.scene).CurrentGLCamera.focallength*cameradistance;
                        NewSceneScale := TGLScene(rci.scene).CurrentGLCamera.scenescale/cameradistance;
                end
                else
                begin
                        NewFocalLength := TGLScene(rci.scene).CurrentGLCamera.focallength;
                        NewSceneScale := TGLScene(rci.scene).CurrentGLCamera.scenescale;
                        cameradistance := 0;
                end;

                MaxDim:=cx;
                if cy > MaxDim then
                        MaxDim:=cy;
                f:=TGLScene(rci.scene).CurrentGLCamera.NearPlaneBias/(cx*NewSceneScale);

                Ratio:=(2 * cx + 2 * 0 - cx) * f;
                Right:=Ratio * cx / (2 * MaxDim);
                Ratio:=(cx - 2 * 0) * f;
                Left:=-Ratio * cx / (2 * MaxDim);
                f:=1/(cy*NewSceneScale);
                Ratio:=(2 * cy + 2 * 0 - cy) * f;
                Top:=Ratio * cy / (2 * MaxDim);
                Ratio:=(cy - 2 * 0) * f;
                Bottom:=-Ratio * cy / (2 * MaxDim);
                NearPlane:=(NewFocalLength * 2 * rci.renderdpi / (25.4 * MaxDim)
                        *TGLScene(rci.scene).CurrentGLCamera.NearPlaneBias);

                Farplane:=NearPlane + TGLScene(rci.scene).CurrentGLCamera.DepthofView;


                glLoadIdentity();
                glFrustum(Left, Right, Bottom, Top, NearPlane, Farplane);

                glMatrixMode(GL_MODELVIEW);
        end;
        end;
end;

function TGLAntiZFightShader.DoUnApply(var rci : TGLRenderContextInfo) : Boolean;
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
 
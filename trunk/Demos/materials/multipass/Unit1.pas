{: Simple TGLShader based multipass demo.<p>

   This demo uses a custom TGLShader subclass to implement the classic
   multipass hidden lines rendering technique: first pass renders model
   with filled triangles, second pass does the wireframe.<p>

   You'll also note the glPolygonOffset call, it displaces fragments depths
   value a little "farther away" so that surface fill depth values do not
   interact with the rendering of the lines (comment out the call and you'll
   see).<br>
   The axis and sphere allow you to see the limit of that simple technique:
   it actually "paints" between the lines, so you cannot use it to make
   transparent wireframed objects with hidden lines - if that thought ever
   blossomed in your mind ;)
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLWin32Viewer, GLMisc, GLTexture, OpenGL12,
  StdCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Torus1: TGLTorus;
    BUBind: TButton;
    Sphere1: TGLSphere;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure BUBindClick(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type

   THiddenLineShader = class (TGLShader)
      private
         BackgroundColor, LineColor : TColorVector;
         PassCount : Integer;
      public
         procedure DoApply(var rci : TRenderContextInfo); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
   end;

procedure THiddenLineShader.DoApply(var rci : TRenderContextInfo);
begin
   // new object getting rendered, 1st pass
   PassCount:=1;

   // backup state
   glPushAttrib(GL_ENABLE_BIT);
   // disable lighting, this is a solid fill
   glDisable(GL_LIGHTING);
   SetGLPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
   // use background color
   glColor3fv(@BackgroundColor);
   // enable and adjust polygon offset
   glEnable(GL_POLYGON_OFFSET_FILL);
   glPolygonOffset(1, 2);
end;

function THiddenLineShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
   case PassCount of
      1 : begin
         // 1st pass completed, we setup for the second
         PassCount:=2;

         // switch to wireframe and its color
         SetGLPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
         glColor3fv(@LineColor);
         // disable polygon offset
         glDisable(GL_POLYGON_OFFSET_LINE);

         Result:=True;
      end;
      2 : begin
         // restore state
         glPopAttrib;

         // we're done
         Result:=False;
      end;
   else
      // doesn't hurt to be cautious
      Assert(False);
      Result:=False;
   end;
end;

procedure TForm1.BUBindClick(Sender: TObject);
var
   shader : THiddenLineShader;
begin
   BUBind.Enabled:=False;

   // instantiates our shader, adjusts colors
   shader:=THiddenLineShader.Create(Self);
   shader.BackgroundColor:=ConvertWinColor(GLSceneViewer1.Buffer.BackgroundColor);
   shader.LineColor:=clrBlue;

   // binds the shader to the 1st material (the one used by the torus)
   GLMaterialLibrary1.Materials[0].Shader:=shader;
end;

//
// Classic mouse movement bits
//

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift=[ssLeft] then
      GLCamera1.MoveAroundTarget(my-y, mx-x)
   else if Shift=[ssRight] then
      GLCamera1.RotateTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

end.

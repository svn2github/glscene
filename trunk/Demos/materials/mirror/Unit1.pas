{: GLMirror demo and sample.<p>

   Depistes its simplistic look, this sample showcases all the dos and don'ts
   of reflections with TGLMirror, this is a powerfull mirroring component,
   but it must be handled with care and knowingly.<p>
   The object that must be mirrored is specified through the "MirrorObject"
   property, you can specify only one, but with proper use of dummy cubes and/or
   proxies, this is not a real limitation, and allows you to select what will be
   reflected (each reflected object must be rendered twice). If no MirrorObject
   is specified, the whole scene will be mirrored.<p>

   First, make sure to have a look at the hierarchy, it is not random: first
   non reflected objects, then mirror, and finally reflecting objects. This
   order must be respected if you want your mirror to be transparent, otherwise,
   you have more flexibility in the scene organization (reordering stuff and
   seeing for yourself is recommended).<br>
   Also note that some of the options (stenciling)
   <b>require</b> a stencil buffer (must be enabled in the viewer's buffer),
   stenciling may not always be hardware accelerated.<p>

   There is also a variety of settings to the right of the screen, those adjust
   internal options that have a direct impact of what the mirror will be able
   to do right, and how much time rendering will take. The scene contains three
   groups of objects of interest:<ul>
   <li>non-reflecting ones: green torus and cylinder, these are behind the mirror,
      there are no particular issues with non-reflecting objects in front of a
      mirror (except objects not reflecting...), but by playing with the settings
      (ClearZBuffer especially) you'll notice they cause some artifacts if improperly
      handle.
   <li>teapot group those are reflected. See how disabling stencil will cause
      the reflected teapot to be visible outside of the mirror. If your mirror
      is in wall (opaque on all sides around the mirror), you may not care about
      stenciling.
   <li>lone inclined gray cylinder: this one is a don't, it's an object that is
      reflecting but that goes through the mirror. If your mirror is transparent,
      that object's part that is on the other side of the mirror will not be visible
      (if you want it visible, duplicate it as second non-reflecting object).
      It also allows to show what happens if PlaneClip is not enabled and you
      have reflecting objects on the other side of the mirror: they get reflected
      on the "wrong" side... so PlaneClip, or better avoid (PlaneClip can make
      your FPS drop on some hardware).
   </ul><p>

   In addition to being opaque, transparent or semi-transparent, the mirror
   can also be textured as usual.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLMisc, GLExtrusion, GLMirror, GLMultiPolygon,
  ExtCtrls, GLCadencer, StdCtrls;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Sphere1: TSphere;
    ReflectingObjects: TDummyCube;
    NonReflectingStuff: TTorus;
    Teapot1: TTeapot;
    Cylinder1: TCylinder;
    GLMirror: TGLMirror;
    ExtrusionSolid1: TExtrusionSolid;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Label1: TLabel;
    CBOpaque: TCheckBox;
    CBStencil: TCheckBox;
    Cylinder2: TCylinder;
    CBClearZ: TCheckBox;
    Cylinder3: TCylinder;
    CBPlaneClip: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CBOpaqueClick(Sender: TObject);
    procedure CBStencilClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBClearZClick(Sender: TObject);
    procedure CBPlaneClipClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
   GLMirror.MirrorOptions:=GLMirror.MirrorOptions+[moClearZBuffer, moMirrorPlaneClip];
end;

procedure TForm1.CBOpaqueClick(Sender: TObject);
begin
   if CBOpaque.Checked then
      GLMirror.MirrorOptions:=GLMirror.MirrorOptions+[moOpaque]
   else GLMirror.MirrorOptions:=GLMirror.MirrorOptions-[moOpaque];
end;

procedure TForm1.CBStencilClick(Sender: TObject);
begin
   if CBStencil.Checked then
      GLMirror.MirrorOptions:=GLMirror.MirrorOptions+[moUseStencil]
   else GLMirror.MirrorOptions:=GLMirror.MirrorOptions-[moUseStencil];
end;

procedure TForm1.CBClearZClick(Sender: TObject);
begin
   if CBClearZ.Checked then
      GLMirror.MirrorOptions:=GLMirror.MirrorOptions+[moClearZBuffer]
   else GLMirror.MirrorOptions:=GLMirror.MirrorOptions-[moClearZBuffer];
end;

procedure TForm1.CBPlaneClipClick(Sender: TObject);
begin
   if CBPlaneClip.Checked then
      GLMirror.MirrorOptions:=GLMirror.MirrorOptions+[moMirrorPlaneClip]
   else GLMirror.MirrorOptions:=GLMirror.MirrorOptions-[moMirrorPlaneClip];
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   GLCamera1.SceneScale:=GLSceneViewer1.Width/380;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

end.

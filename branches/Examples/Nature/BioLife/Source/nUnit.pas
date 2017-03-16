unit nUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  GLScene, GLObjects, GLMisc, ExtCtrls, GLWin32Viewer;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    Panel1: TPanel;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
{Direction  0 0 1        0 1 0
Position    0 0 0        0 0 0
Scale       1 1 1
UP          0 1 0        0 0 1
             Y Up=2D      Z Up=3D}
end.

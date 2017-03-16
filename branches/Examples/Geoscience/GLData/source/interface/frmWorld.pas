{-------------------------------------------------------------------------------
 Unit Name: frmWorld
 Author:    HochwimmerA
 Purpose:   Eyecandy for showing the gldata world :)
 $Id: frmWorld.pas,v 1.2 2003/09/16 05:27:36 hochwimmera Exp $
-------------------------------------------------------------------------------}
unit frmWorld;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ToolWin,
  Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ComCtrls,
  GLWin32Viewer, GLScene, GLObjects, GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TformWorld = class(TForm)
    GLSceneWorld: TGLScene;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    glPlanet: TGLSphere;
    Sun: TGLLightSource;
    pcWorld: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GLSceneViewer1: TGLSceneViewer;
    ActionToolBar1: TActionToolBar;
  private
     
  public
     
  end;

var
  formWorld: TformWorld;

implementation

{$R *.dfm}

end.

//
// This unit is part of the GLScene Project, http://glscene.org
//
{: FMaterialPreview<p>

   Material Preview frame.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>14/07/23 - PW - Changed controls to support FireMonkey platform
      <li>12/07/07 - DaStr - Improved Cross-Platform compatibility
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>16/12/06 - DaStr - Editor enhanced
      <li>03/07/04 - LR  - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit GLX.FMaterialPreview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox,

  //GLS
  //Need to be changed with GLS.* units
  GLScene.FMX.Viewer,
  GLMaterial, GLCrossPlatform, BaseClasses, GLScene, GLTeapot,
  GLGeomObjects, GLObjects, GLHUDObjects, GLCoordinates;

type
  TFrame2 = class(TFrame)
    CBObject: TComboBox;
    CBBackground: TComboBox;
    GLScene: TGLScene;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLSceneViewport: TGLSceneViewport;
    GLCamera: TGLCamera;
    BackgroundSprite: TGLHUDSprite;
    LightSource: TGLLightSource;
    World: TGLDummyCube;
    Light: TGLDummyCube;
    Cube: TGLCube;
    Sphere: TGLSphere;
    Cone: TGLCone;
    Teapot: TGLTeapot;
    FireSphere: TGLSphere;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.

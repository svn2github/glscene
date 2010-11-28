unit Unit1;

{$MODE Delphi}

{: Newton Game Dynamics Physics Engine demo.<p>

  This demo demontrate how to use material effect of newton.
  Manager own pair of material list where we can adjust elasticity or friction
  between two materials. We select material group id for each NGDBehaviors, and
  in material pair implementation, we choose the two group-id wich perform these
  effects.

  We are also able to see id on screen because ShowMaterialESP in True in manager.

  <b>History : </b><font size=-1><ul>
      <li>17/09/10 - FP - Created by Franck Papouin
  </ul>
}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLSimpleNavigation, GLScene, GLCoordinates, GLNGDManager,
  GLBitmapFont, GLWindowsFont, GLCadencer, GLViewer, GLCrossPlatform,
  BaseClasses, GLObjects;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLStoredBitmapFont1: TGLStoredBitmapFont;
    GLNGDManager1: TGLNGDManager;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLRenderPoint1: TGLRenderPoint;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Trampoline: TGLCube;
    GLCube1: TGLCube;
    GLSphere1: TGLSphere;
    GLSphere2: TGLSphere;
    GLDummyCube1: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    GLCube2: TGLCube;
    Friction: TGLCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    GLDummyCube3: TGLDummyCube;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLNGDManager1.Step(deltaTime);
end;

end.


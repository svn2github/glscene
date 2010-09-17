Unit Unit1;

{: Newton Game Dynamics Physics Engine demo.<p>

  This is the simplest exemple of how to create dynamic body suported by NGD.
  To execute the simulation we need to indicate to the physics engine the time
  elapsed for update in GLCadencer1Progress.
  The floor is static, so it can't move.

	<b>History : </b><font size=-1><ul>
      <li>17/09/10 - FP - Created by Franck Papouin
  </ul>

}

Interface

Uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCoordinates, GLSimpleNavigation, GLCadencer,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, GLNGDManager, StdCtrls,
  GLGeomObjects, GLBitmapFont, GLWindowsFont, GLHUDObjects, ExtCtrls;

Type
  TForm1 = Class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Floor: TGLCube;
    GLDummyCube1: TGLDummyCube;
    GLNGDManager1: TGLNGDManager;
    GLResolutionIndependantHUDText1: TGLResolutionIndependantHUDText;
    GLStoredBitmapFont1: TGLStoredBitmapFont;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Procedure GLCadencer1Progress(Sender: TObject;
      Const deltaTime, newTime: Double);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
  Private
    { Déclarations privées }
  Public
    { Déclarations publiques }
  End;

Var
  Form1: TForm1;

Implementation

{$R *.dfm}

uses
  GLColor;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  GLCube1: TGLCube;
  DynNGDBehav: TGLNGDDynamic;
Begin
  GLCube1 := TGLCube.CreateAsChild(GLDummyCube1);
  GLCube1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLCube1.GetOrCreateBehaviour(TGLNGDDynamic) As TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  GLSphere1: TGLSphere;
  DynNGDBehav: TGLNGDDynamic;
Begin
  GLSphere1 := TGLSphere.CreateAsChild(GLDummyCube1);
  GLSphere1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLSphere1.GetOrCreateBehaviour(TGLNGDDynamic) As TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  GLCone1: TGLCone;
  DynNGDBehav: TGLNGDDynamic;
Begin
  GLCone1 := TGLCone.CreateAsChild(GLDummyCube1);
  GLCone1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLCone1.GetOrCreateBehaviour(TGLNGDDynamic) As TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  GLCylinder1: TGLCylinder;
  DynNGDBehav: TGLNGDDynamic;
Begin
  GLCylinder1 := TGLCylinder.CreateAsChild(GLDummyCube1);
  GLCylinder1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLCylinder1.GetOrCreateBehaviour(TGLNGDDynamic)
    As TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  GLCapsule1: TGLCapsule;
  DynNGDBehav: TGLNGDDynamic;
Begin
  GLCapsule1 := TGLCapsule.CreateAsChild(GLDummyCube1);
  GLCapsule1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLCapsule1.GetOrCreateBehaviour(TGLNGDDynamic)
    As TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  GLDummyCube1.DeleteChildren;
  GLSceneViewer1.Invalidate;
End;

Procedure TForm1.GLCadencer1Progress(Sender: TObject;
  Const deltaTime, newTime: Double);
Begin
  GLNGDManager1.Step(deltaTime);
  GLResolutionIndependantHUDText1.Text:='Bodycount:='+inttostr(GLNGDManager1.BodyCount);
End;

End.

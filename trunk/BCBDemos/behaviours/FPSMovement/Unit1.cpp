//---------------------------------------------------------------------------

#include <vcl.h>
#include <Keyboard.hpp>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLGeomObjects"
#pragma link "OpenGL1x"
#pragma link "VectorGeometry"
#pragma link "GLFile3DS"
#pragma link "Octree"
#pragma link "VectorLists"
#pragma link "GLNavigator"
#pragma link "GLCollision"
#pragma link "jpeg"
#pragma link "GLObjects"
#pragma link "GLMisc"
#pragma link "GLWin32Viewer"
#pragma link "GLCadencer"
#pragma link "GLTexture"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLFPSMovement"
#pragma link "Keyboard"
#pragma resource "*.dfm"
TForm1 *Form1;

TGLBFPSMovement *behav, *behav2;
double yangle = 90;
double xangle = 0;
bool WireFrame;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  Map1->LoadFromFile("..\\..\\media\\map.3ds");
  Map1->BuildOctree();
  Map1->Up->SetVector(0, 1, 0);

  Map2->LoadFromFile("..\\..\\media\\beer.3ds");
  Map2->BuildOctree();

  ShowCursor(false);
  SetCursorPos(Screen->Width / 2, Screen->Height / 2);

  behav = GetFPSMovement(Player);
  behav2 = GetFPSMovement(Bot);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  Caption = GLSceneViewer1->FramesPerSecondText();
  GLSceneViewer1->ResetPerformanceMonitor();
//   caption=GLCamera1->Position->AsString;        
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyDown(TObject * Sender, WORD & Key,
                                    TShiftState Shift)
{
//  if(Key=Ord("R") ) resetScene;
  if(Key == VK_ESCAPE)
    Close();

  //show/hide arrows
  if(Key == VK_F1)
    behav->ShowArrows = !behav->ShowArrows;

  //pause / unpause
  if(Key == VK_PAUSE)
    GLCadencer1->Enabled = !GLCadencer1->Enabled;
  //first person
  if(Key == VK_F2)
    GLSceneViewer1->Camera = FirstPersonCamera;
  //third person
  if(Key == VK_F3)
    GLSceneViewer1->Camera = ThirdPersonCamera;
  // solid / wireframe
  if(IsKeyDown(VK_F5))
  {
    WireFrame = !WireFrame;
    if(WireFrame)
    {
      Map1->UseMeshMaterials = false;
      Map1->Material->FrontProperties->PolygonMode = pmLines;
      Map2->UseMeshMaterials = false;
      Map2->Material->FrontProperties->PolygonMode = pmLines;
    }
    else
    {
      Map1->UseMeshMaterials = true;
      Map1->Material->FrontProperties->PolygonMode = pmFill;
      Map2->UseMeshMaterials = true;
      Map2->Material->FrontProperties->PolygonMode = pmFill;
    }
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  float MovementScale = MovManager->MovementScale;

  //) update position according to Keys being pressed
  if(IsKeyDown('W') || IsKeyDown('Z'))
    behav->MoveForward(MovementScale * deltaTime);
  if(IsKeyDown('S'))
    behav->MoveForward(-MovementScale * deltaTime);
  if(IsKeyDown('A') || IsKeyDown('Q'))
    behav->StrafeHorizontal(-MovementScale * deltaTime);
  if(IsKeyDown('D'))
    behav->StrafeHorizontal(MovementScale * deltaTime);

  //move up/down (f||debugging)
  if(IsKeyDown(VK_PRIOR) || IsKeyDown(VK_SPACE))
    behav->StrafeVertical(MovementScale * deltaTime);
  if(IsKeyDown(VK_NEXT))
    behav->StrafeVertical(-MovementScale * deltaTime);

  //move bot
  if(IsKeyDown('I'))
    behav2->MoveForward(MovementScale * deltaTime);
  if(IsKeyDown('K'))
    behav2->MoveForward(-MovementScale * deltaTime);
  if(IsKeyDown('J'))
    behav2->StrafeHorizontal(-MovementScale * deltaTime);
  if(IsKeyDown('L'))
    behav2->StrafeHorizontal(MovementScale * deltaTime);
  if(IsKeyDown('O'))
    behav2->StrafeVertical(MovementScale * deltaTime);
  if(IsKeyDown('P'))
    behav->StrafeVertical(-MovementScale * deltaTime);

  if(IsKeyDown(VK_LEFT))
    behav->TurnHorizontal(-70 * deltaTime);
  if(IsKeyDown(VK_RIGHT))
    behav->TurnHorizontal(70 * deltaTime);
  if(IsKeyDown(VK_UP))
    behav->TurnVertical(-70 * deltaTime);
  if(IsKeyDown(VK_DOWN))
    behav->TurnVertical(70 * deltaTime);

  //update mouse view
  xangle = Mouse->CursorPos.x - Screen->Width / 2;
  yangle = Mouse->CursorPos.y - Screen->Height / 2;
  SetCursorPos(Screen->Width / 2, Screen->Height / 2);
  behav->TurnHorizontal(xangle * 40 * deltaTime);
  behav->TurnVertical(-yangle * 20 * deltaTime);

  GLSceneViewer1->Invalidate();
}

//---------------------------------------------------------------------------


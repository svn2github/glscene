//---------------------------------------------------------------------------

#include <vcl.h>
#include <Keyboard.hpp>
#include <math.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLObjects"
#pragma link "GLMisc"
#pragma link "GLTexture"
#pragma link "GLGeomObjects"
#pragma link "GLCelShader"
#pragma link "AsyncTimer"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLCadencer"
#pragma link "GLScene"
#pragma link "GLFileMD2"
#pragma link "JPEG"
#pragma link "Keyboard"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  float r;
  SetCurrentDir(ExtractFilePath(Application->ExeName) + "..\\..\\media");

  GLActor1->LoadFromFile("waste.md2");
  r = GLActor1->BoundingSphereRadius();
  GLActor1->Scale->SetVector(2.5 / r, 2.5 / r, 2.5 / r);
  GLActor1->AnimationMode = aamLoop;
  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->
    LoadFromFile("wastecell.jpg");
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
  lx = X;
  ly = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::AsyncTimer1Timer(TObject * Sender)
{
  Form1->Caption =
    Format("Cel Shading Demo - %.2f FPS",
           ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  if(IsKeyDown(VK_LBUTTON))
  {
    GLCamera1->MoveAroundTarget(ly - my, lx - mx);
    lx = mx;
    ly = my;
  }

  GLTorus1->TurnAngle = 15 * sin(newTime * 5);
  GLTorus1->PitchAngle = 15 * cos(newTime * 5);
}

//---------------------------------------------------------------------------


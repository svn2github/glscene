//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLWin32Viewer"
#pragma link "GLCadencer"
#pragma link "GLMisc"
#pragma link "GLODEManager"
#pragma link "GLScene"
#pragma link "GLObjects"
#pragma link "GLGeomObjects"
#pragma resource "*.dfm"
TForm1 *Form1;
int my, mx;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  TAffineVector av={-250,0,0};
  ((TGLODEDynamic*)Pin2->Behaviours->Items[0])->AddForce(av);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  GLODEManager1->Step(deltaTime);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  my = Y;
  mx = X;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  if(!Shift.Empty())
    GLCamera1->MoveAroundTarget(my - Y, mx - X);
  my = Y;
  mx = X;
}

//---------------------------------------------------------------------------


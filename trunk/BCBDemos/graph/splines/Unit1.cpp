//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLMisc"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLObjects"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TForm1::MoveCenterNodeTo(int x, int y)
{
  GLLines1->Nodes->Items[1]->AsAffineVector =
    GLSceneViewer1->Buffer->ScreenToWorld(x, y);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  MoveCenterNodeTo(X, Y);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  if(!Shift.Empty())
    MoveCenterNodeTo(X, Y);
}

//---------------------------------------------------------------------------


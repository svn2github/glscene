//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLMisc"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma resource "*.dfm"
TForm1 *Form1;

int mx, my;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject * Sender)
{
  if(CheckBox1->Checked)
  {
    XZGrid->YSamplingScale->Origin = 0;
    YZGrid->XSamplingScale->Origin = 0;
  }
  else
  {
    XZGrid->YSamplingScale->Origin = -1;
    YZGrid->XSamplingScale->Origin = -1;
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar1Change(TObject * Sender)
{
  XYGrid->ZSamplingScale->Origin = -((float)TrackBar1->Position / 10);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  if(!Shift.Empty())
  {
    GLCamera1->MoveAroundTarget(my - Y, mx - X);
    mx = X;
    my = Y;
  }
}

//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
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
	XYGrid->ZSamplingScale->Origin = 0;
  }
  else
  {
	XZGrid->YSamplingScale->Origin = -1;
	YZGrid->XSamplingScale->Origin = -1;
	XYGrid->ZSamplingScale->Origin = -1;
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar1Change(TObject * Sender)
{
  XYGrid->ZSamplingScale->Origin = -((float)TrackBar1->Position / 10);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar2Change(TObject *Sender)
{
  XZGrid->YSamplingScale->Origin = -((float)TrackBar2->Position / 10);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar3Change(TObject *Sender)
{
  YZGrid->XSamplingScale->Origin = -((float)TrackBar3->Position / 10);
}
//---------------------------------------------------------------------------

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
	if (Shift.Contains(ssLeft))
		GLCamera1->MoveAroundTarget(my - Y, mx - X);
	else if (Shift.Contains(ssRight))
		GLCamera1->RotateTarget(my - Y, mx - X, 0);
	mx = X;
	my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
		  TPoint &MousePos, bool &Handled)
{
  GLCamera1->
   AdjustDistanceToTarget(Power(1.1, (float)((float)WheelDelta / 120.0)));
}
//---------------------------------------------------------------------------

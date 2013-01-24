//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;

const int
   cNbPoints = 180;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   // allocate points in the 1st point set
   GLPoints1->Positions->Count = cNbPoints;
   // specify white color for the 1st point set
   // (if a single color is defined, all points will use it,
   // otherwise, it's a per-point coloring)
   GLPoints1->Colors->Add(clrWhite);
   // specify blue color for the 2nd point set
   GLPoints2->Colors->Add(clrBlue);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   int i;
   float f, a, ab, ca, sa;
   TAffineVector *v;

   if (CBAnimate->Checked)
   {
	  // update the 1st point set with values from a math func
	  f = 1+Cos(newTime);
	  ab = newTime*0.1;
	  for (i=0; i < cNbPoints-1; i++) {
		 a = DegToRad((float)4*i)+ ab;
		 SinCos(a, sa, ca);
		 v->V[0] = 2*ca;
		 v->V[1] = 2*Cos(f*a);
		 v->V[2] = 2*sa;
		 GLPoints1->Positions->Add((float)v->V[0],
			  (float)v->V[1], (float)v->V[2]);
	  }
	  // replicate points in second set
	  GLPoints2->Positions = GLPoints1->Positions;
   }
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBAnimateClick(TObject *Sender)
{
   GLPoints1->Static = !CBAnimate->Checked;
   GLPoints2->Static = !CBAnimate->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBPointParamsClick(TObject *Sender)
{
   GLPoints1->PointParameters->Enabled = CBPointParams->Checked;
   GLPoints2->PointParameters->Enabled = CBPointParams->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   LabelFPS->Caption = Format("%.1f FPS",
     ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
 mx = X; my = Y;
}
//---------------------------------------------------------------------------

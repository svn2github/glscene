//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSkydome"
#pragma link "GLTeapot"
#pragma link "GLWin32Viewer"
#pragma link "GLImposter"
#pragma resource "*.dfm"
TForm2 *Form2;


//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::Timer1Timer(TObject *Sender)
{
   LabelFPS->Caption = GLSceneViewer1->FramesPerSecondText();
   if (CBShowImposter->Checked)
	  LabelFPS->Caption = LabelFPS->Caption +" - 3721 imposters";
   GLSceneViewer1->ResetPerformanceMonitor();


  LabelTexSize->Caption = Format("%d x %d - %.1f%%",
  ARRAYOFCONST((impBuilder->TextureSize->X, impBuilder->TextureSize->Y,
		   impBuilder->TextureFillRatio*100)));
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CBShowTeapotClick(TObject *Sender)
{
  GLTeapot1->Visible = CBShowTeapot->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CBShowImposterClick(TObject *Sender)
{
  GLDirectOpenGL1->Visible = CBShowImposter->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormCreate(TObject *Sender)
{
   renderPoint = (TGLRenderPoint *) GLDummyCube1->AddNewChild(__classid(TGLRenderPoint));

/*  to do
   impBuilder = TGLStaticImposterBuilder.Create(Self);
   impBuilder->SampleSize = 64;
   impBuilder->SamplingRatioBias = 1.3;
   impBuilder->Coronas->Items[0].Samples = 32;
   impBuilder->Coronas->Add(15, 24);
   impBuilder->Coronas->Add(30, 24);
   impBuilder->Coronas->Add(45, 16);
   impBuilder->Coronas->Add(60, 16);
   impBuilder->Coronas->Add(85, 16);
   impBuilder->RenderPoint = renderPoint;

   impBuilder->RequestImposterFor(GLTeapot1);
*/
}
//---------------------------------------------------------------------------
void __fastcall TForm2::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
	if (Shift.Contains(ssLeft))
		GLCamera1->MoveAroundTarget(my - Y, mx - X);
	else if (Shift.Contains(ssRight))
		GLCamera1->RotateTarget(my - Y, mx - X, 0);
	mx = X;
	my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm2::GLDirectOpenGL1Render(TObject *Sender, TRenderContextInfo &rci)

{
   int x, y;
   Vectorgeometry::TVector camPos;
   Vectorgeometry::TVector pos;

/*   to do
   TImposter imp;
   imp = impBuilder->ImposterFor(GLTeapot1);
   if ((imp=nil) or (imp->Texture->Handle=0)) Exit;

   imp->BeginRender(rci);
   for (x=-30; x<30; x++) do
	 for (y=-30;y<30; y++) do {
	  MakePoint(pos, x*5, 0, y*4);
	  camPos = VectorSubtract(rci->cameraPosition, pos);
	  imp->Render(rci, pos, camPos, 1);
   }
   imp->EndRender(rci);
*/
}
//---------------------------------------------------------------------------

void __fastcall TForm2::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm2::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  GLCamera1->
   AdjustDistanceToTarget(Power(1.1, (float)((float)WheelDelta / 120.0)));
}
//---------------------------------------------------------------------------

void __fastcall TForm2::FormResize(TObject *Sender)
{
  if(GLSceneViewer1->Width > GLSceneViewer1->Height)
    GLCamera1->SceneScale = (float)GLSceneViewer1->Height / 300;
  else
	GLCamera1->SceneScale = (float)GLSceneViewer1->Width / 360;
}
//---------------------------------------------------------------------------


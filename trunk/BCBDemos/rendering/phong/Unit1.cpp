/*: Per-Pixel phong shading demo.<p>

   The TGLPhongShader implements phong shading through the use of an
   ARB vertex and fragment program. So far only the material and light
   properties are supported, some form of texture support will be
   added in future updates.<p>

*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AsyncTimer"
#pragma link "GLCadencer"
#pragma link "GLMisc"
#pragma link "GLObjects"
#pragma link "GLPhongShader"
#pragma link "GLScene"
#pragma link "GLTeapot"
#pragma link "GLTexture"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  if (Shift.Contains(ssLeft))
  {
    GLCamera1->MoveAroundTarget(my-Y, mx-X);
    mx = X;
    my = Y;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AsyncTimer1Timer(TObject *Sender)
{
  Form1->Caption = "Phong Shader - " + GLSceneViewer1->FramesPerSecondText(1);
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  GLPhongShader1->Enabled = CheckBox1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender,
      const double deltaTime, const double newTime)
{
  CheckBox1->Checked = GLPhongShader1->Enabled;
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

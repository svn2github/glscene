/*: Visibility culling demo/test/sample.<p>

   This sample is used to test and showcase the efficiency (or inefficiency) of
   visibility in various cases. Be aware that the sample may be slow loading
   (the same mesh is loaded multiple times to put some stress).<br>
   In each of the tests, a "square grid" of objects is created and made visible,
   the camera points at the center of the square, making most of the objects
   off-screen. Visibility culling detects that and does not render the off-screen
   or too-far away objects.<p>

   <ul>
   <li>Spheres: this is the default setting, and one in which culling is
      completely inefficient on a T&L board or good OpenGL ICD, mainly because
      the spheres are rendered with build lists that already have some visibility
      culling built-in. If culling is efficient for you in this case, well be
      happy it is, but start looking for a newer graphics board ;)
   <li>Actors: this one is culling friendly, and your framerate can more than
      double by choosing "ObjectBased" mode. This is due to the fact that the
      actor geometry must be resent at each frame, thus limiting T&L capability
      (the AGP stands in the way...). A culled object's geometry is not sent
      at all, and that can reduce the AGP and graphics driver load drastically.
   </ul>
*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include <Jpeg.hpp>
#include "GLFileMD2.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLCadencer"
#pragma link "GLMisc"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTexture"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFileMD2"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
  int i, j;
  TGLSphere *newSphere;
  TGLActor *newActor;

  // Spheres are used as standalone, high-polycount objects
  // that are highly T&L friendly
  for (i=-4; i<4; i++)
    for (j=-4; j<4; j++)
    {
      newSphere = (TGLSphere *) DCSpheres->AddNewChild(__classid(TGLSphere));
      newSphere->Position->SetPoint(i*5, 0, j*5);
      newSphere->Slices = 32;
      newSphere->Stacks = 32;
    }
  // Actors are used as standalone, med-polycount objects
  // that aren't T&L friendly (all geometry must be sent to
  // the hardware at each frame)
  GLMaterialLibrary->Materials->Items[0]->Material->Texture->Image->LoadFromFile("..\\..\\media\\waste.jpg");
  ACReference->LoadFromFile("..\\..\\media\\waste.md2");
  for (i=-3; i<3; i++)
    for (j=-3; j<3; j++)
    {
      newActor = (TGLActor *) DCActors->AddNewChild(__classid(TGLActor));
      newActor->Assign(ACReference);
      newActor->Position->SetPoint(i*10, 0, j*10);
      newActor->CurrentFrame = (i+2)+(j+2)*5;
    }
  ACReference->Visible = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBSpheresClick(TObject *Sender)
{
   DCSpheres->Visible = RBSpheres->Checked;
   DCActors->Visible = RBActors->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencerProgress(TObject *Sender,
      const double deltaTime, const double newTime)
{
   Viewer->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   Caption = Format("%.1f FPS", ARRAYOFCONST((Viewer->FramesPerSecond())));
   Viewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RBNoneClick(TObject *Sender)
{
   if (RBObject->Checked)
      GLScene->VisibilityCulling = vcObjectBased;
   else if (RBHierarchical->Checked)
      GLScene->VisibilityCulling = vcHierarchical;
   else GLScene->VisibilityCulling = vcNone;
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------
/*: A basic sample to demonstrate how transparency & Z-buffer work/fight together.<p>

   In this sample, only the sphere are transparent. The form has a few options
   that allow to adjust in which order objects are rendered, and what kind of
   transparency is used.<p>

   Transparency in GLScene is activated by setting Material.Blending to either
   'bmTransparency' or 'bmAdditive', AND giving a <1.0 value to the Diffuse
   color alpha channel (alpha = 0.0 means absolute transparency, alpha = 1.0
   means absolute opacity).<p>

   How do Z-Buffer & transparency work ? When point has to be rendered, OpenGL
   first checks its distance to the camera, the "Z" axis. If the distance
   check is successfull (the new point is closer), it is rendered, and if
   the point is part of a "transparent" object, then OpenGL will mix the existing
   point's color with our new point's color. If the Z check fails, OpenGL doesn't
   even bother about checking transparency.<p>

   This is why, if you want to render transparent objects, you must make sure
   you render the farthest objects first, to give transparency a chance.
   However this effect can be usefull if you want to render mixed, half-transparent
   half-opaque objects.<p>

   They are two ways to order objects in GLScene :<ul>
   <li>ordering : can be done at design-time in the editor or at runtime with
      MoveUp/MoveDown methods
   <li>sorting : adjust the ObjectSorting property (see help for more details)
   </ul>
*/
#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include <math.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLCadencer"
#pragma link "GLGeomObjects"
#pragma link "GLMisc"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RBSTCClick(TObject *Sender)
{
   // we have 3 objects, move up twice and we're on the top !
   CentralSphere->MoveUp();
   CentralSphere->MoveUp();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBTSCClick(TObject *Sender)
{
   // we have 3 objects, move down twice and we're on the top,
   // then once down, we're in the middle !
   CentralSphere->MoveUp();
   CentralSphere->MoveUp();
   CentralSphere->MoveDown();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBTCSClick(TObject *Sender)
{
   // we have 3 objects, move down twice and we're on the bottom !
   CentralSphere->MoveDown();
   CentralSphere->MoveDown();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBAdditiveClick(TObject *Sender)
{
   // adjust blending mode for both orbiting spheres
   if (CBAdditive->Checked)
      OrbitingSphere1->Material->BlendingMode = bmAdditive;
   else OrbitingSphere1->Material->BlendingMode = bmTransparency;
   OrbitingSphere2->Material->BlendingMode = OrbitingSphere1->Material->BlendingMode;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBSortingClick(TObject *Sender)
{
   // adjust sorting on the parent object
   if (CBSorting->Checked)
      BaseDummyCube->ObjectsSorting = osRenderFarthestFirst;
   else BaseDummyCube->ObjectsSorting = osNone;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender,
      const double deltaTime, const double newTime)
{
   double alpha;

   // move the spheres
   alpha = DegToRad(newTime*60);
   OrbitingSphere1->Position->SetPoint(1.5*cos(alpha), 1.5*sin(alpha), 1.5*sin(alpha));
   alpha = alpha+M_PI/2;
   OrbitingSphere2->Position->SetPoint(1.5*cos(alpha), 1.5*sin(alpha), 1.5*sin(alpha));
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "GLParametricSurfaces.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLMisc"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLObjects"
#pragma link "GLVectorFileObjects"
#pragma link "GLFileNurbs"
#pragma resource "*.dfm"
TForm1 *Form1;
int mx, my;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  // Load the nurbs data
  GLActor1->LoadFromFile("..\\..\\media\\duck1.nurbs");
  GLActor1->AddDataFromFile("..\\..\\media\\duck2.nurbs");
  GLActor1->AddDataFromFile("..\\..\\media\\duck3.nurbs");

  // { Translate FreeForm based on the first mesh object's average
  // control point. Quick and dirty ... or maybe just dirty :P }
  TAffineVectorList *cp =
    ((TMOParametricSurface *) (GLActor1->MeshObjects->Items[0]))->ControlPoints;
  GLActor1->Position->Translate(VectorNegate(VectorScale(cp->Sum(),1.0/cp->Count)));

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

void __fastcall TForm1::TrackBar1Change(TObject * Sender)
{
  for(int i = 0; i < GLActor1->MeshObjects->Count; i++)
    ((TMOParametricSurface *) (GLActor1->MeshObjects->Items[i]))->
      Resolution = TrackBar1->Position;

  GLActor1->StructureChanged();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject * Sender)
{
  if(CheckBox1->Checked)
  {
    GLActor1->Material->FrontProperties->PolygonMode = pmLines;
    GLActor1->Material->FaceCulling = fcNoCull;
  }
  else
  {
    GLActor1->Material->FrontProperties->PolygonMode = pmFill;
    GLActor1->Material->FaceCulling = fcBufferDefault;
  }
}

//---------------------------------------------------------------------------


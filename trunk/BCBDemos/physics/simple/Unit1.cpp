//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include <math.h>
#include "GLODECustomColliders.hpp"
#include "GLODEManager.hpp"
#include "GLGeomObjects.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLCadencer"
#pragma link "GLGraph"
#pragma link "GLMisc"
#pragma link "GLObjects"
#pragma link "GLODEManager"
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
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  ComboBox1->ItemIndex = 0;
  ComboBox2->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender,
      const double deltaTime, const double newTime)
{
  GLODEManager1->Step(deltaTime);
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
    GLCamera1->MoveAroundTarget(my-Y,mx-X);
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SpawnClick(TObject *Sender)
{
  switch (ComboBox1->ItemIndex)
  {
    case 0 : DoSphere(); break;
    case 1 : DoBox(); break;
    case 2 : DoCapsule(); break;
    case 3 : DoCylinder(); break;
    case 4 : DoCone(); break;
  }
}
//---------------------------------------------------------------------------
double Random()
{
  return (double)rand()/(double)RAND_MAX;
}

void TForm1::DoSphere(void)
{
  TGLSphere *sphere;
  TGLODEDynamic *dyn;

  sphere = (TGLSphere *)(ODEObjects->AddNewChild(__classid(TGLSphere)));
  sphere->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  sphere->Radius = 0.3*(Random()+1);
  dyn = new TGLODEDynamic(sphere->Behaviours);
  dyn->Manager = GLODEManager1;
  ((TODEElementSphere *)(dyn->AddNewElement(__classid(TODEElementSphere))))->Radius = sphere->Radius;
}

void TForm1::DoBox(void)
{
  TGLCube *cube;
  TGLODEDynamic *dyn;
  TODEElementBox *elem;

  cube = (TGLCube *)(ODEObjects->AddNewChild(__classid(TGLCube)));
  cube->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  cube->CubeWidth = 0.5*(Random()+1);
  cube->CubeHeight = 0.5*(Random()+1);
  cube->CubeDepth = 0.5*(Random()+1);
  dyn = new TGLODEDynamic(cube->Behaviours);
  dyn->Manager = GLODEManager1;
  elem = (TODEElementBox *) dyn->AddNewElement(__classid(TODEElementBox));
  elem->BoxWidth = cube->CubeWidth;
  elem->BoxHeight = cube->CubeHeight;
  elem->BoxDepth = cube->CubeDepth;
}

void TForm1::DoCapsule(void)
{
  TGLCylinder *capsule;
  TGLODEDynamic *dyn;
  TGLSphere *sphere;
  TODEElementCapsule *elem;

  capsule = (TGLCylinder *)(ODEObjects->AddNewChild(__classid(TGLCylinder)));
  capsule->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  capsule->BottomRadius = 0.25*(Random()+1);
  capsule->TopRadius = capsule->BottomRadius;
  capsule->Height = Random()+1;
  capsule->Parts << cySides;

  sphere = (TGLSphere *) capsule->AddNewChild(__classid(TGLSphere));
  sphere->Position->Y = 0.5*capsule->Height;
  sphere->Radius = capsule->BottomRadius;
  sphere->Bottom = 0;

  sphere = (TGLSphere *) capsule->AddNewChild(__classid(TGLSphere));
  sphere->Position->Y = -0.5*capsule->Height;
  sphere->Radius = capsule->BottomRadius;
  sphere->Top = 0;

  dyn = new TGLODEDynamic(capsule->Behaviours);
  dyn->Manager = GLODEManager1;
  elem = (TODEElementCapsule *) dyn->AddNewElement(__classid(TODEElementCapsule));
  elem->Radius = capsule->BottomRadius;
  elem->Length = capsule->Height;
  elem->Direction->SetVector(0,1,0);
  elem->Up->SetVector(0,0,1);
}

void TForm1::DoCylinder(void)
{
  TGLCylinder *cylinder;
  TGLODEDynamic *dyn;
  TODEElementCylinder *elem;

  cylinder = (TGLCylinder *)(ODEObjects->AddNewChild(__classid(TGLCylinder)));
  cylinder->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  cylinder->BottomRadius = 0.25*(Random()+1);
  cylinder->TopRadius = cylinder->BottomRadius;
  cylinder->Height = Random()+1;

  dyn = new TGLODEDynamic(cylinder->Behaviours);
  dyn->Manager = GLODEManager1;
  elem = (TODEElementCylinder *) dyn->AddNewElement(__classid(TODEElementCylinder));
  elem->Radius = cylinder->BottomRadius;
  elem->Length = cylinder->Height;
}

void TForm1::DoCone(void)
{
  TGLCone *cone;
  TGLODEDynamic *dyn;
  TODEElementCone *elem;

  cone = (TGLCone *)(ODEObjects->AddNewChild(__classid(TGLCone)));
  cone->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  cone->BottomRadius = 0.25*(Random()+1);
  cone->Height = Random()+1;

  dyn = new TGLODEDynamic(cone->Behaviours);
  dyn->Manager = GLODEManager1;
  elem = (TODEElementCone *) dyn->AddNewElement(__classid(TODEElementCone));
  elem->Radius = cone->BottomRadius;
  elem->Length = cone->Height;
  elem->Direction->SetVector(0,1,0);
  elem->Up->SetVector(0,0,1);
  elem->Position->SetPoint(0,-cone->Height/2,0);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLHeightField1GetHeight(const float x,
      const float y, float &z, TVector4f &color, TTexPoint &texPoint)
{
  z = 0.5*cos(x)*sin(y);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  GLODEManager1->Visible = CheckBox1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
  ((TGLODEHeightField *)(GLHeightField1->Behaviours->Behaviour[0]))->RenderContacts = CheckBox2->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
  ((TGLODEHeightField *)(GLHeightField1->Behaviours->Behaviour[0]))->ContactResolution = 0.25+(10-TrackBar1->Position)/20;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ComboBox2Change(TObject *Sender)
{
  if (ComboBox2->ItemIndex == 0)
  {
    GLPlane1->Visible = true;
    GetOdeStatic(GLPlane1)->Manager = GLODEManager1;
  } else {
    GLPlane1->Visible = false;
    GetOdeStatic(GLPlane1)->Manager = NULL;
  }

  if (ComboBox2->ItemIndex == 1)
  {
    GLHeightField1->Visible = true;
    GetODEHeightField(GLHeightField1)->Manager = GLODEManager1;
  } else {
    GLHeightField1->Visible = false;
    GetODEHeightField(GLHeightField1)->Manager = NULL;
  }
}
//---------------------------------------------------------------------------


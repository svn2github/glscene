//---------------------------------------------------------------------------

#include <vcl.h>
#include <Math.hpp>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLWin32Viewer"
#pragma link "GLCadencer"
#pragma link "GLMisc"
#pragma link "GLODEManager"
#pragma link "GLScene"
#pragma link "GLObjects"
#pragma resource "*.dfm"
TForm1 *Form1;

TGLODEPlane *GLODEPlane1;
int mx, my;

float rand()                    // return 0.0~1.0
{
  return (float)RandomRange(0, 1000) / 1000.0;
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  GLODEManager1->Gravity->Y = -9.81;

  GLODEPlane1 = (TGLODEPlane *) ODEObjects->AddNewChild(__classid(TGLODEPlane));
  GLODEPlane1->Manager = GLODEManager1;
  GLODEPlane1->Direction->SetVector(0, 1, 0);
  GLODEPlane1->Position->SetPoint(0, -1, 0);
  TSurfaceModes sm;
  sm << csmBounce;
  GLODEPlane1->Surface->SurfaceMode = sm;
  GLODEPlane1->Surface->Bounce = 1;
  GLODEPlane1->Surface->Bounce_Vel = 0.5;
  GLODEPlane1->Surface->SoftCFM = 0.5;
  TGLPlane *p = (TGLPlane *) GLODEPlane1->AddNewChild(__classid(TGLPlane));
  p->Height = 10;
  p->Width = 10;

  Randomize();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  const double cStep = 0.001;
  int i;
  for(i = 0; i <= Trunc(deltaTime / cStep); i++)
    GLODEManager1->Step(cStep);
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
  if(Shift.Contains(ssShift))
    GLCamera1->MoveAroundTarget(my - Y, mx - X);
  my = Y;
  mx = X;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject * Sender)
{
  TGLCube *cube = (TGLCube *) ODEObjects->AddNewChild(__classid(TGLCube));
  cube->Position->SetPoint(rand(), rand() + 3, rand());
  cube->CubeWidth = rand() + 0.5;
  cube->CubeHeight = rand() + 0.5;
  cube->CubeDepth = rand() + 0.5;
  TVector v = {
    0.5 * rand() + 0.5,
    0.5 * rand() + 0.5,
    0.5 * rand() + 0.5,
    0.5 * rand() + 0.5
  };
  cube->Material->FrontProperties->Diffuse->Color = v;

  TGLODEDynamic *db = new TGLODEDynamic(cube->Behaviours);
  db->Manager = GLODEManager1;

  TODEElementBox *eb =
    (TODEElementBox *) db->AddNewElement(__classid(TODEElementBox));

  eb->Position->SetPoint(0, 0, 0);
  eb->BoxDepth = cube->CubeDepth;
  eb->BoxWidth = cube->CubeWidth;
  eb->BoxHeight = cube->CubeHeight;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject * Sender)
{
  TGLODEDummy *du =
    (TGLODEDummy *) ODEObjects->AddNewChild(__classid(TGLODEDummy));
  du->Manager = GLODEManager1;
  du->Position->SetPoint(rand(), rand() + 3, rand());
  du->Surface->RollingFrictionEnabled = true;
  du->Surface->RollingFrictionCoeff = 0.0005;

  TODEElementSphere *es =
    (TODEElementSphere *) du->AddNewElement(__classid(TODEElementSphere));
  es->Position->SetPoint(0, 0, 0);
  es->Radius = 0.5 * rand() + 0.25;

  du->Color->Red = 0.5 * rand() + 0.5;
  du->Color->Green = 0.5 * rand() + 0.5;
  du->Color->Blue = 0.5 * rand() + 0.5;
  du->VisibleAtRunTime = true;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button3Click(TObject * Sender)
{
  TGLODEDummy *du =
    (TGLODEDummy *) ODEObjects->AddNewChild(__classid(TGLODEDummy));
  du->Manager = GLODEManager1;
  du->Position->SetPoint(rand(), rand() + 3, rand());

  TODEElementCapsule *ec =
    (TODEElementCapsule *) du->AddNewElement(__classid(TODEElementCapsule));
  ec->Position->SetPoint(0, 0, 0);
  ec->Radius = 0.5 * rand() + 0.25;
  ec->Length = rand() + 0.5;

  du->Color->Red = 0.5 * rand() + 0.5;
  du->Color->Green = 0.5 * rand() + 0.5;
  du->Color->Blue = 0.5 * rand() + 0.5;
  du->VisibleAtRunTime = true;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject * Sender)
{
  TGLODEDummy *du =
    (TGLODEDummy *) ODEObjects->AddNewChild(__classid(TGLODEDummy));
  du->Manager = GLODEManager1;
  du->Position->SetPoint(rand(), rand() + 3, rand());

  TODEElementBox *eb =
    (TODEElementBox *) du->AddNewElement(__classid(TODEElementBox));
  eb->Position->SetPoint(0.5 * rand() - 0.25, 0.5 * rand() - 0.25,
                         0.5 * rand() - 0.25);
  eb->BoxWidth = rand() + 0.5;
  eb->BoxHeight = rand() + 0.5;
  eb->BoxDepth = rand() + 0.5;

  TODEElementSphere *es =
    (TODEElementSphere *) du->AddNewElement(__classid(TODEElementSphere));
  es->Position->SetPoint(0.5 * rand() - 0.25, 0.5 * rand() - 0.25,
                         0.5 * rand() - 0.25);
  es->Radius = 0.5 * rand() + 0.25;

  TODEElementCapsule *ec =
    (TODEElementCapsule *) du->AddNewElement(__classid(TODEElementCapsule));
  ec->Position->SetPoint(0.5 * rand() - 0.25, 0.5 * rand() - 0.25,
                         0.5 * rand() - 0.25);
  ec->Radius = 0.5 * rand() + 0.25;
  ec->Length = rand() + 0.5;

  du->Color->Red = 0.5 * rand() + 0.5;
  du->Color->Green = 0.5 * rand() + 0.5;
  du->Color->Blue = 0.5 * rand() + 0.5;
  du->CalibrateCenterOfMass();

  du->VisibleAtRunTime = true;

}

//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBoxClick(TObject * Sender)
{
  TSurfaceModes sm;

  if(CheckBox1->Checked)
    sm << csmBounce;
  if(CheckBox2->Checked)
    sm << csmSoftCFM;
  GLODEPlane1->Surface->SurfaceMode = sm;

}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar1Change(TObject * Sender)
{
  GLODEPlane1->Surface->Bounce = (float)TrackBar1->Position / 100;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar2Change(TObject * Sender)
{
  GLODEPlane1->Surface->SoftCFM = (float)TrackBar2->Position / 100;
}

//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "cg.hpp"
#include "cgGL.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLMisc"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLCadencer"
#pragma link "GLTexture"
#pragma link "GLCgShader"
#pragma link "GLVectorFileObjects"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLFile3DS"
#pragma resource "*.dfm"
TForm1 *Form1;
int my, mx;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  CgShader1->VertexProgram->LoadFromFile("simple_vp.cg");
  CgShader1->VertexProgram->Enabled = false;
  VertexCode->Lines->Assign(CgShader1->VertexProgram->Code);
  ApplyVP->Enabled = false;

  CgShader1->FragmentProgram->LoadFromFile("simple_fp.cg");
  CgShader1->FragmentProgram->Enabled = false;
  FragmentCode->Lines->Assign(CgShader1->FragmentProgram->Code);
  ApplyFP->Enabled = false;

  GLFreeForm1->LoadFromFile("..\\..\\media\\Teapot.3ds");
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CgShader1Initialize(TCustomCgShader * CgShader)
{
  VPName->Caption =
    "Using profile: " + CgShader->VertexProgram->GetProfileString();
  FPName->Caption =
    "Using profile: " + CgShader->FragmentProgram->GetProfileString();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CgShader1ApplyVP(TCgProgram * CgProgram,
                                         TObject * Sender)
{
  // rotate light vector for the "simple lighting" vertex program
  TVector v;
  SetVector(v, ZHmgVector);
  RotateVector(v, YVector, GLCadencer1->CurrentTime);

  TCgParameter *Param = CgProgram->ParamByName("LightVec");
  Param->SetAsVector(v);
  // or using plain Cg API: cgGLSetParameter4fv(Param.Handle, @v);

  // set uniform parameters that change every frame
  Param = CgProgram->ParamByName("ModelViewProj");
  Param->SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX,
                          CG_GL_MATRIX_IDENTITY);

  Param = CgProgram->ParamByName("ModelViewIT");
  Param->SetAsStateMatrix(CG_GL_MODELVIEW_MATRIX,
                          CG_GL_MATRIX_INVERSE_TRANSPOSE);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CBVPClick(TObject * Sender)
{
  CgShader1->VertexProgram->Enabled = CBVP->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CBFPClick(TObject * Sender)
{
  CgShader1->FragmentProgram->Enabled = CBFP->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  GLSceneViewer1->Invalidate();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  Caption = GLSceneViewer1->FramesPerSecondText(2);
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::VertexCodeChange(TObject * Sender)
{
  ApplyVP->Enabled = true;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FragmentCodeChange(TObject * Sender)
{
  ApplyFP->Enabled = true;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::ApplyVPClick(TObject * Sender)
{
  CgShader1->VertexProgram->Code = VertexCode->Lines;
  ApplyVP->Enabled = false;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::ApplyFPClick(TObject * Sender)
{
  CgShader1->FragmentProgram->Code = FragmentCode->Lines;
  ApplyFP->Enabled = false;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject * Sender)
{
  CgShader1->VertexProgram->ListParameters(Memo1->Lines);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject * Sender)
{
  CgShader1->FragmentProgram->ListParameters(Memo2->Lines);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button3Click(TObject * Sender)
{
  CgShader1->VertexProgram->ListCompilation(Memo1->Lines);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button5Click(TObject * Sender)
{
  CgShader1->FragmentProgram->ListCompilation(Memo2->Lines);
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


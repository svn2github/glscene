//---------------------------------------------------------------------------

#include <vcl.h>
#include <math.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLState"
#pragma link "VectorTypes"
#pragma link "GLGraph"
#pragma link "VectorGeometry"
#pragma link "OpenGL1x"
#pragma link "GLUserShader"
#pragma link "GLTexture"
#pragma link "jpeg"
#pragma link "GLCadencer"
#pragma link "GLWaterPlane"
#pragma link "GLWin32Viewer"
#pragma link "GLMisc"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma resource "*.dfm"
TForm1 *Form1;

void TForm1::ClickWater(int x, int y)
{
  TVector ip;
  // create a ripple in the pond on a right-mousebutton click
  GLSceneViewer1->Buffer->
    ScreenVectorIntersectWithPlaneXZ(VectorMake
                                     (x, GLSceneViewer1->Height - y, 0),
                                     GLWaterPlane1->Position->Y, ip);
  GLWaterPlane1->CreateRippleAtWorldPos(ip);
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  SetCurrentDir(ExtractFilePath(Application->ExeName) + "..\\..\\media");

  // Load the cube map which is used both for environment and as reflection texture

  TGLTexture *t = GLMaterialLibrary1->Materials->Items[0]->Material->Texture;
  t->ImageClassName = __classid(TGLCubeMapImage)->ClassName();
  TGLCubeMapImage *img = (TGLCubeMapImage *) t->Image;
  // Load all 6 texture map components of the cube map
  // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
  // and follow the RenderMan specs/conventions
  img->Picture[cmtPX]->LoadFromFile("cm_left.jpg");
  img->Picture[cmtNX]->LoadFromFile("cm_right.jpg");
  img->Picture[cmtPY]->LoadFromFile("cm_top.jpg");
  img->Picture[cmtNY]->LoadFromFile("cm_bottom.jpg");
  img->Picture[cmtPZ]->LoadFromFile("cm_back.jpg");
  img->Picture[cmtNZ]->LoadFromFile("cm_front.jpg");

  GLWaterPlane1->Mask->LoadFromFile("basinMask.bmp");
  GLHeightField1->Material->Texture->Image->LoadFromFile("clover.jpg");
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
  if(Shift.Contains(ssRight))
    ClickWater(X, Y);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  if(Shift.Contains(ssLeft))
  {
    GLCamera1->MoveAroundTarget(my - Y, mx - X);
    // pseudo-fresnel
    TGLMaterial *m = GLMaterialLibrary1->LibMaterialByName("CubeMap")->Material;
    m->FrontProperties->Diffuse->Alpha =
      0.3 + 0.5 * sqrt(1 -
                       GLCamera1->Position->Y / GLCamera1->DistanceToTarget());
  }
  else if(Shift.Contains(ssRight))
    ClickWater(X, Y);
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLUserShader1DoApply(TObject * Sender,
                                             TRenderContextInfo & rci)
{
  // Here is the shader trick: the same cubemap is used in reflection mode
  // for the pond, and in normal mode for the environment sphere
  // Our basic user shader takes care of that.
  int cubeMapMode;
  if(reflectionToggle)
  {
    cubeMapMode = GL_REFLECTION_MAP_ARB;
    rci.GLStates->SetGLState(stBlend);
  }
  else
  {
    cubeMapMode = GL_NORMAL_MAP_ARB;
    rci.GLStates->UnSetGLState(stBlend);
  }

  glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, cubeMapMode);
  glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, cubeMapMode);
  glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, cubeMapMode);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLUserShader1DoUnApply(TObject * Sender, int Pass,
                                               TRenderContextInfo & rci,
                                               bool & Continue)
{
  rci.GLStates->UnSetGLState(stBlend);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1BeforeRender(TObject * Sender)
{
  reflectionToggle = false;     // toggle for environment sphere  
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLDirectOpenGL1Render(TObject * Sender,
                                              TRenderContextInfo & rci)
{
  reflectionToggle = true;      // toggle for pond/water plane  
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  String s;
  s.printf(" / %.3f ms", GLWaterPlane1->LastIterationStepTime * 1000);
  Caption = GLSceneViewer1->FramesPerSecondText() + s;
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  GLSceneViewer1->Invalidate();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLHeightField1GetHeight(const float x,
                                                const float y, float &z,
                                                TVector4f & color,
                                                TTexPoint & texPoint)
{
  z = 0.5 -
    (GLWaterPlane1->Mask->Bitmap->Canvas->
     Pixels[Round(x + 64)][Round(y + 64)] & 0xFF) / 255;
}

//---------------------------------------------------------------------------


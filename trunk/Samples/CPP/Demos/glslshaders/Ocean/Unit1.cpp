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
#pragma link "GLGraph"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLSkydome"
#pragma link "GLUserShader"
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
	SetGLSceneMediaDir();
	// Load the cube map which is used both for environment and as reflection texture
	TGLTexture *tex = MatLib->LibMaterialByName("water")->Material->Texture;
	tex->Image->LoadFromFile("noise.bmp");
	tex->ImageClassName = __classid(TGLCubeMapImage)->ClassName();
	TGLCubeMapImage *img = (TGLCubeMapImage *) tex->Image;
	// Load all 6 texture map components of the cube map
	// The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
	// and follow the RenderMan specs/conventions
	img->Picture[CmtPX]->LoadFromFile("cm_left.jpg");
	img->Picture[CmtNX]->LoadFromFile("cm_right.jpg");
	img->Picture[CmtPY]->LoadFromFile("cm_top.jpg");
	img->Picture[CmtNY]->LoadFromFile("cm_bottom.jpg");
	img->Picture[CmtPZ]->LoadFromFile("cm_back.jpg");
	img->Picture[CmtNZ]->LoadFromFile("cm_front.jpg");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DOInitializeRender(TObject *Sender, TRenderContextInfo &rci)
{
	if ((! (GL_ARB_shader_objects && GL_ARB_vertex_program && GL_ARB_vertex_shader && GL_ARB_fragment_shader)))
	{
	  ShowMessage("Your hardware does not support GLSL to execute this demo!");
	  //	  return (void) NULL;
	}
  if (DOInitialize->Tag != 0)
	exit;
  DOInitialize->Tag = 1;

  GLSceneViewer1->Buffer->RenderingContext->Deactivate();
  GLMemoryViewer1->RenderCubeMapTextures(MatLib->LibMaterialByName("cubeMap")->Material->Texture);
  GLSceneViewer1->Buffer->RenderingContext->Activate();

  TGLProgramHandle *programObject = new TGLProgramHandle(true);
  programObject->AddShader(__classid(TGLVertexShaderHandle), LoadAnsiStringFromFile("ocean_vp.glsl"), true);
  programObject->AddShader(__classid(TGLFragmentShaderHandle), LoadAnsiStringFromFile("ocean_fp.glsl"), true);

  if (! programObject->LinkProgram())
	throw Exception(programObject->InfoLog());

  programObject->UseProgramObject();
  programObject->Uniform1i["NormalMap"] = 0;
  programObject->Uniform1i["EnvironmentMap"] = 1;
  programObject->EndUseProgramObject();

  // initialize the heightmap
  rci.GLStates->TextureBinding[0][ttTexture2D] =
	  MatLib->LibMaterialByName("water")->Material->Texture->Handle;

  // initialize the heightmap
  rci.GLStates->TextureBinding[1][ttTextureCube] =
	 MatLib->LibMaterialByName("cubeMap")->Material->Texture->Handle;

  if (! programObject->ValidateProgram())
	throw Exception(programObject->InfoLog());
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLUserShader1DoApply(TObject *Sender, TRenderContextInfo &rci)

{
  Vectorgeometry::TVector camPos;

  programObject->UseProgramObject();
  programObject->Uniform1f["Time"] = GLCadencer1->CurrentTime * 0.05;
  camPos = GLCamera->AbsolutePosition;
  programObject->Uniform4f["EyePos"] = camPos;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLUserShader1DoUnApply(TObject *Sender, int Pass, TRenderContextInfo &rci,
          bool &Continue)
{
  programObject->EndUseProgramObject();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft))
  {
	GLCamera->MoveAroundTarget(my-Y, mx-X);
  }
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  if ((dmx != 0) || (dmy != 0)) {
	GLCamera->MoveAroundTarget(dmy * 0.3, dmx * 0.3);
	dmx = 0;
	dmy = 0;
  }
  GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLHeightField1GetHeight(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint)
{
  z = 0;
}
//---------------------------------------------------------------------------
const int
  cExtent = 200;

  TGLVBOArrayBufferHandle *vbo;
  int nbVerts;


void __fastcall TForm1::DOOceanPlaneRender(TObject *Sender, TRenderContextInfo &rci)

{
  int x, y;
  TTexPointList *v;
  bool cont;

  GLUserShader1DoApply(Sender, rci);
  glEnableClientState(GL_VERTEX_ARRAY);

  if (! vbo)
  {
	v = new TTexPointList;

	v->Capacity = (cExtent + 1)*(cExtent + 1);
	y = -cExtent;
	while (y < cExtent)
	{
	  x = -cExtent;
	  while (x <= cExtent) {
		v->Add(y, x);
		v->Add(y + 2, x);
		x = x + 2;
	  }
	  y = y + 2;
	  v->Add(y, cExtent);
	  v->Add(y, -cExtent);
	}
	vbo = new TGLVBOArrayBufferHandle(true);
	vbo->Bind();
	vbo->BufferData(v->List, v->DataSize(), GL_STATIC_DRAW_ARB);
	nbVerts = v->Count;

	glVertexPointer(2, GL_FLOAT, 0, NULL);
	glDrawArrays(GL_QUAD_STRIP, 0, nbVerts);

	vbo->UnBind();

	delete v;
  }
  else
  {
	vbo->Bind();
	glVertexPointer(2, GL_FLOAT, 0, NULL);
	glDrawArrays(GL_TRIANGLE_STRIP, 0, nbVerts);
	vbo->UnBind();
  }
  glDisableClientState(GL_VERTEX_ARRAY);
  GLUserShader1DoUnApply(Sender, 0, rci, cont);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1BeforeRender(TObject *Sender)
{
//  GLMemoryViewer1->Buffer->RenderingContext->ShareLists(GLSceneViewer1->Buffer->RenderingContext);
//  GLMemoryViewer1->BeforeRender = NULL;
}
//---------------------------------------------------------------------------

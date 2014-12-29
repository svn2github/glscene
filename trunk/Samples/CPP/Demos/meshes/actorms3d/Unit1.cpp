//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLCustomShader"
#pragma link "GLFBORenderer"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLSArchiveManager"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLSLShader"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLTexture"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}


void __fastcall TForm1::LoadTexture(const String AName, const String ext)
{
  TStream* strm;
///  TGLCompositeImage* img;

  strm = new TStream();
/*
  img = MatLib->TextureByName(AName)->Image as TGLCompositeImage;
  strm = GLSArchiveManager1->Archives[0]->GetContent("Main/"+AName+"."+ext);
  img->LoadFromStream(strm);
*/
 strm->Free();
}


//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  SetGLSceneMediaDir();
///  with GLSArchiveManager1->Archives[0] do
/*
	GLSArchiveManager1->Archives->Items[0]->LoadFromFile("ActorMS3D.zlib");
	LoadTexture("floor_parquet", "JPG");
	GLSArchiveManager1->Archives->Items[0]->LoadTexture("Chair", "PNG");
	GLSArchiveManager1->Archives->Items[0]->LoadTexture("Hair", "PNG");
	GLSArchiveManager1->Archives->Items[0]->LoadTexture("Woman4-Remap-texture", "PNG");
	Actor1->LoadFromStream("Woman4.ms3d", GetContent("Main/Woman4.ms3d"));
	Chair1->LoadFromStream("Chair.ms3d", GetContent("Main/Chair.ms3d"));


  MatLib->TextureByName('Lightspot').Image.LoadFromFile('Flare1.bmp');

  Actor1->AnimationMode = aamNone;
  Actor1->Scale.SetVector(0.1, 0.1, 0.1, 0);
  Chair1->Scale.SetVector(0.35, 0.35, 0.35, 0);

  //The MS3D Model has multiple animations all in sequence.
///  with Actor1.Animations.Add do
  begin
	Reference := aarSkeleton;
	StartFrame := 2; //because first frame is going to be the RootPos
	EndFrame := 855;
	Name := 'Dance';
  end;
  with Actor1.Animations.Add do
  begin
	Reference := aarSkeleton;
	StartFrame := 856;
	EndFrame := 1166;
	Name := 'Sexy Walk';
  end;

///  with Actor1.Animations.Add do
  begin
	Reference := aarSkeleton;
	StartFrame := 1168;
	EndFrame := 1203;
	Name := 'Cartwheel';
  end;

///  with Actor1.Animations.Add do
  begin
	Reference := aarSkeleton;
	StartFrame := 1205;
	EndFrame := 1306;
	Name := 'Hand Flip';
  end;

///  with Actor1.Animations.Add do
  begin
	Reference := aarSkeleton;
	StartFrame := 1308;
	EndFrame := 1395;
	Name := 'Wave';
  end;

  with Actor1.Animations.Add do
  begin
	Reference := aarSkeleton;
	StartFrame := 1397;
	EndFrame := 2014;
	Name := 'Sun Salutation';
  end;

///  with Actor1.Animations.Add do
  begin
	Reference = aarSkeleton;
	StartFrame = 2016;
	EndFrame = 2133;
	Name = 'Sit';
  end;

  FBiasMatrix = CreateScaleAndTranslationMatrix(VectorMake(0.5, 0.5, 0.5),
	VectorMake(0.5, 0.5, 0.5));
  GLSLShader1->VertexProgram->LoadFromFile("shadowmap_vp.glsl");
  GLSLShader1->FragmentProgram->LoadFromFile("shadowmap_fp.glsl");
  GLSLShader1->Enabled = true;
*/
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Actor1EndFrameReached(TObject *Sender)
{
  if (Actor1->AnimationMode == aamNone)
  {
	btnStartStop->Caption = "Start";
	Timer1->Enabled = false;
	aniPos->Enabled = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSLShader1Initialize(TGLCustomGLSLShader *Shader)
{
  Shader->Param["TextureMap"]->AsTexture2D[0] = MatLib->TextureByName("floor_parquet");
  Shader->Param["ShadowMap"]->AsTexture2D[1] = MatLib->TextureByName(GLFrameBuffer->DepthTextureName);
  Shader->Param["LightspotMap"]->AsTexture2D[2] = MatLib->TextureByName("Lightspot");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
  Actor1->NextFrame();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnStartStopClick(TObject *Sender)
{
  if (Actor1->AnimationMode == aamNone)
  {
	if (Actor1->CurrentFrame == Actor1->EndFrame)
	  Actor1->CurrentFrame = Actor1->StartFrame;
	Actor1->AnimationMode = aamPlayOnce;
	btnStartStop->Caption = "Stop";
	Timer1->Enabled = true;
	aniPos->Enabled = false;
  }
  else
  {
	Actor1->AnimationMode = aamNone;
	btnStartStop->Caption = "Start";
	Timer1->Enabled = false;
	aniPos->Enabled = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject *Sender)
{
  Actor1->PrevFrame();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  Actor1->AnimationMode = aamNone;
  GLCadencer1->Enabled = false;
  GLSLShader1->Enabled = false;
}
//---------------------------------------------------------------------------


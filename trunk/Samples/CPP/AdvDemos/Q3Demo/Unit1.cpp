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
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLParticleFX"
#pragma link "GLScene"
#pragma link "GLShadowPlane"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFileMD3"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
	stl     = new TStringList;
	stlBuf  = new TStringList;
	stlPics = new TStringList;

}

void TForm1::FetchStlBuf(String Prefix)
	  {
		String stFileName;

		stFileName = SkinFilePath+Prefix+SkinShortName;
		if (FileExists(stFileName))
		   stl->LoadFromFile(stFileName);
		stlBuf->AddStrings(stl);
	  }

String  __fastcall TForm1::GetMaterialPicFilename(String MatName)
   {
		int n;
		MatName = UpperCase(MatName);

		stlBuf->Strings[0] ="text";
		for (n = 0; n < stlBuf->Count-1; n++)
		{
		 if (Pos(MatName,UpperCase(stlBuf->Strings[n]))==1)
		 {
			  return ExtractFileName(StringReplace(stlBuf->Strings[n],"//","\\", TReplaceFlags()<<rfReplaceAll));
//			  Break;
		 }

		}

   }

void TForm1::DoActorMaterials(TGLActor *Actor)
{
  int t,u;
  for (t = 0; t < Actor->MeshObjects->Count-1; t++)
  {
	for (u = 0; u < Actor->MeshObjects->Items[t]->FaceGroups->Count-1; u++)
	{
	  MatName = Actor->MeshObjects->Items[t]->FaceGroups->Items[u]->MaterialName;
	  PicFileName = GetMaterialPicFilename(MatName);
	  Idx = stlPics->IndexOf(PicFileName);
	  if (Idx=-1)
	  {
		stlPics->AddObject(PicFileName,Actor->MeshObjects->Items[t]->FaceGroups->Items[u]);
		PicFileName = SkinFilePath+ChangeFileExt(PicFileName,GraphicFileExt);
		if (FileExists(PicFileName))
		  MatLib->Materials->GetLibMaterialByName(MatName)->Material->Texture->Image->LoadFromFile(PicFileName);
	  }
	  else;
///abstract	   Actor->MeshObjects->Items[t]->FaceGroups->Items[u]->MaterialName = TFaceGroup(stlPics->Objects[Idx])->MaterialName;
	}
  }
}


//-------------------------------------------------------------
void TForm1::LoadSkin(String SkinShortName,
	  TGLActor *Actor1, TGLActor *Actor2, TGLActor *Actor3,
	  String GraphicFileExt)
{

   TGLMaterialLibrary *MatLib;

   MatLib = Actor1->MaterialLibrary;
///   if (MatLib=NULL) Exit;

	 SkinFilePath = IncludeTrailingBackslash(SkinFilePath);
	 SkinShortName = ChangeFileExt(SkinShortName,".skin");

	 FetchStlBuf("Head_");
	 FetchStlBuf("Upper_");
	 FetchStlBuf("Lower_");

	 DoActorMaterials(Actor1);
	 DoActorMaterials(Actor2);
	 DoActorMaterials(Actor3);
}



void TForm1::BuildModel(void)
{
  String ModelPath = ExtractFilePath(ParamStr(0));
  int I = ModelPath.Pos("Q3Demo");
  if (I != 0) {
	ModelPath.Delete(I+7,ModelPath.Length()-I);
	ModelPath += "Model\\";
	SetCurrentDir(ModelPath);
  }
  // Load model data from MD3 files into the actor
  //
  Legs->LoadFromFile("lower.md3");
  Torso->LoadFromFile("upper.md3");
  Head->LoadFromFile("head.md3");
  Weapon->LoadFromFile("plasma.md3");

  // Load the required tag lists
  // These are used to loacally transform the separate
  // parts of the model into the correct places
  //
  LegsTags = new TMD3TagList;
  LegsTags->LoadFromFile("lower.md3");
  TorsoTags = new TMD3TagList;
  TorsoTags->LoadFromFile("upper.md3");

  // The tag_flash tag in the railgun model gives the
  // transform offset for the nozzle of the gun. I've
  // added a GunSmoke dummycube there to demonstrate with
  // a smoke like effect
  //
  WeaponTags = new TMD3TagList;
  WeaponTags->LoadFromFile("plasma.md3");
  GunSmoke->Matrix = WeaponTags->GetTransform("tag_flash",0);

  // Apply textures to preloaded materials
  // The md3 file loader puts a material into the actors
  // assigned material library (if there is one) with
  // the names of the mesh objects. The skin and/or shader
  // files can tell you which objects need which textures
  // loaded
  //

  // Mrqzzz's quick procedure for loading skins

/// default  LoadSkin('.\model\','default',Head,Torso,Legs,'.jpg');

  // Alternative method
  //LoadQ3Skin('.\model\lower_default.skin',Legs);
  //LoadQ3Skin('.\model\upper_default.skin',Torso);
  //LoadQ3Skin('.\model\head_default.skin',Head);

  // Load the weapon textures
  //
  MatLib->Materials->
	GetLibMaterialByName("plasma2")->Material->Texture->
	  Image->LoadFromFile("plasma2.jpg");

  // Load the animation data from the cfg file
  // This procedure populates an animation list from a
  // file or TStrings object. The last parameter tells
  // it which class of animation is to be loaded.
  //
  LoadQ3Anims(Legs->Animations,"animation.cfg","BOTH");
  LoadQ3Anims(Legs->Animations,"animation.cfg","LEGS");
  LoadQ3Anims(Torso->Animations,"animation.cfg","BOTH");
  LoadQ3Anims(Torso->Animations,"animation.cfg","TORSO");
}


Vectorgeometry::TMatrix __fastcall TForm1::InterpolateMatrix(Vectorgeometry::TMatrix m1,
			Vectorgeometry::TMatrix m2, float delta)
{

}


//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  // Build the model
  BuildModel();

  ModelCube->Scale->SetVector(0.044,0.044,0.044);

  Legs->AnimationMode = aamLoop;
  Torso->AnimationMode = aamLoop;

  // Populate the combo boxes with the names of the
  // loaded animations
  Legs->Animations->SetToStrings(ComboBox1->Items);
  Torso->Animations->SetToStrings(ComboBox2->Items);

  // Set up some initial animations
  ComboBox1->ItemIndex = ComboBox1->Items->IndexOf("LEGS_IDLE");
  ComboBox2->ItemIndex = ComboBox2->Items->IndexOf("TORSO_STAND");

  // And trigger them
  ComboBox1Change(NULL);
  ComboBox2Change(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ComboBox1Change(TObject *Sender)
{
  Legs->SwitchToAnimation(ComboBox1->Text, False);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ComboBox2Change(TObject *Sender)
{
  Torso->SwitchToAnimation(ComboBox2->Text,False);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  stl->Free();
  stlBuf->Free();
  stlPics->Free();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  mx = X; my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(my-Y, mx-X);
  if (Shift.Contains(ssRight))
	GLCamera1->AdjustDistanceToTarget(Power(1.05,my-Y));
  mx = X; my = Y;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::ComboSkinChange(TObject *Sender)
{
  LoadSkin(ComboSkin->Text,Head,Torso,Legs,".jpg");
}
//---------------------------------------------------------------------------

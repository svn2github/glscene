//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "BaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLParticleFX.hpp"
#include "GLScene.hpp"
#include "GLShadowPlane.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLFileMD3.hpp"
#include "Jpeg.hpp"

#include "Q3MD3.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TComboBox *ComboBox1;
	TComboBox *ComboBox2;
	TTrackBar *TrackBar1;
	TTrackBar *TrackBar2;
	TTrackBar *TrackBar3;
	TTrackBar *TrackBar4;
	TComboBox *ComboSkin;
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLDummyCube *ModelCube;
	TGLActor *Legs;
	TGLActor *Torso;
	TGLActor *Head;
	TGLActor *Weapon;
	TGLDummyCube *GunSmoke;
	TGLShadowPlane *GLShadowPlane1;
	TGLParticleFXRenderer *GLParticleFXRenderer1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLMaterialLibrary *MatLib;
	TGLPointLightPFXManager *GLPointLightPFXManager1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall ComboBox2Change(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall ComboSkinChange(TObject *Sender);
private:	// User declarations
	int mx, my;
	int Idx;
	String SkinFilePath;
	String SkinShortName;
	TStringList *stl;
	TStringList *stlBuf;
	TStringList *stlPics;
	String MatName;
	String PicFileName;
	String GraphicFileExt;

	void LoadSkin(String SkinShortName,
	  TGLActor* Actor1, TGLActor* Actor2, TGLActor* Actor3,
	  String GraphicFileExt);
	void FetchStlBuf(String Prefix);
	String __fastcall GetMaterialPicFilename(String MatName);
	void DoActorMaterials(TGLActor* Actor);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
	TMD3TagList *LegsTags;
	TMD3TagList *TorsoTags;
	TMD3TagList *WeaponTags;
	void BuildModel(void);
	__fastcall Vectorgeometry::TMatrix InterpolateMatrix(Vectorgeometry::TMatrix m1,
	        Vectorgeometry::TMatrix m2, float delta);

};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

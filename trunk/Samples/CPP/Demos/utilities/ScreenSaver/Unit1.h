//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "BaseClasses.hpp"
#include "GLBehaviours.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "ScreenSaver.hpp"

#include "Unit2.h"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TScreenSaver *ScreenSaver1;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLDummyCube *DummyCube2;
	TGLDummyCube *DummyCube3;
	TGLTorus *Torus1;
	TGLDummyCube *DummyCube4;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLLightSource *GLLightSource3;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall ScreenSaver1PropertiesRequested(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

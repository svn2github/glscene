//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "BaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLHUDObjects.hpp"
#include "GLLensFlare.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLParticleFX.hpp"
#include "GLScene.hpp"
#include "GLShadowPlane.hpp"
#include "GLSound.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include "GLScreenSaver.hpp"
#include "GLSMBASS.hpp"
#include "GLFileWAV.hpp"
#include "Jpeg.hpp"

//#include "Bass.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TGLSMBASS *GLSMBASS;
	TGLScene *Scene;
	TGLFreeForm *FFFirePlace;
	TGLDummyCube *DCFire;
	TGLLightSource *LSFire;
	TGLDummyCube *DCFireSource;
	TGLCylinder *CYLog;
	TGLDummyCube *DCFirTree;
	TGLDummyCube *DCTree;
	TGLFreeForm *FFFirTree;
	TGLProxyObject *POFirTree2;
	TGLProxyObject *POFirTree3;
	TGLCube *GLCube3;
	TGLCube *GLCube4;
	TGLDummyCube *DCDecoWhite;
	TGLProxyObject *GLProxyObject1;
	TGLProxyObject *GLProxyObject4;
	TGLProxyObject *GLProxyObject6;
	TGLDummyCube *DCDecoGold;
	TGLProxyObject *GLProxyObject2;
	TGLProxyObject *GLProxyObject3;
	TGLProxyObject *GLProxyObject5;
	TGLDummyCube *DCLensFlares;
	TGLLensFlare *GLLensFlare1;
	TGLLensFlare *GLLensFlare2;
	TGLLensFlare *GLLensFlare3;
	TGLLensFlare *GLLensFlare4;
	TGLLensFlare *GLLensFlare5;
	TGLLensFlare *GLLensFlare6;
	TGLDummyCube *DCGifts;
	TGLCube *GLCube1;
	TGLCube *GLCube2;
	TGLShadowPlane *GLShadowPlane;
	TGLFlatText *FTCountDown;
	TGLLensFlare *LSFireLens;
	TGLLightSource *LSRoom;
	TGLDummyCube *DCCameraTarget;
	TGLParticleFXRenderer *ParticleFXRenderer;
	TGLDummyCube *DCTemplates;
	TGLSphere *SPWhiteBall;
	TGLSphere *SPGoldBall;
	TGLHUDSprite *HSGLScene;
	TGLCamera *GLCamera;
	TTimer *Timer;
	TGLCadencer *GLCadencer;
	TGLMaterialLibrary *GLMaterialLibrary;
	TGLPolygonPFXManager *PFXFire;
	TGLSoundLibrary *GLSoundLibrary;
	TGLPolygonPFXManager *PFXTree;
	TGLWindowsBitmapFont *GLWindowsBitmapFont;
	TGLScreenSaver *GLScreenSaver1;
//	TScreenSaver *ScreenSaver;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall ViewerMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall TimerTimer(TObject *Sender);
	void __fastcall GLCadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
//	void __fastcall ScreenSaverCloseQuery(TObject *Sender, bool &CanClose);
//	void __fastcall ScreenSaverPreview(TObject *Sender, HWND previewHwnd);
//	void __fastcall ScreenSaverExecute(TObject *Sender);
//	void __fastcall ScreenSaverPropertiesRequested(TObject *Sender);
	void __fastcall ViewerDblClick(TObject *Sender);
	void __fastcall GLScreenSaver1CloseQuery(TObject *Sender, bool &CanClose);
	void __fastcall GLScreenSaver1Execute(TObject *Sender);
	void __fastcall GLScreenSaver1Preview(TObject *Sender, HWND previewHwnd);
	void __fastcall GLScreenSaver1PropertiesRequested(TObject *Sender);


private:	// User declarations
	int mx, my;
	float fireLight;
	bool inPreview, inSaver;
	Cardinal bStream;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

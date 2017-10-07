//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLGeomObjects.hpp"
#include "GLHUDObjects.hpp"
#include "GLLensFlare.hpp"
#include "GLObjects.hpp"
#include "GLParticleFX.hpp"
#include "GLScene.hpp"
#include "GLShadowPlane.hpp"
#include "GLSMBASS.hpp"
#include "GLSound.hpp"
#include "GLTexture.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include "GLScreenSaver.hpp"
#include "GLFireFX.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
//---------------------------------------------------------------------------
class TMain : public TForm
{
__published:	// IDE-managed Components
        TGLSceneViewer *Viewer;
        TGLScene *Scene;
        TGLMaterialLibrary *GLMaterialLibrary;
        TGLCadencer *GLCadencer;
        TGLSMBASS *GLSMBASS;
        TGLSoundLibrary *GLSoundLibrary;
        TGLPolygonPFXManager *PFXTree;
        TGLPolygonPFXManager *PFXFire;
        TGLWindowsBitmapFont *GLWindowsBitmapFont;
        TTimer *Timer;
        TGLCamera *GLCamera;
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
        TGLFreeForm *GL;
        TGLFireFXManager *GLFireFXManager1;
        TGLDummyCube *Fire;
        TGLSphere *GLSphere1;
        TGLPlane *GLPlane1;
        TGLDummyCube *fireplace;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall TimerTimer(TObject *Sender);
        void __fastcall GLCadencerProgress(TObject *Sender,
          const double deltaTime, const double newTime);
        void __fastcall FormResize(TObject *Sender);
        void __fastcall ViewerDblClick(TObject *Sender);
        void __fastcall ScreenSaverCloseQuery(TObject *Sender,
          bool &CanClose);
        void __fastcall ScreenSaverPreview(TObject *Sender,
          HWND previewHwnd);
        void __fastcall ScreenSaverExecute(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMain *Main;
//---------------------------------------------------------------------------
#endif

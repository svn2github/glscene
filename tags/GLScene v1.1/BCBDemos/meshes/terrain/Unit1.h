//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <GLLensFlare.hpp>      // Pascal unit
#include <VectorGeometry.hpp>   // Pascal unit
#include <GLSMBASS.hpp>         // Pascal unit
#include <GLSound.hpp>          // Pascal unit
#include <GLWin32Viewer.hpp>    // Pascal unit
#include <GLSkydome.hpp>        // Pascal unit
#include <GLBitmapFont.hpp>     // Pascal unit
#include <GLHUDObjects.hpp>     // Pascal unit
#include <GLTexture.hpp>        // Pascal unit
#include <StdCtrls.hpp>         // Pascal unit
#include <GLCadencer.hpp>       // Pascal unit
#include <ExtCtrls.hpp>         // Pascal unit
#include <GLHeightData.hpp>     // Pascal unit
#include <jpeg.hpp>             // Pascal unit
#include <GLMisc.hpp>           // Pascal unit
#include <GLObjects.hpp>        // Pascal unit
#include <GLTerrainRenderer.hpp>        // Pascal unit
#include <GLScene.hpp>          // Pascal unit
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLSceneViewer * GLSceneViewer1;
  TGLBitmapHDS *GLBitmapHDS1;
  TGLScene *GLScene1;
  TGLCamera *GLCamera1;
  TGLDummyCube *DummyCube1;
  TGLTerrainRenderer *TerrainRenderer1;
  TTimer *Timer1;
  TGLCadencer *GLCadencer1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TGLBitmapFont *BitmapFont1;
  TGLHUDText *HUDText1;
  TGLSkyDome *SkyDome1;
  TGLSprite *SPMoon;
  TGLSprite *SPSun;
  TGLDummyCube *DCSound;
  TGLSMBASS *GLSMBASS1;
  TTimer *TISound;
  TGLSoundLibrary *GLSoundLibrary;
  TGLLensFlare *GLLensFlare;
  TGLDummyCube *GLDummyCube1;
  TGLRenderPoint *InitialRenderPoint;
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall FormKeyPress(TObject * Sender, char &Key);
  void __fastcall TISoundTimer(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);

  int mx, my;
  bool fullScreen;
  float FCamHeight;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 

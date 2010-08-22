//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <GLGeomObjects.hpp>    // Pascal unit
#include <OpenGL1x.hpp>         // Pascal unit
#include <VectorGeometry.hpp>   // Pascal unit
#include <GLFile3DS.hpp>        // Pascal unit
#include <Octree.hpp>           // Pascal unit
#include <VectorLists.hpp>      // Pascal unit
#include <GLNavigator.hpp>      // Pascal unit
#include <GLCollision.hpp>      // Pascal unit
#include <jpeg.hpp>             // Pascal unit
#include <GLObjects.hpp>        // Pascal unit
#include <GLMisc.hpp>           // Pascal unit
#include <GLWin32Viewer.hpp>    // Pascal unit
#include <GLCadencer.hpp>       // Pascal unit
#include <GLTexture.hpp>        // Pascal unit
#include <GLScene.hpp>          // Pascal unit
#include <GLVectorFileObjects.hpp>      // Pascal unit
#include <GLFPSMovement.hpp>
#include <ExtCtrls.hpp>         // Pascal unit
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TGLCadencer *GLCadencer1;
  TTimer *Timer1;
  TGLCamera *FirstPersonCamera;
  TGLFreeForm *Map1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TGLLightSource *GLLight;
  TGLDummyCube *World;
  TGLCamera *ThirdPersonCamera;
  TGLSphere *PlayerSphere;
  TGLLightSource *GLLightSource1;
  TGLSphere *PlayerCentre;
  TGLDummyCube *Player;
  TGLFreeForm *Map2;
  TGLDummyCube *Bot;
  TGLSphere *BotCenter;
  TGLSphere *BotSphere;
  TGLNavigator *Navigator1;
  TGLFPSMovementManager *MovManager;
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall FormKeyDown(TObject * Sender, WORD & Key, TShiftState Shift);
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif


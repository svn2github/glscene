//---------------------------------------------------------------------------

#ifndef fFurBallH
#define fFurBallH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <VerletHairClasses.hpp>        // Pascal unit
#include <VerletClasses.hpp>    // Pascal unit
#include <GLNavigator.hpp>      // Pascal unit
#include <GLShadowPlane.hpp>    // Pascal unit
#include <VectorGeometry.hpp>   // Pascal unit
#include <GLExtrusion.hpp>      // Pascal unit
#include <GLTexture.hpp>        // Pascal unit
#include <GLCadencer.hpp>       // Pascal unit
#include <GLMisc.hpp>           // Pascal unit
#include <GLObjects.hpp>        // Pascal unit
#include <GLScene.hpp>          // Pascal unit
#include <GLWin32Viewer.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>         // Pascal unit
#include "dynode.hpp"        // Pascal unit
//---------------------------------------------------------------------------
class TfrmFurBall:public TForm
{
__published:                   // IDE-managed Components
  TGLCadencer * GLCadencer1;
  TGLScene *GLScene1;
  TGLDummyCube *DC_LightHolder;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  TGLSceneViewer *GLSceneViewer1;
  TGLShadowPlane *GLShadowPlane_Floor;
  TGLShadowPlane *GLShadowPlane_Wall;
  TGLSphere *Sphere1;
  TGLDummyCube *DCShadowCaster;
  TGLSphere *FurBall;
  TCheckBox *CheckBox_LockBall;
  TLabel *Label1;
  TCheckBox *CheckBox_FurGravity;
  TCheckBox *CheckBox_WindResistence;
  TGLShadowPlane *GLShadowPlane_Floor2;
  TGLLines *GLLines1;
  TGLShadowPlane *GLShadowPlane_Wall2;
  TGLShadowPlane *GLShadowPlane_Wall3;
  TCheckBox *CheckBox_Bald;
  TLabel *Label_FPS;
  TTimer *Timer1;
  TCheckBox *CheckBox_Shadows;
  TCheckBox *CheckBox_Inertia;
  TTrackBar *TrackBar_WindForce;
  void __fastcall FormClose(TObject * Sender, TCloseAction & Action);
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
  void __fastcall DC_LightHolderProgress(TObject * Sender,
                                         const double deltaTime,
                                         const double newTime);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
  void __fastcall FormMouseWheel(TObject * Sender, TShiftState Shift,
                                 int WheelDelta, TPoint & MousePos,
                                 bool & Handled);
  void __fastcall CheckBox_FurGravityClick(TObject * Sender);
  void __fastcall CheckBox_WindResistenceClick(TObject * Sender);
  void __fastcall TrackBar_WindForceChange(TObject * Sender);
  void __fastcall CheckBox_BaldClick(TObject * Sender);
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall CheckBox_ShadowsClick(TObject * Sender);
  void __fastcall CheckBox_InertiaClick(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
    __fastcall TfrmFurBall(TComponent * Owner);

  PdxBody odeFurBallBody;
  PdxGeom odeFurBallGeom;

  PdxWorld world;
  PdxSpace space;
  TdJointGroupID contactgroup;

  TVerletWorld *VerletWorld;
  TList *HairList;
  TVCSphere *VCSphere;
  float PhysicsTime;

  TVFGravity *Gravity;
  TVFAirResistance *AirResistance;

  void CreateBall();
  void CreateFur();
  void CreateRandomHair();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmFurBall *frmFurBall;
//---------------------------------------------------------------------------
#endif


//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <GLGeomObjects.hpp>    // Pascal unit
#include <GLWin32Viewer.hpp>    // Pascal unit
#include <GLSMBASS.hpp>         // Pascal unit
#include <GLSMFMOD.hpp>         // Pascal unit
#include <GLSound.hpp>          // Pascal unit
#include <GLMisc.hpp>           // Pascal unit
#include <GLObjects.hpp>        // Pascal unit
#include <GLScene.hpp>          // Pascal unit
#include <GLCadencer.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>         // Pascal unit
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene;
  TGLSceneViewer *GLSceneViewer;
  TGLCamera *GLCamera1;
  TGLDummyCube *DummyCube;
  TGLSphere *Sphere;
  TGLLightSource *GLLightSource;
  TGLSMFMOD *GLSMFMOD;
  TGLSoundLibrary *GLSoundLibrary;
  TGLCadencer *GLCadencer1;
  TTimer *Timer;
  TGLSphere *Mickey;
  TGLSphere *Sphere2;
  TGLSphere *Sphere3;
  TGLCone *Cone1;
  TTrackBar *TrackBar;
  TGLPlane *Plane1;
  TGLDisk *Disk1;
  TGLTorus *Torus1;
  TTrackBar *TrackBar1;
  TGLSMBASS *GLSMBASS;
  TPanel *Panel1;
  TLabel *Label1;
  TRadioButton *RBBass;
  TRadioButton *RBFMOD;
  TButton *Button1;
  TButton *Button2;
  void __fastcall SphereProgress(TObject * Sender, const double deltaTime,
                                 const double newTime);
  void __fastcall TrackBarChange(TObject * Sender);
  void __fastcall TrackBar1Change(TObject * Sender);
  void __fastcall TimerTimer(TObject * Sender);
  void __fastcall RBFMODClick(TObject * Sender);
  void __fastcall Button1Click(TObject * Sender);
  void __fastcall Button2Click(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 

//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLWin32Viewer.hpp"
#include "GLCadencer.hpp"
#include "GLMisc.hpp"
#include "GLODEManager.hpp"
#include "GLScene.hpp"
#include "GLObjects.hpp"
#include "GLGeomObjects.hpp"
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLSceneViewer * GLSceneViewer1;
  TGLScene *GLScene1;
  TGLCadencer *GLCadencer1;
  TGLODEManager *GLODEManager1;
  TGLODEJointList *GLODEJointList1;
  TGLDummyCube *GLDummyCube1;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  TGLDummyCube *Machine;
  TGLCube *Arm;
  TGLCylinder *Pin2;
  TGLCube *Slider;
  TGLCylinder *Wheel;
  TGLCylinder *Pin1;
  TGLCylinder *Axle;
  TGLSphere *GLSphere1;
  void __fastcall GLCadencer1Progress(TObject * Sender,
                                      const double deltaTime,
                                      const double newTime);
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 

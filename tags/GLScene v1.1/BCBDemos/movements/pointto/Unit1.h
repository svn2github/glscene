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
#include <GLCadencer.hpp>       // Pascal unit
#include <VectorGeometry.hpp>   // Pascal unit
#include <GLMisc.hpp>           // Pascal unit
#include <GLScene.hpp>          // Pascal unit
#include <GLObjects.hpp>        // Pascal unit
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLSceneViewer * GLSceneViewer1;
  TGLScene *GLScene1;
  TGLCamera *GLCamera1;
  TGLDummyCube *DCSphere;
  TGLArrowLine *ArrowLine;
  TGLLightSource *GLLightSource1;
  TGLSphere *Sphere;
  TGLDummyCube *DCArrow;
  TGLCadencer *GLCadencer1;
  TGLDisk *Disk1;
  TGLDisk *Disk2;
  TGLLines *Lines1;
  TGLPlane *Plane1;
  TGLLines *Lines2;
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
 

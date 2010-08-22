//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLMisc.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <ComCtrls.hpp>
#include "GLGraph.hpp"
#include "GLObjects.hpp"
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TCheckBox *CheckBox1;
  TTrackBar *TrackBar1;
  TLabel *Label1;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  TGLHeightField *GLHeightField1;
  TGLXYZGrid *XYGrid;
  TGLXYZGrid *XZGrid;
  TGLXYZGrid *YZGrid;
  void __fastcall CheckBox1Click(TObject * Sender);
  void __fastcall TrackBar1Change(TObject * Sender);
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


//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "GLWin32Viewer.hpp"
#include "GLCadencer.hpp"
#include "GLMisc.hpp"
#include "GLODEManager.hpp"
#include "GLScene.hpp"
#include <ComCtrls.hpp>
#include "GLObjects.hpp"
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TPanel * Panel1;
  TLabel *Label1;
  TGLSceneViewer *GLSceneViewer1;
  TGLScene *GLScene1;
  TGLCadencer *GLCadencer1;
  TGLODEManager *GLODEManager1;
  TButton *Button1;
  TButton *Button2;
  TButton *Button3;
  TButton *Button4;
  TGroupBox *GroupBox1;
  TCheckBox *CheckBox1;
  TCheckBox *CheckBox2;
  TTrackBar *TrackBar1;
  TTrackBar *TrackBar2;
  TGLDummyCube *ODEObjects;
  TGLDummyCube *GLDummyCube1;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
  void __fastcall Button1Click(TObject * Sender);
  void __fastcall Button2Click(TObject * Sender);
  void __fastcall Button3Click(TObject * Sender);
  void __fastcall Button4Click(TObject * Sender);
  void __fastcall CheckBoxClick(TObject * Sender);
  void __fastcall TrackBar1Change(TObject * Sender);
  void __fastcall TrackBar2Change(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif


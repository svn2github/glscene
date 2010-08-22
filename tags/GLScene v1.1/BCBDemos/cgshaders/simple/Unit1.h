//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "GLMisc.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLCadencer.hpp"
#include "GLTexture.hpp"
#include "GLCgShader.hpp"
#include <ComCtrls.hpp>
#include "GLVectorFileObjects.hpp"
#include "GLGraph.hpp"
#include "GLObjects.hpp"
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TPanel * Panel1;
  TSplitter *Splitter1;
  TPanel *Panel2;
  TPanel *Panel3;
  TGLScene *GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TTimer *Timer1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TGLCadencer *GLCadencer1;
  TCgShader *CgShader1;
  TPageControl *PageControl1;
  TTabSheet *TabSheet1;
  TTabSheet *TabSheet2;
  TPanel *Panel4;
  TLabel *VPName;
  TCheckBox *CBVP;
  TPanel *Panel5;
  TPanel *Panel6;
  TPanel *Panel7;
  TButton *ApplyVP;
  TMemo *Memo1;
  TLabel *Label1;
  TButton *Button2;
  TButton *Button3;
  TPanel *Panel8;
  TLabel *FPName;
  TCheckBox *CBFP;
  TPanel *Panel9;
  TPanel *Panel10;
  TLabel *Label3;
  TMemo *Memo2;
  TButton *Button4;
  TButton *Button5;
  TPanel *Panel11;
  TButton *ApplyFP;
  TMemo *FragmentCode;
  TMemo *VertexCode;
  TGLFreeForm *GLFreeForm1;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  TGLXYZGrid *GLXYZGrid1;
  void __fastcall CgShader1Initialize(TCustomCgShader * CgShader);
  void __fastcall CgShader1ApplyVP(TCgProgram * CgProgram, TObject * Sender);
  void __fastcall CBVPClick(TObject * Sender);
  void __fastcall CBFPClick(TObject * Sender);
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
  void __fastcall VertexCodeChange(TObject * Sender);
  void __fastcall FragmentCodeChange(TObject * Sender);
  void __fastcall ApplyVPClick(TObject * Sender);
  void __fastcall ApplyFPClick(TObject * Sender);
  void __fastcall Button2Click(TObject * Sender);
  void __fastcall Button4Click(TObject * Sender);
  void __fastcall Button3Click(TObject * Sender);
  void __fastcall Button5Click(TObject * Sender);
  void __fastcall Timer1Timer(TObject * Sender);
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


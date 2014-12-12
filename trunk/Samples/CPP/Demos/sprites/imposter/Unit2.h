//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSkydome.hpp"
#include "GLTeapot.hpp"
#include "GLWin32Viewer.hpp"
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *LabelTexSize;
	TLabel *Label2;
	TLabel *LabelFPS;
	TCheckBox *CBShowTeapot;
	TCheckBox *CBShowImposter;
	TComboBox *CBSampleSize;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLSkyDome *GLSkyDome1;
	TGLDirectOpenGL *GLDirectOpenGL1;
	TGLTeapot *GLTeapot1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall CBShowTeapotClick(TObject *Sender);
	void __fastcall CBShowImposterClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLDirectOpenGL1Render(TObject *Sender, TRenderContextInfo &rci);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall FormResize(TObject *Sender);


private:	// User declarations
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
  TGLRenderPoint *renderPoint;
  TGLStaticImposterBuilder(void) *impBuilder;
  int mx, my;

};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif

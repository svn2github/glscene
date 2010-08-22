//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLCadencer.hpp"
#include "GLMisc.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLTexture.hpp"
#include "GLWin32Viewer.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// Composants gérés par l'EDI
        TLabel *LFogStart;
        TLabel *LFogEnd;
        TLabel *LFogColor;
        TShape *SFogColor;
        TLabel *LFogDensity;
        TGLSceneViewer *GLSceneViewer1;
        TCheckBox *CBFogEnable;
        TRadioGroup *RGFogDistance;
        TRadioGroup *RGFogMode;
        TCheckBox *CBApplyToBackground;
        TGroupBox *GBTexture;
        TCheckBox *CBTextureEnabled;
        TCheckBox *CBTextureIgnoreFog;
        TEdit *EFogStart;
        TEdit *EFogEnd;
        TEdit *EFogDensity;
        TGLScene *GLScene1;
        TGLDummyCube *GLDummyCube1;
        TGLLightSource *GLLightSource1;
        TGLCamera *GLCamera1;
        TGLCadencer *GLCadencer1;
        TColorDialog *ColorDialog1;
        TGLMaterialLibrary *GLMaterialLibrary1;
        void __fastcall GLSceneViewer1MouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall GLSceneViewer1MouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
        void __fastcall CBFogEnableClick(TObject *Sender);
        void __fastcall EFogStartChange(TObject *Sender);
        void __fastcall SFogColorMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall RGFogModeClick(TObject *Sender);
        void __fastcall CBApplyToBackgroundClick(TObject *Sender);
        void __fastcall CBTextureEnabledClick(TObject *Sender);
        void __fastcall CBTextureIgnoreFogClick(TObject *Sender);
private:	// Déclarations de l'utilisateur
        void ApplyFogSettings(void);
        int mx, my;
public:		// Déclarations de l'utilisateur
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

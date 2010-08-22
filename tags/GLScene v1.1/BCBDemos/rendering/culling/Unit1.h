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
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// Composants gérés par l'EDI
        TLabel *Label1;
        TLabel *Label2;
        TGLSceneViewer *Viewer;
        TRadioButton *RBNone;
        TRadioButton *RBObject;
        TRadioButton *RBHierarchical;
        TPanel *Panel1;
        TRadioButton *RBSpheres;
        TRadioButton *RBActors;
        TGLScene *GLScene;
        TGLLightSource *GLLightSource1;
        TGLDummyCube *DCTarget;
        TGLCamera *GLCamera1;
        TGLDummyCube *DCSpheres;
        TGLDummyCube *DCActors;
        TGLActor *ACReference;
        TGLCadencer *GLCadencer;
        TTimer *Timer1;
        TGLMaterialLibrary *GLMaterialLibrary;
        void __fastcall RBSpheresClick(TObject *Sender);
        void __fastcall GLCadencerProgress(TObject *Sender,
          const double deltaTime, const double newTime);
        void __fastcall Timer1Timer(TObject *Sender);
        void __fastcall RBNoneClick(TObject *Sender);
private:	// Déclarations de l'utilisateur
public:		// Déclarations de l'utilisateur
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

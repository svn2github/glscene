//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLCadencer.hpp"
#include "GLGeomObjects.hpp"
#include "GLMisc.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TGLSceneViewer *GLSceneViewer1;
        TRadioButton *RBSTC;
        TRadioButton *RBTSC;
        TRadioButton *RBTCS;
        TCheckBox *CBSorting;
        TCheckBox *CBAdditive;
        TGLScene *GLScene1;
        TGLLightSource *GLLightSource1;
        TGLDummyCube *BaseDummyCube;
        TGLSphere *OrbitingSphere1;
        TGLSphere *OrbitingSphere2;
        TGLDummyCube *DCCentral;
        TGLTorus *Torus1;
        TGLCone *Cone1;
        TGLSphere *CentralSphere;
        TGLCamera *GLCamera1;
        TGLCadencer *GLCadencer1;
        void __fastcall RBSTCClick(TObject *Sender);
        void __fastcall RBTSCClick(TObject *Sender);
        void __fastcall RBTCSClick(TObject *Sender);
        void __fastcall CBAdditiveClick(TObject *Sender);
        void __fastcall CBSortingClick(TObject *Sender);
        void __fastcall GLCadencer1Progress(TObject *Sender,
          const double deltaTime, const double newTime);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

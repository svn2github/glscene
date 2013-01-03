//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "BaseClasses.hpp"
#include "GLBlur.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLPolyhedron.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLTeapot.hpp"
#include "GLWin32Viewer.hpp"
//---------------------------------------------------------------------------
class TForm7 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *Light;
	TGLCube *GLCube1;
	TGLSphere *GLSphere1;
	TGLTorus *GLTorus1;
	TGLIcosahedron *GLIcosahedron1;
	TGLTeapot *GLTeapot1;
	TGLMotionBlur *GLMotionBlur1;
	TGLCube *GLCube2;
	TGLCube *GLCube3;
	TGLCamera *Cam;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
public:		// User declarations
	__fastcall TForm7(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm7 *Form7;
//---------------------------------------------------------------------------
#endif

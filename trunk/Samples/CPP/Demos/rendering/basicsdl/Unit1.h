//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Forms.hpp>
#include "BaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLScene.hpp"
#include "GLContext.hpp"
#include "GLUtils.hpp"
#include "GLTeapot.hpp"
#include "OpenGL1x.hpp"
#include "GLSDLContext.hpp"
#include "JPEG.hpp"

//---------------------------------------------------------------------------
class TDataModule1 : public TDataModule
{
__published:	// IDE-managed Components
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLTeapot *Teapot1;
	TGLCamera *GLCamera1;
	TGLSDLViewer *GLSDLViewer1;
	void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall GLSDLViewer1EventPollDone(TObject *Sender);
	void __fastcall GLSDLViewer1Resize(TObject *Sender);
private:	// User declarations
	bool firstPassDone;
public:		// User declarations
	__fastcall TDataModule1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDataModule1 *DataModule1;
//---------------------------------------------------------------------------
#endif

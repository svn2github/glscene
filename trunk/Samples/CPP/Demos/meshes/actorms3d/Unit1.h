//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLCustomShader.hpp"
#include "GLFBORenderer.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLSArchiveManager.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLSLShader.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLTexture.hpp"
#include "GLUtils.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TButton *Button2;
	TButton *btnStartStop;
	TButton *Button4;
	TComboBox *aniBox;
	TTrackBar *aniPos;
	TGLScene *GLScene1;
	TGLCamera *GLCamera1;
	TGLCamera *GLCamera2;
	TGLLightSource *Light2;
	TGLSphere *GLSphere2;
	TGLFBORenderer *GLFrameBuffer;
	TGLDummyCube *Root;
	TGLLightSource *GLLightSource1;
	TGLDirectOpenGL *GLDirectOpenGL1;
	TGLActor *Actor1;
	TGLPlane *GLPlane1;
	TGLFreeForm *Chair1;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *MatLib;
	TGLSLShader *GLSLShader1;
	TGLSimpleNavigation *GLNavigation;
	TTimer *Timer1;
	TGLSArchiveManager *GLSArchiveManager1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Actor1EndFrameReached(TObject *Sender);
	void __fastcall GLSLShader1Initialize(TGLCustomGLSLShader *Shader);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall btnStartStopClick(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:	// User declarations
	String FAppPath;
	void __fastcall SetAppPath(const String Value);
	void __fastcall LoadTexture(const String AName, const String ext);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

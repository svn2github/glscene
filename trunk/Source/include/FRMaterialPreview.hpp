// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRMaterialPreview.pas' rev: 24.00 (Win32)

#ifndef FrmaterialpreviewHPP
#define FrmaterialpreviewHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLHUDObjects.hpp>	// Pascal unit
#include <GLViewer.hpp>	// Pascal unit
#include <GLTeapot.hpp>	// Pascal unit
#include <GLGeomObjects.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLWin32Viewer.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Frmaterialpreview
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TRMaterialPreview;
class PASCALIMPLEMENTATION TRMaterialPreview : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	Glscene::TGLScene* GLScene1;
	Glwin32viewer::TGLSceneViewer* SceneViewer;
	Vcl::Stdctrls::TComboBox* CBObject;
	Glscene::TGLCamera* Camera;
	Globjects::TGLCube* Cube;
	Globjects::TGLSphere* Sphere;
	Glscene::TGLLightSource* LightSource;
	Vcl::Stdctrls::TComboBox* CBBackground;
	Glhudobjects::TGLHUDSprite* BackGroundSprite;
	Glgeomobjects::TGLCone* Cone;
	Glteapot::TGLTeapot* Teapot;
	Globjects::TGLDummyCube* World;
	Globjects::TGLDummyCube* Light;
	Globjects::TGLSphere* FireSphere;
	Glmaterial::TGLMaterialLibrary* GLMaterialLibrary;
	void __fastcall CBObjectChange(System::TObject* Sender);
	void __fastcall CBBackgroundChange(System::TObject* Sender);
	void __fastcall SceneViewerMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SceneViewerMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SceneViewerMouseWheel(System::TObject* Sender, System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos, bool &Handled);
	
private:
	Glmaterial::TGLAbstractLibMaterial* FLibMaterial;
	Glmaterial::TGLMaterial* __fastcall GetMaterial(void);
	void __fastcall SetMaterial(Glmaterial::TGLMaterial* const Value);
	Glmaterial::TGLAbstractLibMaterial* __fastcall GetLibMaterial(void);
	void __fastcall SetLibMaterial(Glmaterial::TGLAbstractLibMaterial* const Value);
	
public:
	__fastcall virtual TRMaterialPreview(System::Classes::TComponent* AOwner);
	__property Glmaterial::TGLMaterial* Material = {read=GetMaterial, write=SetMaterial};
	__property Glmaterial::TGLAbstractLibMaterial* LibMaterial = {read=GetLibMaterial, write=SetLibMaterial};
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TRMaterialPreview(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TRMaterialPreview(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frmaterialpreview */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRMATERIALPREVIEW)
using namespace Frmaterialpreview;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrmaterialpreviewHPP

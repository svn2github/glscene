// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLScene.FMX.Viewer.pas' rev: 24.00 (Win32)

#ifndef Glscene_Fmx_ViewerHPP
#define Glscene_Fmx_ViewerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <FMX.Types.hpp>	// Pascal unit
#include <FMX.Types3D.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscene
{
namespace Fmx
{
namespace Viewer
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLSceneViewport;
class PASCALIMPLEMENTATION TGLSceneViewport : public Fmx::Types::TControl
{
	typedef Fmx::Types::TControl inherited;
	
private:
	Glscene::TGLSceneBuffer* FGLSBuffer;
	Fmx::Types::TBitmap* FFMXBuffer;
	Fmx::Types3d::TContext3D* FFMXContext;
	Fmx::Types3d::TMultisample FMultisample;
	HWND FParentHandle;
	HDC FOwnDC;
	bool FDrawing;
	System::Classes::TNotifyEvent FPostRender;
	void __fastcall SetBuffer(Glscene::TGLSceneBuffer* const Value);
	Glscene::TGLCamera* __fastcall GetGLSceneCamera(void);
	void __fastcall SetGLSceneCamera(Glscene::TGLCamera* const Value);
	void __fastcall CopyBuffer(System::TObject* Sender);
	void __fastcall SetBeforeRender(const System::Classes::TNotifyEvent Value);
	System::Classes::TNotifyEvent __fastcall GetBeforeRender(void);
	void __fastcall SetAfterRender(const System::Classes::TNotifyEvent Value);
	System::Classes::TNotifyEvent __fastcall GetAfterRender(void);
	
protected:
	virtual void __fastcall Paint(void);
	
public:
	__fastcall virtual TGLSceneViewport(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSceneViewport(void);
	HIDESBASE void __fastcall Realign(void);
	
__published:
	__property System::Classes::TNotifyEvent BeforeRender = {read=GetBeforeRender, write=SetBeforeRender};
	__property System::Classes::TNotifyEvent PostRender = {read=FPostRender, write=FPostRender};
	__property System::Classes::TNotifyEvent AfterRender = {read=GetAfterRender, write=SetAfterRender};
	__property Glscene::TGLSceneBuffer* Buffer = {read=FGLSBuffer, write=SetBuffer};
	__property Glscene::TGLCamera* GLSceneCamera = {read=GetGLSceneCamera, write=SetGLSceneCamera};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Viewer */
}	/* namespace Fmx */
}	/* namespace Glscene */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENE_FMX_VIEWER)
using namespace Glscene::Fmx::Viewer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENE_FMX)
using namespace Glscene::Fmx;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENE)
using namespace Glscene;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glscene_Fmx_ViewerHPP

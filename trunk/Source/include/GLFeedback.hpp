// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFeedback.pas' rev: 24.00 (Win32)

#ifndef GlfeedbackHPP
#define GlfeedbackHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfeedback
{
//-- type declarations -------------------------------------------------------
enum TFeedbackMode : unsigned char { fm2D, fm3D, fm3DColor, fm3DColorTexture, fm4DColorTexture };

class DELPHICLASS TGLFeedback;
class PASCALIMPLEMENTATION TGLFeedback : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	bool FActive;
	Vectorlists::TSingleList* FBuffer;
	unsigned FMaxBufferSize;
	bool FBuffered;
	float FCorrectionScaling;
	TFeedbackMode FMode;
	
protected:
	void __fastcall SetMaxBufferSize(const unsigned Value);
	void __fastcall SetMode(const TFeedbackMode Value);
	
public:
	__fastcall virtual TGLFeedback(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFeedback(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	void __fastcall BuildMeshFromBuffer(Vectorlists::TAffineVectorList* Vertices = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* Normals = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TVectorList* Colors = (Vectorlists::TVectorList*)(0x0), Vectorlists::TAffineVectorList* TexCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TIntegerList* VertexIndices = (Vectorlists::TIntegerList*)(0x0));
	__property bool Buffered = {read=FBuffered, nodefault};
	__property Vectorlists::TSingleList* Buffer = {read=FBuffer};
	__property float CorrectionScaling = {read=FCorrectionScaling};
	
__published:
	__property unsigned MaxBufferSize = {read=FMaxBufferSize, write=SetMaxBufferSize, nodefault};
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TFeedbackMode Mode = {read=FMode, write=SetMode, nodefault};
	__property Visible = {default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFeedback(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfeedback */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFEEDBACK)
using namespace Glfeedback;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfeedbackHPP

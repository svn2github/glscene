// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPipelineTransformation.pas' rev: 24.00 (Win32)

#ifndef GlpipelinetransformationHPP
#define GlpipelinetransformationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glpipelinetransformation
{
//-- type declarations -------------------------------------------------------
enum TGLPipelineTransformationState : unsigned char { trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged, trsViewProjChanged, trsFrustum };

typedef System::Set<TGLPipelineTransformationState, TGLPipelineTransformationState::trsModelViewChanged, TGLPipelineTransformationState::trsFrustum>  TGLPipelineTransformationStates;

struct TTransformationRec;
typedef TTransformationRec *PTransformationRec;

struct DECLSPEC_DRECORD TTransformationRec
{
public:
	TGLPipelineTransformationStates FStates;
	Vectortypes::TMatrix4f FModelMatrix;
	Vectortypes::TMatrix4f FViewMatrix;
	Vectortypes::TMatrix4f FProjectionMatrix;
	Vectortypes::TMatrix4f FInvModelMatrix;
	Vectortypes::TMatrix3f FNormalModelMatrix;
	Vectortypes::TMatrix4f FModelViewMatrix;
	Vectortypes::TMatrix4f FInvModelViewMatrix;
	Vectortypes::TMatrix4f FViewProjectionMatrix;
	Vectorgeometry::TFrustum FFrustum;
};


typedef void __fastcall (__closure *TOnMatricesPush)(void);

class DELPHICLASS TGLTransformation;
class PASCALIMPLEMENTATION TGLTransformation : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<TTransformationRec> _TGLTransformation__1;
	
	
private:
	int FStackPos;
	_TGLTransformation__1 FStack;
	bool FLoadMatricesEnabled;
	TOnMatricesPush FOnPush;
	Vectortypes::TMatrix4f __fastcall GetModelMatrix(void);
	Vectortypes::TMatrix4f __fastcall GetViewMatrix(void);
	Vectortypes::TMatrix4f __fastcall GetProjectionMatrix(void);
	Vectortypes::TMatrix4f __fastcall GetModelViewMatrix(void);
	Vectortypes::TMatrix4f __fastcall GetInvModelViewMatrix(void);
	Vectortypes::TMatrix4f __fastcall GetInvModelMatrix(void);
	Vectortypes::TMatrix3f __fastcall GetNormalModelMatrix(void);
	Vectortypes::TMatrix4f __fastcall GetViewProjectionMatrix(void);
	Vectorgeometry::TFrustum __fastcall GetFrustum(void);
	void __fastcall SetModelMatrix(const Vectortypes::TMatrix4f &AMatrix);
	void __fastcall SetViewMatrix(const Vectortypes::TMatrix4f &AMatrix);
	void __fastcall SetProjectionMatrix(const Vectortypes::TMatrix4f &AMatrix);
	
protected:
	void __fastcall LoadModelViewMatrix(void);
	void __fastcall LoadProjectionMatrix(void);
	void __fastcall DoMatrcesLoaded(void);
	__property TOnMatricesPush OnPush = {read=FOnPush, write=FOnPush};
	
public:
	__fastcall TGLTransformation(void);
	void __fastcall IdentityAll(void);
	void __fastcall Push(PTransformationRec AValue = (PTransformationRec)(0x0));
	void __fastcall Pop(void);
	void __fastcall ReplaceFromStack(void);
	TTransformationRec __fastcall StackTop(void);
	__property Vectortypes::TMatrix4f ModelMatrix = {read=GetModelMatrix, write=SetModelMatrix};
	__property Vectortypes::TMatrix4f ViewMatrix = {read=GetViewMatrix, write=SetViewMatrix};
	__property Vectortypes::TMatrix4f ProjectionMatrix = {read=GetProjectionMatrix, write=SetProjectionMatrix};
	__property Vectortypes::TMatrix4f InvModelMatrix = {read=GetInvModelMatrix};
	__property Vectortypes::TMatrix4f ModelViewMatrix = {read=GetModelViewMatrix};
	__property Vectortypes::TMatrix3f NormalModelMatrix = {read=GetNormalModelMatrix};
	__property Vectortypes::TMatrix4f InvModelViewMatrix = {read=GetInvModelViewMatrix};
	__property Vectortypes::TMatrix4f ViewProjectionMatrix = {read=GetViewProjectionMatrix};
	__property Vectorgeometry::TFrustum Frustum = {read=GetFrustum};
	__property bool LoadMatricesEnabled = {read=FLoadMatricesEnabled, write=FLoadMatricesEnabled, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLTransformation(void) { }
	
};


typedef Opengladapter::TGLExtensionsAndEntryPoints* __fastcall (*TGLCall)(void);

//-- var, const, procedure ---------------------------------------------------
static const System::Byte MAX_MATRIX_STACK_DEPTH = System::Byte(0x80);
#define cAllStatesChanged (System::Set<TGLPipelineTransformationState, TGLPipelineTransformationState::trsModelViewChanged, TGLPipelineTransformationState::trsFrustum> () << TGLPipelineTransformationState::trsModelViewChanged << TGLPipelineTransformationState::trsInvModelViewChanged << TGLPipelineTransformationState::trsInvModelChanged << TGLPipelineTransformationState::trsNormalModelChanged << TGLPipelineTransformationState::trsViewProjChanged << TGLPipelineTransformationState::trsFrustum )
extern PACKAGE TGLCall vLocalGL;
}	/* namespace Glpipelinetransformation */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPIPELINETRANSFORMATION)
using namespace Glpipelinetransformation;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlpipelinetransformationHPP

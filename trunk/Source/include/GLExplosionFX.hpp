// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLExplosionFx.pas' rev: 24.00 (Win32)

#ifndef GlexplosionfxHPP
#define GlexplosionfxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glexplosionfx
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLBExplosionFX;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBExplosionFX : public Glscene::TGLObjectPreEffect
{
	typedef Glscene::TGLObjectPreEffect inherited;
	
private:
	Vectorlists::TAffineVectorList* FTriList;
	Vectorlists::TAffineVectorList* FRotList;
	Vectorlists::TAffineVectorList* FDirList;
	Vectorlists::TAffineVectorList* FPosList;
	bool FEnabled;
	int FFaceCount;
	float FSpeed;
	Glcoordinates::TGLCoordinates3* FDirection;
	int FMaxSteps;
	int FStep;
	void __fastcall SetTriList(Vectorlists::TAffineVectorList* Value);
	void __fastcall SetRotList(Vectorlists::TAffineVectorList* Value);
	void __fastcall SetDirList(Vectorlists::TAffineVectorList* Value);
	void __fastcall SetPosList(Vectorlists::TAffineVectorList* Value);
	void __fastcall SetDirection(Glcoordinates::TGLCoordinates3* value);
	void __fastcall SetEnabled(bool value);
	
protected:
	__property Vectorlists::TAffineVectorList* TriList = {read=FTriList, write=SetTriList};
	__property Vectorlists::TAffineVectorList* RotList = {read=FRotList, write=SetRotList};
	__property Vectorlists::TAffineVectorList* DirList = {read=FDirList, write=SetDirList};
	__property Vectorlists::TAffineVectorList* PosList = {read=FPosList, write=SetPosList};
	__property int FaceCount = {read=FFaceCount, write=FFaceCount, nodefault};
	void __fastcall CacheInfo(void);
	
public:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property int Step = {read=FStep, nodefault};
	__fastcall virtual TGLBExplosionFX(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBExplosionFX(void);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall Reset(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property int MaxSteps = {read=FMaxSteps, write=FMaxSteps, nodefault};
	__property float Speed = {read=FSpeed, write=FSpeed};
	__property Glcoordinates::TGLCoordinates3* Direction = {read=FDirection, write=SetDirection};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glexplosionfx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLEXPLOSIONFX)
using namespace Glexplosionfx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlexplosionfxHPP

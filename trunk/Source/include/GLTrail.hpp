// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTrail.pas' rev: 24.00 (Win32)

#ifndef GltrailHPP
#define GltrailHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <MeshUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLMesh.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltrail
{
//-- type declarations -------------------------------------------------------
enum TMarkStyle : unsigned char { msUp, msDirection, msFaceCamera, msRight };

class DELPHICLASS TGLTrail;
class PASCALIMPLEMENTATION TGLTrail : public Glmesh::TGLMesh
{
	typedef Glmesh::TGLMesh inherited;
	
private:
	int fVertLimit;
	float fTimeLimit;
	float fMinDistance;
	float fAlpha;
	bool fAlphaFade;
	float fUVScale;
	System::StaticArray<Vectortypes::TVector3f, 2000> fVerts;
	System::StaticArray<Vectorgeometry::TTexPoint, 2000> fUVs;
	System::StaticArray<double, 2000> fTimeStamps;
	int fVertStart;
	int fVertEnd;
	int fVertCount;
	Vectortypes::TVector3f fLastV0Pos;
	Vectortypes::TVector3f fLastPos;
	Vectortypes::TVector3f fLastDir;
	Vectortypes::TVector3f fLastUp;
	float FLastUVs;
	Vectortypes::TVector3f fLastP1;
	Vectortypes::TVector3f fLastP2;
	Glscene::TGLBaseSceneObject* FTrailObject;
	TMarkStyle FMarkStyle;
	float FMarkWidth;
	bool FEnabled;
	float FAntiZFightOffset;
	void __fastcall SetTrailObject(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetMarkStyle(const TMarkStyle Value);
	void __fastcall SetAlpha(const float Value);
	void __fastcall SetAlphaFade(const bool Value);
	void __fastcall SetMinDistance(const float Value);
	void __fastcall SetTimeLimit(const float Value);
	void __fastcall SetUVScale(const float Value);
	void __fastcall SetVertLimit(const int Value);
	void __fastcall SetMarkWidth(const float Value);
	void __fastcall SetEnabled(const bool Value);
	bool __fastcall StoreAntiZFightOffset(void);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	__fastcall virtual TGLTrail(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTrail(void);
	void __fastcall CreateMark(Glscene::TGLBaseSceneObject* obj, float width, double CurrentTime)/* overload */;
	void __fastcall CreateMark(const Vectortypes::TVector3f &APos, const Vectortypes::TVector3f &ADir, const Vectortypes::TVector3f &AUp, float AWidth, double ACurrentTime)/* overload */;
	bool __fastcall CreateMark(const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, double CurrentTime)/* overload */;
	void __fastcall ClearMarks(void);
	
__published:
	__property float AntiZFightOffset = {read=FAntiZFightOffset, write=FAntiZFightOffset, stored=StoreAntiZFightOffset};
	__property int VertLimit = {read=fVertLimit, write=SetVertLimit, default=150};
	__property float TimeLimit = {read=fTimeLimit, write=SetTimeLimit};
	__property float MinDistance = {read=fMinDistance, write=SetMinDistance};
	__property float Alpha = {read=fAlpha, write=SetAlpha};
	__property bool AlphaFade = {read=fAlphaFade, write=SetAlphaFade, default=1};
	__property float UVScale = {read=fUVScale, write=SetUVScale};
	__property TMarkStyle MarkStyle = {read=FMarkStyle, write=SetMarkStyle, default=2};
	__property Glscene::TGLBaseSceneObject* TrailObject = {read=FTrailObject, write=SetTrailObject, default=0};
	__property float MarkWidth = {read=FMarkWidth, write=SetMarkWidth};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTrail(Glscene::TGLBaseSceneObject* aParentOwner) : Glmesh::TGLMesh(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word cMaxVerts = System::Word(0x7d0);
}	/* namespace Gltrail */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTRAIL)
using namespace Gltrail;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltrailHPP

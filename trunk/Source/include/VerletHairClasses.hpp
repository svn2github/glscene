// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VerletHairClasses.pas' rev: 24.00 (Win32)

#ifndef VerlethairclassesHPP
#define VerlethairclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VerletClasses.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Verlethairclasses
{
//-- type declarations -------------------------------------------------------
enum TVHStiffness : unsigned char { vhsFull, vhsSkip1Node, vhsSkip2Node, vhsSkip3Node, vhsSkip4Node, vhsSkip5Node, vhsSkip6Node, vhsSkip7Node, vhsSkip8Node, vhsSkip9Node };

typedef System::Set<TVHStiffness, TVHStiffness::vhsFull, TVHStiffness::vhsSkip9Node>  TVHStiffnessSet;

class DELPHICLASS TVerletHair;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletHair : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Verletclasses::TVerletNodeList* FNodeList;
	int FLinkCount;
	float FRootDepth;
	Verletclasses::TVerletWorld* FVerletWorld;
	float FHairLength;
	void *FData;
	TVHStiffnessSet FStiffness;
	System::Classes::TList* FStiffnessList;
	Verletclasses::TVerletNode* __fastcall GetAnchor(void);
	Verletclasses::TVerletNode* __fastcall GetRoot(void);
	float __fastcall GetLinkLength(void);
	void __fastcall AddStickStiffness(const int ANodeSkip);
	void __fastcall SetStiffness(const TVHStiffnessSet Value);
	
public:
	void __fastcall BuildHair(const Vectortypes::TVector3f &AAnchorPosition, const Vectortypes::TVector3f &AHairDirection);
	void __fastcall BuildStiffness(void);
	void __fastcall ClearStiffness(void);
	void __fastcall Clear(void);
	__fastcall TVerletHair(Verletclasses::TVerletWorld* const AVerletWorld, const float ARootDepth, const float AHairLength, int ALinkCount, const Vectortypes::TVector3f &AAnchorPosition, const Vectortypes::TVector3f &AHairDirection, const TVHStiffnessSet AStiffness);
	__fastcall virtual ~TVerletHair(void);
	__property Verletclasses::TVerletNodeList* NodeList = {read=FNodeList};
	__property Verletclasses::TVerletWorld* VerletWorld = {read=FVerletWorld};
	__property float RootDepth = {read=FRootDepth};
	__property float LinkLength = {read=GetLinkLength};
	__property int LinkCount = {read=FLinkCount, nodefault};
	__property float HairLength = {read=FHairLength};
	__property TVHStiffnessSet Stiffness = {read=FStiffness, write=SetStiffness, nodefault};
	__property void * Data = {read=FData, write=FData};
	__property Verletclasses::TVerletNode* Anchor = {read=GetAnchor};
	__property Verletclasses::TVerletNode* Root = {read=GetRoot};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Verlethairclasses */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VERLETHAIRCLASSES)
using namespace Verlethairclasses;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VerlethairclassesHPP

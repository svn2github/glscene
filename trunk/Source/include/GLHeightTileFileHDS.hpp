// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLHeightTileFileHDS.pas' rev: 24.00 (Win32)

#ifndef GlheighttilefilehdsHPP
#define GlheighttilefilehdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLHeightData.hpp>	// Pascal unit
#include <HeightTileFile.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glheighttilefilehds
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLHeightTileFileHDS;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLHeightTileFileHDS : public Glheightdata::THeightDataSource
{
	typedef Glheightdata::THeightDataSource inherited;
	
private:
	bool FInfiniteWrap;
	bool FInverted;
	System::UnicodeString FHTFFileName;
	Heighttilefile::THeightTileFile* FHTF;
	int FMinElevation;
	
protected:
	void __fastcall SetHTFFileName(const System::UnicodeString val);
	void __fastcall SetInfiniteWrap(bool val);
	void __fastcall SetInverted(bool val);
	void __fastcall SetMinElevation(int val);
	
public:
	__fastcall virtual TGLHeightTileFileHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHeightTileFileHDS(void);
	virtual void __fastcall StartPreparingData(Glheightdata::THeightData* HeightData);
	virtual int __fastcall Width(void);
	virtual int __fastcall Height(void);
	Heighttilefile::THeightTileFile* __fastcall OpenHTF(void);
	
__published:
	__property System::UnicodeString HTFFileName = {read=FHTFFileName, write=SetHTFFileName};
	__property bool InfiniteWrap = {read=FInfiniteWrap, write=SetInfiniteWrap, default=1};
	__property bool Inverted = {read=FInverted, write=SetInverted, default=1};
	__property int MinElevation = {read=FMinElevation, write=SetMinElevation, default=-32768};
	__property MaxPoolSize;
	__property DefaultHeight = {default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glheighttilefilehds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLHEIGHTTILEFILEHDS)
using namespace Glheighttilefilehds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlheighttilefilehdsHPP

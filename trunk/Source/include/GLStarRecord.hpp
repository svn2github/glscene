// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLStarRecord.pas' rev: 24.00 (Win32)

#ifndef GlstarrecordHPP
#define GlstarrecordHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glstarrecord
{
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLStarRecord
{
public:
	System::Word RA;
	short DEC;
	System::Byte BVColorIndex;
	System::Byte VMagnitude;
};
#pragma pack(pop)


typedef TGLStarRecord *PGLStarRecord;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Vectortypes::TVector3f __fastcall StarRecordPositionYUp(const TGLStarRecord &starRecord);
extern PACKAGE Vectortypes::TVector3f __fastcall StarRecordPositionZUp(const TGLStarRecord &starRecord);
extern PACKAGE Vectortypes::TVector4f __fastcall StarRecordColor(const TGLStarRecord &starRecord, float bias);
}	/* namespace Glstarrecord */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSTARRECORD)
using namespace Glstarrecord;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlstarrecordHPP

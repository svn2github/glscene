// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileMD5.pas' rev: 24.00 (Win32)

#ifndef Glfilemd5HPP
#define Glfilemd5HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLUtils.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfilemd5
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLMD5VectorFile;
class PASCALIMPLEMENTATION TGLMD5VectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
private:
	System::Classes::TStringList* FMD5String;
	System::Classes::TStringList* FTempString;
	System::Classes::TStringList* FBoneNames;
	int FCurrentPos;
	Glvectorfileobjects::TSkeletonFrame* FBasePose;
	Vectorlists::TAffineVectorList* FFramePositions;
	Vectorlists::TQuaternionList* FFrameQuaternions;
	Vectorlists::TIntegerList* FJointFlags;
	int FNumFrames;
	int FFirstFrame;
	int FFrameRate;
	int FNumJoints;
	System::UnicodeString __fastcall ReadLine(void);
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLMD5VectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLMD5VectorFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::Classes::TStringList* vMD5TextureExtensions;
}	/* namespace Glfilemd5 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEMD5)
using namespace Glfilemd5;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfilemd5HPP

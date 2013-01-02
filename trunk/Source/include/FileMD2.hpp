// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileMD2.pas' rev: 24.00 (Win32)

#ifndef Filemd2HPP
#define Filemd2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <TypesMD2.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Filemd2
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TFileMD2;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFileMD2 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FiFrames;
	int FiVertices;
	int FiTriangles;
	void __fastcall FreeLists(void);
	
public:
	Typesmd2::TIndexList fIndexList;
	Typesmd2::TVertexList fVertexList;
	System::Classes::TStrings* FrameNames;
	__fastcall virtual TFileMD2(void);
	__fastcall virtual ~TFileMD2(void);
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	__property int iFrames = {read=FiFrames, nodefault};
	__property int iVertices = {read=FiVertices, nodefault};
	__property int iTriangles = {read=FiTriangles, nodefault};
	__property Typesmd2::TIndexList IndexList = {read=fIndexList};
	__property Typesmd2::TVertexList VertexList = {read=fVertexList};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filemd2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FILEMD2)
using namespace Filemd2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Filemd2HPP

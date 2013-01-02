// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Q3MD3.pas' rev: 24.00 (Win32)

#ifndef Q3md3HPP
#define Q3md3HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <FileMD3.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Q3md3
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TMD3TagList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMD3TagList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<Filemd3::TMD3Tag> _TMD3TagList__1;
	
	
private:
	_TMD3TagList__1 FTags;
	int FNumTags;
	int FNumFrames;
	Filemd3::TMD3Tag __fastcall GetTag(int index);
	
public:
	void __fastcall LoadFromFile(System::UnicodeString FileName);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	Vectortypes::TMatrix4f __fastcall GetTransform(System::UnicodeString TagName, int Frame);
	__property int TagCount = {read=FNumTags, nodefault};
	__property int FrameCount = {read=FNumFrames, nodefault};
	__property Filemd3::TMD3Tag Tags[int index] = {read=GetTag};
public:
	/* TObject.Create */ inline __fastcall TMD3TagList(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMD3TagList(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall LoadQ3Anims(Glvectorfileobjects::TActorAnimations* Animations, System::UnicodeString FileName, System::UnicodeString NamePrefix)/* overload */;
extern PACKAGE void __fastcall LoadQ3Anims(Glvectorfileobjects::TActorAnimations* Animations, System::Classes::TStrings* Strings, System::UnicodeString NamePrefix)/* overload */;
extern PACKAGE void __fastcall LoadQ3Skin(System::UnicodeString FileName, Glvectorfileobjects::TGLActor* Actor);
}	/* namespace Q3md3 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_Q3MD3)
using namespace Q3md3;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Q3md3HPP

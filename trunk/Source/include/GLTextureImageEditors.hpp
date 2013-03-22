// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTextureImageEditors.pas' rev: 24.00 (Win32)

#ifndef GltextureimageeditorsHPP
#define GltextureimageeditorsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLProcTextures.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltextureimageeditors
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLTextureImageEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureImageEditor : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gltexture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLTextureImageEditor(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLTextureImageEditor(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TGLTextureImageEditorClass;

class DELPHICLASS TGLBlankTIE;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBlankTIE : public TGLTextureImageEditor
{
	typedef TGLTextureImageEditor inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gltexture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLBlankTIE(void) : TGLTextureImageEditor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBlankTIE(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLPersistentTIE;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPersistentTIE : public TGLTextureImageEditor
{
	typedef TGLTextureImageEditor inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gltexture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLPersistentTIE(void) : TGLTextureImageEditor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLPersistentTIE(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLPicFileTIE;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPicFileTIE : public TGLTextureImageEditor
{
	typedef TGLTextureImageEditor inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gltexture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLPicFileTIE(void) : TGLTextureImageEditor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLPicFileTIE(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLProcTextureNoiseTIE;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLProcTextureNoiseTIE : public TGLTextureImageEditor
{
	typedef TGLTextureImageEditor inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gltexture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLProcTextureNoiseTIE(void) : TGLTextureImageEditor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLProcTextureNoiseTIE(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool __fastcall EditGLTextureImage(Gltexture::TGLTextureImage* aTexImage);
extern PACKAGE void __fastcall RegisterGLTextureImageEditor(Gltexture::TGLTextureImageClass aTexImageClass, TGLTextureImageEditorClass texImageEditor);
extern PACKAGE void __fastcall UnRegisterGLTextureImageEditor(TGLTextureImageEditorClass texImageEditor);
}	/* namespace Gltextureimageeditors */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXTUREIMAGEEDITORS)
using namespace Gltextureimageeditors;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltextureimageeditorsHPP

// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLScriptBase.pas' rev: 24.00 (Win32)

#ifndef GlscriptbaseHPP
#define GlscriptbaseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscriptbase
{
//-- type declarations -------------------------------------------------------
enum TGLScriptState : unsigned char { ssUncompiled, ssCompileErrors, ssCompiled, ssRunningErrors, ssRunning };

class DELPHICLASS TGLScriptBase;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLScriptBase : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
private:
	System::Classes::TStringList* FText;
	System::UnicodeString FDescription;
	System::Classes::TStringList* FErrors;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	virtual TGLScriptState __fastcall GetState(void) = 0 ;
	void __fastcall SetText(System::Classes::TStringList* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLScriptBase(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLScriptBase(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Compile(void) = 0 ;
	virtual void __fastcall Start(void) = 0 ;
	virtual void __fastcall Stop(void) = 0 ;
	virtual void __fastcall Execute(void) = 0 ;
	virtual void __fastcall Invalidate(void) = 0 ;
	virtual System::Variant __fastcall Call(System::UnicodeString aName, System::Variant *aParams, const int aParams_Size) = 0 ;
	__property System::Classes::TStringList* Errors = {read=FErrors};
	__property TGLScriptState State = {read=GetState, nodefault};
	
__published:
	__property System::Classes::TStringList* Text = {read=FText, write=SetText};
	__property System::UnicodeString Description = {read=FDescription, write=FDescription};
};

#pragma pack(pop)

class DELPHICLASS TGLScripts;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLScripts : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	TGLScriptBase* operator[](int index) { return Items[index]; }
	
protected:
	HIDESBASE TGLScriptBase* __fastcall GetItems(int index);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	virtual bool __fastcall CanAdd(Xcollection::TXCollectionItemClass aClass);
	__property TGLScriptBase* Items[int index] = {read=GetItems/*, default*/};
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLScripts(System::Classes::TPersistent* aOwner) : Xcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLScripts(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLScriptLibrary;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLScriptLibrary : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLScripts* FScripts;
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteScriptsData(System::Classes::TStream* Stream);
	void __fastcall ReadScriptsData(System::Classes::TStream* Stream);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLScriptLibrary(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLScriptLibrary(void);
	
__published:
	__property TGLScripts* Scripts = {read=FScripts};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glscriptbase */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCRIPTBASE)
using namespace Glscriptbase;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscriptbaseHPP

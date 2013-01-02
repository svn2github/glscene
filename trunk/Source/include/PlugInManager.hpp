// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PlugInManager.pas' rev: 24.00 (Win32)

#ifndef PluginmanagerHPP
#define PluginmanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <PlugInIntf.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pluginmanager
{
//-- type declarations -------------------------------------------------------
struct TPlugInEntry;
typedef TPlugInEntry *PPlugInEntry;

struct DECLSPEC_DRECORD TPlugInEntry
{
public:
	System::Sysutils::TFileName Path;
	NativeUInt Handle;
	int FileSize;
	System::TDateTime FileDate;
	Pluginintf::TEnumResourceNames EnumResourcenames;
	Pluginintf::TGetServices GetServices;
	Pluginintf::TGetVendor GetVendor;
	Pluginintf::TGetDescription GetDescription;
	Pluginintf::TGetVersion GetVersion;
};


class DELPHICLASS TResourceManager;
class DELPHICLASS TPlugInManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TResourceManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	virtual void __fastcall Notify(TPlugInManager* Sender, System::Classes::TOperation Operation, Pluginintf::TPIServiceType Service, int PlugIn) = 0 ;
public:
	/* TComponent.Create */ inline __fastcall virtual TResourceManager(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TResourceManager(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TPlugInList;
class PASCALIMPLEMENTATION TPlugInList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
public:
	PPlugInEntry operator[](int Index) { return Objects[Index]; }
	
private:
	TPlugInManager* FOwner;
	PPlugInEntry __fastcall GetPlugInEntry(int Index);
	void __fastcall SetPlugInEntry(int Index, PPlugInEntry AEntry);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadPlugIns(System::Classes::TReader* Reader);
	void __fastcall WritePlugIns(System::Classes::TWriter* Writer);
	
public:
	__fastcall virtual TPlugInList(TPlugInManager* AOwner);
	void __fastcall ClearList(void);
	__property PPlugInEntry Objects[int Index] = {read=GetPlugInEntry, write=SetPlugInEntry/*, default*/};
	__property TPlugInManager* Owner = {read=FOwner};
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TPlugInList(void) { }
	
};


struct TResManagerEntry;
typedef TResManagerEntry *PResManagerEntry;

struct DECLSPEC_DRECORD TResManagerEntry
{
public:
	TResourceManager* Manager;
	Pluginintf::TPIServices Services;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TPlugInManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TPlugInList* FLibraryList;
	System::Classes::TList* FResManagerList;
	
protected:
	void __fastcall DoNotify(System::Classes::TOperation Operation, Pluginintf::TPIServiceType Service, int PlugIn);
	PResManagerEntry __fastcall FindResManager(TResourceManager* AManager);
	int __fastcall GetIndexFromFilename(System::UnicodeString FileName);
	PPlugInEntry __fastcall GetPlugInFromFilename(System::UnicodeString FileName);
	
public:
	__fastcall virtual TPlugInManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TPlugInManager(void);
	int __fastcall AddPlugIn(System::Sysutils::TFileName Path);
	void __fastcall EditPlugInList(void);
	void __fastcall RegisterResourceManager(TResourceManager* AManager, Pluginintf::TPIServices Services);
	void __fastcall RemovePlugIn(int Index);
	void __fastcall UnRegisterRessourceManager(TResourceManager* AManager, Pluginintf::TPIServices Services);
	
__published:
	__property TPlugInList* PlugIns = {read=FLibraryList, write=FLibraryList};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Pluginmanager */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PLUGINMANAGER)
using namespace Pluginmanager;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PluginmanagerHPP

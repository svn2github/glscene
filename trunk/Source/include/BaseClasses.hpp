// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'BaseClasses.pas' rev: 24.00 (Win32)

#ifndef BaseclassesHPP
#define BaseclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Baseclasses
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TProgressTimes
{
public:
	double deltaTime;
	double newTime;
};


typedef void __fastcall (__closure *TGLProgressEvent)(System::TObject* Sender, const double deltaTime, const double newTime);

__interface IGLNotifyAble;
typedef System::DelphiInterface<IGLNotifyAble> _di_IGLNotifyAble;
__interface  INTERFACE_UUID("{00079A6C-D46E-4126-86EE-F9E2951B4593}") IGLNotifyAble  : public System::IInterface 
{
	
public:
	virtual void __fastcall NotifyChange(System::TObject* Sender) = 0 ;
};

__interface IGLProgessAble;
typedef System::DelphiInterface<IGLProgessAble> _di_IGLProgessAble;
__interface  INTERFACE_UUID("{95E44548-B0FE-4607-98D0-CA51169AF8B5}") IGLProgessAble  : public System::IInterface 
{
	
public:
	virtual void __fastcall DoProgress(const TProgressTimes &progressTime) = 0 ;
};

class DELPHICLASS TGLUpdateAbleObject;
class PASCALIMPLEMENTATION TGLUpdateAbleObject : public Persistentclasses::TGLInterfacedPersistent
{
	typedef Persistentclasses::TGLInterfacedPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	int FUpdating;
	System::Classes::TNotifyEvent FOnNotifyChange;
	
public:
	__fastcall virtual TGLUpdateAbleObject(System::Classes::TPersistent* AOwner);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	__property int Updating = {read=FUpdating, nodefault};
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	__property System::Classes::TPersistent* Owner = {read=FOwner};
	__property System::Classes::TNotifyEvent OnNotifyChange = {read=FOnNotifyChange, write=FOnNotifyChange};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLUpdateAbleObject(void) { }
	
private:
	void *__IGLNotifyAble;	/* IGLNotifyAble */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00079A6C-D46E-4126-86EE-F9E2951B4593}
	operator _di_IGLNotifyAble()
	{
		_di_IGLNotifyAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IGLNotifyAble*(void) { return (IGLNotifyAble*)&__IGLNotifyAble; }
	#endif
	
};


class DELPHICLASS TGLCadenceAbleComponent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCadenceAbleComponent : public Glcrossplatform::TGLComponent
{
	typedef Glcrossplatform::TGLComponent inherited;
	
public:
	virtual void __fastcall DoProgress(const TProgressTimes &progressTime);
public:
	/* TComponent.Create */ inline __fastcall virtual TGLCadenceAbleComponent(System::Classes::TComponent* AOwner) : Glcrossplatform::TGLComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLCadenceAbleComponent(void) { }
	
private:
	void *__IGLProgessAble;	/* IGLProgessAble */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {95E44548-B0FE-4607-98D0-CA51169AF8B5}
	operator _di_IGLProgessAble()
	{
		_di_IGLProgessAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IGLProgessAble*(void) { return (IGLProgessAble*)&__IGLProgessAble; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLUpdateAbleComponent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLUpdateAbleComponent : public TGLCadenceAbleComponent
{
	typedef TGLCadenceAbleComponent inherited;
	
public:
	virtual void __fastcall NotifyChange(System::TObject* Sender);
public:
	/* TComponent.Create */ inline __fastcall virtual TGLUpdateAbleComponent(System::Classes::TComponent* AOwner) : TGLCadenceAbleComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLUpdateAbleComponent(void) { }
	
private:
	void *__IGLNotifyAble;	/* IGLNotifyAble */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00079A6C-D46E-4126-86EE-F9E2951B4593}
	operator _di_IGLNotifyAble()
	{
		_di_IGLNotifyAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IGLNotifyAble*(void) { return (IGLNotifyAble*)&__IGLNotifyAble; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TNotifyCollection;
class PASCALIMPLEMENTATION TNotifyCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
private:
	System::Classes::TNotifyEvent FOnNotifyChange;
	
protected:
	virtual void __fastcall Update(System::Classes::TCollectionItem* item);
	
public:
	__fastcall TNotifyCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass AItemClass);
	__property System::Classes::TNotifyEvent OnNotifyChange = {read=FOnNotifyChange, write=FOnNotifyChange};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TNotifyCollection(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Baseclasses */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_BASECLASSES)
using namespace Baseclasses;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// BaseclassesHPP

// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVerletSkeletonColliders.pas' rev: 24.00 (Win32)

#ifndef GlverletskeletoncollidersHPP
#define GlverletskeletoncollidersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VerletClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glverletskeletoncolliders
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSCVerletBase;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCVerletBase : public Glvectorfileobjects::TSkeletonCollider
{
	typedef Glvectorfileobjects::TSkeletonCollider inherited;
	
private:
	Verletclasses::TVerletConstraint* FVerletConstraint;
	
public:
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToVerletWorld(Verletclasses::TVerletWorld* VerletWorld);
	__property Verletclasses::TVerletConstraint* VerletConstraint = {read=FVerletConstraint};
public:
	/* TSkeletonCollider.Create */ inline __fastcall virtual TSCVerletBase(void) : Glvectorfileobjects::TSkeletonCollider() { }
	/* TSkeletonCollider.CreateOwned */ inline __fastcall TSCVerletBase(Glvectorfileobjects::TSkeletonColliderList* AOwner) : Glvectorfileobjects::TSkeletonCollider(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCVerletBase(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TSkeletonCollider(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCVerletBase(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSCVerletSphere;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCVerletSphere : public TSCVerletBase
{
	typedef TSCVerletBase inherited;
	
private:
	float FRadius;
	
protected:
	void __fastcall SetRadius(const float val);
	
public:
	__fastcall virtual TSCVerletSphere(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToVerletWorld(Verletclasses::TVerletWorld* VerletWorld);
	virtual void __fastcall AlignCollider(void);
	__property float Radius = {read=FRadius, write=SetRadius};
public:
	/* TSkeletonCollider.CreateOwned */ inline __fastcall TSCVerletSphere(Glvectorfileobjects::TSkeletonColliderList* AOwner) : TSCVerletBase(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCVerletSphere(Persistentclasses::TVirtualReader* reader) : TSCVerletBase(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCVerletSphere(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSCVerletCapsule;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCVerletCapsule : public TSCVerletBase
{
	typedef TSCVerletBase inherited;
	
private:
	float FRadius;
	float FLength;
	
protected:
	void __fastcall SetRadius(const float val);
	void __fastcall SetLength(const float val);
	
public:
	__fastcall virtual TSCVerletCapsule(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToVerletWorld(Verletclasses::TVerletWorld* VerletWorld);
	virtual void __fastcall AlignCollider(void);
	__property float Radius = {read=FRadius, write=SetRadius};
	__property float Length = {read=FLength, write=SetLength};
public:
	/* TSkeletonCollider.CreateOwned */ inline __fastcall TSCVerletCapsule(Glvectorfileobjects::TSkeletonColliderList* AOwner) : TSCVerletBase(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCVerletCapsule(Persistentclasses::TVirtualReader* reader) : TSCVerletBase(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCVerletCapsule(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall AddSCVerletConstriantsToVerletWorld(Glvectorfileobjects::TSkeletonColliderList* colliders, Verletclasses::TVerletWorld* world);
}	/* namespace Glverletskeletoncolliders */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVERLETSKELETONCOLLIDERS)
using namespace Glverletskeletoncolliders;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlverletskeletoncollidersHPP

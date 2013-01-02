// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLODESkeletonColliders.pas' rev: 24.00 (Win32)

#ifndef GlodeskeletoncollidersHPP
#define GlodeskeletoncollidersHPP

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
#include <ODEImport.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glodeskeletoncolliders
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSCODEBase;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCODEBase : public Glvectorfileobjects::TSkeletonCollider
{
	typedef Glvectorfileobjects::TSkeletonCollider inherited;
	
private:
	Odeimport::TdxGeom *FGeom;
	
public:
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToSpace(Odeimport::PdxSpace Space);
	virtual void __fastcall AlignCollider(void);
	__property Odeimport::PdxGeom Geom = {read=FGeom};
public:
	/* TSkeletonCollider.Create */ inline __fastcall virtual TSCODEBase(void) : Glvectorfileobjects::TSkeletonCollider() { }
	/* TSkeletonCollider.CreateOwned */ inline __fastcall TSCODEBase(Glvectorfileobjects::TSkeletonColliderList* AOwner) : Glvectorfileobjects::TSkeletonCollider(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCODEBase(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TSkeletonCollider(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCODEBase(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSCODESphere;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCODESphere : public TSCODEBase
{
	typedef TSCODEBase inherited;
	
private:
	float FRadius;
	
protected:
	void __fastcall SetRadius(const float val);
	
public:
	__fastcall virtual TSCODESphere(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToSpace(Odeimport::PdxSpace Space);
	__property float Radius = {read=FRadius, write=SetRadius};
public:
	/* TSkeletonCollider.CreateOwned */ inline __fastcall TSCODESphere(Glvectorfileobjects::TSkeletonColliderList* AOwner) : TSCODEBase(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCODESphere(Persistentclasses::TVirtualReader* reader) : TSCODEBase(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCODESphere(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSCODECCylinder;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCODECCylinder : public TSCODEBase
{
	typedef TSCODEBase inherited;
	
private:
	float FRadius;
	float FLength;
	
protected:
	void __fastcall SetRadius(const float val);
	void __fastcall SetLength(const float val);
	
public:
	__fastcall virtual TSCODECCylinder(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToSpace(Odeimport::PdxSpace Space);
	__property float Radius = {read=FRadius, write=SetRadius};
	__property float Length = {read=FLength, write=SetLength};
public:
	/* TSkeletonCollider.CreateOwned */ inline __fastcall TSCODECCylinder(Glvectorfileobjects::TSkeletonColliderList* AOwner) : TSCODEBase(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCODECCylinder(Persistentclasses::TVirtualReader* reader) : TSCODEBase(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCODECCylinder(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSCODEBox;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSCODEBox : public TSCODEBase
{
	typedef TSCODEBase inherited;
	
private:
	float FBoxWidth;
	float FBoxHeight;
	float FBoxDepth;
	
protected:
	void __fastcall SetBoxWidth(const float val);
	void __fastcall SetBoxHeight(const float val);
	void __fastcall SetBoxDepth(const float val);
	
public:
	__fastcall virtual TSCODEBox(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall AddToSpace(Odeimport::PdxSpace Space);
	__property float BoxWidth = {read=FBoxWidth, write=SetBoxWidth};
	__property float BoxHeight = {read=FBoxHeight, write=SetBoxHeight};
	__property float BoxDepth = {read=FBoxDepth, write=SetBoxDepth};
public:
	/* TSkeletonCollider.CreateOwned */ inline __fastcall TSCODEBox(Glvectorfileobjects::TSkeletonColliderList* AOwner) : TSCODEBase(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSCODEBox(Persistentclasses::TVirtualReader* reader) : TSCODEBase(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSCODEBox(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall AddSCODEGeomsToODESpace(Glvectorfileobjects::TSkeletonColliderList* colliders, Odeimport::PdxSpace space);
}	/* namespace Glodeskeletoncolliders */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLODESKELETONCOLLIDERS)
using namespace Glodeskeletoncolliders;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlodeskeletoncollidersHPP

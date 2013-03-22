// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVerletClasses.pas' rev: 24.00 (Win32)

#ifndef GlverletclassesHPP
#define GlverletclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VerletClasses.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <SpatialPartitioning.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glverletclasses
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLVerletNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLVerletNode : public Verletclasses::TVerletNode
{
	typedef Verletclasses::TVerletNode inherited;
	
private:
	Vectortypes::TVector3f FRelativePosition;
	Glscene::TGLBaseSceneObject* FGLBaseSceneObject;
	void __fastcall SetGLBaseSceneObject(Glscene::TGLBaseSceneObject* const Value);
	
protected:
	virtual void __fastcall SetLocation(const Vectortypes::TVector3f &Value);
	
public:
	virtual void __fastcall Verlet(const Verletclasses::TVerletProgressTimes &vpt);
	__property Glscene::TGLBaseSceneObject* GLBaseSceneObject = {read=FGLBaseSceneObject, write=SetGLBaseSceneObject};
	__property Vectortypes::TVector3f RelativePosition = {read=FRelativePosition, write=FRelativePosition};
public:
	/* TVerletNode.CreateOwned */ inline __fastcall virtual TGLVerletNode(Verletclasses::TVerletWorld* const aOwner) : Verletclasses::TVerletNode(aOwner) { }
	/* TVerletNode.Destroy */ inline __fastcall virtual ~TGLVerletNode(void) { }
	
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLVerletNode(void) : Verletclasses::TVerletNode() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLVerletNode(Persistentclasses::TVirtualReader* reader) : Verletclasses::TVerletNode(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Verletclasses::TVCFloor* __fastcall CreateVCPlaneFromGLPlane(Globjects::TGLPlane* Plane, Verletclasses::TVerletWorld* VerletWorld, float Offset);
}	/* namespace Glverletclasses */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVERLETCLASSES)
using namespace Glverletclasses;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlverletclassesHPP

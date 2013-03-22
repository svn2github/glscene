// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPolyhedron.pas' rev: 24.00 (Win32)

#ifndef GlpolyhedronHPP
#define GlpolyhedronHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glpolyhedron
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLDodecahedron;
class PASCALIMPLEMENTATION TGLDodecahedron : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
public:
	/* TGLCustomSceneObject.Create */ inline __fastcall virtual TGLDodecahedron(System::Classes::TComponent* AOwner) : Glscene::TGLSceneObject(AOwner) { }
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLDodecahedron(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLDodecahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLIcosahedron;
class PASCALIMPLEMENTATION TGLIcosahedron : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
public:
	/* TGLCustomSceneObject.Create */ inline __fastcall virtual TGLIcosahedron(System::Classes::TComponent* AOwner) : Glscene::TGLSceneObject(AOwner) { }
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLIcosahedron(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLIcosahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLOctahedron;
class PASCALIMPLEMENTATION TGLOctahedron : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
public:
	/* TGLCustomSceneObject.Create */ inline __fastcall virtual TGLOctahedron(System::Classes::TComponent* AOwner) : Glscene::TGLSceneObject(AOwner) { }
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLOctahedron(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLOctahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLTetrahedron;
class PASCALIMPLEMENTATION TGLTetrahedron : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
public:
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
public:
	/* TGLCustomSceneObject.Create */ inline __fastcall virtual TGLTetrahedron(System::Classes::TComponent* AOwner) : Glscene::TGLSceneObject(AOwner) { }
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLTetrahedron(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTetrahedron(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glpolyhedron */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPOLYHEDRON)
using namespace Glpolyhedron;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlpolyhedronHPP

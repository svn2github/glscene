// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSilhouette.pas' rev: 24.00 (Win32)

#ifndef GlsilhouetteHPP
#define GlsilhouetteHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsilhouette
{
//-- type declarations -------------------------------------------------------
enum TGLSilhouetteStyle : unsigned char { ssOmni, ssParallel };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLSilhouetteParameters
{
public:
	Vectortypes::TVector3f SeenFrom;
	Vectortypes::TVector3f LightDirection;
	TGLSilhouetteStyle Style;
	bool CappingRequired;
};
#pragma pack(pop)


class DELPHICLASS TGLSilhouette;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSilhouette : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Vectorlists::TVectorList* FVertices;
	Vectorlists::TIntegerList* FIndices;
	Vectorlists::TIntegerList* FCapIndices;
	TGLSilhouetteParameters FParameters;
	
protected:
	void __fastcall SetIndices(Vectorlists::TIntegerList* const value);
	void __fastcall SetCapIndices(Vectorlists::TIntegerList* const value);
	void __fastcall SetVertices(Vectorlists::TVectorList* const value);
	
public:
	__fastcall virtual TGLSilhouette(void);
	__fastcall virtual ~TGLSilhouette(void);
	__property TGLSilhouetteParameters Parameters = {read=FParameters, write=FParameters};
	__property Vectorlists::TVectorList* Vertices = {read=FVertices, write=SetVertices};
	__property Vectorlists::TIntegerList* Indices = {read=FIndices, write=SetIndices};
	__property Vectorlists::TIntegerList* CapIndices = {read=FCapIndices, write=SetCapIndices};
	void __fastcall Flush(void);
	void __fastcall Clear(void);
	void __fastcall ExtrudeVerticesToInfinity(const Vectortypes::TVector3f &origin);
	void __fastcall AddEdgeToSilhouette(const Vectortypes::TVector3f &v0, const Vectortypes::TVector3f &v1, bool tightButSlow);
	void __fastcall AddIndexedEdgeToSilhouette(const int Vi0, const int Vi1);
	void __fastcall AddCapToSilhouette(const Vectortypes::TVector3f &v0, const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, bool tightButSlow);
	void __fastcall AddIndexedCapToSilhouette(const int vi0, const int vi1, const int vi2);
};

#pragma pack(pop)

class DELPHICLASS TBaseConnectivity;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseConnectivity : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	bool FPrecomputeFaceNormal;
	virtual int __fastcall GetEdgeCount(void);
	virtual int __fastcall GetFaceCount(void);
	
public:
	__property int EdgeCount = {read=GetEdgeCount, nodefault};
	__property int FaceCount = {read=GetFaceCount, nodefault};
	__property bool PrecomputeFaceNormal = {read=FPrecomputeFaceNormal, nodefault};
	virtual void __fastcall CreateSilhouette(const TGLSilhouetteParameters &ASilhouetteParameters, TGLSilhouette* &ASilhouette, bool AddToSilhouette);
	__fastcall virtual TBaseConnectivity(bool APrecomputeFaceNormal);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TBaseConnectivity(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TConnectivity;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TConnectivity : public TBaseConnectivity
{
	typedef TBaseConnectivity inherited;
	
protected:
	Vectorlists::TIntegerList* FEdgeVertices;
	Vectorlists::TIntegerList* FEdgeFaces;
	Vectorlists::TByteList* FFaceVisible;
	Vectorlists::TIntegerList* FFaceVertexIndex;
	Vectorlists::TAffineVectorList* FFaceNormal;
	Vectorlists::TIntegerList* FVertexMemory;
	Vectorlists::TAffineVectorList* FVertices;
	virtual int __fastcall GetEdgeCount(void);
	virtual int __fastcall GetFaceCount(void);
	int __fastcall ReuseOrFindVertexID(const Vectortypes::TVector3f &seenFrom, TGLSilhouette* aSilhouette, int index);
	
public:
	virtual void __fastcall Clear(void);
	virtual void __fastcall CreateSilhouette(const TGLSilhouetteParameters &silhouetteParameters, TGLSilhouette* &aSilhouette, bool AddToSilhouette);
	int __fastcall AddIndexedEdge(int vertexIndex0, int vertexIndex1, int FaceID);
	int __fastcall AddIndexedFace(int vi0, int vi1, int vi2);
	int __fastcall AddFace(const Vectortypes::TVector3f &vertex0, const Vectortypes::TVector3f &vertex1, const Vectortypes::TVector3f &vertex2);
	int __fastcall AddQuad(const Vectortypes::TVector3f &vertex0, const Vectortypes::TVector3f &vertex1, const Vectortypes::TVector3f &vertex2, const Vectortypes::TVector3f &vertex3);
	__property int EdgeCount = {read=GetEdgeCount, nodefault};
	__property int FaceCount = {read=GetFaceCount, nodefault};
	__fastcall virtual TConnectivity(bool APrecomputeFaceNormal);
	__fastcall virtual ~TConnectivity(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsilhouette */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSILHOUETTE)
using namespace Glsilhouette;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsilhouetteHPP

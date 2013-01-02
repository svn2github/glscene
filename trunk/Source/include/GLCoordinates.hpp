// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCoordinates.pas' rev: 24.00 (Win32)

#ifndef GlcoordinatesHPP
#define GlcoordinatesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcoordinates
{
//-- type declarations -------------------------------------------------------
enum TGLCoordinatesStyle : unsigned char { CsPoint2D, CsPoint, CsVector, CsUnknown };

class DELPHICLASS TGLCustomCoordinates;
class PASCALIMPLEMENTATION TGLCustomCoordinates : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
public:
	float operator[](const int AIndex) { return Coordinate[AIndex]; }
	
private:
	Vectortypes::TVector4f FCoords;
	TGLCoordinatesStyle FStyle;
	Vectortypes::TVector4f *FPDefaultCoords;
	void __fastcall SetAsPoint2D(const Vectortypes::TVector2f &Value);
	void __fastcall SetAsVector(const Vectortypes::TVector4f &Value);
	void __fastcall SetAsAffineVector(const Vectortypes::TVector3f &Value);
	Vectortypes::TVector3f __fastcall GetAsAffineVector(void);
	Vectortypes::TVector2f __fastcall GetAsPoint2D(void);
	System::UnicodeString __fastcall GetAsString(void);
	float __fastcall GetCoordinate(const int AIndex);
	void __fastcall SetCoordinate(const int AIndex, const float AValue);
	float __fastcall GetDirectCoordinate(const int Index);
	void __fastcall SetDirectCoordinate(const int Index, const float AValue);
	
protected:
	void __fastcall SetDirectVector(const Vectortypes::TVector4f &V);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	
public:
	__fastcall TGLCustomCoordinates(System::Classes::TPersistent* AOwner, const Vectortypes::TVector4f &AValue, const TGLCoordinatesStyle AStyle);
	__fastcall virtual ~TGLCustomCoordinates(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall WriteToFiler(System::Classes::TWriter* Writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* Reader);
	void __fastcall Initialize(const Vectortypes::TVector4f &Value);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property TGLCoordinatesStyle Style = {read=FStyle, write=FStyle, nodefault};
	void __fastcall Translate(const Vectortypes::TVector4f &TranslationVector)/* overload */;
	void __fastcall Translate(const Vectortypes::TVector3f &TranslationVector)/* overload */;
	void __fastcall AddScaledVector(const float Factor, const Vectortypes::TVector4f &TranslationVector)/* overload */;
	void __fastcall AddScaledVector(const float Factor, const Vectortypes::TVector3f &TranslationVector)/* overload */;
	void __fastcall Rotate(const Vectortypes::TVector3f &AnAxis, float AnAngle)/* overload */;
	void __fastcall Rotate(const Vectortypes::TVector4f &AnAxis, float AnAngle)/* overload */;
	void __fastcall Normalize(void);
	void __fastcall Invert(void);
	void __fastcall Scale(float Factor);
	float __fastcall VectorLength(void);
	float __fastcall VectorNorm(void);
	float __fastcall MaxXYZ(void);
	HIDESBASE bool __fastcall Equals(const Vectortypes::TVector4f &AVector);
	void __fastcall SetVector(const float X, const float Y, float Z = 0.000000E+00)/* overload */;
	void __fastcall SetVector(const float X, const float Y, const float Z, const float W)/* overload */;
	void __fastcall SetVector(const Vectortypes::TVector3f &V)/* overload */;
	void __fastcall SetVector(const Vectortypes::TVector4f &V)/* overload */;
	void __fastcall SetPoint(const float X, const float Y, const float Z)/* overload */;
	void __fastcall SetPoint(const Vectortypes::TVector3f &V)/* overload */;
	void __fastcall SetPoint(const Vectortypes::TVector4f &V)/* overload */;
	void __fastcall SetPoint2D(const float X, const float Y)/* overload */;
	void __fastcall SetPoint2D(const Vectortypes::TVector3f &Vector)/* overload */;
	void __fastcall SetPoint2D(const Vectortypes::TVector4f &Vector)/* overload */;
	void __fastcall SetPoint2D(const Vectortypes::TVector2f &Vector)/* overload */;
	void __fastcall SetToZero(void);
	System::PSingle __fastcall AsAddress(void);
	__property Vectortypes::TVector4f AsVector = {read=FCoords, write=SetAsVector};
	__property Vectortypes::TVector3f AsAffineVector = {read=GetAsAffineVector, write=SetAsAffineVector};
	__property Vectortypes::TVector2f AsPoint2D = {read=GetAsPoint2D, write=SetAsPoint2D};
	__property float X = {read=GetCoordinate, write=SetCoordinate, index=0};
	__property float Y = {read=GetCoordinate, write=SetCoordinate, index=1};
	__property float Z = {read=GetCoordinate, write=SetCoordinate, index=2};
	__property float W = {read=GetCoordinate, write=SetCoordinate, index=3};
	__property float Coordinate[const int AIndex] = {read=GetCoordinate, write=SetCoordinate/*, default*/};
	__property System::UnicodeString AsString = {read=GetAsString};
	__property Vectortypes::TVector4f DirectVector = {read=FCoords, write=SetDirectVector};
	__property float DirectX = {read=GetDirectCoordinate, write=SetDirectCoordinate, index=0};
	__property float DirectY = {read=GetDirectCoordinate, write=SetDirectCoordinate, index=1};
	__property float DirectZ = {read=GetDirectCoordinate, write=SetDirectCoordinate, index=2};
	__property float DirectW = {read=GetDirectCoordinate, write=SetDirectCoordinate, index=3};
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLCustomCoordinates(System::Classes::TPersistent* AOwner) : Baseclasses::TGLUpdateAbleObject(AOwner) { }
	
};


class DELPHICLASS TGLCoordinates2;
class PASCALIMPLEMENTATION TGLCoordinates2 : public TGLCustomCoordinates
{
	typedef TGLCustomCoordinates inherited;
	
__published:
	__property X = {stored=false, index=0, default=0};
	__property Y = {stored=false, index=1, default=0};
public:
	/* TGLCustomCoordinates.CreateInitialized */ inline __fastcall TGLCoordinates2(System::Classes::TPersistent* AOwner, const Vectortypes::TVector4f &AValue, const TGLCoordinatesStyle AStyle) : TGLCustomCoordinates(AOwner, AValue, AStyle) { }
	/* TGLCustomCoordinates.Destroy */ inline __fastcall virtual ~TGLCoordinates2(void) { }
	
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLCoordinates2(System::Classes::TPersistent* AOwner) : TGLCustomCoordinates(AOwner) { }
	
};


class DELPHICLASS TGLCoordinates3;
class PASCALIMPLEMENTATION TGLCoordinates3 : public TGLCustomCoordinates
{
	typedef TGLCustomCoordinates inherited;
	
__published:
	__property X = {stored=false, index=0, default=0};
	__property Y = {stored=false, index=1, default=0};
	__property Z = {stored=false, index=2, default=0};
public:
	/* TGLCustomCoordinates.CreateInitialized */ inline __fastcall TGLCoordinates3(System::Classes::TPersistent* AOwner, const Vectortypes::TVector4f &AValue, const TGLCoordinatesStyle AStyle) : TGLCustomCoordinates(AOwner, AValue, AStyle) { }
	/* TGLCustomCoordinates.Destroy */ inline __fastcall virtual ~TGLCoordinates3(void) { }
	
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLCoordinates3(System::Classes::TPersistent* AOwner) : TGLCustomCoordinates(AOwner) { }
	
};


class DELPHICLASS TGLCoordinates4;
class PASCALIMPLEMENTATION TGLCoordinates4 : public TGLCustomCoordinates
{
	typedef TGLCustomCoordinates inherited;
	
__published:
	__property X = {stored=false, index=0, default=0};
	__property Y = {stored=false, index=1, default=0};
	__property Z = {stored=false, index=2, default=0};
	__property W = {stored=false, index=3, default=0};
public:
	/* TGLCustomCoordinates.CreateInitialized */ inline __fastcall TGLCoordinates4(System::Classes::TPersistent* AOwner, const Vectortypes::TVector4f &AValue, const TGLCoordinatesStyle AStyle) : TGLCustomCoordinates(AOwner, AValue, AStyle) { }
	/* TGLCustomCoordinates.Destroy */ inline __fastcall virtual ~TGLCoordinates4(void) { }
	
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLCoordinates4(System::Classes::TPersistent* AOwner) : TGLCustomCoordinates(AOwner) { }
	
};


typedef TGLCoordinates3 TGLCoordinates;

__interface IGLCoordinatesUpdateAble;
typedef System::DelphiInterface<IGLCoordinatesUpdateAble> _di_IGLCoordinatesUpdateAble;
__interface  INTERFACE_UUID("{ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}") IGLCoordinatesUpdateAble  : public System::IInterface 
{
	
public:
	virtual void __fastcall CoordinateChanged(TGLCustomCoordinates* Sender) = 0 ;
};

class DELPHICLASS TGLCoordinatesUpdateAbleComponent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCoordinatesUpdateAbleComponent : public Baseclasses::TGLUpdateAbleComponent
{
	typedef Baseclasses::TGLUpdateAbleComponent inherited;
	
public:
	virtual void __fastcall CoordinateChanged(TGLCustomCoordinates* Sender) = 0 ;
public:
	/* TComponent.Create */ inline __fastcall virtual TGLCoordinatesUpdateAbleComponent(System::Classes::TComponent* AOwner) : Baseclasses::TGLUpdateAbleComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLCoordinatesUpdateAbleComponent(void) { }
	
private:
	void *__IGLCoordinatesUpdateAble;	/* IGLCoordinatesUpdateAble */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}
	operator _di_IGLCoordinatesUpdateAble()
	{
		_di_IGLCoordinatesUpdateAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IGLCoordinatesUpdateAble*(void) { return (IGLCoordinatesUpdateAble*)&__IGLCoordinatesUpdateAble; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool VUseDefaultCoordinateSets;
}	/* namespace Glcoordinates */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCOORDINATES)
using namespace Glcoordinates;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcoordinatesHPP

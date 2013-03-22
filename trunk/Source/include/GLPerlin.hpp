// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPerlin.pas' rev: 24.00 (Win32)

#ifndef GlperlinHPP
#define GlperlinHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLPerlinBase.hpp>	// Pascal unit
#include <GLHeightData.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glperlin
{
//-- type declarations -------------------------------------------------------
enum TGLPerlinInterpolation : unsigned char { pi_none, pi_simple, pi_linear, pi_Smoothed, pi_Cosine, pi_cubic };

class DELPHICLASS TGLBasePerlinOctav;
class PASCALIMPLEMENTATION TGLBasePerlinOctav : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	double FAmplitude;
	double FScale;
	TGLPerlinInterpolation FInterpolation;
	TGLPerlinInterpolation FSmoothing;
	
public:
	DYNAMIC void __fastcall Generate(void) = 0 ;
	__property TGLPerlinInterpolation Interpolation = {read=FInterpolation, write=FInterpolation, nodefault};
	__property TGLPerlinInterpolation Smoothing = {read=FSmoothing, write=FSmoothing, nodefault};
	__property double Amplitude = {read=FAmplitude, write=FAmplitude};
	__property double Scale = {read=FScale, write=FScale};
public:
	/* TObject.Create */ inline __fastcall TGLBasePerlinOctav(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBasePerlinOctav(void) { }
	
};


typedef System::TMetaClass* TGLPerlinOctav;

class DELPHICLASS TGLBasePerlin;
class PASCALIMPLEMENTATION TGLBasePerlin : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	double FPersistence;
	int FNumber_Of_Octaves;
	System::Classes::TList* FOctaves;
	TGLPerlinOctav FOctavClass;
	TGLPerlinInterpolation FInterpolation;
	TGLPerlinInterpolation FSmoothing;
	
protected:
	double __fastcall PerlinNoise_1D(double x);
	double __fastcall PerlinNoise_2D(double x, double y);
	TGLBasePerlinOctav* __fastcall GetOctave(int index);
	void __fastcall SetPersistence(double val);
	void __fastcall Set_Number_Of_Octaves(int val);
	
public:
	__fastcall virtual TGLBasePerlin(System::Classes::TComponent* AOwner);
	DYNAMIC void __fastcall Generate(void) = 0 ;
	__property TGLBasePerlinOctav* Octaves[int index] = {read=GetOctave};
	
__published:
	__property TGLPerlinInterpolation Smoothing = {read=FSmoothing, write=FSmoothing, nodefault};
	__property TGLPerlinInterpolation Interpolation = {read=FInterpolation, write=FInterpolation, nodefault};
	__property double Persistence = {read=FPersistence, write=SetPersistence};
	__property int Number_Of_Octaves = {read=FNumber_Of_Octaves, write=Set_Number_Of_Octaves, nodefault};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLBasePerlin(void) { }
	
};


class DELPHICLASS TGL1DPerlin;
class PASCALIMPLEMENTATION TGL1DPerlin : public TGLBasePerlin
{
	typedef TGLBasePerlin inherited;
	
__published:
	double __fastcall GetPerlinValue_1D(double x);
public:
	/* TGLBasePerlin.Create */ inline __fastcall virtual TGL1DPerlin(System::Classes::TComponent* AOwner) : TGLBasePerlin(AOwner) { }
	
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TGL1DPerlin(void) { }
	
};


class DELPHICLASS TGL2DPerlinOctav;
class PASCALIMPLEMENTATION TGL2DPerlinOctav : public TGLBasePerlinOctav
{
	typedef TGLBasePerlinOctav inherited;
	
public:
	Glperlinbase::T2DPerlinArray Data;
	int Width;
	int Height;
	int XStart;
	int YStart;
	int XStep;
	int YStep;
	int YRate;
	DYNAMIC void __fastcall Generate(void);
	double __fastcall GetDataSmoothed(int x, int y);
	double __fastcall GetData(int x, int y);
	double __fastcall GetCubic(double x, double y);
	double __fastcall GetCosine(double x, double y);
	double __fastcall GetPerling(double x, double y);
	void __fastcall Generate_CubicInterpolate(void);
	void __fastcall Generate_SmoothInterpolate(void);
	void __fastcall Generate_NonInterpolated(void);
public:
	/* TObject.Create */ inline __fastcall TGL2DPerlinOctav(void) : TGLBasePerlinOctav() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGL2DPerlinOctav(void) { }
	
};


class DELPHICLASS TGL2DPerlin;
class PASCALIMPLEMENTATION TGL2DPerlin : public TGLBasePerlin
{
	typedef TGLBasePerlin inherited;
	
public:
	int Width;
	int Height;
	int XStart;
	int YStart;
	int XStep;
	int YStep;
	double MaxValue;
	double MinValue;
	__fastcall virtual TGL2DPerlin(System::Classes::TComponent* AOwner);
	DYNAMIC void __fastcall Generate(void);
	double __fastcall GetPerlinValue_2D(double x, double y);
	void __fastcall MakeBitmap(Vcl::Graphics::TBitmap* Param);
	void __fastcall SetHeightData(Glheightdata::THeightData* heightData);
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TGL2DPerlin(void) { }
	
};


class DELPHICLASS TGLPerlinHDS;
class PASCALIMPLEMENTATION TGLPerlinHDS : public Glheightdata::THeightDataSource
{
	typedef Glheightdata::THeightDataSource inherited;
	
private:
	TGLPerlinInterpolation FInterpolation;
	TGLPerlinInterpolation FSmoothing;
	double FPersistence;
	int FNumber_Of_Octaves;
	System::Classes::TStrings* FLines;
	bool FLinesChanged;
	int FXStart;
	int FYStart;
	
public:
	double MaxValue;
	double MinValue;
	bool Stall;
	__fastcall virtual TGLPerlinHDS(System::Classes::TComponent* AOwner);
	virtual void __fastcall StartPreparingData(Glheightdata::THeightData* heightData);
	void __fastcall WaitFor(void);
	__property System::Classes::TStrings* Lines = {read=FLines};
	__property bool LinesChanged = {read=FLinesChanged, write=FLinesChanged, nodefault};
	
__published:
	__property TGLPerlinInterpolation Interpolation = {read=FInterpolation, write=FInterpolation, nodefault};
	__property TGLPerlinInterpolation Smoothing = {read=FSmoothing, write=FSmoothing, nodefault};
	__property double Persistence = {read=FPersistence, write=FPersistence};
	__property int Number_Of_Octaves = {read=FNumber_Of_Octaves, write=FNumber_Of_Octaves, nodefault};
	__property MaxPoolSize;
	__property int XStart = {read=FXStart, write=FXStart, nodefault};
	__property int YStart = {read=FYStart, write=FYStart, nodefault};
public:
	/* THeightDataSource.Destroy */ inline __fastcall virtual ~TGLPerlinHDS(void) { }
	
};


class DELPHICLASS TGLPerlinHDSThread;
class PASCALIMPLEMENTATION TGLPerlinHDSThread : public Glheightdata::THeightDataThread
{
	typedef Glheightdata::THeightDataThread inherited;
	
public:
	TGL2DPerlin* Perlin;
	TGLPerlinHDS* PerlinSource;
	void __fastcall OpdateOutSide(void);
	virtual void __fastcall Execute(void);
public:
	/* THeightDataThread.Destroy */ inline __fastcall virtual ~TGLPerlinHDSThread(void) { }
	
public:
	/* TThread.Create */ inline __fastcall TGLPerlinHDSThread(void)/* overload */ : Glheightdata::THeightDataThread() { }
	/* TThread.Create */ inline __fastcall TGLPerlinHDSThread(bool CreateSuspended)/* overload */ : Glheightdata::THeightDataThread(CreateSuspended) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glperlin */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPERLIN)
using namespace Glperlin;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlperlinHPP

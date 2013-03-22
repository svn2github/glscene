// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLProcTextures.pas' rev: 24.00 (Win32)

#ifndef GlproctexturesHPP
#define GlproctexturesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glproctextures
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLProcTextureNoise;
class PASCALIMPLEMENTATION TGLProcTextureNoise : public Gltexture::TGLTextureImage
{
	typedef Gltexture::TGLTextureImage inherited;
	
private:
	Glgraphics::TGLImage* FNoiseMap;
	int FWidth;
	int FHeight;
	System::Byte FMinCut;
	float FNoiseSharpness;
	float FNoiseAnimate;
	bool FSeamless;
	int FNoiseRandSeed;
	
protected:
	System::StaticArray<float, 768> FGradients;
	System::StaticArray<System::Byte, 256> PERM;
	virtual int __fastcall GetWidth(void);
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetDepth(void);
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	float __fastcall Noise(float x, float y);
	void __fastcall SetMinCut(const System::Byte val);
	void __fastcall SetSeamless(const bool val);
	void __fastcall SetWidth(const int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetNoiseSharpness(const float val);
	void __fastcall SetNoiseRandSeed(const int val);
	void __fastcall UpdateNoise(void);
	
public:
	__fastcall virtual TGLProcTextureNoise(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLProcTextureNoise(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32(void);
	virtual void __fastcall ReleaseBitmap32(void);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall NoiseAnimate(float speed);
	void __fastcall SetPermFromData(System::Byte *inPERM, const int inPERM_Size);
	void __fastcall SetPermToDefault(void);
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, default=128};
	__property int Height = {read=GetHeight, write=SetHeight, default=128};
	__property int Depth = {read=GetDepth, nodefault};
	__property System::Byte MinCut = {read=FMinCut, write=SetMinCut, nodefault};
	__property float NoiseSharpness = {read=FNoiseSharpness, write=SetNoiseSharpness};
	__property bool Seamless = {read=FSeamless, write=SetSeamless, nodefault};
	__property int NoiseRandSeed = {read=FNoiseRandSeed, write=SetNoiseRandSeed, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word GRADIENT_TABLE_SIZE = System::Word(0x100);
static const System::Int8 DAMP_SHIFT = System::Int8(0x14);
}	/* namespace Glproctextures */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPROCTEXTURES)
using namespace Glproctextures;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlproctexturesHPP

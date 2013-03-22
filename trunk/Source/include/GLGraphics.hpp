// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGraphics.pas' rev: 24.00 (Win32)

#ifndef GlgraphicsHPP
#define GlgraphicsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <Vcl.Imaging.pngimage.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <ImageUtils.hpp>	// Pascal unit
#include <GLUtils.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glgraphics
{
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLPixel24
{
public:
	System::Byte r;
	System::Byte g;
	System::Byte b;
};
#pragma pack(pop)


typedef TGLPixel24 *PGLPixel24;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLPixel32
{
public:
	System::Byte r;
	System::Byte g;
	System::Byte b;
	System::Byte a;
};
#pragma pack(pop)


typedef TGLPixel32 *PGLPixel32;

typedef System::StaticArray<TGLPixel32, 268435456> TGLPixel32Array;

typedef TGLPixel32Array *PGLPixel32Array;

enum TGLLODStreamingState : unsigned char { ssKeeping, ssLoading, ssLoaded, ssTransfered };

struct DECLSPEC_DRECORD TGLImageLevelDesc
{
public:
	int Width;
	int Height;
	int Depth;
	Glcontext::TGLUnpackPBOHandle* PBO;
	void *MapAddress;
	unsigned Offset;
	unsigned StreamOffset;
	unsigned Size;
	TGLLODStreamingState State;
};


typedef System::Int8 TGLImageLODRange;

typedef System::StaticArray<TGLImageLevelDesc, 16> TGLImagePiramid;

class DELPHICLASS TGLBaseImage;
class PASCALIMPLEMENTATION TGLBaseImage : public Applicationfileio::TDataFile
{
	typedef Applicationfileio::TDataFile inherited;
	
private:
	System::Classes::TStream* FSourceStream;
	TGLImageLODRange FStreamLevel;
	Glcontext::TFinishTaskEvent* FFinishEvent;
	void __stdcall ImageStreamingTask(void);
	
protected:
	TGLPixel32Array *fData;
	TGLImagePiramid FLOD;
	TGLImageLODRange fLevelCount;
	unsigned fColorFormat;
	Gltextureformat::TGLInternalFormat fInternalFormat;
	unsigned fDataType;
	int fElementSize;
	bool fCubeMap;
	bool fTextureArray;
	virtual PGLPixel32Array __fastcall GetData(void);
	int __fastcall GetWidth(void);
	int __fastcall GetHeight(void);
	int __fastcall GetDepth(void);
	void * __fastcall GetLevelAddress(System::Byte ALevel)/* overload */;
	void * __fastcall GetLevelAddress(System::Byte ALevel, System::Byte AFace)/* overload */;
	int __fastcall GetLevelWidth(TGLImageLODRange ALOD);
	int __fastcall GetLevelHeight(TGLImageLODRange ALOD);
	int __fastcall GetLevelDepth(TGLImageLODRange ALOD);
	Glcontext::TGLUnpackPBOHandle* __fastcall GetLevelPBO(TGLImageLODRange ALOD);
	int __fastcall GetLevelOffset(TGLImageLODRange ALOD);
	int __fastcall GetLevelSizeInByte(TGLImageLODRange ALOD);
	TGLLODStreamingState __fastcall GetLevelStreamingState(TGLImageLODRange ALOD);
	void __fastcall SetLevelStreamingState(TGLImageLODRange ALOD, TGLLODStreamingState AState);
	void __fastcall SaveHeader(void);
	void __fastcall LoadHeader(void);
	void __fastcall StartStreaming(void);
	void __fastcall DoStreaming(void);
	
public:
	__fastcall virtual TGLBaseImage(void);
	__fastcall virtual ~TGLBaseImage(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	virtual void __fastcall RegisterAsOpenGLTexture(Glcontext::TGLTextureHandle* AHandle, bool aMipmapGen, unsigned aTexFormat, /* out */ int &texWidth, /* out */ int &texHeight, /* out */ int &texDepth);
	virtual bool __fastcall AssignFromTexture(Glcontext::TGLTextureHandle* AHandle, const bool CastToFormat, const Gltextureformat::TGLInternalFormat intFormat = (Gltextureformat::TGLInternalFormat)(0x1f), const unsigned colorFormat = (unsigned)(0x0), const unsigned dataType = (unsigned)(0x0));
	bool __fastcall ConvertCrossToCubeMap(void);
	bool __fastcall ConvertToVolume(const int col, const int row, const bool MakeArray);
	unsigned __fastcall DataSize(void);
	bool __fastcall IsEmpty(void);
	bool __fastcall IsCompressed(void);
	bool __fastcall IsVolume(void);
	void __fastcall Narrow(void);
	virtual void __fastcall GenerateMipmap(Imageutils::TImageFilterFunction AFilter);
	virtual void __fastcall UnMipmap(void);
	__property PGLPixel32Array Data = {read=GetData};
	void __fastcall SetErrorImage(void);
	void __fastcall UpdateLevelsInfo(void);
	__property int LevelWidth[TGLImageLODRange ALOD] = {read=GetLevelWidth};
	__property int LevelHeight[TGLImageLODRange ALOD] = {read=GetLevelHeight};
	__property int LevelDepth[TGLImageLODRange ALOD] = {read=GetLevelDepth};
	__property Glcontext::TGLUnpackPBOHandle* LevelPixelBuffer[TGLImageLODRange ALOD] = {read=GetLevelPBO};
	__property int LevelOffset[TGLImageLODRange ALOD] = {read=GetLevelOffset};
	__property int LevelSizeInByte[TGLImageLODRange ALOD] = {read=GetLevelSizeInByte};
	__property TGLLODStreamingState LevelStreamingState[TGLImageLODRange ALOD] = {read=GetLevelStreamingState, write=SetLevelStreamingState};
	__property TGLImageLODRange LevelCount = {read=fLevelCount, nodefault};
	__property Gltextureformat::TGLInternalFormat InternalFormat = {read=fInternalFormat, nodefault};
	__property unsigned ColorFormat = {read=fColorFormat, nodefault};
	__property unsigned DataType = {read=fDataType, nodefault};
	__property int ElementSize = {read=fElementSize, nodefault};
	__property bool CubeMap = {read=fCubeMap, nodefault};
	__property bool TextureArray = {read=fTextureArray, nodefault};
};


typedef System::TMetaClass* TGLBaseImageClass;

class DELPHICLASS TGLImage;
class PASCALIMPLEMENTATION TGLImage : public TGLBaseImage
{
	typedef TGLBaseImage inherited;
	
private:
	bool FVerticalReverseOnAssignFromBitmap;
	bool FBlank;
	unsigned fOldColorFormat;
	unsigned fOldDataType;
	void __fastcall DataConvertTask(void);
	
protected:
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetDepth(const int val);
	void __fastcall SetBlank(const bool Value);
	void __fastcall SetCubeMap(const bool val);
	void __fastcall SetArray(const bool val);
	PGLPixel32Array __fastcall GetScanLine(int index);
	void __fastcall AssignFrom24BitsBitmap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFrom32BitsBitmap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFromPngImage(Vcl::Imaging::Pngimage::TPngImage* aPngImage);
	
public:
	__fastcall virtual TGLImage(void);
	__fastcall virtual ~TGLImage(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall AssignFromBitmap24WithoutRGBSwap(Vcl::Graphics::TBitmap* aBitmap);
	void __fastcall AssignFromTexture2D(unsigned textureHandle)/* overload */;
	void __fastcall AssignFromTexture2D(Glcontext::TGLTextureHandle* textureHandle)/* overload */;
	Vcl::Graphics::TBitmap* __fastcall Create32BitsBitmap(void);
	__property int Width = {read=GetWidth, write=SetWidth, nodefault};
	__property int Height = {read=GetHeight, write=SetHeight, nodefault};
	__property int Depth = {read=GetDepth, write=SetDepth, nodefault};
	__property unsigned ColorFormat = {read=fColorFormat, nodefault};
	__property Gltextureformat::TGLInternalFormat InternalFormat = {read=fInternalFormat, write=fInternalFormat, nodefault};
	__property unsigned DataType = {read=fDataType, nodefault};
	__property int ElementSize = {read=fElementSize, nodefault};
	__property bool CubeMap = {read=fCubeMap, write=SetCubeMap, nodefault};
	__property bool TextureArray = {read=fTextureArray, write=SetArray, nodefault};
	__property PGLPixel32Array ScanLine[int index] = {read=GetScanLine};
	__property bool VerticalReverseOnAssignFromBitmap = {read=FVerticalReverseOnAssignFromBitmap, write=FVerticalReverseOnAssignFromBitmap, nodefault};
	__property bool Blank = {read=FBlank, write=SetBlank, nodefault};
	void __fastcall SetColorFormatDataType(const unsigned AColorFormat, const unsigned ADataType);
	void __fastcall SetAlphaFromIntensity(void);
	void __fastcall SetAlphaTransparentForColor(const System::Uitypes::TColor aColor)/* overload */;
	void __fastcall SetAlphaTransparentForColor(const TGLPixel32 aColor)/* overload */;
	void __fastcall SetAlphaTransparentForColor(const TGLPixel24 aColor)/* overload */;
	void __fastcall SetAlphaToValue(const System::Byte aValue);
	void __fastcall SetAlphaToFloatValue(const float aValue);
	void __fastcall InvertAlpha(void);
	void __fastcall SqrtAlpha(void);
	void __fastcall BrightnessCorrection(const float factor);
	void __fastcall GammaCorrection(const float gamma);
	void __fastcall DownSampleByFactor2(void);
	void __fastcall ReadPixels(const System::Types::TRect &area);
	void __fastcall DrawPixels(const float x, const float y);
	void __fastcall GrayScaleToNormalMap(const float scale, bool wrapX = true, bool wrapY = true);
	void __fastcall NormalizeNormalMap(void);
	void __fastcall AssignToBitmap(Vcl::Graphics::TBitmap* aBitmap);
	virtual void __fastcall GenerateMipmap(Imageutils::TImageFilterFunction AFilter);
	virtual void __fastcall UnMipmap(void);
};


typedef TGLImage TGLBitmap32;

class DELPHICLASS TRasterFileFormat;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TRasterFileFormat : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TGLBaseImageClass BaseImageClass;
	System::UnicodeString Extension;
	System::UnicodeString Description;
	int DescResID;
public:
	/* TObject.Create */ inline __fastcall TRasterFileFormat(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TRasterFileFormat(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TRasterFileFormatsList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TRasterFileFormatsList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	__fastcall virtual ~TRasterFileFormatsList(void);
	HIDESBASE void __fastcall Add(const System::UnicodeString Ext, const System::UnicodeString Desc, int DescID, TGLBaseImageClass AClass);
	TGLBaseImageClass __fastcall FindExt(System::UnicodeString ext);
	TGLBaseImageClass __fastcall FindFromFileName(const System::UnicodeString fileName);
	TGLBaseImageClass __fastcall FindFromStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall Remove(TGLBaseImageClass AClass);
	void __fastcall BuildFilterStrings(TGLBaseImageClass imageFileClass, System::UnicodeString &descriptions, System::UnicodeString &filters, bool formatsThatCanBeOpened = true, bool formatsThatCanBeSaved = false);
	System::UnicodeString __fastcall FindExtByIndex(int index, bool formatsThatCanBeOpened = true, bool formatsThatCanBeSaved = false);
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TRasterFileFormatsList(void) : Persistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TRasterFileFormatsList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS EInvalidRasterFile;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidRasterFile : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidRasterFile(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidRasterFile(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidRasterFile(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidRasterFile(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidRasterFile(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidRasterFile(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidRasterFile(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool vVerticalFlipDDS;
extern PACKAGE TRasterFileFormatsList* __fastcall GetRasterFileFormats(void);
extern PACKAGE void __fastcall RegisterRasterFormat(const System::UnicodeString AExtension, const System::UnicodeString ADescription, TGLBaseImageClass AClass);
extern PACKAGE void __fastcall UnregisterRasterFormat(TGLBaseImageClass AClass);
extern PACKAGE System::UnicodeString __fastcall RasterFileFormatExtensionByIndex(int index);
extern PACKAGE void __fastcall Div2(int &Value);
extern PACKAGE int __fastcall GetImageLodNumber(int w, int h, int d, bool IsVolume);
extern PACKAGE void __fastcall GammaCorrectRGBArray(void * base, int pixelCount, float gamma);
extern PACKAGE void __fastcall BrightenRGBArray(void * base, int pixelCount, float factor);
extern PACKAGE void __fastcall BGR24ToRGB24(void * src, void * dest, int pixelCount);
extern PACKAGE void __fastcall BGR24ToRGBA32(void * src, void * dest, int pixelCount);
extern PACKAGE void __fastcall RGB24ToRGBA32(void * src, void * dest, int pixelCount);
extern PACKAGE void __fastcall BGRA32ToRGBA32(void * src, void * dest, int pixelCount);
}	/* namespace Glgraphics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGRAPHICS)
using namespace Glgraphics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgraphicsHPP

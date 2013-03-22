// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSCUDA.pas' rev: 24.00 (Win32)

#ifndef GlscudaHPP
#define GlscudaHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLS_CL_Platform.hpp>	// Pascal unit
#include <GLS_CUDA_API.hpp>	// Pascal unit
#include <GLS_CUDA_Runtime.hpp>	// Pascal unit
#include <GLSCUDAParser.hpp>	// Pascal unit
#include <GLS_CUDA_FourierTransform.hpp>	// Pascal unit
#include <GLSCUDACompiler.hpp>	// Pascal unit
#include <GLSCUDAContext.hpp>	// Pascal unit
#include <GLSCUDADataAccess.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscuda
{
//-- type declarations -------------------------------------------------------
enum TCUDAChange : unsigned int { cuchDevice, cuchContext, cuchSize, cuchAddresMode, cuchFlag, cuchFilterMode, cuchArray, cuchFormat, cuchMapping };

typedef System::Set<TCUDAChange, TCUDAChange::cuchDevice, TCUDAChange::cuchMapping>  TCUDAChanges;

enum TCuAddresMode : unsigned int { amWrap, amClamp, amMirror };

enum TCuFilterMode : unsigned int { fmPoint, fmLinear };

enum TCUDAChannelType : unsigned int { ctUndefined, ctUInt8, ctUInt16, ctUInt32, ctInt8, ctInt16, ctInt32, ctHalfFloat, ctFloat, ctDouble };

enum TCUDAChannelNum : unsigned int { cnOne, cnTwo, cnThree, cnFour };

struct DECLSPEC_DRECORD TChannelTypeAndNum
{
public:
	TCUDAChannelType F;
	TCUDAChannelNum C;
};


enum TCUDAMapping : unsigned int { grmDefault, grmReadOnly, grmWriteDiscard };

class DELPHICLASS TCUDAComponent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAComponent : public Glscudacontext::TCUDAHandlesMaster
{
	typedef Glscudacontext::TCUDAHandlesMaster inherited;
	
private:
	TCUDAComponent* FMaster;
	Persistentclasses::TPersistentObjectList* FItems;
	void __fastcall SetMaster(TCUDAComponent* AMaster);
	TCUDAComponent* __fastcall GetItem(const int i);
	int __fastcall GetItemsCount(void);
	
protected:
	Gls_cuda_api::TCUresult FStatus;
	TCUDAChanges FChanges;
	virtual Glscudacontext::TCUDAContext* __fastcall GetContext(void);
	void __fastcall CollectStatus(Gls_cuda_api::TCUresult AStatus);
	DYNAMIC void __fastcall GetChildren(System::Classes::TGetChildProc AProc, System::Classes::TComponent* Root);
	void __fastcall AddItem(TCUDAComponent* AItem);
	void __fastcall RemoveItem(TCUDAComponent* AItem);
	void __fastcall DeleteItems(void);
	virtual void __fastcall SetName(const System::Classes::TComponentName NewName);
	virtual bool __fastcall GetIsAllocated(void) = 0 ;
	
public:
	__fastcall virtual ~TCUDAComponent(void);
	virtual void __fastcall CuNotifyChange(TCUDAChange AChange);
	DYNAMIC System::Classes::TComponent* __fastcall GetParentComponent(void);
	DYNAMIC void __fastcall SetParentComponent(System::Classes::TComponent* Value);
	DYNAMIC bool __fastcall HasParent(void);
	TCUDAComponent* __fastcall GetItemByName(const System::UnicodeString name);
	System::UnicodeString __fastcall MakeUniqueName(const System::UnicodeString BaseName);
	__property TCUDAComponent* Master = {read=FMaster, write=SetMaster};
	__property Glscudacontext::TCUDAContext* Context = {read=GetContext};
	__property TCUDAComponent* Items[const int i] = {read=GetItem};
	__property int ItemsCount = {read=GetItemsCount, nodefault};
	__property Gls_cuda_api::TCUresult Status = {read=FStatus, nodefault};
	__property bool IsAllocated = {read=GetIsAllocated, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TCUDAComponent(System::Classes::TComponent* AOwner) : Glscudacontext::TCUDAHandlesMaster(AOwner) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TCUDAComponentClass;

class DELPHICLASS TCUDAModule;
class DELPHICLASS TCUDAFunction;
class DELPHICLASS TCUDATexture;
class DELPHICLASS TCUDAConstant;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAModule : public TCUDAComponent
{
	typedef TCUDAComponent inherited;
	
private:
	Gls_cuda_api::TCUmodule *FHandle;
	System::Classes::TStringList* FCode;
	Glscudacompiler::TGLSCUDACompilerOutput FCodeType;
	Glscudacompiler::TGLSCUDACompiler* FCompiler;
	void __fastcall SetCode(System::Classes::TStringList* const Value);
	void __fastcall SetCompiler(Glscudacompiler::TGLSCUDACompiler* const Value);
	TCUDAFunction* __fastcall GetKernelFunction(const System::UnicodeString AName);
	TCUDATexture* __fastcall GetKernelTexture(const System::UnicodeString AName);
	TCUDAConstant* __fastcall GetKernelConstant(const System::UnicodeString AName);
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	void __fastcall OnChangeCode(System::TObject* Sender);
	virtual void __fastcall Loaded(void);
	virtual Glscudacontext::TCUDAContext* __fastcall GetContext(void);
	virtual bool __fastcall GetIsAllocated(void);
	
public:
	__fastcall virtual TCUDAModule(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAModule(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall LoadFromFile(const System::UnicodeString AFilename);
	void __fastcall LoadFromSource(void);
	void __fastcall Unload(void);
	void __fastcall LoadAndCompile(void);
	__property Glscudacontext::TCUDAContext* Context = {read=GetContext};
	__property Glscudacompiler::TGLSCUDACompilerOutput CodeType = {read=FCodeType, nodefault};
	__property TCUDAFunction* KernelFunction[const System::UnicodeString AName] = {read=GetKernelFunction};
	__property TCUDATexture* KernelTexture[const System::UnicodeString AName] = {read=GetKernelTexture};
	__property TCUDAConstant* KernelConstant[const System::UnicodeString AName] = {read=GetKernelConstant};
	
__published:
	__property System::Classes::TStringList* Code = {read=FCode, write=SetCode};
	__property Glscudacompiler::TGLSCUDACompiler* Compiler = {read=FCompiler, write=SetCompiler};
};

#pragma pack(pop)

enum TGLResourceType : unsigned int { rtTexture, rtBuffer };

class DELPHICLASS TCUDAGraphicResource;
class DELPHICLASS TCUDAMemData;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAGraphicResource : public TCUDAComponent
{
	typedef TCUDAComponent inherited;
	
protected:
	System::StaticArray<Gls_cuda_api::PCUgraphicsResource, 8> FHandle;
	TCUDAMapping FMapping;
	TGLResourceType FResourceType;
	Glcontext::TGLVirtualHandle* FGLContextHandle;
	int FMapCounter;
	virtual bool __fastcall GetIsAllocated(void);
	void __fastcall OnGLHandleAllocate(Glcontext::TGLVirtualHandle* Sender, unsigned &Handle);
	void __fastcall OnGLHandleDestroy(Glcontext::TGLVirtualHandle* Sender, unsigned &Handle);
	virtual void __fastcall BindArrayToTexture(TCUDAMemData* &cudaArray, unsigned ALeyer, unsigned ALevel) = 0 ;
	void __fastcall SetArray(TCUDAMemData* &AArray, Gls_cuda_api::PCUarray AHandle, bool ForGLTexture, bool Volume);
	virtual unsigned __fastcall GetAttributeArraySize(const System::UnicodeString Attr) = 0 ;
	virtual void * __fastcall GetAttributeArrayAddress(const System::UnicodeString Attr) = 0 ;
	virtual unsigned __fastcall GetElementArrayDataSize(void) = 0 ;
	virtual void * __fastcall GetElementArrayAddress(void) = 0 ;
	virtual void __fastcall SetMapping(const TCUDAMapping Value);
	__property TCUDAMapping Mapping = {read=FMapping, write=SetMapping, default=0};
	
public:
	virtual void __fastcall MapResources(void) = 0 ;
	virtual void __fastcall UnMapResources(void) = 0 ;
public:
	/* TCUDAComponent.Destroy */ inline __fastcall virtual ~TCUDAGraphicResource(void) { }
	
public:
	/* TComponent.Create */ inline __fastcall virtual TCUDAGraphicResource(System::Classes::TComponent* AOwner) : TCUDAComponent(AOwner) { }
	
};

#pragma pack(pop)

enum TCUDAMemType : unsigned int { mtHost, mtDevice, mtArray };

enum TCUDAMemMapFlag : unsigned int { mmfPortable, mmfFastWrite };

typedef System::Set<TCUDAMemMapFlag, TCUDAMemMapFlag::mmfPortable, TCUDAMemMapFlag::mmfFastWrite>  TCUDAMemMapFlags;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAMemData : public TCUDAComponent
{
	typedef TCUDAComponent inherited;
	
private:
	void *FData;
	void *FMappedMemory;
	Gls_cuda_api::TCUarray *FHandle;
	int FWidth;
	int FHeight;
	int FDepth;
	unsigned FPitch;
	int FElementSize;
	int FDataSize;
	TCUDAChannelType FChannelsType;
	TCUDAChannelNum fChannelsNum;
	TCUDAMemType FMemoryType;
	TCUDATexture* FTexture;
	bool FOpenGLRefArray;
	bool FMapping;
	void __fastcall SetMemoryType(const TCUDAMemType AType);
	void __fastcall SetWidth(const int Value);
	void __fastcall SetHeight(const int Value);
	void __fastcall SetDepth(const int Value);
	void __fastcall SetChannelType(const TCUDAChannelType Value);
	void __fastcall SetChannelNum(const TCUDAChannelNum Value);
	void * __fastcall GetData(void);
	Gls_cuda_api::PCUarray __fastcall GetArrayHandle(void);
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	virtual bool __fastcall GetIsAllocated(void);
	
public:
	__fastcall virtual TCUDAMemData(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAMemData(void);
	virtual void __fastcall CuNotifyChange(TCUDAChange AChange);
	void __fastcall Map(const TCUDAMemMapFlags AFlags = TCUDAMemMapFlags() );
	void __fastcall UnMap(void);
	template<typename EType> Glscudadataaccess::GCUDAHostElementAccess__1<EType>* __fastcall Data(int X)/* overload */;
	template<typename EType> Glscudadataaccess::GCUDAHostElementAccess__1<EType>* __fastcall Data(int X, int Y)/* overload */;
	template<typename EType> Glscudadataaccess::GCUDAHostElementAccess__1<EType>* __fastcall Data(int X, int Y, int Z)/* overload */;
	void __fastcall FillMem(const void *Value);
	void __fastcall CopyTo(TCUDAMemData* const ADstMemData)/* overload */;
	void __fastcall CopyTo(Glgraphics::TGLImage* const AGLImage)/* overload */;
	void __fastcall CopyTo(TCUDAGraphicResource* const AGLGraphic, System::UnicodeString aAttr = System::UnicodeString())/* overload */;
	void __fastcall CopyFrom(TCUDAMemData* const ASrcMemData)/* overload */;
	void __fastcall CopyFrom(Glgraphics::TGLImage* const AGLImage)/* overload */;
	void __fastcall CopyFrom(TCUDAGraphicResource* const AGLGraphic, System::UnicodeString aAttr = System::UnicodeString())/* overload */;
	void __fastcall SubCopyTo(TCUDAMemData* const ADstMemData, int *ASrcXYZ, int *ADstXYZ, int *ASizes);
	__property int ElementSize = {read=FElementSize, nodefault};
	__property int DataSize = {read=FDataSize, nodefault};
	__property unsigned Pitch = {read=FPitch, nodefault};
	__property void * RawData = {read=GetData};
	__property void * MappedMemoryAddress = {read=FMappedMemory};
	__property Gls_cuda_api::PCUarray ArrayHandle = {read=GetArrayHandle};
	
__published:
	__property int Width = {read=FWidth, write=SetWidth, default=256};
	__property int Height = {read=FHeight, write=SetHeight, default=0};
	__property int Depth = {read=FDepth, write=SetDepth, default=0};
	__property TCUDAMemType MemoryType = {read=FMemoryType, write=SetMemoryType, default=0};
	__property TCUDAChannelType ChannelsType = {read=FChannelsType, write=SetChannelType, default=4};
	__property TCUDAChannelNum ChannelsNum = {read=fChannelsNum, write=SetChannelNum, default=0};
};

#pragma pack(pop)

class DELPHICLASS TCUDAUniform;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAUniform : public TCUDAComponent
{
	typedef TCUDAComponent inherited;
	
protected:
	void *FHandle;
	unsigned FSize;
	System::UnicodeString FKernelName;
	Glscudaparser::TCUDAType FType;
	System::UnicodeString FCustomType;
	bool FRef;
	bool FDefined;
	void __fastcall SetKernelName(const System::UnicodeString AName);
	void __fastcall SetType(Glscudaparser::TCUDAType AValue);
	void __fastcall SetCustomType(const System::UnicodeString AValue);
	void __fastcall SetSize(const unsigned AValue);
	void __fastcall SetRef(bool AValue);
	void __fastcall SetDefined(bool AValue);
	__property System::UnicodeString KernelName = {read=FKernelName, write=SetKernelName};
	__property Glscudaparser::TCUDAType DataType = {read=FType, write=SetType, nodefault};
	__property System::UnicodeString CustomType = {read=FCustomType, write=SetCustomType};
	__property unsigned Size = {read=FSize, write=SetSize, nodefault};
	__property bool Reference = {read=FRef, write=SetRef, nodefault};
	virtual bool __fastcall GetIsAllocated(void);
	
public:
	__fastcall virtual TCUDAUniform(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAUniform(void);
	__property bool IsValueDefined = {read=FDefined, write=SetDefined, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAConstant : public TCUDAUniform
{
	typedef TCUDAUniform inherited;
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	void * __fastcall GetDeviceAddress(void);
	
public:
	__property void * DeviceAddress = {read=GetDeviceAddress};
	
__published:
	__property KernelName = {default=0};
	__property DataType;
	__property CustomType = {default=0};
	__property Size;
	__property Reference;
public:
	/* TCUDAUniform.Create */ inline __fastcall virtual TCUDAConstant(System::Classes::TComponent* AOwner) : TCUDAUniform(AOwner) { }
	/* TCUDAUniform.Destroy */ inline __fastcall virtual ~TCUDAConstant(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TCUDAFuncParam;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAFuncParam : public TCUDAUniform
{
	typedef TCUDAUniform inherited;
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	
public:
	__fastcall virtual TCUDAFuncParam(System::Classes::TComponent* AOwner);
	
__published:
	__property KernelName = {default=0};
	__property DataType;
	__property CustomType = {default=0};
	__property Size;
	__property Reference;
public:
	/* TCUDAUniform.Destroy */ inline __fastcall virtual ~TCUDAFuncParam(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCUDAFunction : public TCUDAComponent
{
	typedef TCUDAComponent inherited;
	
private:
	System::UnicodeString FKernelName;
	Gls_cuda_api::TCUfunction *FHandle;
	bool FAutoSync;
	Glscudacontext::TCUDADimensions* FBlockShape;
	Glscudacontext::TCUDADimensions* FGrid;
	int ParamOffset;
	bool FLaunching;
	System::Classes::TNotifyEvent FOnParameterSetup;
	void __fastcall SetBlockShape(Glscudacontext::TCUDADimensions* const AShape);
	void __fastcall SetGrid(Glscudacontext::TCUDADimensions* const AGrid);
	void __fastcall SetKernelName(const System::UnicodeString AName);
	Gls_cuda_api::PCUfunction __fastcall GetHandle(void);
	void __fastcall SetSharedMemorySize(int Value);
	int __fastcall GetSharedMemorySize(void);
	int __fastcall GetMaxThreadPerBlock(void);
	int __fastcall GetConstMemorySize(void);
	int __fastcall GetLocalMemorySize(void);
	int __fastcall GetNumRegisters(void);
	TCUDAFuncParam* __fastcall GetParameter(const System::UnicodeString AName);
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	virtual bool __fastcall GetIsAllocated(void);
	
public:
	__fastcall virtual TCUDAFunction(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAFunction(void);
	void __fastcall SetParam(int Value)/* overload */;
	void __fastcall SetParam(unsigned Value)/* overload */;
	void __fastcall SetParam(float Value)/* overload */;
	void __fastcall SetParam(const Vectortypes::TVector2i &Value)/* overload */;
	void __fastcall SetParam(const Vectortypes::TVector3i &Value)/* overload */;
	void __fastcall SetParam(const Vectortypes::TVector4i &Value)/* overload */;
	void __fastcall SetParam(const Vectortypes::TVector2f &Value)/* overload */;
	void __fastcall SetParam(const Vectortypes::TVector3f &Value)/* overload */;
	void __fastcall SetParam(const Vectortypes::TVector4f &Value)/* overload */;
	void __fastcall SetParam(TCUDAMemData* MemData)/* overload */;
	void __fastcall SetParam(TCUDATexture* TexRef)/* overload */;
	void __fastcall SetParam(void * Ptr)/* overload */;
	__property TCUDAFuncParam* Parameters[const System::UnicodeString AName] = {read=GetParameter};
	void __fastcall Launch(bool Grided = true);
	__property Gls_cuda_api::PCUfunction Handle = {read=GetHandle};
	__property int SharedMemorySize = {read=GetSharedMemorySize, write=SetSharedMemorySize, nodefault};
	__property int MaxThreadPerBlock = {read=GetMaxThreadPerBlock, nodefault};
	__property int ConstMemorySize = {read=GetConstMemorySize, nodefault};
	__property int LocalMemorySize = {read=GetLocalMemorySize, nodefault};
	__property int NumRegisters = {read=GetNumRegisters, nodefault};
	
__published:
	__property System::UnicodeString KernelName = {read=FKernelName, write=SetKernelName};
	__property bool AutoSync = {read=FAutoSync, write=FAutoSync, default=1};
	__property Glscudacontext::TCUDADimensions* BlockShape = {read=FBlockShape, write=SetBlockShape};
	__property Glscudacontext::TCUDADimensions* Grid = {read=FGrid, write=SetGrid};
	__property System::Classes::TNotifyEvent OnParameterSetup = {read=FOnParameterSetup, write=FOnParameterSetup};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDATexture : public TCUDAComponent
{
	typedef TCUDAComponent inherited;
	
private:
	System::UnicodeString FKernelName;
	Gls_cuda_api::TCUtexref *FHandle;
	TCUDAMemData* fArray;
	TCuAddresMode fAddressModeS;
	TCuAddresMode fAddressModeT;
	TCuAddresMode fAddressModeR;
	bool fNormalizedCoord;
	bool fReadAsInteger;
	TCuFilterMode fFilterMode;
	TCUDAChannelType fFormat;
	TCUDAChannelNum fChannelNum;
	void __fastcall SetKernelName(const System::UnicodeString AName);
	void __fastcall SetAddressModeS(const TCuAddresMode AMode);
	void __fastcall SetAddressModeT(const TCuAddresMode AMode);
	void __fastcall SetAddressModeR(const TCuAddresMode AMode);
	void __fastcall SetNormalizedCoord(const bool flag);
	void __fastcall SetReadAsInteger(const bool flag);
	void __fastcall SetFilterMode(const TCuFilterMode mode);
	void __fastcall SetFormat(TCUDAChannelType AValue);
	void __fastcall SetChannelNum(TCUDAChannelNum AValue);
	void __fastcall SetArray(TCUDAMemData* Value);
	Gls_cuda_api::PCUtexref __fastcall GetHandle(void);
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	virtual bool __fastcall GetIsAllocated(void);
	
public:
	__fastcall virtual TCUDATexture(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDATexture(void);
	__property Gls_cuda_api::PCUtexref Handle = {read=GetHandle};
	
__published:
	__property System::UnicodeString KernelName = {read=FKernelName, write=SetKernelName};
	__property TCuAddresMode AddressModeS = {read=fAddressModeS, write=SetAddressModeS, default=1};
	__property TCuAddresMode AddressModeT = {read=fAddressModeT, write=SetAddressModeT, default=1};
	__property TCuAddresMode AddressModeR = {read=fAddressModeR, write=SetAddressModeR, default=1};
	__property bool NormalizedCoord = {read=fNormalizedCoord, write=SetNormalizedCoord, default=1};
	__property bool ReadAsInteger = {read=fReadAsInteger, write=SetReadAsInteger, default=0};
	__property TCuFilterMode FilterMode = {read=fFilterMode, write=SetFilterMode, default=0};
	__property TCUDAChannelType Format = {read=fFormat, write=SetFormat, nodefault};
	__property TCUDAChannelNum ChannelNum = {read=fChannelNum, write=SetChannelNum, nodefault};
	__property TCUDAMemData* MemDataArray = {read=fArray, write=SetArray};
};

#pragma pack(pop)

class DELPHICLASS TGLSCUDA;
class PASCALIMPLEMENTATION TGLSCUDA : public TCUDAComponent
{
	typedef TCUDAComponent inherited;
	
private:
	Glscudacontext::TGLSCUDADevice* fDevice;
	Glscudacontext::TCUDAContext* fContext;
	Glscudacontext::TOnOpenGLInteropInit FOnOpenGLInteropInit;
	void __fastcall SetDevice(Glscudacontext::TGLSCUDADevice* const Value);
	void __fastcall SetOnOpenGLInteropInit(Glscudacontext::TOnOpenGLInteropInit AEvent);
	TCUDAModule* __fastcall GetModule(const int i);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual Glscudacontext::TCUDAContext* __fastcall GetContext(void);
	virtual bool __fastcall GetIsAllocated(void);
	
public:
	__fastcall virtual TGLSCUDA(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSCUDA(void);
	__property Glscudacontext::TCUDAContext* Context = {read=GetContext};
	__property TCUDAModule* Modules[const int i] = {read=GetModule};
	
__published:
	__property Glscudacontext::TGLSCUDADevice* ComputingDevice = {read=fDevice, write=SetDevice};
	__property Glscudacontext::TOnOpenGLInteropInit OnOpenGLInteropInit = {read=FOnOpenGLInteropInit, write=SetOnOpenGLInteropInit};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _cudasModuleAbsent;
#define Glscuda_cudasModuleAbsent System::LoadResourceString(&Glscuda::_cudasModuleAbsent)
extern PACKAGE System::ResourceString _cudasInvalidParamType;
#define Glscuda_cudasInvalidParamType System::LoadResourceString(&Glscuda::_cudasInvalidParamType)
extern PACKAGE System::ResourceString _cudasOnlyHostData;
#define Glscuda_cudasOnlyHostData System::LoadResourceString(&Glscuda::_cudasOnlyHostData)
extern PACKAGE System::ResourceString _cudasOutOfRange;
#define Glscuda_cudasOutOfRange System::LoadResourceString(&Glscuda::_cudasOutOfRange)
extern PACKAGE System::ResourceString _cudasInvalidValue;
#define Glscuda_cudasInvalidValue System::LoadResourceString(&Glscuda::_cudasInvalidValue)
extern PACKAGE System::ResourceString _cudasWrongParamSetup;
#define Glscuda_cudasWrongParamSetup System::LoadResourceString(&Glscuda::_cudasWrongParamSetup)
extern PACKAGE System::ResourceString _cudasLaunchFailed;
#define Glscuda_cudasLaunchFailed System::LoadResourceString(&Glscuda::_cudasLaunchFailed)
extern PACKAGE System::ResourceString _cudasFuncNotConnected;
#define Glscuda_cudasFuncNotConnected System::LoadResourceString(&Glscuda::_cudasFuncNotConnected)
extern PACKAGE System::ResourceString _cudasFailMap;
#define Glscuda_cudasFailMap System::LoadResourceString(&Glscuda::_cudasFailMap)
extern PACKAGE System::ResourceString _cudasFailUnmap;
#define Glscuda_cudasFailUnmap System::LoadResourceString(&Glscuda::_cudasFailUnmap)
extern PACKAGE TChannelTypeAndNum __fastcall GetChannelTypeAndNum(Glscudaparser::TCUDAType AType);
extern PACKAGE void __fastcall RegisterCUDAComponentNameChangeEvent(System::Classes::TNotifyEvent ANotifyEvent);
extern PACKAGE void __fastcall DeRegisterCUDAComponentNameChangeEvent(void);
}	/* namespace Glscuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCUDA)
using namespace Glscuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscudaHPP

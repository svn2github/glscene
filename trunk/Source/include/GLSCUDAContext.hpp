// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSCUDAContext.pas' rev: 24.00 (Win32)

#ifndef GlscudacontextHPP
#define GlscudacontextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLS_CUDA_API.hpp>	// Pascal unit
#include <GLS_CUDA_Runtime.hpp>	// Pascal unit
#include <GLS_CL_Platform.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLSGenerics.hpp>	// Pascal unit
#include <System.SyncObjs.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscudacontext
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCUDADimensions;
class PASCALIMPLEMENTATION TCUDADimensions : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Gls_cuda_api::TDim3 FXYZ;
	Gls_cuda_api::TDim3 FMaxXYZ;
	bool FReadOnly;
	int __fastcall GetDimComponent(int index);
	void __fastcall SetDimComponent(int index, int Value);
	int __fastcall GetMaxDimComponent(int index);
	void __fastcall SetMaxDimComponent(int index, int Value);
	
public:
	__fastcall virtual TCUDADimensions(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property int MaxSizeX = {read=GetMaxDimComponent, write=SetMaxDimComponent, index=0, nodefault};
	__property int MaxSizeY = {read=GetMaxDimComponent, write=SetMaxDimComponent, index=1, nodefault};
	__property int MaxSizeZ = {read=GetMaxDimComponent, write=SetMaxDimComponent, index=2, nodefault};
	__property bool ReadOnlyValue = {read=FReadOnly, write=FReadOnly, nodefault};
	
__published:
	__property int SizeX = {read=GetDimComponent, write=SetDimComponent, index=0, default=1};
	__property int SizeY = {read=GetDimComponent, write=SetDimComponent, index=1, default=1};
	__property int SizeZ = {read=GetDimComponent, write=SetDimComponent, index=2, default=1};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCUDADimensions(void) { }
	
};


typedef void __fastcall (__closure *TOnOpenGLInteropInit)(/* out */ Glcontext::TGLContext* &Context);

class DELPHICLASS TCUDADevice;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDADevice : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int fID;
	int fHandle;
	int fGFlops;
	Gls_cuda_runtime::TCudaDeviceProp fDeviceProperties;
	bool FSuitable;
	bool FUsed;
	TCUDADimensions* fMaxThreadsDim;
	TCUDADimensions* fMaxGridSize;
	
protected:
	System::UnicodeString __fastcall GetName(void);
	
public:
	__fastcall TCUDADevice(void);
	__fastcall virtual ~TCUDADevice(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	unsigned __fastcall TotalMemory(void);
	
__published:
	__property System::UnicodeString Name = {read=GetName};
	__property NativeUInt TotalGlobalMem = {read=fDeviceProperties.totalGlobalMem, nodefault};
	__property NativeUInt SharedMemPerBlock = {read=fDeviceProperties.sharedMemPerBlock, nodefault};
	__property int RegsPerBlock = {read=fDeviceProperties.regsPerBlock, nodefault};
	__property int WarpSize = {read=fDeviceProperties.warpSize, nodefault};
	__property NativeUInt MemPitch = {read=fDeviceProperties.memPitch, nodefault};
	__property int MaxThreadsPerBlock = {read=fDeviceProperties.maxThreadsPerBlock, nodefault};
	__property TCUDADimensions* MaxThreadsDim = {read=fMaxThreadsDim};
	__property TCUDADimensions* MaxGridSize = {read=fMaxGridSize};
	__property int ClockRate = {read=fDeviceProperties.clockRate, nodefault};
	__property NativeUInt TotalConstMem = {read=fDeviceProperties.totalConstMem, nodefault};
	__property int Major = {read=fDeviceProperties.major, nodefault};
	__property int Minor = {read=fDeviceProperties.minor, nodefault};
	__property NativeUInt TextureAlignment = {read=fDeviceProperties.textureAlignment, nodefault};
	__property int DeviceOverlap = {read=fDeviceProperties.deviceOverlap, nodefault};
	__property int MultiProcessorCount = {read=fDeviceProperties.multiProcessorCount, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TGLSCUDADevice;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSCUDADevice : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FSelectDeviceName;
	TCUDADevice* __fastcall GetDevice(void);
	void __fastcall SetDevice(TCUDADevice* AValue);
	void __fastcall SetDeviceName(const System::UnicodeString AName);
	
public:
	__fastcall virtual TGLSCUDADevice(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSCUDADevice(void);
	bool __fastcall Suitable(void);
	
__published:
	__property System::UnicodeString SelectDevice = {read=FSelectDeviceName, write=SetDeviceName};
	__property TCUDADevice* Device = {read=GetDevice, write=SetDevice};
};

#pragma pack(pop)

class DELPHICLASS TCUDAHandlesMaster;
class DELPHICLASS TCUDAContext;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAHandlesMaster : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	virtual TCUDAContext* __fastcall GetContext(void) = 0 ;
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
public:
	/* TComponent.Create */ inline __fastcall virtual TCUDAHandlesMaster(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TCUDAHandlesMaster(void) { }
	
};

#pragma pack(pop)

typedef Glsgenerics::GThreadList__1<TCUDAHandlesMaster*>* TCUDAHandleList;

class PASCALIMPLEMENTATION TCUDAContext : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Gls_cuda_api::TCUcontext *fHandle;
	TCUDADevice* FDevice;
	TOnOpenGLInteropInit FOnOpenGLInteropInit;
	Glsgenerics::GThreadList__1<TCUDAHandlesMaster*>* FHandleList;
	void __fastcall SetDevice(TCUDADevice* ADevice);
	
public:
	__fastcall TCUDAContext(void);
	__fastcall virtual ~TCUDAContext(void);
	void __fastcall DestroyAllHandles(void);
	void __fastcall Requires(void);
	void __fastcall Release(void);
	bool __fastcall IsValid(void);
	__property TCUDADevice* Device = {read=FDevice, write=SetDevice};
	__property TOnOpenGLInteropInit OnOpenGLInteropInit = {read=FOnOpenGLInteropInit, write=FOnOpenGLInteropInit};
};


typedef Glsgenerics::GList__1<TCUDADevice*>* TCUDADeviceList;

typedef Glsgenerics::GList__1<TCUDAContext*>* TCUDAContextList;

class DELPHICLASS CUDAContextManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION CUDAContextManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<Glsgenerics::GList__1<TCUDAContext*>*> _CUDAContextManager__1;
	
	
private:
	static Glsgenerics::GList__1<TCUDADevice*>* fDeviceList;
	static Glsgenerics::GList__1<TCUDAContext*>* fContextList;
	static _CUDAContextManager__1 FContextStacks;
	
protected:
	__classmethod TCUDADevice* __fastcall GetDevice(int i);
	__classmethod TCUDADevice* __fastcall GetNextUnusedDevice();
	__classmethod void __fastcall RegisterContext(TCUDAContext* aContext);
	__classmethod void __fastcall UnRegisterContext(TCUDAContext* aContext);
	__classmethod Glsgenerics::GList__1<TCUDAContext*>* __fastcall GetThreadStack();
	__classmethod TCUDAContext* __fastcall GetContext(int i);
	
public:
	__classmethod void __fastcall Init();
	__classmethod void __fastcall Done();
	__classmethod void __fastcall CreateContext(TCUDAContext* aContext);
	__classmethod void __fastcall DestroyContext(TCUDAContext* aContext);
	__classmethod void __fastcall CreateContextOf(TCUDADevice* ADevice);
	__classmethod void __fastcall DestroyContextOf(TCUDADevice* ADevice);
	__classmethod void __fastcall PushContext(TCUDAContext* aContext);
	__classmethod TCUDAContext* __fastcall PopContext();
	__classmethod void __fastcall FillUnusedDeviceList(System::Classes::TStringList* &AList);
	__classmethod TCUDADevice* __fastcall GetDeviceByName(const System::UnicodeString AName);
	__classmethod int __fastcall DeviceCount();
	__property TCUDADevice* Devices[int i] = {read=GetDevice};
	__classmethod TCUDADevice* __fastcall GetMaxGflopsDevice();
	__classmethod int __fastcall ContextCount();
	__classmethod TCUDAContext* __fastcall GetCurrentThreadContext();
	__property TCUDAContext* Contexts[int i] = {read=GetContext};
public:
	/* TObject.Create */ inline __fastcall CUDAContextManager(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~CUDAContextManager(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glscudacontext */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCUDACONTEXT)
using namespace Glscudacontext;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscudacontextHPP

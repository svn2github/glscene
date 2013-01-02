// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLContext.pas' rev: 24.00 (Win32)

#ifndef GlcontextHPP
#define GlcontextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <Vcl.Consts.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <System.SyncObjs.hpp>	// Pascal unit
#include <GLSGenerics.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLPipelineTransformation.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <GLSLog.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcontext
{
//-- type declarations -------------------------------------------------------
enum TGLRCOption : unsigned char { rcoDoubleBuffered, rcoStereo, rcoDebug, rcoOGL_ES };

typedef System::Set<TGLRCOption, TGLRCOption::rcoDoubleBuffered, TGLRCOption::rcoOGL_ES>  TGLRCOptions;

enum TGLContextLayer : unsigned char { clUnderlay2, clUnderlay1, clMainPlane, clOverlay1, clOverlay2 };

class DELPHICLASS TFinishTaskEvent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFinishTaskEvent : public System::Syncobjs::TEvent
{
	typedef System::Syncobjs::TEvent inherited;
	
public:
	__fastcall TFinishTaskEvent(void);
public:
	/* THandleObject.Destroy */ inline __fastcall virtual ~TFinishTaskEvent(void) { }
	
};

#pragma pack(pop)

typedef void __stdcall (__closure *TTaskProcedure)(void);

struct DECLSPEC_DRECORD TServiceContextTask
{
public:
	TTaskProcedure Task;
	TFinishTaskEvent* Event;
};


typedef Glsgenerics::GThreadList__1<TServiceContextTask>* TServiceContextTaskList;

class DELPHICLASS TAbstractMultitextureCoordinator;
class DELPHICLASS TGLContext;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAbstractMultitextureCoordinator : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TGLContext* FOwner;
	
public:
	__fastcall virtual TAbstractMultitextureCoordinator(TGLContext* AOwner);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TAbstractMultitextureCoordinator(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TAbstractMultitextureCoordinatorClass;

enum TGLContextAcceleration : unsigned char { chaUnknown, chaHardware, chaSoftware };

enum TGLAntiAliasing : unsigned char { aaDefault, aaNone, aa2x, aa2xHQ, aa4x, aa4xHQ, aa6x, aa8x, aa16x, csa8x, csa8xHQ, csa16x, csa16xHQ };

enum TVSyncMode : unsigned char { vsmSync, vsmNoSync };

class DELPHICLASS TGLContextManager;
class PASCALIMPLEMENTATION TGLContext : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FColorBits;
	int FAlphaBits;
	int FDepthBits;
	int FStencilBits;
	int FAccumBits;
	int FAuxBuffers;
	TGLAntiAliasing FAntiAliasing;
	TGLRCOptions FOptions;
	System::Classes::TNotifyEvent FOnDestroyContext;
	TGLContextManager* FManager;
	int FActivationCount;
	int FOwnedHandlesCount;
	bool FIsPraparationNeed;
	void __fastcall SetColorBits(const int aColorBits);
	void __fastcall SetAlphaBits(const int aAlphaBits);
	void __fastcall SetDepthBits(const int val);
	void __fastcall SetStencilBits(const int aStencilBits);
	void __fastcall SetAccumBits(const int aAccumBits);
	void __fastcall SetAuxBuffers(const int aAuxBuffers);
	void __fastcall SetOptions(const TGLRCOptions aOptions);
	void __fastcall SetAntiAliasing(const TGLAntiAliasing val);
	void __fastcall SetAcceleration(const TGLContextAcceleration val);
	bool __fastcall GetActive(void);
	void __fastcall SetActive(const bool aActive);
	void __fastcall SetLayer(const TGLContextLayer Value);
	
protected:
	Opengladapter::TGLExtensionsAndEntryPoints* FGL;
	TAbstractMultitextureCoordinator* FXGL;
	Glstate::TGLStateCache* FGLStates;
	Glpipelinetransformation::TGLTransformation* FTransformation;
	TGLContextAcceleration FAcceleration;
	TGLContextLayer FLayer;
	System::Classes::TThreadList* FSharedContexts;
	System::Syncobjs::TCriticalSection* FLock;
	void __fastcall PropagateSharedContext(void);
	virtual void __fastcall DoCreateContext(HDC ADeviceHandle) = 0 ;
	virtual void __fastcall DoCreateMemoryContext(HWND outputDevice, int width, int height, int BufferCount = 0x1) = 0 ;
	virtual bool __fastcall DoShareLists(TGLContext* aContext) = 0 ;
	virtual void __fastcall DoDestroyContext(void) = 0 ;
	virtual void __fastcall DoActivate(void) = 0 ;
	virtual void __fastcall DoDeactivate(void) = 0 ;
	__classmethod TGLContext* __fastcall ServiceContext();
	void __fastcall MakeGLCurrent(void);
	TAbstractMultitextureCoordinator* __fastcall GetXGL(void);
	
public:
	__fastcall virtual TGLContext(void);
	__fastcall virtual ~TGLContext(void);
	__property Glstate::TGLStateCache* GLStates = {read=FGLStates};
	__property Glpipelinetransformation::TGLTransformation* PipelineTransformation = {read=FTransformation};
	__property TGLContextManager* Manager = {read=FManager};
	__property int ColorBits = {read=FColorBits, write=SetColorBits, nodefault};
	__property int AlphaBits = {read=FAlphaBits, write=SetAlphaBits, nodefault};
	__property int DepthBits = {read=FDepthBits, write=SetDepthBits, nodefault};
	__property int StencilBits = {read=FStencilBits, write=SetStencilBits, nodefault};
	__property int AccumBits = {read=FAccumBits, write=SetAccumBits, nodefault};
	__property int AuxBuffers = {read=FAuxBuffers, write=SetAuxBuffers, nodefault};
	__property TGLAntiAliasing AntiAliasing = {read=FAntiAliasing, write=SetAntiAliasing, nodefault};
	__property TGLContextLayer Layer = {read=FLayer, write=SetLayer, nodefault};
	__property TGLRCOptions Options = {read=FOptions, write=SetOptions, nodefault};
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	__property TGLContextAcceleration Acceleration = {read=FAcceleration, write=SetAcceleration, nodefault};
	__property System::Classes::TNotifyEvent OnDestroyContext = {read=FOnDestroyContext, write=FOnDestroyContext};
	void __fastcall CreateContext(HDC ADeviceHandle)/* overload */;
	void __fastcall CreateMemoryContext(HWND outputDevice, int width, int height, int BufferCount = 0x1);
	void __fastcall ShareLists(TGLContext* aContext);
	void __fastcall DestroyContext(void);
	void __fastcall Activate(void);
	void __fastcall Deactivate(void);
	void __fastcall PrepareHandlesData(void);
	virtual bool __fastcall IsValid(void) = 0 ;
	virtual void __fastcall SwapBuffers(void) = 0 ;
	TGLContext* __fastcall FindCompatibleContext(void);
	void __fastcall DestroyAllHandles(void);
	virtual void * __fastcall RenderOutputDevice(void) = 0 ;
	__property Opengladapter::TGLExtensionsAndEntryPoints* GL = {read=FGL};
	__property TAbstractMultitextureCoordinator* MultitextureCoordinator = {read=GetXGL};
	__property bool IsPraparationNeed = {read=FIsPraparationNeed, nodefault};
};


typedef System::TMetaClass* TGLContextClass;

class DELPHICLASS TGLScreenControlingContext;
class PASCALIMPLEMENTATION TGLScreenControlingContext : public TGLContext
{
	typedef TGLContext inherited;
	
private:
	int FWidth;
	int FHeight;
	bool FFullScreen;
	
public:
	__property int Width = {read=FWidth, write=FWidth, nodefault};
	__property int Height = {read=FHeight, write=FHeight, nodefault};
	__property bool FullScreen = {read=FFullScreen, write=FFullScreen, nodefault};
public:
	/* TGLContext.Create */ inline __fastcall virtual TGLScreenControlingContext(void) : TGLContext() { }
	/* TGLContext.Destroy */ inline __fastcall virtual ~TGLScreenControlingContext(void) { }
	
};


struct TGLRCHandle;
typedef TGLRCHandle *PGLRCHandle;

struct DECLSPEC_DRECORD TGLRCHandle
{
public:
	TGLContext* FRenderingContext;
	unsigned FHandle;
	bool FChanged;
};


typedef void __fastcall (__closure *TOnPrepareHandleData)(TGLContext* AContext);

class DELPHICLASS TGLContextHandle;
class PASCALIMPLEMENTATION TGLContextHandle : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TList* FHandles;
	TGLRCHandle *FLastHandle;
	TOnPrepareHandleData FOnPrepare;
	unsigned __fastcall GetHandle(void);
	TGLContext* __fastcall GetContext(void);
	PGLRCHandle __fastcall SearchRC(TGLContext* AContext);
	PGLRCHandle __fastcall RCItem(int AIndex);
	void __fastcall CheckCurrentRC(void);
	
protected:
	void __fastcall ContextDestroying(void);
	__classmethod virtual bool __fastcall Transferable();
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	virtual unsigned __fastcall DoAllocateHandle(void) = 0 ;
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle) = 0 ;
	
public:
	__fastcall virtual TGLContextHandle(void);
	__fastcall TGLContextHandle(bool failIfAllocationFailed);
	__fastcall virtual ~TGLContextHandle(void);
	__property unsigned Handle = {read=GetHandle, nodefault};
	__property TGLContext* RenderingContext = {read=GetContext};
	bool __fastcall IsDataNeedUpdate(void);
	bool __fastcall IsDataComplitelyUpdated(void);
	void __fastcall NotifyDataUpdated(void);
	void __fastcall NotifyChangesOfData(void);
	__classmethod virtual bool __fastcall IsSupported();
	bool __fastcall IsAllocatedForContext(TGLContext* AContext = (TGLContext*)(0x0));
	bool __fastcall IsShared(void);
	unsigned __fastcall AllocateHandle(void);
	void __fastcall DestroyHandle(void);
	__property TOnPrepareHandleData OnPrapare = {read=FOnPrepare, write=FOnPrepare};
};


class DELPHICLASS TGLVirtualHandle;
typedef void __fastcall (__closure *TGLVirtualHandleEvent)(TGLVirtualHandle* Sender, unsigned &handle);

class PASCALIMPLEMENTATION TGLVirtualHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
private:
	TGLVirtualHandleEvent FOnAllocate;
	TGLVirtualHandleEvent FOnDestroy;
	int FTag;
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	__classmethod virtual bool __fastcall Transferable();
	
public:
	__property TGLVirtualHandleEvent OnAllocate = {read=FOnAllocate, write=FOnAllocate};
	__property TGLVirtualHandleEvent OnDestroy = {read=FOnDestroy, write=FOnDestroy};
	__property int Tag = {read=FTag, write=FTag, nodefault};
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLVirtualHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLVirtualHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLVirtualHandle(void) { }
	
};


class DELPHICLASS TGLVirtualHandleTransf;
class PASCALIMPLEMENTATION TGLVirtualHandleTransf : public TGLVirtualHandle
{
	typedef TGLVirtualHandle inherited;
	
protected:
	__classmethod virtual bool __fastcall Transferable();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLVirtualHandleTransf(void) : TGLVirtualHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLVirtualHandleTransf(bool failIfAllocationFailed) : TGLVirtualHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLVirtualHandleTransf(void) { }
	
};


class DELPHICLASS TGLListHandle;
class PASCALIMPLEMENTATION TGLListHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	void __fastcall NewList(unsigned mode);
	void __fastcall EndList(void);
	void __fastcall CallList(void);
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLListHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLListHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLListHandle(void) { }
	
};


class DELPHICLASS TGLTextureHandle;
class PASCALIMPLEMENTATION TGLTextureHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
private:
	Gltextureformat::TGLTextureTarget FTarget;
	void __fastcall SetTarget(Gltextureformat::TGLTextureTarget ATarget);
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	__property Gltextureformat::TGLTextureTarget Target = {read=FTarget, write=SetTarget, nodefault};
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLTextureHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLTextureHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLTextureHandle(void) { }
	
};


class DELPHICLASS TGLSamplerHandle;
class PASCALIMPLEMENTATION TGLSamplerHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLSamplerHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLSamplerHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLSamplerHandle(void) { }
	
};


class DELPHICLASS TGLQueryHandle;
class PASCALIMPLEMENTATION TGLQueryHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
private:
	bool FActive;
	
protected:
	__classmethod virtual bool __fastcall Transferable();
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	virtual unsigned __fastcall GetTarget(void) = 0 ;
	virtual Glstate::TQueryType __fastcall GetQueryType(void) = 0 ;
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	void __fastcall BeginQuery(void);
	void __fastcall EndQuery(void);
	bool __fastcall IsResultAvailable(void);
	int __fastcall CounterBits(void);
	int __fastcall QueryResultInt(void);
	unsigned __fastcall QueryResultUInt(void);
	__int64 __fastcall QueryResultInt64(void);
	unsigned __int64 __fastcall QueryResultUInt64(void);
	System::ByteBool __fastcall QueryResultBool(void);
	__property unsigned Target = {read=GetTarget, nodefault};
	__property Glstate::TQueryType QueryType = {read=GetQueryType, nodefault};
	__property bool Active = {read=FActive, nodefault};
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLQueryHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLQueryHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLQueryHandle(void) { }
	
};


class DELPHICLASS TGLOcclusionQueryHandle;
class PASCALIMPLEMENTATION TGLOcclusionQueryHandle : public TGLQueryHandle
{
	typedef TGLQueryHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	virtual Glstate::TQueryType __fastcall GetQueryType(void);
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	int __fastcall PixelCount(void);
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLOcclusionQueryHandle(void) : TGLQueryHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLOcclusionQueryHandle(bool failIfAllocationFailed) : TGLQueryHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLOcclusionQueryHandle(void) { }
	
};


class DELPHICLASS TGLBooleanOcclusionQueryHandle;
class PASCALIMPLEMENTATION TGLBooleanOcclusionQueryHandle : public TGLQueryHandle
{
	typedef TGLQueryHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	virtual Glstate::TQueryType __fastcall GetQueryType(void);
	
public:
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLBooleanOcclusionQueryHandle(void) : TGLQueryHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLBooleanOcclusionQueryHandle(bool failIfAllocationFailed) : TGLQueryHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLBooleanOcclusionQueryHandle(void) { }
	
};


class DELPHICLASS TGLTimerQueryHandle;
class PASCALIMPLEMENTATION TGLTimerQueryHandle : public TGLQueryHandle
{
	typedef TGLQueryHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	virtual Glstate::TQueryType __fastcall GetQueryType(void);
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	int __fastcall Time(void);
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLTimerQueryHandle(void) : TGLQueryHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLTimerQueryHandle(bool failIfAllocationFailed) : TGLQueryHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLTimerQueryHandle(void) { }
	
};


class DELPHICLASS TGLPrimitiveQueryHandle;
class PASCALIMPLEMENTATION TGLPrimitiveQueryHandle : public TGLQueryHandle
{
	typedef TGLQueryHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	virtual Glstate::TQueryType __fastcall GetQueryType(void);
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	int __fastcall PrimitivesGenerated(void);
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLPrimitiveQueryHandle(void) : TGLQueryHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLPrimitiveQueryHandle(bool failIfAllocationFailed) : TGLQueryHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLPrimitiveQueryHandle(void) { }
	
};


class DELPHICLASS TGLBufferObjectHandle;
class PASCALIMPLEMENTATION TGLBufferObjectHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
private:
	int FSize;
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	virtual unsigned __fastcall GetTarget(void) = 0 ;
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	__fastcall TGLBufferObjectHandle(void * p, int size, unsigned bufferUsage);
	virtual void __fastcall Bind(void) = 0 ;
	virtual void __fastcall UnBind(void) = 0 ;
	virtual void __fastcall BindRange(unsigned index, NativeInt offset, NativeInt size);
	virtual void __fastcall BindBase(unsigned index);
	virtual void __fastcall UnBindBase(unsigned index);
	void __fastcall BufferData(void * p, int size, unsigned bufferUsage);
	void __fastcall BindBufferData(void * p, int size, unsigned bufferUsage);
	void __fastcall BufferSubData(int offset, int size, void * p);
	void * __fastcall MapBuffer(unsigned access);
	void * __fastcall MapBufferRange(int offset, int len, unsigned access);
	void __fastcall Flush(int offset, int len);
	bool __fastcall UnmapBuffer(void);
	__classmethod virtual bool __fastcall IsSupported();
	__property unsigned Target = {read=GetTarget, nodefault};
	__property int BufferSize = {read=FSize, nodefault};
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLBufferObjectHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLBufferObjectHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLBufferObjectHandle(void) { }
	
};


class DELPHICLASS TGLVBOHandle;
class PASCALIMPLEMENTATION TGLVBOHandle : public TGLBufferObjectHandle
{
	typedef TGLBufferObjectHandle inherited;
	
private:
	unsigned __fastcall GetVBOTarget(void);
	
public:
	__property unsigned VBOTarget = {read=GetVBOTarget, nodefault};
public:
	/* TGLBufferObjectHandle.CreateFromData */ inline __fastcall TGLVBOHandle(void * p, int size, unsigned bufferUsage) : TGLBufferObjectHandle(p, size, bufferUsage) { }
	
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLVBOHandle(void) : TGLBufferObjectHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLVBOHandle(bool failIfAllocationFailed) : TGLBufferObjectHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLVBOHandle(void) { }
	
};


class DELPHICLASS TGLVBOArrayBufferHandle;
class PASCALIMPLEMENTATION TGLVBOArrayBufferHandle : public TGLVBOHandle
{
	typedef TGLVBOHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	
public:
	virtual void __fastcall Bind(void);
	virtual void __fastcall UnBind(void);
public:
	/* TGLBufferObjectHandle.CreateFromData */ inline __fastcall TGLVBOArrayBufferHandle(void * p, int size, unsigned bufferUsage) : TGLVBOHandle(p, size, bufferUsage) { }
	
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLVBOArrayBufferHandle(void) : TGLVBOHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLVBOArrayBufferHandle(bool failIfAllocationFailed) : TGLVBOHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLVBOArrayBufferHandle(void) { }
	
};


class DELPHICLASS TGLVBOElementArrayHandle;
class PASCALIMPLEMENTATION TGLVBOElementArrayHandle : public TGLVBOHandle
{
	typedef TGLVBOHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	
public:
	virtual void __fastcall Bind(void);
	virtual void __fastcall UnBind(void);
public:
	/* TGLBufferObjectHandle.CreateFromData */ inline __fastcall TGLVBOElementArrayHandle(void * p, int size, unsigned bufferUsage) : TGLVBOHandle(p, size, bufferUsage) { }
	
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLVBOElementArrayHandle(void) : TGLVBOHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLVBOElementArrayHandle(bool failIfAllocationFailed) : TGLVBOHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLVBOElementArrayHandle(void) { }
	
};


class DELPHICLASS TGLPackPBOHandle;
class PASCALIMPLEMENTATION TGLPackPBOHandle : public TGLBufferObjectHandle
{
	typedef TGLBufferObjectHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	
public:
	virtual void __fastcall Bind(void);
	virtual void __fastcall UnBind(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLBufferObjectHandle.CreateFromData */ inline __fastcall TGLPackPBOHandle(void * p, int size, unsigned bufferUsage) : TGLBufferObjectHandle(p, size, bufferUsage) { }
	
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLPackPBOHandle(void) : TGLBufferObjectHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLPackPBOHandle(bool failIfAllocationFailed) : TGLBufferObjectHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLPackPBOHandle(void) { }
	
};


class DELPHICLASS TGLUnpackPBOHandle;
class PASCALIMPLEMENTATION TGLUnpackPBOHandle : public TGLBufferObjectHandle
{
	typedef TGLBufferObjectHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	
public:
	virtual void __fastcall Bind(void);
	virtual void __fastcall UnBind(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLBufferObjectHandle.CreateFromData */ inline __fastcall TGLUnpackPBOHandle(void * p, int size, unsigned bufferUsage) : TGLBufferObjectHandle(p, size, bufferUsage) { }
	
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLUnpackPBOHandle(void) : TGLBufferObjectHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLUnpackPBOHandle(bool failIfAllocationFailed) : TGLBufferObjectHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLUnpackPBOHandle(void) { }
	
};


class DELPHICLASS TGLTransformFeedbackBufferHandle;
class PASCALIMPLEMENTATION TGLTransformFeedbackBufferHandle : public TGLBufferObjectHandle
{
	typedef TGLBufferObjectHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	
public:
	virtual void __fastcall Bind(void);
	virtual void __fastcall UnBind(void);
	void __fastcall BeginTransformFeedback(unsigned primitiveMode);
	void __fastcall EndTransformFeedback(void);
	virtual void __fastcall BindRange(unsigned index, NativeInt offset, NativeInt size);
	virtual void __fastcall BindBase(unsigned index);
	virtual void __fastcall UnBindBase(unsigned index);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLBufferObjectHandle.CreateFromData */ inline __fastcall TGLTransformFeedbackBufferHandle(void * p, int size, unsigned bufferUsage) : TGLBufferObjectHandle(p, size, bufferUsage) { }
	
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLTransformFeedbackBufferHandle(void) : TGLBufferObjectHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLTransformFeedbackBufferHandle(bool failIfAllocationFailed) : TGLBufferObjectHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLTransformFeedbackBufferHandle(void) { }
	
};


class DELPHICLASS TGLTextureBufferHandle;
class PASCALIMPLEMENTATION TGLTextureBufferHandle : public TGLBufferObjectHandle
{
	typedef TGLBufferObjectHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	
public:
	virtual void __fastcall Bind(void);
	virtual void __fastcall UnBind(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLBufferObjectHandle.CreateFromData */ inline __fastcall TGLTextureBufferHandle(void * p, int size, unsigned bufferUsage) : TGLBufferObjectHandle(p, size, bufferUsage) { }
	
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLTextureBufferHandle(void) : TGLBufferObjectHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLTextureBufferHandle(bool failIfAllocationFailed) : TGLBufferObjectHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLTextureBufferHandle(void) { }
	
};


class DELPHICLASS TGLUniformBufferHandle;
class PASCALIMPLEMENTATION TGLUniformBufferHandle : public TGLBufferObjectHandle
{
	typedef TGLBufferObjectHandle inherited;
	
protected:
	virtual unsigned __fastcall GetTarget(void);
	
public:
	virtual void __fastcall Bind(void);
	virtual void __fastcall UnBind(void);
	virtual void __fastcall BindRange(unsigned index, NativeInt offset, NativeInt size);
	virtual void __fastcall BindBase(unsigned index);
	virtual void __fastcall UnBindBase(unsigned index);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLBufferObjectHandle.CreateFromData */ inline __fastcall TGLUniformBufferHandle(void * p, int size, unsigned bufferUsage) : TGLBufferObjectHandle(p, size, bufferUsage) { }
	
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLUniformBufferHandle(void) : TGLBufferObjectHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLUniformBufferHandle(bool failIfAllocationFailed) : TGLBufferObjectHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLUniformBufferHandle(void) { }
	
};


class DELPHICLASS TGLVertexArrayHandle;
class PASCALIMPLEMENTATION TGLVertexArrayHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
protected:
	__classmethod virtual bool __fastcall Transferable();
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	void __fastcall Bind(void);
	void __fastcall UnBind(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLVertexArrayHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLVertexArrayHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLVertexArrayHandle(void) { }
	
};


enum TGLFramebufferStatus : unsigned char { fsComplete, fsIncompleteAttachment, fsIncompleteMissingAttachment, fsIncompleteDuplicateAttachment, fsIncompleteDimensions, fsIncompleteFormats, fsIncompleteDrawBuffer, fsIncompleteReadBuffer, fsUnsupported, fsIncompleteMultisample, fsStatusError };

class DELPHICLASS TGLFramebufferHandle;
class PASCALIMPLEMENTATION TGLFramebufferHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
protected:
	__classmethod virtual bool __fastcall Transferable();
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	void __fastcall Bind(void);
	void __fastcall BindForDrawing(void);
	void __fastcall BindForReading(void);
	void __fastcall UnBind(void);
	void __fastcall UnBindForDrawing(void);
	void __fastcall UnBindForReading(void);
	void __fastcall Attach1DTexture(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level);
	void __fastcall Attach2DTexture(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level);
	void __fastcall Attach3DTexture(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level, int layer);
	void __fastcall AttachLayer(unsigned target, unsigned attachment, unsigned texture, int level, int layer);
	void __fastcall AttachRenderBuffer(unsigned target, unsigned attachment, unsigned renderbuffertarget, unsigned renderbuffer);
	void __fastcall AttachTexture(unsigned target, unsigned attachment, unsigned texture, int level);
	void __fastcall AttachTextureLayer(unsigned target, unsigned attachment, unsigned texture, int level, int layer);
	void __fastcall Blit(int srcX0, int srcY0, int srcX1, int srcY1, int dstX0, int dstY0, int dstX1, int dstY1, unsigned mask, unsigned filter);
	int __fastcall GetAttachmentParameter(unsigned target, unsigned attachment, unsigned pname);
	int __fastcall GetAttachmentObjectType(unsigned target, unsigned attachment);
	int __fastcall GetAttachmentObjectName(unsigned target, unsigned attachment);
	TGLFramebufferStatus __fastcall GetStatus(void);
	TGLFramebufferStatus __fastcall GetStringStatus(/* out */ System::UnicodeString &clarification);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLFramebufferHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLFramebufferHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLFramebufferHandle(void) { }
	
};


class DELPHICLASS TGLRenderbufferHandle;
class PASCALIMPLEMENTATION TGLRenderbufferHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	void __fastcall Bind(void);
	void __fastcall UnBind(void);
	void __fastcall SetStorage(unsigned internalformat, int width, int height);
	void __fastcall SetStorageMultisample(unsigned internalformat, int samples, int width, int height);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLRenderbufferHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLRenderbufferHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLRenderbufferHandle(void) { }
	
};


class DELPHICLASS TGLARBProgramHandle;
class PASCALIMPLEMENTATION TGLARBProgramHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
private:
	bool FReady;
	System::UnicodeString FInfoLog;
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	virtual __classmethod unsigned __fastcall GetTarget() = 0 ;
	
public:
	void __fastcall LoadARBProgram(System::UnicodeString AText);
	void __fastcall Enable(void);
	void __fastcall Disable(void);
	void __fastcall Bind(void);
	__property bool Ready = {read=FReady, nodefault};
	__property System::UnicodeString InfoLog = {read=FInfoLog};
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLARBProgramHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLARBProgramHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLARBProgramHandle(void) { }
	
};


class DELPHICLASS TGLARBVertexProgramHandle;
class PASCALIMPLEMENTATION TGLARBVertexProgramHandle : public TGLARBProgramHandle
{
	typedef TGLARBProgramHandle inherited;
	
protected:
	__classmethod virtual unsigned __fastcall GetTarget();
	
public:
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLARBVertexProgramHandle(void) : TGLARBProgramHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLARBVertexProgramHandle(bool failIfAllocationFailed) : TGLARBProgramHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLARBVertexProgramHandle(void) { }
	
};


class DELPHICLASS TGLARBFragmentProgramHandle;
class PASCALIMPLEMENTATION TGLARBFragmentProgramHandle : public TGLARBProgramHandle
{
	typedef TGLARBProgramHandle inherited;
	
protected:
	__classmethod virtual unsigned __fastcall GetTarget();
	
public:
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLARBFragmentProgramHandle(void) : TGLARBProgramHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLARBFragmentProgramHandle(bool failIfAllocationFailed) : TGLARBProgramHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLARBFragmentProgramHandle(void) { }
	
};


class DELPHICLASS TGLARBGeometryProgramHandle;
class PASCALIMPLEMENTATION TGLARBGeometryProgramHandle : public TGLARBProgramHandle
{
	typedef TGLARBProgramHandle inherited;
	
protected:
	__classmethod virtual unsigned __fastcall GetTarget();
	
public:
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLARBGeometryProgramHandle(void) : TGLARBProgramHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLARBGeometryProgramHandle(bool failIfAllocationFailed) : TGLARBProgramHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLARBGeometryProgramHandle(void) { }
	
};


class DELPHICLASS TGLSLHandle;
class PASCALIMPLEMENTATION TGLSLHandle : public TGLContextHandle
{
	typedef TGLContextHandle inherited;
	
protected:
	virtual void __fastcall DoDestroyHandle(unsigned &AHandle);
	
public:
	System::UnicodeString __fastcall InfoLog(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLSLHandle(void) : TGLContextHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLSLHandle(bool failIfAllocationFailed) : TGLContextHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLSLHandle(void) { }
	
};


class DELPHICLASS TGLShaderHandle;
class PASCALIMPLEMENTATION TGLShaderHandle : public TGLSLHandle
{
	typedef TGLSLHandle inherited;
	
private:
	unsigned FShaderType;
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
public:
	void __fastcall ShaderSource(const System::AnsiString source)/* overload */;
	bool __fastcall CompileShader(void);
	__property unsigned ShaderType = {read=FShaderType, nodefault};
public:
	/* TGLContextHandle.Create */ inline __fastcall virtual TGLShaderHandle(void) : TGLSLHandle() { }
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLShaderHandle(bool failIfAllocationFailed) : TGLSLHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLShaderHandle(void) { }
	
};


typedef System::TMetaClass* TGLShaderHandleClass;

class DELPHICLASS TGLVertexShaderHandle;
class PASCALIMPLEMENTATION TGLVertexShaderHandle : public TGLShaderHandle
{
	typedef TGLShaderHandle inherited;
	
public:
	__fastcall virtual TGLVertexShaderHandle(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLVertexShaderHandle(bool failIfAllocationFailed) : TGLShaderHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLVertexShaderHandle(void) { }
	
};


class DELPHICLASS TGLGeometryShaderHandle;
class PASCALIMPLEMENTATION TGLGeometryShaderHandle : public TGLShaderHandle
{
	typedef TGLShaderHandle inherited;
	
public:
	__fastcall virtual TGLGeometryShaderHandle(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLGeometryShaderHandle(bool failIfAllocationFailed) : TGLShaderHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLGeometryShaderHandle(void) { }
	
};


class DELPHICLASS TGLFragmentShaderHandle;
class PASCALIMPLEMENTATION TGLFragmentShaderHandle : public TGLShaderHandle
{
	typedef TGLShaderHandle inherited;
	
public:
	__fastcall virtual TGLFragmentShaderHandle(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLFragmentShaderHandle(bool failIfAllocationFailed) : TGLShaderHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLFragmentShaderHandle(void) { }
	
};


class DELPHICLASS TGLTessControlShaderHandle;
class PASCALIMPLEMENTATION TGLTessControlShaderHandle : public TGLShaderHandle
{
	typedef TGLShaderHandle inherited;
	
public:
	__fastcall virtual TGLTessControlShaderHandle(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLTessControlShaderHandle(bool failIfAllocationFailed) : TGLShaderHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLTessControlShaderHandle(void) { }
	
};


class DELPHICLASS TGLTessEvaluationShaderHandle;
class PASCALIMPLEMENTATION TGLTessEvaluationShaderHandle : public TGLShaderHandle
{
	typedef TGLShaderHandle inherited;
	
public:
	__fastcall virtual TGLTessEvaluationShaderHandle(void);
	__classmethod virtual bool __fastcall IsSupported();
public:
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLTessEvaluationShaderHandle(bool failIfAllocationFailed) : TGLShaderHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLTessEvaluationShaderHandle(void) { }
	
};


class DELPHICLASS TGLProgramHandle;
class PASCALIMPLEMENTATION TGLProgramHandle : public TGLSLHandle
{
	typedef TGLSLHandle inherited;
	
public:
	__classmethod virtual bool __fastcall IsValid(const unsigned ID);
	
private:
	System::UnicodeString FName;
	int __fastcall GetUniform1i(const System::UnicodeString index);
	void __fastcall SetUniform1i(const System::UnicodeString index, int val);
	Vectortypes::TVector2i __fastcall GetUniform2i(const System::UnicodeString index);
	void __fastcall SetUniform2i(const System::UnicodeString index, const Vectortypes::TVector2i &Value);
	Vectortypes::TVector3i __fastcall GetUniform3i(const System::UnicodeString index);
	void __fastcall SetUniform3i(const System::UnicodeString index, const Vectortypes::TVector3i &Value);
	Vectortypes::TVector4i __fastcall GetUniform4i(const System::UnicodeString index);
	void __fastcall SetUniform4i(const System::UnicodeString index, const Vectortypes::TVector4i &Value);
	float __fastcall GetUniform1f(const System::UnicodeString index);
	void __fastcall SetUniform1f(const System::UnicodeString index, float val);
	Vectortypes::TVector2f __fastcall GetUniform2f(const System::UnicodeString index);
	void __fastcall SetUniform2f(const System::UnicodeString index, const Vectortypes::TVector2f &val);
	Vectortypes::TVector3f __fastcall GetUniform3f(const System::UnicodeString index);
	void __fastcall SetUniform3f(const System::UnicodeString index, const Vectortypes::TVector3f &val);
	Vectortypes::TVector4f __fastcall GetUniform4f(const System::UnicodeString index);
	void __fastcall SetUniform4f(const System::UnicodeString index, const Vectortypes::TVector4f &val);
	Vectortypes::TMatrix2f __fastcall GetUniformMatrix2fv(const System::UnicodeString index);
	void __fastcall SetUniformMatrix2fv(const System::UnicodeString index, const Vectortypes::TMatrix2f &val);
	Vectortypes::TMatrix3f __fastcall GetUniformMatrix3fv(const System::UnicodeString index);
	void __fastcall SetUniformMatrix3fv(const System::UnicodeString index, const Vectortypes::TMatrix3f &val);
	Vectortypes::TMatrix4f __fastcall GetUniformMatrix4fv(const System::UnicodeString index);
	void __fastcall SetUniformMatrix4fv(const System::UnicodeString index, const Vectortypes::TMatrix4f &val);
	unsigned __fastcall GetUniformTextureHandle(const System::UnicodeString index, const int TextureIndex, const Gltextureformat::TGLTextureTarget TextureTarget);
	void __fastcall SetUniformTextureHandle(const System::UnicodeString index, const int TextureIndex, const Gltextureformat::TGLTextureTarget TextureTarget, const unsigned Value);
	void __fastcall SetUniformBuffer(const System::UnicodeString index, TGLUniformBufferHandle* Value);
	
protected:
	virtual unsigned __fastcall DoAllocateHandle(void);
	
public:
	__property System::UnicodeString Name = {read=FName, write=FName};
	__fastcall virtual TGLProgramHandle(void);
	void __fastcall AddShader(TGLShaderHandleClass shaderType, const System::UnicodeString shaderSource, bool treatWarningsAsErrors = false);
	void __fastcall AttachObject(TGLShaderHandle* shader);
	void __fastcall DetachAllObject(void);
	void __fastcall BindAttribLocation(int index, const System::UnicodeString aName);
	void __fastcall BindFragDataLocation(int index, const System::UnicodeString aName);
	bool __fastcall LinkProgram(void);
	bool __fastcall ValidateProgram(void);
	int __fastcall GetAttribLocation(const System::UnicodeString aName);
	int __fastcall GetUniformLocation(const System::UnicodeString aName);
	Opengltokens::PGLint __fastcall GetUniformOffset(const System::UnicodeString aName);
	int __fastcall GetUniformBlockIndex(const System::UnicodeString aName);
	int __fastcall GetVaryingLocation(const System::UnicodeString aName);
	void __fastcall AddActiveVarying(const System::UnicodeString aName);
	int __fastcall GetUniformBufferSize(const System::UnicodeString aName);
	void __fastcall UseProgramObject(void);
	void __fastcall EndUseProgramObject(void);
	void __fastcall SetUniformi(const System::UnicodeString index, const int val)/* overload */;
	void __fastcall SetUniformi(const System::UnicodeString index, const Vectortypes::TVector2i &val)/* overload */;
	void __fastcall SetUniformi(const System::UnicodeString index, const Vectortypes::TVector3i &val)/* overload */;
	void __fastcall SetUniformi(const System::UnicodeString index, const Vectortypes::TVector4i &val)/* overload */;
	void __fastcall SetUniformf(const System::UnicodeString index, const float val)/* overload */;
	void __fastcall SetUniformf(const System::UnicodeString index, const Vectortypes::TVector2f &val)/* overload */;
	void __fastcall SetUniformf(const System::UnicodeString index, const Vectortypes::TVector3f &val)/* overload */;
	void __fastcall SetUniformf(const System::UnicodeString index, const Vectortypes::TVector4f &val)/* overload */;
	__property int Uniform1i[const System::UnicodeString index] = {read=GetUniform1i, write=SetUniform1i};
	__property Vectortypes::TVector2i Uniform2i[const System::UnicodeString index] = {read=GetUniform2i, write=SetUniform2i};
	__property Vectortypes::TVector3i Uniform3i[const System::UnicodeString index] = {read=GetUniform3i, write=SetUniform3i};
	__property Vectortypes::TVector4i Uniform4i[const System::UnicodeString index] = {read=GetUniform4i, write=SetUniform4i};
	__property float Uniform1f[const System::UnicodeString index] = {read=GetUniform1f, write=SetUniform1f};
	__property Vectortypes::TVector2f Uniform2f[const System::UnicodeString index] = {read=GetUniform2f, write=SetUniform2f};
	__property Vectortypes::TVector3f Uniform3f[const System::UnicodeString index] = {read=GetUniform3f, write=SetUniform3f};
	__property Vectortypes::TVector4f Uniform4f[const System::UnicodeString index] = {read=GetUniform4f, write=SetUniform4f};
	__property Vectortypes::TMatrix2f UniformMatrix2fv[const System::UnicodeString index] = {read=GetUniformMatrix2fv, write=SetUniformMatrix2fv};
	__property Vectortypes::TMatrix3f UniformMatrix3fv[const System::UnicodeString index] = {read=GetUniformMatrix3fv, write=SetUniformMatrix3fv};
	__property Vectortypes::TMatrix4f UniformMatrix4fv[const System::UnicodeString index] = {read=GetUniformMatrix4fv, write=SetUniformMatrix4fv};
	__property unsigned UniformTextureHandle[const System::UnicodeString index][const int TextureIndex][const Gltextureformat::TGLTextureTarget TextureTarget] = {read=GetUniformTextureHandle, write=SetUniformTextureHandle};
	__property TGLUniformBufferHandle* UniformBuffer[const System::UnicodeString index] = {write=SetUniformBuffer};
public:
	/* TGLContextHandle.CreateAndAllocate */ inline __fastcall TGLProgramHandle(bool failIfAllocationFailed) : TGLSLHandle(failIfAllocationFailed) { }
	/* TGLContextHandle.Destroy */ inline __fastcall virtual ~TGLProgramHandle(void) { }
	
};


struct DECLSPEC_DRECORD TGLContextNotification
{
public:
	System::TObject* obj;
	System::Classes::TNotifyEvent event;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLContextManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<TGLContextNotification> _TGLContextManager__1;
	
	
private:
	System::Classes::TThreadList* FList;
	bool FTerminated;
	_TGLContextManager__1 FNotifications;
	int FCreatedRCCount;
	System::Classes::TThreadList* FHandles;
	System::Classes::TThread* FThread;
	System::Syncobjs::TEvent* FServiceStarter;
	Glsgenerics::GThreadList__1<TServiceContextTask>* FThreadTask;
	TGLContext* FServiceContext;
	
protected:
	void __fastcall Lock(void);
	void __fastcall UnLock(void);
	void __fastcall RegisterContext(TGLContext* aContext);
	void __fastcall UnRegisterContext(TGLContext* aContext);
	void __fastcall ContextCreatedBy(TGLContext* aContext);
	void __fastcall DestroyingContextBy(TGLContext* aContext);
	void __fastcall CreateServiceContext(void);
	void __fastcall QueueTaskDepleted(void);
	__property System::Syncobjs::TEvent* ServiceStarter = {read=FServiceStarter};
	__property TGLContext* ServiceContext = {read=FServiceContext};
	
public:
	__fastcall TGLContextManager(void);
	__fastcall virtual ~TGLContextManager(void);
	TGLContext* __fastcall CreateContext(TGLContextClass AClass = 0x0);
	int __fastcall ContextCount(void);
	void __fastcall LastContextDestroyNotification(System::TObject* anObject, System::Classes::TNotifyEvent anEvent);
	void __fastcall RemoveNotification(System::TObject* anObject);
	void __fastcall Terminate(void);
	void __fastcall DestroyAllHandles(void);
	void __fastcall NotifyPreparationNeed(void);
};

#pragma pack(pop)

class DELPHICLASS EGLContext;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLContext : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLContext(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLContext(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLContext(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLContext(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLContext(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLContext(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLContext(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLContext(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLContext(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLContext(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLContext(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLContext(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLContext(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS EPBuffer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EPBuffer : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EPBuffer(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EPBuffer(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EPBuffer(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EPBuffer(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EPBuffer(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EPBuffer(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EPBuffer(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EPBuffer(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EPBuffer(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EPBuffer(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EPBuffer(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EPBuffer(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EPBuffer(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS EGLShader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLShader : public EGLContext
{
	typedef EGLContext inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLShader(const System::UnicodeString Msg) : EGLContext(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLShader(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EGLContext(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLShader(NativeUInt Ident)/* overload */ : EGLContext(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLShader(System::PResStringRec ResStringRec)/* overload */ : EGLContext(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLShader(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EGLContext(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLShader(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : EGLContext(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLShader(const System::UnicodeString Msg, int AHelpContext) : EGLContext(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLShader(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EGLContext(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLShader(NativeUInt Ident, int AHelpContext)/* overload */ : EGLContext(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLShader(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EGLContext(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLShader(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EGLContext(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLShader(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EGLContext(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLShader(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::StaticArray<unsigned, 4> MRT_BUFFERS;
extern PACKAGE System::ResourceString _cIncompatibleContexts;
#define Glcontext_cIncompatibleContexts System::LoadResourceString(&Glcontext::_cIncompatibleContexts)
extern PACKAGE System::ResourceString _cDeleteContextFailed;
#define Glcontext_cDeleteContextFailed System::LoadResourceString(&Glcontext::_cDeleteContextFailed)
extern PACKAGE System::ResourceString _cContextActivationFailed;
#define Glcontext_cContextActivationFailed System::LoadResourceString(&Glcontext::_cContextActivationFailed)
extern PACKAGE System::ResourceString _cContextDeactivationFailed;
#define Glcontext_cContextDeactivationFailed System::LoadResourceString(&Glcontext::_cContextDeactivationFailed)
extern PACKAGE System::ResourceString _cUnableToCreateLegacyContext;
#define Glcontext_cUnableToCreateLegacyContext System::LoadResourceString(&Glcontext::_cUnableToCreateLegacyContext)
extern PACKAGE System::ResourceString _cNoActiveRC;
#define Glcontext_cNoActiveRC System::LoadResourceString(&Glcontext::_cNoActiveRC)
extern PACKAGE System::ResourceString _glsFailedToShare;
#define Glcontext_glsFailedToShare System::LoadResourceString(&Glcontext::_glsFailedToShare)
extern PACKAGE TGLContextManager* GLContextManager;
extern PACKAGE bool vIgnoreOpenGLErrors;
extern PACKAGE bool vContextActivationFailureOccurred;
extern PACKAGE TAbstractMultitextureCoordinatorClass vMultitextureCoordinatorClass;
extern PACKAGE TGLContext* __fastcall CurrentGLContext(void);
extern PACKAGE TGLContext* __fastcall SafeCurrentGLContext(void);
extern PACKAGE Opengladapter::TGLExtensionsAndEntryPoints* __fastcall GL(void);
extern PACKAGE bool __fastcall IsMainThread(void);
extern PACKAGE bool __fastcall IsServiceContextAvaible(void);
extern PACKAGE Vcl::Forms::TForm* __fastcall GetServiceWindow(void);
extern PACKAGE void __fastcall RegisterGLContextClass(TGLContextClass aGLContextClass);
extern PACKAGE void __fastcall AddTaskForServiceContext(TTaskProcedure ATask, TFinishTaskEvent* FinishEvent = (TFinishTaskEvent*)(0x0));
}	/* namespace Glcontext */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCONTEXT)
using namespace Glcontext;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcontextHPP

// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCameraController.pas' rev: 24.00 (Win32)

#ifndef GlcameracontrollerHPP
#define GlcameracontrollerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Contnrs.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLSmoothNavigator.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcameracontroller
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLCameraController;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLCameraController : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLCameraController(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLCameraController(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCameraController(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLCameraController(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCameraController(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCameraController(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCameraController(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLCameraController(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLCameraJobList;
class DELPHICLASS TGLCameraController;
class DELPHICLASS TGLCameraJob;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCameraJobList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	TGLCameraJob* operator[](const int AIndex) { return Items[AIndex]; }
	
private:
	TGLCameraController* FController;
	TGLCameraJob* __fastcall GetCameraJob(const int AIndex);
	void __fastcall SetCameraJob(const int AIndex, TGLCameraJob* const Value);
	
public:
	__fastcall TGLCameraJobList(TGLCameraController* AController);
	HIDESBASE int __fastcall Add(TGLCameraJob* ACameraJob);
	__property TGLCameraJob* Items[const int AIndex] = {read=GetCameraJob, write=SetCameraJob/*, default*/};
	HIDESBASE TGLCameraJob* __fastcall First(void);
	HIDESBASE TGLCameraJob* __fastcall Last(void);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLCameraJobList(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLCameraJob : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLCameraJobList* FJoblist;
	
protected:
	bool FAbort;
	bool FInit;
	bool FRunning;
	double FElapsedTime;
	double FDeltaTime;
	double FStartTime;
	double FProceedTime;
	
public:
	__fastcall virtual TGLCameraJob(TGLCameraJobList* const AJoblist);
	__fastcall virtual ~TGLCameraJob(void);
	void __fastcall Abort(void);
	virtual void __fastcall Step(void) = 0 ;
	virtual void __fastcall Init(void) = 0 ;
	__property bool Running = {read=FRunning, write=FRunning, nodefault};
	__property double ElapsedTime = {read=FElapsedTime, write=FElapsedTime};
	__property double StartTime = {read=FStartTime, write=FStartTime};
	__property double ProceedTime = {read=FProceedTime, write=FProceedTime};
};


class DELPHICLASS TGLMoveToPosJob;
class PASCALIMPLEMENTATION TGLMoveToPosJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Vectortypes::TVector4f FInitialPos;
	Vectortypes::TVector4f FFinalPos;
	
public:
	double X;
	double Y;
	double Z;
	double Time;
	virtual void __fastcall Step(void);
	virtual void __fastcall Init(void);
	__property Vectortypes::TVector4f InitialPos = {read=FInitialPos};
	__property Vectortypes::TVector4f FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLMoveToPosJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLMoveToPosJob(void) { }
	
};


class DELPHICLASS TGLZoomToDistanceJob;
class PASCALIMPLEMENTATION TGLZoomToDistanceJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Vectortypes::TVector4f FInitialPos;
	Vectortypes::TVector4f FFinalPos;
	
public:
	double Distance;
	double Time;
	virtual void __fastcall Step(void);
	virtual void __fastcall Init(void);
	__property Vectortypes::TVector4f InitialPos = {read=FInitialPos};
	__property Vectortypes::TVector4f FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLZoomToDistanceJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLZoomToDistanceJob(void) { }
	
};


class DELPHICLASS TGLOrbitToPosJob;
class PASCALIMPLEMENTATION TGLOrbitToPosJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Vectortypes::TVector4f FFinalPos;
	Vectortypes::TVector2f FRotateSpeed;
	Vectortypes::TVector4f FCameraUpVector;
	Vectortypes::TVector4f FTargetPosition;
	double FTime;
	
public:
	virtual void __fastcall Step(void);
	virtual void __fastcall Init(void);
	__property Vectortypes::TVector2f RotateSpeed = {read=FRotateSpeed};
	__property Vectortypes::TVector4f CameraUpVector = {read=FCameraUpVector};
	__property Vectortypes::TVector4f TargetPosition = {read=FTargetPosition};
	__property Vectortypes::TVector4f FinalPos = {read=FFinalPos};
	__property double Time = {read=FTime};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLOrbitToPosJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLOrbitToPosJob(void) { }
	
};


class DELPHICLASS TGLSmoothOrbitToPos;
class PASCALIMPLEMENTATION TGLSmoothOrbitToPos : public TGLOrbitToPosJob
{
	typedef TGLOrbitToPosJob inherited;
	
private:
	float FCutoffAngle;
	bool FNeedToRecalculateZoom;
	Vectortypes::TMatrix4f FShouldBeMatrix;
	Glsmoothnavigator::TGLNavigatorSmoothChangeVector* FSmoothNavigator;
	
public:
	__fastcall virtual TGLSmoothOrbitToPos(TGLCameraJobList* const AJoblist);
	virtual void __fastcall Step(void);
	__property float CutoffAngle = {read=FCutoffAngle, write=FCutoffAngle};
	__property bool NeedToRecalculateZoom = {read=FNeedToRecalculateZoom, write=FNeedToRecalculateZoom, nodefault};
public:
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLSmoothOrbitToPos(void) { }
	
};


class DELPHICLASS TGLOrbitToPosAdvJob;
class PASCALIMPLEMENTATION TGLOrbitToPosAdvJob : public TGLCameraJob
{
	typedef TGLCameraJob inherited;
	
private:
	Vectortypes::TVector4f FInitialPos;
	Vectortypes::TVector4f FFinalPos;
	Vectortypes::TVector4f FInitialUp;
	Vectortypes::TVector4f FInitialDir;
	Vectortypes::TVector4f FRotAxis;
	double FAngle;
	
public:
	double X;
	double Y;
	double Z;
	double Time;
	bool PreferUpAxis;
	virtual void __fastcall Step(void);
	virtual void __fastcall Init(void);
	__property Vectortypes::TVector4f InitialPos = {read=FInitialPos};
	__property Vectortypes::TVector4f InitialUp = {read=FInitialUp};
	__property Vectortypes::TVector4f InitialDir = {read=FInitialDir};
	__property Vectortypes::TVector4f FinalPos = {read=FFinalPos};
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLOrbitToPosAdvJob(TGLCameraJobList* const AJoblist) : TGLCameraJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLOrbitToPosAdvJob(void) { }
	
};


class DELPHICLASS TGLSmoothOrbitToPosAdvJob;
class PASCALIMPLEMENTATION TGLSmoothOrbitToPosAdvJob : public TGLOrbitToPosAdvJob
{
	typedef TGLOrbitToPosAdvJob inherited;
	
private:
	Vectortypes::TVector4f FPreviousPosition;
	Glsmoothnavigator::TGLNavigatorSmoothChangeVector* FSmoothNavigator;
	bool FRestoreUpVector;
	
public:
	virtual void __fastcall Step(void);
	virtual void __fastcall Init(void);
public:
	/* TGLCameraJob.Create */ inline __fastcall virtual TGLSmoothOrbitToPosAdvJob(TGLCameraJobList* const AJoblist) : TGLOrbitToPosAdvJob(AJoblist) { }
	/* TGLCameraJob.Destroy */ inline __fastcall virtual ~TGLSmoothOrbitToPosAdvJob(void) { }
	
};


typedef void __fastcall (__closure *TGLCameraJobEvent)(TGLCameraJob* Sender);

class PASCALIMPLEMENTATION TGLCameraController : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLCameraJobList* FCameraJobList;
	Glscene::TGLBaseSceneObject* FCamera;
	Glscene::TGLBaseSceneObject* FCameraTarget;
	TGLCameraJobEvent FOnJobAdded;
	TGLCameraJobEvent FOnJobFinished;
	TGLCameraJobEvent FOnJobStep;
	double FsoSafeDist;
	double FsoTimeToSafePlacement;
	double FsoTimeToOrbit;
	double FsoTimeToZoomBackIn;
	void __fastcall CheckAssignments(bool Extended);
	void __fastcall SetOnJobAdded(const TGLCameraJobEvent Value);
	void __fastcall SetOnJobFinished(const TGLCameraJobEvent Value);
	void __fastcall SetOnJobStep(const TGLCameraJobEvent Value);
	void __fastcall SetCamera(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetCameraTarget(Glscene::TGLBaseSceneObject* const Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCameraController(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCameraController(void);
	TGLMoveToPosJob* __fastcall MoveToPos(double x, double y, double z, double time);
	TGLOrbitToPosJob* __fastcall OrbitToPos(double x, double y, double z, double time);
	TGLSmoothOrbitToPos* __fastcall OrbitToPosSmooth(const Vectortypes::TVector4f &ATargetPosition, const double ATime, Glsmoothnavigator::TGLNavigatorSmoothChangeVector* const ASmoothNavigator, const bool AFNeedToRecalculateZoom, const Vectorgeometry::PVector ACameraUpVector = (Vectorgeometry::PVector)(0x0));
	TGLOrbitToPosAdvJob* __fastcall OrbitToPosAdvanced(double x, double y, double z, double time, bool PreferUpAxis = true);
	TGLSmoothOrbitToPosAdvJob* __fastcall OrbitToPosAdvancedSmooth(const double x, const double y, const double z, const double time, Glsmoothnavigator::TGLNavigatorSmoothChangeVector* const ASmoothNavigator, const bool PreferUpAxis = true);
	TGLZoomToDistanceJob* __fastcall ZoomToDistance(double Distance, double Time);
	void __fastcall SafeOrbitAndZoomToPos(double x, double y, double z);
	void __fastcall StopMovement(void);
	void __fastcall Step(const double deltaTime, const double newTime);
	__property TGLCameraJobList* CameraJobList = {read=FCameraJobList};
	
__published:
	__property Glscene::TGLBaseSceneObject* Camera = {read=FCamera, write=SetCamera};
	__property Glscene::TGLBaseSceneObject* CameraTarget = {read=FCameraTarget, write=SetCameraTarget};
	__property double soSafeDistance = {read=FsoSafeDist, write=FsoSafeDist};
	__property double soTimeToSafePlacement = {read=FsoTimeToSafePlacement, write=FsoTimeToSafePlacement};
	__property double soTimeToOrbit = {read=FsoTimeToOrbit, write=FsoTimeToOrbit};
	__property double soTimeToZoomBackIn = {read=FsoTimeToZoomBackIn, write=FsoTimeToZoomBackIn};
	__property TGLCameraJobEvent OnJobAdded = {read=FOnJobAdded, write=SetOnJobAdded};
	__property TGLCameraJobEvent OnJobStep = {read=FOnJobStep, write=SetOnJobStep};
	__property TGLCameraJobEvent OnJobFinished = {read=FOnJobFinished, write=SetOnJobFinished};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glcameracontroller */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCAMERACONTROLLER)
using namespace Glcameracontroller;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcameracontrollerHPP

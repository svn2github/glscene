// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NewtonImport_JointLibrary.pas' rev: 24.00 (Win32)

#ifndef Newtonimport_jointlibraryHPP
#define Newtonimport_jointlibraryHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <NewtonImport.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Newtonimport_jointlibrary
{
//-- type declarations -------------------------------------------------------
typedef void __cdecl (*NewtonUserJointDestructorCallback)(const Newtonimport::PNewtonUserJoint me);

typedef NewtonUserJointDestructorCallback *PNewtonUserJointDestructorCallback;

typedef void __cdecl (*NewtonUserJointSubmitConstraintCallback)(const Newtonimport::PNewtonUserJoint me, float timestep, int threadIndex);

typedef NewtonUserJointSubmitConstraintCallback *PNewtonUserJointSubmitConstraintCallback;

typedef void __cdecl (*BlankJointGetInfo)(const Newtonimport::PNewtonUserJoint me, Newtonimport::PNewtonJointRecord info);

typedef BlankJointGetInfo *PBlankJointGetInfo;

typedef void __cdecl (*DGRaycastVehicleTireTransformCallback)(Newtonimport::PNewtonUserJoint car);

typedef DGRaycastVehicleTireTransformCallback *PDGRaycastVehicleTireTransformCallback;

//-- var, const, procedure ---------------------------------------------------
#define JointLibraryDLL L"dJointLibrary.dll"
extern "C" void __cdecl CustomDestroyJoint(const Newtonimport::PNewtonUserJoint joint);
extern "C" Newtonimport::PNewtonJoint __cdecl CustomGetNewtonJoint(const Newtonimport::PNewtonUserJoint joint);
extern "C" int __cdecl CustomGetJointID(const Newtonimport::PNewtonUserJoint joint);
extern "C" void __cdecl CustomSetJointID(const Newtonimport::PNewtonUserJoint joint, int rttI);
extern "C" Newtonimport::PNewtonBody __cdecl CustomGetBody0(const Newtonimport::PNewtonUserJoint joint);
extern "C" Newtonimport::PNewtonBody __cdecl CustomGetBody1(const Newtonimport::PNewtonUserJoint joint);
extern "C" int __cdecl CustomGetBodiesCollisionState(const Newtonimport::PNewtonUserJoint joint);
extern "C" void __cdecl CustomSetBodiesCollisionState(const Newtonimport::PNewtonUserJoint joint, int state);
extern "C" void * __cdecl CustomGetUserData(const Newtonimport::PNewtonUserJoint joint);
extern "C" void __cdecl CustomSetUserData(const Newtonimport::PNewtonUserJoint joint, void * userData);
extern "C" void __cdecl CustomSetDestructorCallback(const Newtonimport::PNewtonUserJoint joint, NewtonUserJointDestructorCallback callback);
extern "C" void __cdecl CustomSetSubmitContraintCallback(const Newtonimport::PNewtonUserJoint joint, NewtonUserJointSubmitConstraintCallback callback);
extern "C" Newtonimport::PNewtonUserJoint __cdecl CustomCreateBlankJoint(int maxDof, const Newtonimport::PNewtonBody body0, const Newtonimport::PNewtonBody body1, BlankJointGetInfo info);
extern "C" Newtonimport::PNewtonUserJoint __cdecl CreateCustomKinematicController(const Newtonimport::PNewtonBody targetBody, Newtonimport::PFloat attachmentPointInGlobalSpace);
extern "C" void __cdecl CustomKinematicControllerSetPickMode(const Newtonimport::PNewtonUserJoint pick, int mode);
extern "C" void __cdecl CustomKinematicControllerSetMaxLinearFriction(const Newtonimport::PNewtonUserJoint pick, float accel);
extern "C" void __cdecl CustomKinematicControllerSetMaxAngularFriction(const Newtonimport::PNewtonUserJoint pick, float alpha);
extern "C" void __cdecl CustomKinematicControllerSetTargetPosit(const Newtonimport::PNewtonUserJoint pick, Newtonimport::PFloat posit);
extern "C" void __cdecl CustomKinematicControllerSetTargetRotation(const Newtonimport::PNewtonUserJoint pick, Newtonimport::PFloat rotation);
extern "C" void __cdecl CustomKinematicControllerSetTargetMatrix(const Newtonimport::PNewtonUserJoint pick, Newtonimport::PFloat matrix);
extern "C" void __cdecl CustomKinematicControllerGetTargetMatrix(const Newtonimport::PNewtonUserJoint pick, Newtonimport::PFloat matrix);
extern "C" Newtonimport::PNewtonUserJoint __cdecl CreateCustomJoint6DOF(const Newtonimport::PFloat pinsAndPivotChildFrame, const Newtonimport::PFloat pinsAndPivotParentFrame, const Newtonimport::PNewtonBody child, const Newtonimport::PNewtonBody parent);
extern "C" void __cdecl CustomJoint6DOF_SetLinearLimits(Newtonimport::PNewtonUserJoint customJoint6DOF, const Newtonimport::PFloat minLinearLimits, const Newtonimport::PFloat maxLinearLimits);
extern "C" void __cdecl CustomJoint6DOF_SetAngularLimits(Newtonimport::PNewtonUserJoint customJoint6DOF, const Newtonimport::PFloat minAngularLimits, const Newtonimport::PFloat maxAngularLimits);
extern "C" void __cdecl CustomJoint6DOF_GetLinearLimits(Newtonimport::PNewtonUserJoint customJoint6DOF, Newtonimport::PFloat minLinearLimits, Newtonimport::PFloat maxLinearLimits);
extern "C" void __cdecl CustomJoint6DOF_GetAngularLimits(Newtonimport::PNewtonUserJoint customJoint6DOF, Newtonimport::PFloat minAngularLimits, Newtonimport::PFloat maxAngularLimits);
extern "C" void __cdecl CustomJoint6DOF_SetReverseUniversal(Newtonimport::PNewtonUserJoint customJoint6DOF, int order);
extern "C" Newtonimport::PNewtonUserJoint __cdecl CreateCustomBallAndSocket(const Newtonimport::PFloat pinsAndPivotChildFrame, const Newtonimport::PNewtonBody child, const Newtonimport::PNewtonBody parent);
extern "C" void __cdecl BallAndSocketSetConeAngle(Newtonimport::PNewtonUserJoint ballJoint, float angle);
extern "C" void __cdecl BallAndSocketSetTwistAngle(Newtonimport::PNewtonUserJoint ballJoint, float minAngle, float maxAngle);
extern "C" Newtonimport::PNewtonUserJoint __cdecl CreateCustomHinge(const Newtonimport::PFloat pinsAndPivotChildFrame, const Newtonimport::PNewtonBody child, const Newtonimport::PNewtonBody parent);
extern "C" void __cdecl HingeEnableLimits(Newtonimport::PNewtonUserJoint hingeJoint, int state);
extern "C" void __cdecl HingeSetLimits(Newtonimport::PNewtonUserJoint hingeJoint, float minAngle, float maxAngle);
extern "C" float __cdecl HingeGetJointAngle(const Newtonimport::PNewtonUserJoint hingeJoint);
extern "C" void __cdecl HingeGetPinAxis(const Newtonimport::PNewtonUserJoint hingeJoint, Newtonimport::PFloat Pin);
extern "C" float __cdecl HingeCalculateJointOmega(const Newtonimport::PNewtonUserJoint hingeJoint);
extern "C" Newtonimport::PNewtonUserJoint __cdecl CreateCustomSlider(const Newtonimport::PFloat pinsAndPivotChildFrame, const Newtonimport::PNewtonBody child, const Newtonimport::PNewtonBody parent);
extern "C" void __cdecl SliderEnableLimits(Newtonimport::PNewtonUserJoint sliderJoint, int state);
extern "C" void __cdecl SliderSetLimits(Newtonimport::PNewtonUserJoint sliderJoint, float mindist, float maxdist);
extern "C" Newtonimport::PNewtonUserJoint __cdecl CreateCustomPlayerController(const Newtonimport::PFloat pins, const Newtonimport::PNewtonBody player, float maxStairStepFactor, float cushion);
extern "C" void __cdecl CustomPlayerControllerSetVelocity(const Newtonimport::PNewtonUserJoint playerController, float forwardSpeed, float sideSpeed, float heading);
extern "C" void __cdecl CustomPlayerControllerGetVisualMaTrix(const Newtonimport::PNewtonUserJoint playerController, Newtonimport::PFloat matrix);
extern "C" float __cdecl CustomPlayerControllerGetMaxSlope(const Newtonimport::PNewtonUserJoint playerController);
extern "C" void __cdecl CustomPlayerControllerSetMaxSlope(const Newtonimport::PNewtonUserJoint playerController, float maxSlopeAngleIndRadian);
extern "C" Newtonimport::PNewtonCollision __cdecl CustomPlayerControllerGetSensorShape(const Newtonimport::PNewtonUserJoint playerController);
extern "C" Newtonimport::PNewtonUserJoint __cdecl DGRaycastVehicleCreate(int maxTireCount, const Newtonimport::PFloat cordenateSytemInLocalSpace, Newtonimport::PNewtonBody carBody);
extern "C" void __cdecl DGRaycastVehicleAddTire(Newtonimport::PNewtonUserJoint car, void * userData, const Newtonimport::PFloat localPosition, float mass, float radius, float width, float friction, float suspensionLength, float springConst, float springDamper, int castMode);
extern "C" void __cdecl DGRaycastVehicleSetTireTransformCallback(Newtonimport::PNewtonUserJoint car, DGRaycastVehicleTireTransformCallback callback);
extern "C" int __cdecl DGRaycastVehicleGetTiresCount(Newtonimport::PNewtonUserJoint car);
extern "C" void * __cdecl DGRaycastVehicleGetTiresUserData(Newtonimport::PNewtonUserJoint car, int tireIndex);
extern "C" void __cdecl DGRaycastVehicleGetTireMatrix(Newtonimport::PNewtonUserJoint car, int tireIndex, Newtonimport::PFloat tireMatrix);
extern "C" void __cdecl DGRaycastVehicleInitNormalizeTireLateralForce(Newtonimport::PNewtonUserJoint car, int pointsCount, Newtonimport::PFloat piceSizeStepAxis, Newtonimport::PFloat normalizedForceValue);
extern "C" void __cdecl DGRaycastVehicleInitNormalizeTireLongitudinalForce(Newtonimport::PNewtonUserJoint car, int pointsCount, Newtonimport::PFloat piceSizeStepAxis, Newtonimport::PFloat normalizedForceValue);
}	/* namespace Newtonimport_jointlibrary */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NEWTONIMPORT_JOINTLIBRARY)
using namespace Newtonimport_jointlibrary;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Newtonimport_jointlibraryHPP

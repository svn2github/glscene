unit PhysX;

interface

uses
  Winapi.Windows,
   
  GLScene,
  GLVectorGeometry,
  GLGeomObjects,
  GLVectorFileObjects;

const
  DLL = 'Blitzpx.dll';

function nxBodyCreatePlane(x: single; y: single; z: single): Integer; cdecl;
  stdcall; external DLL name '_pxBodyCreatePlane@12';
function nxBodyCreateCube(dx: single; dy: single; dz: single; mass: single)
  : Integer; cdecl; stdcall; external DLL name '_pxBodyCreateCube@16';
function nxBodyCreateSphere(radius: single; mass: single): Integer; cdecl;
  stdcall; external DLL name '_pxBodyCreateSphere@8';
function nxBodyCreateCapsule(height: single; radius: single; mass: single)
  : Integer; cdecl; stdcall; external DLL name '_pxBodyCreateCapsule@12';
function nxBodyCreateCylinder(radius: single; height: single; nbEdge: Integer;
  mass: single): Integer; cdecl; stdcall;
  external DLL name '_pxBodyCreateCylinder@16';
function nxBodyCreateHull(vbank: psingle; nvert: Integer; mass: single)
  : Integer; cdecl; stdcall; external DLL name '_pxBodyCreateHull@12';
function nxBodyCreateHullFromSSM(surf: Integer; mass: single): Integer; cdecl;
  stdcall; external DLL name '_pxBodyCreateHullFromSSM@8';

function nxCreateTriMeshPmap(vbank: PInteger; fbank: PInteger;
  MESH_NBVERTICES: Integer; MESH_NBFACES: Integer; file_name: PChar;
  pMap: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxCreateTriMeshPmap@24';
function nxCreateTriMeshFromPmap(triangleMesh: Integer; mass: single): Integer;
  cdecl; stdcall; external DLL name '_pxCreateTriMeshFromPmap@8';
function nxCreateTriMesh(vbank: psingle; fbank: PInteger;
  MESH_NBVERTICES: Integer; MESH_NBFACES: Integer; mass: single): Integer;
  cdecl; stdcall; external DLL name '_pxCreateTriMesh@20';
function nxCreateTerrain(nSize: Integer; bank: Integer; scale_x: single;
  scale_y: single; scale_z: single): Integer; cdecl; stdcall;
  external DLL name '_pxCreateTerrain@20';
function nxCreateTerrainFromMesh(vbank: Integer; fbank: Integer;
  MESH_NBVERTICES: Integer; MESH_NBFACES: Integer; axis: single): Integer;
  cdecl; stdcall; external DLL name '_pxCreateTerrainFromMesh@20';
procedure nxCreateTerrainPmap(vbank: Integer; fbank: Integer;
  MESH_NBVERTICES: Integer; MESH_NBFACES: Integer; axis: single;
  file_name: PChar; pMap: Integer); cdecl; stdcall;
  external DLL name '_pxCreateTerrainPmap@28';
function nxTestTriMesh(entity: Integer; mass: single): Integer; cdecl; stdcall;
  external DLL name '_pxTestTriMesh@8';
function nxCreateTriMeshToFile(vbank: psingle; fbank: PInteger;
  MESH_NBVERTICES: Integer; MESH_NBFACES: Integer; fname: PChar): Integer;
  cdecl; stdcall; external DLL name '_pxCreateTriMeshToFile@20';
function nxCreateTriMeshFromFile(fname: PChar; mass: single): Integer; cdecl;
  stdcall; external DLL name '_pxCreateTriMeshFromFile@8';

function nxCreateForceField(): Integer; cdecl; stdcall;
  external DLL name '_pxCreateForceField@0';
procedure nxForceFieldUpdate(FField: Integer); cdecl; stdcall;
  external DLL name '_pxForceFieldUpdate@4';
function nxForceFieldGetConstant(FField: Integer): single; cdecl; stdcall;
  external DLL name '_pxForceFieldGetConstant@4';

function nxShapesGetNumber(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxShapesGetNumber@4';
function nxShapesGetShape(body: Integer; num: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxShapesGetShape@8';
function nxShapesGetPositionX(shape: Integer): single; cdecl; stdcall;
  external DLL name '_pxShapesGetPositionX@4';
function nxShapesGetPositionY(shape: Integer): single; cdecl; stdcall;
  external DLL name '_pxShapesGetPositionY@4';
function nxShapesGetPositionZ(shape: Integer): single; cdecl; stdcall;
  external DLL name '_pxShapesGetPositionZ@4';
function nxShapesGetRotationPitch(shape: Integer): single; cdecl; stdcall;
  external DLL name '_pxShapesGetRotationPitch@4';
function nxShapesGetRotationYaw(shape: Integer): single; cdecl; stdcall;
  external DLL name '_pxShapesGetRotationYaw@4';
function nxShapesGetRotationRoll(shape: Integer): single; cdecl; stdcall;
  external DLL name '_pxShapesGetRotationRoll@4';
procedure nxShapesSetPosition(num: Integer; pos_x: single; pos_y: single;
  pos_z: single); cdecl; stdcall; external DLL name '_pxShapesSetPosition@16';
procedure nxShapesSetRotation(num: Integer; pitch: single; yaw: single;
  roll: single); cdecl; stdcall; external DLL name '_pxShapesSetRotation@16';
function nxContactGetShape(body: Integer; coll: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxContactGetShape@8';

function nxCreateSoftBody(OBJfile: PChar; TETfile: PChar; vbank: Integer;
  numVert: Integer; pos_x: single; pos_y: single; pos_z: single; rot_x: single;
  rot_y: single; rot_z: single): Integer; cdecl; stdcall;
  external DLL name '_pxCreateSoftBody@40';
procedure nxUpdateSoftBody(SB: Integer); cdecl; stdcall;
  external DLL name '_pxUpdateSoftBody@4';

procedure nxSoftBodyLinkMesh(softBody: Integer; vbank: Integer;
  numVert: Integer); cdecl; stdcall; external DLL name '_pxSoftBodyLinkMesh@12';
function nxSoftBodyGetNumVert(SB: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxSoftBodyGetNumVert@4';
procedure nxSoftBodyGetVertexPos(SB: Integer; vbank: Integer; numVert: Integer);
  cdecl; stdcall; external DLL name '_pxSoftBodyGetVertexPos@12';
procedure nxSoftBodyGetNormals(SB: Integer; nbank: Integer; numVert: Integer);
  cdecl; stdcall; external DLL name '_pxSoftBodyGetNormals@12';
function nxSoftBodyIsSleeping(SB: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxSoftBodyIsSleeping@4';
procedure nxSoftBodyPutToSleep(SB: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodyPutToSleep@4';
procedure nxSoftBodyWakeUp(SB: Integer; interval: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodyWakeUp@8';

procedure nxSoftBodySetAttachmentResponseCoefficient(softBody: Integer;
  coef: single); cdecl; stdcall;
  external DLL name '_pxSoftBodySetAttachmentResponseCoefficient@8';
procedure nxSoftBodySetAttachmentTearFactor(softBody: Integer; coef: single);
  cdecl; stdcall; external DLL name '_pxSoftBodySetAttachmentTearFactor@8';
procedure nxSoftBodySetCollisionResponseCoefficient(softBody: Integer;
  coef: single); cdecl; stdcall;
  external DLL name '_pxSoftBodySetCollisionResponseCoefficient@8';
procedure nxSoftBodySetDampingCoefficient(softBody: Integer; coef: single);
  cdecl; stdcall; external DLL name '_pxSoftBodySetDampingCoefficient@8';
procedure nxSoftBodySetExternalAcceleration(softBody: Integer; x: single;
  y: single; z: single); cdecl; stdcall;
  external DLL name '_pxSoftBodySetExternalAcceleration@16';
procedure nxSoftBodySetFriction(softBody: Integer; fric: single); cdecl;
  stdcall; external DLL name '_pxSoftBodySetFriction@8';
procedure nxSoftBodySetLinkRadius(radius: single); cdecl; stdcall;
  external DLL name '_pxSoftBodySetLinkRadius@4';
procedure nxSoftBodySetMask(softBody: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodySetMask@8';
procedure nxSoftBodySetMaskCombine(softBody: Integer; mask: Integer); cdecl;
  stdcall; external DLL name '_pxSoftBodySetMaskCombine@8';
procedure nxSoftBodyMaskClear(softBody: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodyMaskClear@4';
procedure nxSoftBodySetName(softBody: Integer; name: PChar); cdecl; stdcall;
  external DLL name '_pxSoftBodySetName@8';
procedure nxSoftBodySetParticleRadius(softBody: Integer; radius: single); cdecl;
  stdcall; external DLL name '_pxSoftBodySetParticleRadius@8';
procedure nxSoftBodySetSleepLinearVelocity(softBody: Integer; vel: single);
  cdecl; stdcall; external DLL name '_pxSoftBodySetSleepLinearVelocity@8';
procedure nxSoftBodySetSolverIterations(softBody: Integer; iter: Integer);
  cdecl; stdcall; external DLL name '_pxSoftBodySetSolverIterations@8';
procedure nxSoftBodySetStretchingStiffness(softBody: Integer; coef: single);
  cdecl; stdcall; external DLL name '_pxSoftBodySetStretchingStiffness@8';
procedure nxSoftBodySetVolumeStiffness(softBody: Integer; coef: single); cdecl;
  stdcall; external DLL name '_pxSoftBodySetVolumeStiffness@8';
procedure nxSoftBodySetGravity(softBody: Integer; stat: Integer); cdecl;
  stdcall; external DLL name '_pxSoftBodySetGravity@8';
procedure nxSoftBodySetStatic(softBody: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodySetStatic@8';
procedure nxSoftBodySetCollision(softBody: Integer; stat: Integer); cdecl;
  stdcall; external DLL name '_pxSoftBodySetCollision@8';

function nxSoftBodyGetAttachmentResponseCoefficient(softBody: Integer): single;
  cdecl; stdcall;
  external DLL name '_pxSoftBodyGetAttachmentResponseCoefficient@4';
function nxSoftBodyGetAttachmentTearFactor(softBody: Integer): single; cdecl;
  stdcall; external DLL name '_pxSoftBodyGetAttachmentTearFactor@4';
function nxSoftBodyGetCollisionResponseCoefficient(softBody: Integer): single;
  cdecl; stdcall;
  external DLL name '_pxSoftBodyGetCollisionResponseCoefficient@4';
function nxSoftBodyGetDampingCoefficient(softBody: Integer): single; cdecl;
  stdcall; external DLL name '_pxSoftBodyGetDampingCoefficient@4';
function nxSoftBodyGetFriction(softBody: Integer): single; cdecl; stdcall;
  external DLL name '_pxSoftBodyGetFriction@4';
function nxSoftBodyGetName(softBody: Integer): PChar; cdecl; stdcall;
  external DLL name '_pxSoftBodyGetName@4';
function nxSoftBodyGetParticleRadius(softBody: Integer): single; cdecl; stdcall;
  external DLL name '_pxSoftBodyGetParticleRadius@4';
function nxSoftBodyGetSleepLinearVelocity(softBody: Integer): single; cdecl;
  stdcall; external DLL name '_pxSoftBodyGetSleepLinearVelocity@4';
function nxSoftBodyGetSolverIterations(softBody: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxSoftBodyGetSolverIterations@4';
function nxSoftBodyGetStretchingStiffness(softBody: Integer): single; cdecl;
  stdcall; external DLL name '_pxSoftBodyGetStretchingStiffness@4';
function nxSoftBodyGetVolumeStiffness(softBody: Integer): single; cdecl;
  stdcall; external DLL name '_pxSoftBodyGetVolumeStiffness@4';

procedure nxSoftBodyAttachToShape(softBody: Integer; body: Integer;
  flag: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodyAttachToShape@12';
procedure nxSoftBodyAttachToCollidingShapes(softBody: Integer;
  tearable: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodyAttachToCollidingShapes@8';
procedure nxSoftBodyDetachFromShape(softBody: Integer; body: Integer); cdecl;
  stdcall; external DLL name '_pxSoftBodyDetachFromShape@8';
procedure nxSoftBodyAttachVertexToGlobalPosition(softBody: Integer;
  vertID: Integer; x: single; y: single; z: single); cdecl; stdcall;
  external DLL name '_pxSoftBodyAttachVertexToGlobalPosition@20';
procedure nxSoftBodyFreeVertex(softBody: Integer; vertID: Integer); cdecl;
  stdcall; external DLL name '_pxSoftBodyFreeVertex@8';
procedure nxSoftBodyAddForceAtPos(softBody: Integer; pos_x: single;
  pos_y: single; pos_z: single; magnitude: single; radius: single;
  mode: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodyAddForceAtPos@28';
procedure nxSoftBodyAddForceAtVertex(softBody: Integer; x: single; y: single;
  z: single; vertID: Integer; mode: Integer); cdecl; stdcall;
  external DLL name '_pxSoftBodyAddForceAtVertex@24';

procedure nxIrrGetPosMat(body: Integer; irrMat: Integer); cdecl; stdcall;
  external DLL name '_pxIrrGetPosMat@8';
function nxIrrGetPosElem(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxIrrGetPosElem@4';
function nxIrrGetRotElem(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxIrrGetRotElem@4';

function nxParticleCreateEmitter(): Integer; cdecl; stdcall;
  external DLL name '_pxParticleCreateEmitter@0';
procedure nxParticleEmitSetAngDamping(pEmitter: Integer; damp: Integer); cdecl;
  stdcall; external DLL name '_pxParticleEmitSetAngDamping@8';
procedure nxParticleEmitSetLinDamping(pEmitter: Integer; damp: Integer); cdecl;
  stdcall; external DLL name '_pxParticleEmitSetLinDamping@8';
procedure nxParticleEmitSetMass(pEmitter: Integer; mass: single); cdecl;
  stdcall; external DLL name '_pxParticleEmitSetMass@8';
procedure nxParticleEmitSetRadius(pEmitter: Integer; radius: single); cdecl;
  stdcall; external DLL name '_pxParticleEmitSetRadius@8';
procedure nxParticleEmitSetPosition(pEmitter: Integer; x: single; y: single;
  z: single); cdecl; stdcall; external DLL name '_pxParticleEmitSetPosition@16';
procedure nxParticleEmitSetRotation(pEmitter: Integer; pitch: single;
  yaw: single; roll: single); cdecl; stdcall;
  external DLL name '_pxParticleEmitSetRotation@16';
procedure nxParticleEmitSetRandRadius(pEmitter: Integer; radius: single); cdecl;
  stdcall; external DLL name '_pxParticleEmitSetRandRadius@8';
procedure nxParticleEmitSetStartSpeed(pEmitter: Integer; min: single;
  max: single); cdecl; stdcall;
  external DLL name '_pxParticleEmitSetStartSpeed@12';
procedure nxParticleEmitSetTDAcceleration(pEmitter: Integer; x: single;
  y: single; z: single); cdecl; stdcall;
  external DLL name '_pxParticleEmitSetTDAcceleration@16';
procedure nxParticleEmitSetScaleFactor(pEmitter: Integer; radius: single;
  rate: single); cdecl; stdcall;
  external DLL name '_pxParticleEmitSetScaleFactor@12';
function nxParticleEmitDeleteFirstParticle(pEmitter: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxParticleEmitDeleteFirstParticle@4';
procedure nxParticleEmitDeleteParticle(pEmitter: Integer; particle: Integer);
  cdecl; stdcall; external DLL name '_pxParticleEmitDeleteParticle@8';

function nxParticleEmitGetAngDamping(pEmitter: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleEmitGetAngDamping@4';
function nxParticleEmitGetLinDamping(pEmitter: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleEmitGetLinDamping@4';
function nxParticleEmitGetMass(pEmitter: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleEmitGetMass@4';
function nxParticleEmitGetRadius(pEmitter: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleEmitGetRadius@4';
function nxParticleEmitGetPositionX(pEmitter: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleEmitGetPositionX@4';
function nxParticleEmitGetPositionY(pEmitter: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleEmitGetPositionY@4';
function nxParticleEmitGetPositionZ(pEmitter: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleEmitGetPositionZ@4';
function nxParticleEmitGetRotationPitch(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetRotationPitch@4';
function nxParticleEmitGetRotationYaw(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetRotationYaw@4';
function nxParticleEmitGetRotationRoll(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetRotationRoll@4';
function nxParticleEmitGetRandRadius(pEmitter: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleEmitGetRandRadius@4';
function nxParticleEmitGetStartSpeedMax(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetStartSpeedMax@4';
function nxParticleEmitGetStartSpeedMin(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetStartSpeedMin@4';
function nxParticleEmitGetTDAccelerationX(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetTDAccelerationX@4';
function nxParticleEmitGetTDAccelerationY(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetTDAccelerationY@4';
function nxParticleEmitGetTDAccelerationZ(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetTDAccelerationZ@4';
function nxParticleEmitGetScaleFactorRadius(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetScaleFactorRadius@4';
function nxParticleEmitGetScaleFactorRate(pEmitter: Integer): single; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetScaleFactorRate@4';
function nxParticleEmitGetNumberParticles(pEmitter: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxParticleEmitGetNumberParticles@4';

function nxParticleEmitAddParticle(pEmitter: Integer; entity: Integer): Integer;
  cdecl; stdcall; external DLL name '_pxParticleEmitAddParticle@8';
procedure nxParticleEmitDelete(pEmitter: Integer); cdecl; stdcall;
  external DLL name '_pxParticleEmitDelete@4';
procedure nxParticleUpdateEmitter(pEmitter: Integer); cdecl; stdcall;
  external DLL name '_pxParticleUpdateEmitter@4';
function nxParticleGetEntity(particle: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxParticleGetEntity@4';
function nxParticleGetBody(particle: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxParticleGetBody@4';
function nxParticleGetradius(particle: Integer): single; cdecl; stdcall;
  external DLL name '_pxParticleGetradius@4';

procedure nxKinematicSet(body: Integer); cdecl; stdcall;
  external DLL name '_pxKinematicSet@4';
procedure nxKinematicClear(body: Integer); cdecl; stdcall;
  external DLL name '_pxKinematicClear@4';
procedure nxKinematicMove(body: Integer; x: single; y: single; z: single);
  cdecl; stdcall; external DLL name '_pxKinematicMove@16';
procedure nxKinematicSetPosition(body: Integer; x: single; y: single;
  z: single); cdecl; stdcall; external DLL name '_pxKinematicSetPosition@16';
procedure nxKinematicSetRotation(body: Integer; pitch: single; yaw: single;
  roll: single); cdecl; stdcall; external DLL name '_pxKinematicSetRotation@16';

function nxCreateMagnet(minforce: single; middleforce: single; maxforce: single)
  : Integer; cdecl; stdcall; external DLL name '_pxCreateMagnet@12';
procedure nxMagnetActivate(mdata: Integer; mmode: Integer; fmode: Integer);
  cdecl; stdcall; external DLL name '_pxMagnetActivate@12';
procedure nxMagnetSetPosition(mdata: Integer; pos_x: single; pos_y: single;
  pos_z: single); cdecl; stdcall; external DLL name '_pxMagnetSetPosition@16';
procedure nxMagnetSetMaxRadius(mdata: Integer; radius: single); cdecl; stdcall;
  external DLL name '_pxMagnetSetMaxRadius@8';
procedure nxMagnetSetMinRadius(mdata: Integer; radius: single); cdecl; stdcall;
  external DLL name '_pxMagnetSetMinRadius@8';
procedure nxMagnetSetMaxForce(mdata: Integer; force: single); cdecl; stdcall;
  external DLL name '_pxMagnetSetMaxForce@8';
procedure nxMagnetSetMidForce(mdata: Integer; force: single); cdecl; stdcall;
  external DLL name '_pxMagnetSetMidForce@8';
procedure nxMagnetSetMinForce(mdata: Integer; force: single); cdecl; stdcall;
  external DLL name '_pxMagnetSetMinForce@8';
procedure nxMagnetSetMask(mdata: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxMagnetSetMask@8';
function nxMagnetGetPositionX(mdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxMagnetGetPositionX@4';
function nxMagnetGetPositionY(mdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxMagnetGetPositionY@4';
function nxMagnetGetPositionZ(mdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxMagnetGetPositionZ@4';
function nxMagnetGetMaxRadius(mdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxMagnetGetMaxRadius@4';
function nxMagnetGetMinRadius(mdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxMagnetGetMinRadius@4';
function nxMagnetGetMaxForce(mdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxMagnetGetMaxForce@4';
function nxMagnetGetMidForce(mdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxMagnetGetMidForce@4';
function nxMagnetGetMinForce(mdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxMagnetGetMinForce@4';
function nxMagnetGetMask(mdata: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxMagnetGetMask@4';
procedure nxMagnetDelete(mdata: Integer); cdecl; stdcall;
  external DLL name '_pxMagnetDelete@4';

function nxCreateWaterCirPlane(radius: single; depth: single): Integer; cdecl;
  stdcall; external DLL name '_pxCreateWaterCirPlane@8';
function nxCreateWaterRectPlane(width: single; height: single; depth: single)
  : Integer; cdecl; stdcall; external DLL name '_pxCreateWaterRectPlane@12';
function nxCreateWaterInfinPlane(depth: single): Integer; cdecl; stdcall;
  external DLL name '_pxCreateWaterInfinPlane@4';
procedure nxWaterSetDimension(water: Integer; width: single; height: single);
  cdecl; stdcall; external DLL name '_pxWaterSetDimension@12';
procedure nxWaterSetRadius(water: Integer; radius: single); cdecl; stdcall;
  external DLL name '_pxWaterSetRadius@8';
procedure nxWaterSetPosition(water: Integer; pos_x: single; pos_y: single;
  pos_z: single); cdecl; stdcall; external DLL name '_pxWaterSetPosition@16';
procedure nxWaterSetRotation(water: Integer; angle: single); cdecl; stdcall;
  external DLL name '_pxWaterSetRotation@8';
procedure nxWaterSetFluxion(water: Integer; fl_x: single; fl_y: single;
  fl_z: single); cdecl; stdcall; external DLL name '_pxWaterSetFluxion@16';
function nxWaterGetWidth(water: Integer): single; cdecl; stdcall;
  external DLL name '_pxWaterGetWidth@4';
function nxWaterGetHeight(water: Integer): single; cdecl; stdcall;
  external DLL name '_pxWaterGetHeight@4';
function nxWaterGetRadius(water: Integer): single; cdecl; stdcall;
  external DLL name '_pxWaterGetRadius@4';
function nxWaterGetPositionX(water: Integer): single; cdecl; stdcall;
  external DLL name '_pxWaterGetPositionX@4';
function nxWaterGetPositionY(water: Integer): single; cdecl; stdcall;
  external DLL name '_pxWaterGetPositionY@4';
function nxWaterGetPositionZ(water: Integer): single; cdecl; stdcall;
  external DLL name '_pxWaterGetPositionZ@4';
function nxWaterGetRotation(water: Integer): single; cdecl; stdcall;
  external DLL name '_pxWaterGetRotation@4';
function nxWaterGetDepth(water: Integer): single; cdecl; stdcall;
  external DLL name '_pxWaterGetDepth@4';
procedure nxWaterDelete(water: Integer); cdecl; stdcall;
  external DLL name '_pxWaterDelete@4';

function nxCreateKep(buo: single; radius: single; maxdis: single): Integer;
  cdecl; stdcall; external DLL name '_pxCreateKep@12';
procedure nxKepAddToBody(kdata: Integer; body: Integer); cdecl; stdcall;
  external DLL name '_pxKepAddToBody@8';
procedure nxKepSetLocalPosition(kdata: Integer; pos_x: single; pos_y: single;
  pos_z: single); cdecl; stdcall; external DLL name '_pxKepSetLocalPosition@16';
procedure nxKepSetGlobalPosition(kdata: Integer; pos_x: single; pos_y: single;
  pos_z: single); cdecl; stdcall;
  external DLL name '_pxKepSetGlobalPosition@16';
procedure nxKepSetAngularDamping(kdata: Integer; angdamp: single); cdecl;
  stdcall; external DLL name '_pxKepSetAngularDamping@8';
procedure nxKepSetLinearDamping(kdata: Integer; lindamp: single); cdecl;
  stdcall; external DLL name '_pxKepSetLinearDamping@8';
procedure nxWaterUpdate(water: Integer); cdecl; stdcall;
  external DLL name '_pxWaterUpdate@4';
function nxKepGetPosX(kdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxKepGetPosX@4';
function nxKepGetPosY(kdata: Integer): single cdecl; stdcall;
  external DLL name '_pxKepGetPosY@4';
function nxKepGetPosZ(kdata: Integer): single; cdecl; stdcall;
  external DLL name '_pxKepGetPosZ@4';
function nxKepGetNumber(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxKepGetNumber@4';
function nxKepGetKepFromBody(body: Integer; num: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxKepGetKepFromBody@8';

function nxTriggerCreateCube(dx: single; dy: single; dz: single): Integer;
  cdecl; stdcall; external DLL name '_pxTriggerCreateCube@12';
function nxTriggerCreateSphere(radius: single): Integer; cdecl; stdcall;
  external DLL name '_pxTriggerCreateSphere@4';
function nxTriggerCreateCapsule(height: single; radius: single): Integer; cdecl;
  stdcall; external DLL name '_pxTriggerCreateCapsule@8';
function nxTriggerCreateCylinder(radius: single; height: single;
  nbEdge: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxTriggerCreateCylinder@12';
function nxTriggerCreateHull(vbank: Integer; nvert: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxTriggerCreateHull@8';
procedure nxTriggerSetPosition(body: Integer; x: single; y: single; z: single);
  cdecl; stdcall; external DLL name '_pxTriggerSetPosition@16';
procedure nxTriggerSetRotation(body: Integer; pitch: single; yaw: single;
  roll: single); cdecl; stdcall; external DLL name '_pxTriggerSetRotation@16';
function nxTriggerGetNumBody(trigger: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxTriggerGetNumBody@4';
function nxTriggerGetBody(trigger: Integer; num: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxTriggerGetBody@8';
function nxTriggerGetBodyTrigger(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxTriggerGetBodyTrigger@4';
procedure nxUpdateTriggers(); cdecl; stdcall;
  external DLL name '_pxUpdateTriggers@0';

function nxCreateCompoundDesc(): Integer; cdecl; stdcall;
  external DLL name '_pxCreateCompoundDesc@0';
function nxCompoundAddCubeShape(compoundDesc: Integer; dx: single; dy: single;
  dz: single): Integer; cdecl; stdcall;
  external DLL name '_pxCompoundAddCubeShape@16';
function nxCompoundAddSphereShape(compoundDesc: Integer; radius: single)
  : Integer; cdecl; stdcall; external DLL name '_pxCompoundAddSphereShape@8';
function nxCompoundAddCapsuleShape(compoundDesc: Integer; radius: single;
  height: single): Integer; cdecl; stdcall;
  external DLL name '_pxCompoundAddCapsuleShape@12';
function nxCompoundAddCylinderShape(compoundDesc: Integer; radius: single;
  height: single; nbEdge: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxCompoundAddCylinderShape@16';
function nxCompoundAddHullShape(compoundDesc: Integer; vbank: Integer;
  nvert: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxCompoundAddHullShape@12';
procedure nxCompoundSetShapePos(shape: Integer; x: single; y: single;
  z: single); cdecl; stdcall; external DLL name '_pxCompoundSetShapePos@16';
procedure nxCompoundSetShapeRot(shape: Integer; pitch: single; yaw: single;
  roll: single); cdecl; stdcall; external DLL name '_pxCompoundSetShapeRot@16';
function nxCreateCompound(compoundDesc: Integer; mass: single): Integer; cdecl;
  stdcall; external DLL name '_pxCreateCompound@8';
function nxCreateStaticCompound(compoundDesc: Integer; mass: single): Integer;
  cdecl; stdcall; external DLL name '_pxCreateStaticCompound@8';

function nxCreateCloth(entity: Integer; surf: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxCreateCloth@8';
function nxCreateTearableCloth(entity: Integer; surf: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxCreateTearableCloth@8';
function nxCreateMetalCloth(entity: Integer; surf: Integer; coreActor: Integer;
  impThr: single; depth: single): Integer; cdecl; stdcall;
  external DLL name '_pxCreateMetalCloth@20';
function nxCreateClothSpec(vbank: psingle; fbank: PInteger;
  MESH_NBVERTICES: Integer; MESH_NBFACES: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxCreateClothSpec@16';
function nxCreateTearebleClothSpec(vbank: Integer; fbank: Integer;
  MESH_NBVERTICES: Integer; MESH_NBFACES: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxCreateTearebleClothSpec@16';
function nxCreateMetalClothSpec(vbank: Integer; fbank: Integer;
  MESH_NBVERTICES: Integer; MESH_NBFACES: Integer; coreActor: Integer;
  impThr: single; depth: single): Integer; cdecl; stdcall;
  external DLL name '_pxCreateMetalClothSpec@28';

function nxClothGetAttachmentResponseCoefficient(Cloth: Integer): single; cdecl;
  stdcall; external DLL name '_pxClothGetAttachmentResponseCoefficient@4';
function nxClothGetAttachmentTearFactor(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetAttachmentTearFactor@4';
function nxClothGetBendingStiffness(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetBendingStiffness@4';
function nxClothGetCollisionResponseCoefficient(Cloth: Integer): single; cdecl;
  stdcall; external DLL name '_pxClothGetCollisionResponseCoefficient@4';
function nxClothGetDampingCoefficient(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetDampingCoefficient@4';
function nxClothGetDensity(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetDensity@4';
function nxClothGetFriction(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetFriction@4';
function nxClothGetPressure(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetPressure@4';
function nxClothGetSleepLinearVelocity(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetSleepLinearVelocity@4';
function nxClothGetStretchingStiffness(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetStretchingStiffness@4';
function nxClothGetTearFactor(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetTearFactor@4';
function nxClothGetThickness(Cloth: Integer): single; cdecl; stdcall;
  external DLL name '_pxClothGetThickness@4';
function nxClothGetNumVertices(Cloth: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxClothGetNumVertices@4';
function nxClothIsSleeping(Cloth: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxClothIsSleeping@4';
function nxClothGetEntity(Cloth: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxClothGetEntity@4';
function nxClothGetUserData(Cloth: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxClothGetUserData@4';
procedure nxClothPutToSleep(Cloth: Integer); cdecl; stdcall;
  external DLL name '_pxClothPutToSleep@4';
procedure nxClothWakeUp(Cloth: Integer); cdecl; stdcall;
  external DLL name '_pxClothWakeUp@4';
procedure nxClothSetGravity(Cloth: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxClothSetGravity@8';

procedure nxClothSetTeareble(Cloth: Integer); cdecl; stdcall;
  external DLL name '_pxClothSetTeareble@4';
procedure nxClothSetTearFactor(Cloth: Integer; coef: single); cdecl; stdcall;
  external DLL name '_pxClothSetTearFactor@8';

function nxClothSetAttachmentResponseCoefficient(Cloth: Integer; coef: single)
  : single; cdecl; stdcall;
  external DLL name '_pxClothSetAttachmentResponseCoefficient@8';
function nxClothSetAttachmentTearFactor(Cloth: Integer; coef: single): single;
  cdecl; stdcall; external DLL name '_pxClothSetAttachmentTearFactor@8';
procedure nxClothSetBending(Cloth: Integer); cdecl; stdcall;
  external DLL name '_pxClothSetBending@4';
function nxClothSetBendingStiffness(Cloth: Integer; coef: single): single;
  cdecl; stdcall; external DLL name '_pxClothSetBendingStiffness@8';
function nxClothSetCollisionResponseCoefficient(Cloth: Integer; coef: single)
  : single; cdecl; stdcall;
  external DLL name '_pxClothSetCollisionResponseCoefficient@8';
function nxClothSetDampingCoefficient(Cloth: Integer; coef: single): single;
  cdecl; stdcall; external DLL name '_pxClothSetDampingCoefficient@8';
function nxClothSetComDampingCoefficient(Cloth: Integer; coef: single): single;
  cdecl; stdcall; external DLL name '_pxClothSetComDampingCoefficient@8';
function nxClothSetFriction(Cloth: Integer; coef: single): single; cdecl;
  stdcall; external DLL name '_pxClothSetFriction@8';
function nxClothSetPressure(Cloth: Integer; coef: single): single; cdecl;
  stdcall; external DLL name '_pxClothSetPressure@8';
function nxClothSetSleepLinearVelocity(Cloth: Integer; coef: single): single;
  cdecl; stdcall; external DLL name '_pxClothSetSleepLinearVelocity@8';
function nxClothSetStretchingStiffness(Cloth: Integer; coef: single): single;
  cdecl; stdcall; external DLL name '_pxClothSetStretchingStiffness@8';
function nxClothSetThickness(Cloth: Integer; coef: single): single; cdecl;
  stdcall; external DLL name '_pxClothSetThickness@8';
procedure nxClothSetUserData(Cloth: Integer; userdata: Integer); cdecl; stdcall;
  external DLL name '_pxClothSetUserData@8';
procedure nxClothMaskSet(Cloth: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxClothMaskSet@8';
procedure nxClothMaskCombineSet(Cloth: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxClothMaskCombineSet@8';
procedure nxClothsSetCollision(Cloth: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxClothsSetCollision@8';
procedure nxClothsSetGravity(Cloth: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxClothsSetGravity@8';

procedure nxClothSetVertexPos(Cloth: Integer; vbank: Integer; numVert: Integer);
  cdecl; stdcall; external DLL name '_pxClothSetVertexPos@12';
procedure nxClothGetVertexPos(Cloth: Integer; vbank: psingle; numVert: Integer);
  cdecl; stdcall; external DLL name '_pxClothGetVertexPos@12';
procedure nxClothGetNormals(Cloth: Integer; nbank: Integer); cdecl; stdcall;
  external DLL name '_pxClothGetNormals@8';

procedure nxClothAttachVertexToPos(Cloth: Integer; vID: Integer; pos_x: single;
  pos_y: single; pos_z: single); cdecl; stdcall;
  external DLL name '_pxClothAttachVertexToPos@20';
procedure nxClothAttachToCollidingShapes(Cloth: Integer; flag: Integer); cdecl;
  stdcall; external DLL name '_pxClothAttachToCollidingShapes@8';
procedure nxClothAttachToCore(Cloth: Integer; body: Integer; Threshold: single;
  depth: single); cdecl; stdcall; external DLL name '_pxClothAttachToCore@16';
procedure nxClothAttachToShape(Cloth: Integer; body: Integer; flag: Integer);
  cdecl; stdcall; external DLL name '_pxClothAttachToShape@12';
procedure nxClothDetachFromShape(Cloth: Integer; body: Integer); cdecl; stdcall;
  external DLL name '_pxClothDetachFromShape@8';
procedure nxClothFreeVertex(Cloth: Integer; vID: Integer); cdecl; stdcall;
  external DLL name '_pxClothFreeVertex@8';
procedure nxClothAddForceAtPos(Cloth: Integer; pos_x: single; pos_y: single;
  pos_z: single; magnitude: single; radius: single); cdecl; stdcall;
  external DLL name '_pxClothAddForceAtPos@24';
procedure nxClothAddForceAtVertex(Cloth: Integer; nx: single; ny: single;
  nz: single; vID: Integer); cdecl; stdcall;
  external DLL name '_pxClothAddForceAtVertex@20';
procedure nxUpdateCloth(); cdecl; stdcall; external DLL name '_pxUpdateCloth@0';
procedure nxClothDelete(Cloth: Integer); cdecl; stdcall;
  external DLL name '_pxClothDelete@4';

procedure nxClothGetTearVertex(Cloth: Integer; vbank: Integer); cdecl; stdcall;
  external DLL name '_pxClothGetTearVertex@8';
procedure nxClothGetTearIndices(Cloth: Integer; ibank: Integer); cdecl; stdcall;
  external DLL name '_pxClothGetTearIndices@8';
procedure nxClothGetTearPIndices(Cloth: Integer; ibank: Integer); cdecl;
  stdcall; external DLL name '_pxClothGetTearPIndices@8';
function nxClothGetNumTearVertex(Cloth: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxClothGetNumTearVertex@4';
function nxClothGetNumTearTris(Cloth: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxClothGetNumTearTris@4';

function nxCreateSpringAndDamperEffector(body1: Integer; body2: Integer)
  : Integer; cdecl; stdcall;
  external DLL name '_pxCreateSpringAndDamperEffector@8';
procedure nxSetLinearSpring(spring: Integer; SpringRelaxed: single;
  CompressForce: single; CompressSaturate: single; StretchForce: single;
  StretchSaturate: single); cdecl; stdcall;
  external DLL name '_pxSetLinearSpring@24';
procedure nxSetLinearDamper(spring: Integer; CompressForce: single;
  CompressSaturate: single; StretchForce: single; StretchSaturate: single);
  cdecl; stdcall; external DLL name '_pxSetLinearDamper@20';
procedure nxDeleteEffector(effector: Integer); cdecl; stdcall;
  external DLL name '_pxDeleteEffector@4'

  function nxWheelAddToBody(body: Integer; pos_x: single; pos_y: single;
  pos_z: single): Integer; cdecl; stdcall;
  external DLL name '_pxWheelAddToBody@16';
procedure nxWheelSetRadius(wheel: Integer; radius: single); cdecl; stdcall;
  external DLL name '_pxWheelSetRadius@8';
procedure nxWheelSetRotation(wheel: Integer; pitch: single; yaw: single;
  roll: single); cdecl; stdcall; external DLL name '_pxWheelSetRotation@16';
procedure nxWheelSetMotorTorque(wheel: Integer; torque: single); cdecl; stdcall;
  external DLL name '_pxWheelSetMotorTorque@8';
procedure nxWheelSetSteerAngle(wheel: Integer; angle: single); cdecl; stdcall;
  external DLL name '_pxWheelSetSteerAngle@8';
procedure nxWheelSetBrakeTorque(wheel: Integer; torque: single); cdecl; stdcall;
  external DLL name '_pxWheelSetBrakeTorque@8';
procedure nxWheelSetSuspension(wheel: Integer; susp: single; rest: single;
  damping: single); cdecl; stdcall;
  external DLL name '_pxWheelSetSuspension@16';
procedure nxWheelSetFrictionToSide(wheel: Integer; friction: single); cdecl;
  stdcall; external DLL name '_pxWheelSetFrictionToSide@8';
procedure nxWheelSetFrictionToFront(wheel: Integer; friction: single); cdecl;
  stdcall; external DLL name '_pxWheelSetFrictionToFront@8';
procedure nxWheelSetCollisionGroup(wheel: Integer; group: Integer); cdecl;
  stdcall; external DLL name '_pxWheelSetCollisionGroup@8';

function nxWheelGetSteerAngle(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetSteerAngle@4';
function nxWheelGetAxleSpeed(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetAxleSpeed@4';
function nxWheelGetRadius(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetRadius@4';
function nxWheelGetSuspensionTravel(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetSuspensionTravel@4';
function nxWheelGetSuspensionRestitution(wheel: Integer): single; cdecl;
  stdcall; external DLL name '_pxWheelGetSuspensionRestitution@4';
function nxWheelGetSuspensionDamping(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetSuspensionDamping@4';

function nxWheelGetPositionX(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetPositionX@4';
function nxWheelGetPositionY(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetPositionY@4';
function nxWheelGetPositionZ(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetPositionZ@4';
procedure nxWheelUpdateSpec(wheel: Integer; mode: Integer); cdecl; stdcall;
  external DLL name '_pxWheelUpdateSpec@8';
function nxWheelGetPositionXSpec(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetPositionXSpec@4';
function nxWheelGetPositionYSpec(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetPositionYSpec@4';
function nxWheelGetPositionZSpec(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetPositionZSpec@4';
function nxWheelGetRotationPitchSpec(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetRotationPitchSpec@4';
function nxWheelGetRotationYawSpec(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetRotationYawSpec@4';
function nxWheelGetRotationRollSpec(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetRotationRollSpec@4';

function nxWheelGetContactX(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactX@4';
function nxWheelGetContactY(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactY@4';
function nxWheelGetContactZ(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactZ@4';
function nxWheelGetContactForce(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactForce@4';
function nxWheelGetContactLatDirectionX(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactLatDirectionX@4';
function nxWheelGetContactLatDirectionY(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactLatDirectionY@4';
function nxWheelGetContactLatDirectionZ(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactLatDirectionZ@4';
function nxWheelGetContactLatImpulse(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactLatImpulse@4';
function nxWheelGetContactLonDirectionX(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactLonDirectionX@4';
function nxWheelGetContactLonDirectionY(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactLonDirectionY@4';
function nxWheelGetContactLonDirectionZ(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactLonDirectionZ@4';
function nxWheelGetContactLonImpulse(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetContactLonImpulse@4';
function nxWheelGetContactMaterial(wheel: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxWheelGetContactMaterial@4';
function nxWheelGetFrictionToSide(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetFrictionToSide@4';
function nxWheelGetFrictionToFront(wheel: Integer): single; cdecl; stdcall;
  external DLL name '_pxWheelGetFrictionToSide@4';

procedure nxWheelSetMask(wheel: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxWheelSetMask@8';
procedure nxWheelSetMaskCombine(wheel: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxWheelSetMaskCombine@8';
procedure nxWheelClearMask(wheel: Integer); cdecl; stdcall;
  external DLL name '_pxWheelClearMask@4';
procedure nxWheelDelete(wheel: Integer); cdecl; stdcall;
  external DLL name '_pxWheelDelete@4';
function nxWheelSetEntity(entity: Integer; wheel: Integer; step: Integer)
  : single; cdecl; stdcall; external DLL name '_pxWheelSetEntity@12';

function nxJointCreateSuspFront(body0: Integer; body1: Integer; x: single;
  y: single; z: single): Integer; cdecl; stdcall;
  external DLL name '_pxJointCreateSuspFront@20';
function nxJointCreateSuspBack(body0: Integer; body1: Integer; x: single;
  y: single; z: single): Integer; cdecl; stdcall;
  external DLL name '_pxJointCreateSuspBack@20';
procedure nxJointSuspSetSteerN(joint: Integer; x: single; y: single; z: single);
  cdecl; stdcall; external DLL name '_pxJointSuspSetSteerN@16';
procedure nxJointSuspSetTurnN(joint: Integer; x: single; y: single; z: single);
  cdecl; stdcall; external DLL name '_pxJointSuspSetTurnN@16';
procedure nxJointSuspSetLinLimit(joint: Integer; lim: single); cdecl; stdcall;
  external DLL name '_pxJointSuspSetLinLimit@8';
procedure nxJointSuspSetLinParameter(joint: Integer; spring: single;
  rest: single; damp: single); cdecl; stdcall;
  external DLL name '_pxJointSuspSetLinParameter@16';
procedure nxJointSuspSetAngLimit(joint: Integer; lim: single); cdecl; stdcall;
  external DLL name '_pxJointSuspSetAngLimit@8';
procedure nxJointSuspSetAngParameter(joint: Integer; spring: single;
  rest: single; damp: single); cdecl; stdcall;
  external DLL name '_pxJointSuspSetAngParameter@16';
procedure nxJointSuspSetAngle(joint: Integer; angle: single); cdecl; stdcall;
  external DLL name '_pxJointSuspSetAngle@8';
procedure nxJointSuspSetSpeed(joint: Integer; speed: single); cdecl; stdcall;
  external DLL name '_pxJointSuspSetSpeed@8';
procedure nxJointSuspSetBrake(joint: Integer; mode: Integer); cdecl; stdcall;
  external DLL name '_pxJointSuspSetBrake@8';

procedure nxCCDSkeletonEnable(mode: Integer); cdecl; stdcall;
  external DLL name '_pxCCDSkeletonEnable@4';
procedure nxCCDSkeletonSetEpsilon(eps: single); cdecl; stdcall;
  external DLL name '_pxCCDSkeletonSetEpsilon@4';
procedure nxBodySetCCDSkeleton(body: Integer; x: single; y: single; z: single);
  cdecl; stdcall; external DLL name '_pxBodySetCCDSkeleton@16';
procedure nxBodySetCCDSkeletonThreshold(body: Integer; thres: single); cdecl;
  stdcall; external DLL name '_pxBodySetCCDSkeletonThreshold@8';
procedure nxBodySetFlagCCDSkeletonDynamic(body: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFlagCCDSkeletonDynamic@4';
function nxBodyGetCCDSkeleton(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxBodyGetCCDSkeleton@4';
procedure nxBodyDeleteCCDSkeleton(ccd: Integer); cdecl; stdcall;
  external DLL name '_pxBodyDeleteCCDSkeleton@4';

function nxCreateRay(): Integer; cdecl; stdcall;
  external DLL name '_pxCreateRay@0';
procedure nxRaySetDir(ray: Integer; nx: single; ny: single; nz: single); cdecl;
  stdcall; external DLL name '_pxRaySetDir@16';
procedure nxRaySetPosition(ray: Integer; x: single; y: single; z: single);
  cdecl; stdcall; external DLL name '_pxRaySetPosition@16';
function nxRayGetDistance(ray: Integer; mode: Integer): single; cdecl; stdcall;
  external DLL name '_pxRayGetDistance@8';
function nxRayGetBody(ray: Integer; mode: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxRayGetBody@8';
function nxRayGetMaterial(ray: Integer; mode: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxRayGetMaterial@8';
function nxRayGetPickX(ray: Integer; mode: Integer): single; cdecl; stdcall;
  external DLL name '_pxRayGetPickX@8';
function nxRayGetPickY(ray: Integer; mode: Integer): single; cdecl; stdcall;
  external DLL name '_pxRayGetPickY@8';
function nxRayGetPickZ(ray: Integer; mode: Integer): single; cdecl; stdcall;
  external DLL name '_pxRayGetPickZ@8';
function nxRayGetPickNX(ray: Integer; mode: Integer): single; cdecl; stdcall;
  external DLL name '_pxRayGetPickNX@8';
function nxRayGetPickNY(ray: Integer; mode: Integer): single; cdecl; stdcall;
  external DLL name '_pxRayGetPickNY@8';
function nxRayGetPickNZ(ray: Integer; mode: Integer): single; cdecl; stdcall;
  external DLL name '_pxRayGetPickNZ@8';

function nxRegWriteDriverPath(str: PChar): Integer; cdecl; stdcall;
  external DLL name '_pxRegWriteDriverPath@4';
function nxRegWriteString(RootKey: Integer; Path: PChar; name: PChar;
  Data: PChar): Integer; cdecl; stdcall;
  external DLL name '_pxRegWriteString@16';
function nxRegWriteInt(RootKey: Integer; Path: PChar; name: PChar;
  Data: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxRegWriteInt@16';
function nxRegReadInt(RootKey: Integer; Path: PChar; name: PChar): Integer;
  cdecl; stdcall; external DLL name '_pxRegReadInt@12';
function nxRegReadString(RootKey: Integer; Path: PChar; name: PChar): PChar;
  cdecl; stdcall; external DLL name '_pxRegReadString@12';
function nxRegDeleteValue(RootKey: Integer; Path: PChar; name: PChar): Integer;
  cdecl; stdcall; external DLL name '_pxRegDeleteValue@12';
function nxRegDeleteKey(RootKey: Integer; Path: PChar; name: PChar): Integer;
  cdecl; stdcall; external DLL name '_pxRegDeleteKey@12';

function nxGetContacts(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxGetContacts@4';
function nxContactGetBody(body: Integer; coll: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxContactGetBody@8';
function nxContactGetPointX(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetPointX@8';
function nxContactGetPointY(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetPointY@8';
function nxContactGetPointZ(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetPointZ@8';
function nxContactGetPointNX(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetPointNX@8';
function nxContactGetPointNY(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetPointNY@8';
function nxContactGetPointNZ(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetPointNZ@8';
function nxContactGetForceN(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetForceN@8';
function nxContactGetForceNX(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetForceNX@8';
function nxContactGetForceNY(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetForceNY@8';
function nxContactGetForceNZ(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetForceNZ@8';
function nxContactGetForceT(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetForceT@8';
function nxContactGetForceTX(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetForceTX@8';
function nxContactGetForceTY(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetForceTY@8';
function nxContactGetForceTZ(body: Integer; coll: Integer): single; cdecl;
  stdcall; external DLL name '_pxContactGetForceTZ@8';
function nxContactEventsOnStartTouch(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxContactEventsOnStartTouch@4';
function nxContactEventsOnEndTouch(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxContactEventsOnEndTouch@4';

procedure nxMaskSet(body: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxMaskSet@8';
procedure nxMaskCombineSet(body: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxMaskCombineSet@8';
procedure nxMaskClear(body: Integer); cdecl; stdcall;
  external DLL name '_pxMaskClear@4';
procedure nxBodySetCollisionGroup(body: Integer; group: Integer); cdecl;
  stdcall; external DLL name '_pxBodySetCollisionGroup@8';
procedure nxBodySetCollisionGroupPair(group1: Integer; group2: Integer); cdecl;
  stdcall; external DLL name '_pxBodySetCollisionGroupPair@8';
procedure nxBodySetCollisionGroupFlag(group1: Integer; group2: Integer;
  flag: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetCollisionGroupFlag@12';
procedure nxBodySetMagnetMask(body: Integer; mask: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetMagnetMask@8';
function nxBodyGetMagnetMask(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxBodyGetMagnetMask@4';

procedure nxBodySetFrozen(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozen@8';
procedure nxBodySetFrozenRotX(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozenRotX@8';
procedure nxBodySetFrozenRotY(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozenRotY@8';
procedure nxBodySetFrozenRotZ(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozenRotZ@8';
procedure nxBodySetFrozenRot(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozenRot@8';
procedure nxBodySetFrozenPosX(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozenPosX@8';
procedure nxBodySetFrozenPosY(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozenPosY@8';
procedure nxBodySetFrozenPosZ(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozenPosZ@8';
procedure nxBodySetFrozenPos(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFrozenPos@8';
procedure nxBodySetFlagGravity(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFlagGravity@8';
procedure nxBodySetFlagCollision(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFlagCollision@8';
procedure nxBodySetFlagResponse(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFlagResponse@8';
procedure nxBodySetFlagContacttable(body: Integer; stat: Integer); cdecl;
  stdcall; external DLL name '_pxBodySetFlagContacttable@8';
procedure nxBodySetFlagMagniteble(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFlagMagniteble@8';
procedure nxBodySetFlagTriggertable(body: Integer; stat: Integer); cdecl;
  stdcall; external DLL name '_pxBodySetFlagTriggertable@8';
procedure nxBodySetFlagRayCast(body: Integer; stat: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetFlagRayCast@8';

procedure nxDeleteBody(num: Integer); cdecl; stdcall;
  external DLL name '_pxDeleteBody@4';
procedure nxDeleteJoint(joint: Integer); cdecl; stdcall;
  external DLL name '_pxDeleteJoint@4';

function nxJointCreateHinge(body1: Integer; body2: Integer; x: single;
  y: single; z: single; nx: single; ny: single; nz: single): Integer; cdecl;
  stdcall; external DLL name '_pxJointCreateHinge@32';
function nxJointCreateSpherical(body1: Integer; body2: Integer; x: single;
  y: single; z: single; nx: single; ny: single; nz: single): Integer; cdecl;
  stdcall; external DLL name '_pxJointCreateSpherical@32';
function nxJointCreateDistance(body1: Integer; body2: Integer; p1_x: single;
  p1_y: single; p1_z: single; p2_x: single; p2_y: single; p2_z: single)
  : Integer; cdecl; stdcall; external DLL name '_pxJointCreateDistance@32';
function nxJointCreateFixed(body1: Integer; body2: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxJointCreateFixed@8';
function nxJointCreateCylindrical(body1: Integer; body2: Integer; x: single;
  y: single; z: single; nx: single; ny: single; nz: single; min_limit: single;
  max_limit: single): Integer; cdecl; stdcall;
  external DLL name '_pxJointCreateCylindrical@40';
function nxJointCreatePrismatic(body1: Integer; body2: Integer; x: single;
  y: single; z: single; nx: single; ny: single; nz: single; min_limit: single;
  max_limit: single): Integer; cdecl; stdcall;
  external DLL name '_pxJointCreatePrismatic@40';
function nxJointCreateOnLine(body1: Integer; x: single; y: single; z: single;
  nx: single; ny: single; nz: single): Integer; cdecl; stdcall;
  external DLL name '_pxJointCreateOnLine@28';
function nxJointCreateInPlane(body1: Integer; x: single; y: single; z: single;
  nx: single; ny: single; nz: single): Integer; cdecl; stdcall;
  external DLL name '_pxJointCreateInPlane@28';
function nxJointCreatePulley(body1: Integer; body2: Integer; distance: single;
  stiff: single; k_ratio: single): Integer; cdecl; stdcall;
  external DLL name '_pxJointCreatePulley@20';

function nxJointCreateD6Joint(body1: Integer; body2: Integer; x: single;
  y: single; z: single; nx: single; ny: single; nz: single): Integer; cdecl;
  stdcall; external DLL name '_pxJointCreateD6Joint@32';
procedure nxD6JointSetPosMotion(joint: Integer; xMot: Integer; yMot: Integer;
  zMot: Integer); cdecl; stdcall; external DLL name '_pxD6JointSetPosMotion@16';
procedure nxD6JointSetAngleMotion(joint: Integer; twistMot: Integer;
  swing1Mot: Integer; swing2Mot: Integer); cdecl; stdcall;
  external DLL name '_pxD6JointSetAngleMotion@16';
procedure nxD6JointSetLinearLimit(joint: Integer; lim: single); cdecl; stdcall;
  external DLL name '_pxD6JointSetLinearLimit@8';
procedure nxD6JointSetSwingLimit(joint: Integer; swing1Lim: single;
  swing2Lim: single); cdecl; stdcall;
  external DLL name '_pxD6JointSetSwingLimit@12';
procedure nxD6JointSetTwistLimit(joint: Integer; lowLim: single;
  heighLim: single); cdecl; stdcall;
  external DLL name '_pxD6JointSetTwistLimit@12';

procedure nxJointHingeSetLimit(joint: Integer; min: single; max: single); cdecl;
  stdcall; external DLL name '_pxJointHingeSetLimit@12';
procedure nxJointHingeSetSpring(joint: Integer; spr: single; targetVal: single);
  cdecl; stdcall; external DLL name '_pxJointHingeSetSpring@12';
procedure nxJointSphericalSetLimitAngle(joint: Integer; angle: single;
  hardn: single; restit: single); cdecl; stdcall;
  external DLL name '_pxJointSphericalSetLimitAngle@16';
procedure nxJointSphericalSetLimitTwist(joint: Integer; mintwist: single;
  maxtwist: single; spr: single; damp: single; targetVal: single); cdecl;
  stdcall; external DLL name '_pxJointSphericalSetLimitTwist@24';
procedure nxJointSphericalSetLimitSpring(joint: Integer; spr: single;
  damp: single; targetVal: single); cdecl; stdcall;
  external DLL name '_pxJointSphericalSetLimitSpring@16';
procedure nxJointDistanceSetPoint(joint: Integer; mindist: single;
  maxdist: single); cdecl; stdcall;
  external DLL name '_pxJointDistanceSetPoint@12';
procedure nxJointDistanceSetSpring(joint: Integer; spr: single; damp: single);
  cdecl; stdcall; external DLL name '_pxJointDistanceSetSpring@12';
procedure nxJointAddLimitPlane(joint: Integer; x: single; y: single; z: single;
  nx: single; ny: single; nz: single); cdecl; stdcall;
  external DLL name '_pxJointAddLimitPlane@28';
procedure nxJointPulleySetAnchor(joint: Integer; x1: single; y1: single;
  z1: single; x2: single; y2: single; z2: single); cdecl; stdcall;
  external DLL name '_pxJointPulleySetAnchor@28';
procedure nxJointPulleySetLocalAttachBody(joint: Integer; x1: single;
  y1: single; z1: single; x2: single; y2: single; z2: single); cdecl; stdcall;
  external DLL name '_pxJointPulleySetLocalAttachBody@28';
procedure nxJointHingeSetMotor(joint: Integer; force: single;
  velTarget: single); cdecl; stdcall;
  external DLL name '_pxJointHingeSetMotor@12';
procedure nxJointSetBreakable(joint: Integer; force: single; torque: single);
  cdecl; stdcall; external DLL name '_pxJointSetBreakable@12';
function nxJointIsBroken(joint: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxJointIsBroken@4';
procedure nxJointHingeSetCollision(joint: Integer); cdecl; stdcall;
  external DLL name '_pxJointHingeSetCollision@4';
procedure nxJointSphericalSetCollision(joint: Integer); cdecl; stdcall;
  external DLL name '_pxJointSphericalSetCollision@4';
procedure nxJointDistanceSetCollision(joint: Integer); cdecl; stdcall;
  external DLL name '_pxJointDistanceSetCollision@4';
procedure nxJointCylindricalSetCollision(joint: Integer); cdecl; stdcall;
  external DLL name '_pxJointCylindricalSetCollision@4';

procedure nxSetGravity(gx: single; gy: single; gz: single); cdecl; stdcall;
  external DLL name '_pxSetGravity@12';
procedure nxBodySetMass(num: Integer; mass: single); cdecl; stdcall;
  external DLL name '_pxBodySetMass@8';
procedure nxBodySetCMassLocalPosition(num: Integer; x: single; y: single;
  z: single); cdecl; stdcall;
  external DLL name '_pxBodySetCMassLocalPosition@16';
procedure nxBodySetCMassGlobalPosition(num: Integer; x: single; y: single;
  z: single); cdecl; stdcall;
  external DLL name '_pxBodySetCMassGlobalPosition@16';
procedure nxBodySetMassSpaceInertiaTensor(num: Integer; x: single; y: single;
  z: single); cdecl; stdcall;
  external DLL name '_pxBodySetMassSpaceInertiaTensor@16';
function nxBodyGetCMassGlobalPositionX(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetCMassGlobalPositionX@4';
function nxBodyGetCMassGlobalPositionY(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetCMassGlobalPositionY@4';
function nxBodyGetCMassGlobalPositionZ(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetCMassGlobalPositionZ@4';
function nxBodyGetCMassLocalPositionX(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetCMassLocalPositionX@4';
function nxBodyGetCMassLocalPositionY(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetCMassLocalPositionY@4';
function nxBodyGetCMassLocalPositionZ(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetCMassLocalPositionZ@4';
function nxBodyGetMass(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetMass@4';

procedure nxBodySetMyForce(body: Integer; lx: single; ly: single; lz: single);
  cdecl; stdcall; external DLL name '_pxBodySetMyForce@16';
procedure nxBodyAddForce(num: Integer; vx: single; vy: single; vz: single;
  mode: Integer); cdecl; stdcall; external DLL name '_pxBodyAddForce@20';
procedure nxBodyAddForceAtPos(num: Integer; vx: single; vy: single; vz: single;
  px: single; py: single; pz: single; mode: Integer); cdecl; stdcall;
  external DLL name '_pxBodyAddForceAtPos@32';
procedure nxBodyAddForceAtLocalPos(body: Integer; vx: single; vy: single;
  vz: single; px: single; py: single; pz: single; mode: Integer); cdecl;
  stdcall; external DLL name '_pxBodyAddForceAtLocalPos@32';
procedure nxBodyAddLocalForce(num: Integer; vx: single; vy: single; vz: single;
  mode: Integer); cdecl; stdcall; external DLL name '_pxBodyAddLocalForce@20';
procedure nxBodyAddLocalForceAtPos(num: Integer; vx: single; vy: single;
  vz: single; px: single; py: single; pz: single; mode: Integer); cdecl;
  stdcall; external DLL name '_pxBodyAddLocalForceAtPos@32';
procedure nxBodyAddLocalForceAtLocalPos(body: Integer; vx: single; vy: single;
  vz: single; px: single; py: single; pz: single; mode: Integer); cdecl;
  stdcall; external DLL name '_pxBodyAddLocalForceAtLocalPos@32';

procedure nxBodyAddTorque(body: Integer; vx: single; vy: single; vz: single;
  mode: Integer); cdecl; stdcall; external DLL name '_pxBodyAddTorque@20';
procedure nxBodyAddLocalTorque(body: Integer; vx: single; vy: single;
  vz: single; mode: Integer); cdecl; stdcall;
  external DLL name '_pxBodyAddLocalTorque@20';
procedure nxBodySetAngularSpeed(body: Integer; ax: single; ay: single;
  az: single); cdecl; stdcall; external DLL name '_pxBodySetAngularSpeed@16';
procedure nxBodySetLinearSpeed(body: Integer; lx: single; ly: single;
  lz: single); cdecl; stdcall; external DLL name '_pxBodySetLinearSpeed@16';
procedure nxBodySetLocalLinearSpeed(body: Integer; lx: single; ly: single;
  lz: single); cdecl; stdcall;
  external DLL name '_pxBodySetLocalLinearSpeed@16';
procedure nxBodySetLocalAngularSpeed(body: Integer; lx: single; ly: single;
  lz: single); cdecl; stdcall;
  external DLL name '_pxBodySetLocalAngularSpeed@16';
procedure nxBodySetAngularDamping(body: Integer; angdamp: single); cdecl;
  stdcall; external DLL name '_pxBodySetAngularDamping@8';
procedure nxBodySetLinearDamping(body: Integer; lindamp: single); cdecl;
  stdcall; external DLL name '_pxBodySetLinearDamping@8';
procedure nxBodySetAngularMomentum(body: Integer; ax: single; ay: single;
  az: single); cdecl; stdcall; external DLL name '_pxBodySetAngularMomentum@16';
procedure nxBodySetLinearMomentum(body: Integer; lx: single; ly: single;
  lz: single); cdecl; stdcall; external DLL name '_pxBodySetLinearMomentum@16';
procedure nxBodySetMaxAngularSpeed(body: Integer; speed: single); cdecl;
  stdcall; external DLL name '_pxBodySetMaxAngularSpeed@8';
procedure nxBodySetSleepEnergyThreshold(body: Integer; tres: single); cdecl;
  stdcall; external DLL name '_pxBodySetSleepEnergyThreshold@8';
procedure nxBodySetSleepAngularVelocity(body: Integer; tres: single); cdecl;
  stdcall; external DLL name '_pxBodySetSleepAngularVelocity@8';
procedure nxBodySetSleepLinearVelocity(body: Integer; tres: single); cdecl;
  stdcall; external DLL name '_pxBodySetSleepLinearVelocity@8';
procedure nxBodySetSleepWakeUp(body: Integer; wakeCounterValue: single); cdecl;
  stdcall; external DLL name '_pxBodySetSleepWakeUp@8';
procedure nxBodySetSleepPut(body: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetSleepPut@4';
procedure nxBodySetSolverIterationCount(body: Integer; itercount: Integer);
  cdecl; stdcall; external DLL name '_pxBodySetSolverIterationCount@8';
procedure nxBodySetBodyName(body: Integer; name: PChar); cdecl; stdcall;
  external DLL name '_pxBodySetBodyName@8';
procedure nxBodySetBodyEntity(body: Integer; entity: Integer); cdecl; stdcall;
  external DLL name '_pxBodySetBodyEntity@8';
procedure nxBodySetBodyUserData(body: Integer; userdata: Integer); cdecl;
  stdcall; external DLL name '_pxBodySetBodyUserData@8';
procedure nxMoveBodyToPoint(body: Integer; maxspeed: single; x: single;
  y: single; z: single); cdecl; stdcall;
  external DLL name '_pxMoveBodyToPoint@20';
procedure nxAddBodytoBody(body1: Integer; body2: Integer); cdecl; stdcall;
  external DLL name '_pxAddBodytoBody@8';
function nxCopyBody(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxCopyBody@4';

function nxCreateMaterial(): Integer; cdecl; stdcall;
  external DLL name '_pxCreateMaterial@0';
procedure nxMaterialSetToBody(body: Integer; mat: Integer); cdecl; stdcall;
  external DLL name '_pxMaterialSetToBody@8';
procedure nxMaterialSetRestitution(mat: Integer; rest: single); cdecl; stdcall;
  external DLL name '_pxMaterialSetRestitution@8';
procedure nxMaterialSetRestitutionCombineMode(mat: Integer; mode: Integer);
  cdecl; stdcall; external DLL name '_pxMaterialSetRestitutionCombineMode@8';
procedure nxMaterialSetStFriction(mat: Integer; fric: single); cdecl; stdcall;
  external DLL name '_pxMaterialSetStFriction@8';
procedure nxMaterialSetDyFriction(mat: Integer; fric: single); cdecl; stdcall;
  external DLL name '_pxMaterialSetDyFriction@8';
procedure nxMaterialSetFrictionV(mat: Integer; sfric: single; dfric: single);
  cdecl; stdcall; external DLL name '_pxMaterialSetFrictionV@12';
procedure nxMaterialSetFrictionCombineMode(mat: Integer; mode: Integer); cdecl;
  stdcall; external DLL name '_pxMaterialSetFrictionCombineMode@8';
function nxCreateAnisotripicMaterial(nx: single; ny: single; nz: single)
  : Integer; cdecl; stdcall;
  external DLL name '_pxCreateAnisotripicMaterial@12';
procedure nxMaterialSetFlagStrongFriction(mat: Integer); cdecl; stdcall;
  external DLL name '_pxMaterialSetFlagStrongFriction@4';
function nxGetMaterial(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxGetMaterial@4';

procedure nxBodySetPosition(num: Integer; pos_x: single; pos_y: single;
  pos_z: single); cdecl; stdcall; external DLL name '_pxBodySetPosition@16';
procedure nxBodySetRotation(num: Integer; pitch: single; yaw: single;
  roll: single); cdecl; stdcall; external DLL name '_pxBodySetRotation@16';

function nxBodyGetPositionX(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetPositionX@4';
function nxBodyGetPositionY(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetPositionY@4';
function nxBodyGetPositionZ(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetPositionZ@4';
function nxBodyGetRotationPitch(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetRotationPitch@4';
function nxBodyGetRotationYaw(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetRotationYaw@4';
function nxBodyGetRotationRoll(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetRotationRoll@4';
function nxBodyGetAngularSpeed(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetAngularSpeed@4';
function nxBodyGetAngularSpeedX(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetAngularSpeedX@4';
function nxBodyGetAngularSpeedY(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetAngularSpeedY@4';
function nxBodyGetAngularSpeedZ(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetAngularSpeedZ@4';
function nxBodyGetLocalAngularSpeedX(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalAngularSpeedX@4';
function nxBodyGetLocalAngularSpeedY(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalAngularSpeedY@4';
function nxBodyGetLocalAngularSpeedZ(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalAngularSpeedZ@4';
function nxBodyGetLinearSpeed(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearSpeed@4';
function nxBodyGetLinearSpeedX(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearSpeedX@4';
function nxBodyGetLinearSpeedY(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearSpeedY@4';
function nxBodyGetLinearSpeedZ(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearSpeedZ@4';
function nxBodyGetLocalLinearSpeedX(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalLinearSpeedX@4';
function nxBodyGetLocalLinearSpeedY(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalLinearSpeedY@4';
function nxBodyGetLocalLinearSpeedZ(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalLinearSpeedZ@4';
function nxBodyGetLinearVecSpeedX(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearVecSpeedX@4';
function nxBodyGetLinearVecSpeedY(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearVecSpeedY@4';
function nxBodyGetLinearVecSpeedZ(num: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearVecSpeedZ@4';
function nxBodyGetLocalPointSpeed(body: Integer; x: single; y: single;
  z: single): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalPointSpeed@16';
function nxBodyGetLocalPointSpeedX(body: Integer; x: single; y: single;
  z: single): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalPointSpeedX@16';
function nxBodyGetLocalPointSpeedY(body: Integer; x: single; y: single;
  z: single): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalPointSpeedY@16';
function nxBodyGetLocalPointSpeedZ(body: Integer; x: single; y: single;
  z: single): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLocalPointSpeedZ@16';

function nxBodyGetAngularDamping(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetAngularDamping@4';
function nxBodyGetLinearDamping(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearDamping@4';
function nxBodyGetAngularMomentum(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetAngularMomentum@4';
function nxBodyGetLinearMomentum(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetLinearMomentum@4';
function nxBodyGetMaxAngularVelocity(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetMaxAngularVelocity@4';
function nxGetBodyName(body: Integer): PChar; cdecl; stdcall;
  external DLL name '_pxGetBodyName@4';
function nxGetBodyEntity(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxGetBodyEntity@4';
function nxGetBodyUserData(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxGetBodyUserData@4';
function nxBodyGetSleepAngularVelocity(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetSleepAngularVelocity@4';
function nxBodyGetSleepLinearVelocity(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetSleepLinearVelocity@4';
function nxBodyIsDynamic(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxBodyIsDynamic@4';
function nxBodyIsSleeping(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxBodyIsSleeping@4';

function nxGetNumberAllBody(): Integer; cdecl; stdcall;
  external DLL name '_pxGetNumberAllBody@0';
function pxCreateWorld(plane: Integer; password: PChar): Integer; cdecl;
  stdcall; external DLL name '_pxCreateWorld@8';
function nxCreateWorldWithoutDrivers(plane: Integer; password: PChar): Integer;
  cdecl; stdcall; external DLL name '_pxCreateWorldWithoutDrivers@8';
procedure nxDestroyWorld(); cdecl; stdcall;
  external DLL name '_pxDestroyWorld@0';
procedure nxRenderPhysic(time: single; sinc: Integer); cdecl; stdcall;
  external DLL name '_pxRenderPhysic@8';
procedure nxSetTiming(maxTimeStep: single; maxIter: Integer;
  StepMethod: Integer); cdecl; stdcall; external DLL name '_pxSetTiming@12';
procedure nxSDKSetParameter(param: single); cdecl; stdcall;
  external DLL name '_pxSDKSetParameter@4';
procedure nxSetPause(pause: Integer); cdecl; stdcall;
  external DLL name '_pxSetPause@4';
function nxChekPPU(): Integer; cdecl; stdcall; external DLL name '_pxChekPPU@0';
function nxChekPPUMode(): Integer; cdecl; stdcall;
  external DLL name '_pxChekPPUMode@0';
procedure nxSetHardwareSimulation(mode: Integer); cdecl; stdcall;
  external DLL name '_pxSetHardwareSimulation@4';
procedure nxDriverSetMode(mode: Integer); cdecl; stdcall;
  external DLL name '_pxDriverSetMode@4';

function nxCreateScene(): Integer; cdecl; stdcall;
  external DLL name '_pxCreateScene@0';
procedure nxSceneChange(scene: Integer); cdecl; stdcall;
  external DLL name '_pxSceneChange@4';

function nxBodySetEntity(entity: Integer; body: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxBodySetEntity@8';
procedure nxBodyAddEntity(entity: Integer; body: Integer); cdecl; stdcall;
  external DLL name '_pxBodyAddEntity@8';
function nxDX9BodySetEntity(entity: Integer; body: Integer): Integer; cdecl;
  stdcall; external DLL name '_pxDX9BodySetEntity@8';

function nxBufferCreate(size: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxBufferCreate@4';
function nxIntBufferCreate(size: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxIntBufferCreate@4';
procedure nxBufferSetElement(buff: Integer; nelem: Integer; elem: single);
  cdecl; stdcall; external DLL name '_pxBufferSetElement@12';
function nxBufferGetElement(buff: Integer; elem: Integer): single; cdecl;
  stdcall; external DLL name '_pxBufferGetElement@8';
procedure nxBufferDelete(buff: Integer); cdecl; stdcall;
  external DLL name '_pxBufferDelete@4';

function peeksint(addr: Integer): Integer; cdecl; stdcall;
  external DLL name '_peeksint@4';

function nxGetAngleBetweenVec(v1x: single; v1y: single; v1z: single;
  v2x: single; v2y: single; v2z: single): single; cdecl; stdcall;
  external DLL name '_pxGetAngleBetweenVec@24';

function nxBodyGetMatrix(body: Integer): Integer; cdecl; stdcall;
  external DLL name '_pxBodyGetMatrix@4';
procedure nxBodyDeleteMatrix(mat: Integer); cdecl; stdcall;
  external DLL name '_pxBodyDeleteMatrix@4';
function nxBodyGetMatrixElement(mat: Integer; r: Integer; c: Integer): single;
  cdecl; stdcall; external DLL name '_pxBodyGetMatrixElement@12';
function nxBodyGetRotationQuatX(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetRotationQuatX@4';
function nxBodyGetRotationQuatY(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetRotationQuatY@4';
function nxBodyGetRotationQuatZ(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetRotationQuatZ@4';
function nxBodyGetRotationQuatW(body: Integer): single; cdecl; stdcall;
  external DLL name '_pxBodyGetRotationQuatW@4';

function nxCreateWorld(plane: Boolean): Integer;
function nxCreateTriMeshFromFreeForm(ff: TGLFreeForm): Integer;
function nxCreateClothFromFreeForm(ff: TGLFreeForm): Integer;
procedure nxPositionSceneObject(SceneObject: TGLBaseSceneObject;
  PhysXObject: Integer);
procedure nxDrawCloth(ff: TGLFreeForm; Cloth: Integer);
function nxCreateHullFromFreeForm(ff: TGLFreeForm; mass: Integer): Integer;

// -----------------------------------------------------------------------
// -----------------------------------------------------------------------
// -----------------------------------------------------------------------
implementation

// -----------------------------------------------------------------------
// -----------------------------------------------------------------------
// -----------------------------------------------------------------------

function nxCreateWorld(plane: Boolean): Integer;
begin
  Result := pxCreateWorld(Integer(plane), PChar(''));
end;

procedure nxDrawCloth(ff: TGLFreeForm; Cloth: Integer);
var
  vertpos: array of TAffineVector;
  i, vertcount: Integer;
begin
  vertcount := nxClothGetNumVertices(Cloth);
  SetLength(vertpos, vertcount);
  nxClothGetVertexPos(Cloth, @vertpos[0], vertcount);
  for i := 0 to vertcount - 1 do
    ff.MeshObjects[0].Vertices[i] := vertpos[i];
  ff.StructureChanged;
  vertpos := nil;
end;

function nxCreateTriMeshFromFreeForm(ff: TGLFreeForm): Integer;
var
  i: Integer;
  Vertices: array of TAffineVector;
  indices: array of Integer;
begin
  SetLength(Vertices, ff.MeshObjects[0].Vertices.Count);
  SetLength(indices, ff.MeshObjects[0].FaceGroups[0].TriangleCount * 3);

  for i := 0 to ff.MeshObjects[0].Vertices.Count - 1 do
    Vertices[i] := ff.MeshObjects[0].Vertices[i];

  for i := 0 to ff.MeshObjects[0].FaceGroups[0].TriangleCount - 1 do
  begin
    with (ff.MeshObjects[0].FaceGroups[0] as TFGVertexIndexList) do
    begin
      indices[i * 3] := VertexIndices[i * 3 + 0];
      indices[i * 3 + 1] := VertexIndices[i * 3 + 1];
      indices[i * 3 + 2] := VertexIndices[i * 3 + 2];
    end;
  end;

  Result := nxCreateTriMesh(@Vertices[0], @indices[0],
    ff.MeshObjects[0].Vertices.Count, ff.MeshObjects[0].FaceGroups[0]
    .TriangleCount, 0);
  Vertices := nil;
  indices := nil;
end;

function nxCreateHullFromFreeForm(ff: TGLFreeForm; mass: Integer): Integer;
var
  i: Integer;
  Vertices: array of TAffineVector;
begin
  SetLength(Vertices, ff.MeshObjects[0].Vertices.Count);

  for i := 0 to ff.MeshObjects[0].Vertices.Count - 1 do
    Vertices[i] := ff.MeshObjects[0].Vertices[i];

  Result := nxBodyCreateHull(@Vertices[0],
    ff.MeshObjects[0].Vertices.Count, mass);
  Vertices := nil;
end;

function nxCreateClothFromFreeForm(ff: TGLFreeForm): Integer;
var
  i: Integer;
  Vertices: array of TAffineVector;
  indices: array of Integer;
begin
  SetLength(Vertices, ff.MeshObjects[0].Vertices.Count);
  SetLength(indices, ff.MeshObjects[0].FaceGroups[0].TriangleCount * 3);

  for i := 0 to ff.MeshObjects[0].Vertices.Count - 1 do
    Vertices[i] := ff.MeshObjects[0].Vertices[i];

  for i := 0 to ff.MeshObjects[0].FaceGroups[0].TriangleCount - 1 do
  begin
    with (ff.MeshObjects[0].FaceGroups[0] as TFGVertexIndexList) do
    begin
      indices[i * 3] := VertexIndices[i * 3 + 0];
      indices[i * 3 + 1] := VertexIndices[i * 3 + 1];
      indices[i * 3 + 2] := VertexIndices[i * 3 + 2];
    end;
  end;

  Result := nxCreateClothSpec(@Vertices[0], @indices[0],
    ff.MeshObjects[0].Vertices.Count, ff.MeshObjects[0].FaceGroups[0]
    .TriangleCount);
  Vertices := nil;
  indices := nil;
end;

procedure nxPositionSceneObject(SceneObject: TGLBaseSceneObject;
  PhysXObject: Integer);
var
  q: TQuaternion;
begin
  MakeVector(q.ImagPart, nxBodyGetRotationQuatX(PhysXObject),
    nxBodyGetRotationQuatY(PhysXObject), nxBodyGetRotationQuatZ(PhysXObject));
  q.RealPart := nxBodyGetRotationQuatW(PhysXObject);
  SceneObject.Matrix := QuaternionToMatrix(q);
  if SceneObject is TGLCapsule then
    SceneObject.pitch(90);
  SceneObject.Position.SetPoint(nxBodyGetPositionX(PhysXObject),
    nxBodyGetPositionY(PhysXObject), nxBodyGetPositionZ(PhysXObject));
end;

end.

{*************************************************************************
 *                                                                       *
 * Open Dynamics Engine, Copyright (C) 2001,2002 Russell L. Smith.       *
 * All rights reserved.  Email: russ@q12.org   Web: www.q12.org          *
 *                                                                       *
 * This library is free software; you can redistribute it and/or         *
 * modify it under the terms of EITHER:                                  *
 *   (1) The GNU Lesser General Public License as published by the Free  *
 *       Software Foundation; either version 2.1 of the License, or (at  *
 *       your option) any later version. The text of the GNU Lesser      *
 *       General Public License is included with this library in the     *
 *       file LICENSE.TXT.                                               *
 *   (2) The BSD-style license that is included with this library in     *
 *       the file LICENSE-BSD.TXT.                                       *
 *                                                                       *
 * This library is distributed in the hope that it will be useful,       *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the files    *
 * LICENSE.TXT and LICENSE-BSD.TXT for more details.                     *
 *                                                                       *
 *************************************************************************}

{*************************************************************************
 *                                                                       *
 * ODE Delphi Import unit : 0.8.6                                        *
 *                                                                       *
 *   Created by Mattias Fagerlund ( mattias@cambrianlabs.com )  and      *
 *              Christophe ( chroma@skynet.be ) Hosten                   *
 *                                                                       *
 *  Special thanks to Eric Grange for his help                           *
 *                                                                       *
 *  All real work was of course performed by Russell L. Smith,           *
 *    who created ODE.                                                   *
 *                                                                       *
 *  Convertion started 2002-09-16.                                       *
 *                                                                       *
 *  There is no Delphi documentation for ODE, see the original ODE files *
 *  for information http://opende.sourceforge.net/ode-docs.html          *
 *                                                                       *
 *************************************************************************}

 {
  Some notes;

  Sometimes it's easier and faster to refer to the members of the objects
  directly, like Body.Pos, Geom.Data or Body.Mass, instead of calling the built
  in routines. Be very careful if you do this, because some of the built in
  routines alter states when called.

  Examples

  Geom.Body := Body; // DON'T DO THIS
  bGeomSetBody(Geom, Body); // This method must be used

  Setting the mass of a body is another example. Typically, reading members is
  fine, but before writing directly to member fields yourself, you should check
  the c source and see what the function/procedure does. The source can be found
  at http://www.q12.org/ode/

  ***********************************************************************

  Change history

  2004.02.18 - CH - New single and double dll
  2003.09.08 - CH - New single and double dll. Now handles cones and terrain
  2003.07.23 - CH - New single dll, now handles Plane2D
  2003.07.18 - CH - Added set and get UniversalParam, new dll deployed
  2003.07.07 - MF - DelphiODE now defaults to Single precision, because TriMesh
    only works with Single precision. We're hoping this will be corrected in the
    future
  2003.07.05 - CH - updated file to support new tri-collider code
  2003.06.13 - CH - Creted new DLL, adding dWorldStepFast and
    dJointTypePlane2D
  2003.06.12 - MF - fixed Single support, which was slightly broken
  2003.06.10 - MF - Removed GeomTransformGroup as they're not in
    the DLL.
  2003.02.11 - JV - added syntax enforcement on some enumerated types
  2003.02.01 - CH - removed dGeomGroup and all it's procedures / functions
    due to deprecation
  2003.02.01 - MF - added a few new functions
  2003.01.20 - CH - compiled a new DLL and added the new functions.
  2002.10.31 - CH - compiled a new dll version, with some minor updates to
    friction among other things
  2002.10.10 - MF - added the functions needed to support cylinder and
    GeomTransformGroup.
  2002.10.10 - CH - compiled a new DLL with the new cylinder geom and the
    new GeomTransformGroup.
  2002.09.25 - MF - I'm having issues with the single precision DLL, it seems less
    stable. DelphiODE will still default to double precision. The goal is single
    precision though, so the Tri-Collider can be used.
  2002.09.24 - CH - New Single and Double precision DLLs created
  2002.09.22 - DL - Preliminary Linux support added by
  2002.09.16 - MF & CH - Conversion started

  MF = Mattias Fagerlund
  CH = Christophe Hosten (Chroma)
  JV = John Villar
  DL = Dominique Louis
 }

unit ODEImport;

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Types,
  Libc,
  {$ENDIF}
  Classes;

const
  {$IFDEF WIN32}
  ODEDLL = 'ode.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  ODEDLL = 'libode.so';
  {$ENDIF}
  {$IFDEF MACOS}
  ODEDLL = 'libode.dylib';
  {$ENDIF}

type
  // ********************************************************************
  // ********************************************************************
  //
  //   ODE precision:
  //
  //   ODE can be run in Single or Double precision, Single is less precise,
  //   but requires less memory.
  //
  //   If you choose to run in Single mode, you must deploy the single precision
  //   dll (this is default)
  //
  //   If you choose to run in Double mode, you must delpoy the double precision
  //   dll (named ode-Double.dll and located in the dll directory)
  //
  //  A major issue is that Single and Double support different function sets,
  //  but we gope this will be sorted out in the future. The TriList only
  //  works in single mode, and the Plane2D joint only works in double mode.

  {$define cPLANE2D} // Use Plane2D (now exists in both single and double)
  {$define cSINGLE}  // Add a "$" before "define" to make DelphiODE single based

  {$ifdef cSINGLE}
    {$define cTRILIST} // Use TriList (currently only exists in single dll)
    TdReal = single;
  {$else}
    TdReal = double;
  {$endif}

  PdReal = ^TdReal;

  {define cODEDebugEnabled} // Debug mode


  // Pointers to internal ODE structures. These I haven't been able to reproduce
  // in delphi, they're C++ classes. 
  TODEID = cardinal;
  TdJointID = TODEID;
  TdJointGroupID = TODEID;

  TdRealArray = array[0..15] of TdReal;
  PdRealArray = ^TdRealArray;

  // typedef dReal dVector3[4];
  // Q: Why isn't TdVector3 = array[0..2] of TdReal?
  // A: Because of SIMD alignment.
  TdVector3 = array[0..3] of TdReal;
  PdVector3 = ^TdVector3;

  // typedef dReal dVector4[4];
  TdVector4 = array[0..3] of TdReal;
  PdVector4 = ^TdVector4;

  // typedef dReal dMatrix3[4*3];
  TdMatrix3 = array[0..4*3-1] of TdReal;
  PdMatrix3 = ^TdMatrix3;

  TdMatrix3_As3x4 = array[0..2, 0..3] of TdReal;

  // typedef dReal dMatrix4[4*4];
  TdMatrix4 = array[0..4*4-1] of TdReal;
  PdMatrix4 = ^TdMatrix4;

  // typedef dReal dMatrix6[8*6];
  TdMatrix6 = array[0..8*6-1] of TdReal;
  PdMatrix6 = ^TdMatrix6;

  // typedef dReal dQuaternion[4];
  TdQuaternion = TdVector4;//array[0..3] of TdReal;
  PdQuaternion = ^TdQuaternion;

  // No typedef for AABB
  TdAABB = array[0..5] of TdReal;


(*enum {
  dxBodyFlagFiniteRotation = 1,		// use finite rotations
  dxBodyFlagFiniteRotationAxis = 2,	// use finite rotations only along axis
  dxBodyDisabled = 4			// body is disabled
};*)


// Delphi 5 can't handle enums like this :(
(*  TBodyFlags =
    (dxBodyFlagFiniteRotation = 1,		// use finite rotations
    dxBodyFlagFiniteRotationAxis = 2,	// use finite rotations only along axis
    dxBodyDisabled = 4,			          // body is disabled
    dxBodyNoGravity = 8);             // body is not influenced by gravity*)

(* Change: New Type added, syntax enforcement *)
  TBodyFlags = Integer;

(* These consts now have defined types *)
const
  dxBodyFlagFiniteRotation: TBodyFlags = 1;		  // use finite rotations
  dxBodyFlagFiniteRotationAxis: TBodyFlags = 2;	// use finite rotations only along axis
  dxBodyDisabled: TBodyFlags = 4;			          // body is disabled
  dxBodyNoGravity: TBodyFlags = 8;              // body is not influenced by gravity

(*typedef struct dMass {
  dReal mass;   // total mass of the rigid body
  dVector4 c;   // center of gravity position in body frame (x,y,z)
  dMatrix3 I;   // 3x3 inertia tensor in body frame, about POR
} dMass;*)

type
  TdMass = record
    mass : TdReal; // total mass of the rigid body
    c : TdVector4; // center of gravity position in body frame (x,y,z)
    I : TdMatrix3; // 3x3 inertia tensor in body frame, about POR
  end;
  PdMass = ^TdMass;

(*struct dBase {
  void *operator new (size_t size) { return dAlloc (size); }
  void operator delete (void *ptr, size_t size) { dFree (ptr,size); }
  void *operator new[] (size_t size) { return dAlloc (size); }
  void operator delete[] (void *ptr, size_t size) { dFree (ptr,size); }
};

struct dObject : public dBase {
  dxWorld *world;		// world this object is in
  dObject *next;		// next object of this type in list
  dObject **tome;		// pointer to previous object's next ptr
  void *userdata;		// user settable data
  int tag;			// used by dynamics algorithms
};*)


  PdxWorld = ^TdxWorld;

  PdObject = ^TdObject;
  PPdObject = ^PdObject;
  TdObject = record
    World     : PdxWorld;  // world this object is in
    next      : PdObject;	// next object of this type in list
    tome      : PPdObject;	// pointer to previous object's next ptr
    userdata  : pointer;		// user settable data
    tag       : integer;		// used by dynamics algorithms
  end;

(*struct dxBody : public dObject {
  dxJointNode *firstjoint;	// list of attached joints
  int flags;			  // some dxBodyFlagXXX flags
  dMass mass;			  // mass parameters about POR
  dMatrix3 invI;		// inverse of mass.I
  dReal invMass;		// 1 / mass.mass
  dVector3 pos;			// position of POR (point of reference)
  dQuaternion q;		// orientation quaternion
  dMatrix3 R;			  // rotation matrix, always corresponds to q
  dVector3 lvel,avel;		// linear and angular velocity of POR
  dVector3 facc,tacc;		// force and torque accululators
  dVector3 finite_rot_axis;	// finite rotation axis, unit length or 0=none
}; *)

  PdxBody = ^TdxBody;
  TdxBody = record
    BaseObject : TdObject;

    {$ifdef cSINGLE}
    Padding : byte;
    {$endif}

    firstjoint : TdJointID;	// list of attached joints
    flags : integer;			  // some dxBodyFlagXXX flags
    mass : TdMass;			    // mass parameters about POR
    invI : TdMatrix3 ;		  // inverse of mass.I
    invMass : TdReal;		    // 1 / mass.mass
    pos : TdVector3;			  // position of POR (point of reference)
    q : TdQuaternion;		    // orientation quaternion
    R : TdMatrix3;			    // rotation matrix, always corresponds to q
    lvel,avel : TdVector3;	// linear and angular velocity of POR
    facc,tacc : TdVector3 ;	// force and torque accululators
    finite_rot_axis : TdVector3 ;	// finite rotation axis, unit length or 0=none
  end;

  TBodyList = class(TList)
  private
    function GetItems(i: integer): PdxBody;
    procedure SetItems(i: integer; const Value: PdxBody);
  public
    property Items[i : integer] : PdxBody read GetItems write SetItems; default;
    procedure DeleteAllBodies;
  end;


(*struct dxWorld : public dBase {
  dxBody *firstbody;		// body linked list
  dxJoint *firstjoint;	// joint linked list
  int nb,nj;			      // number of bodies and joints in lists
  dVector3 gravity;		  // gravity vector (m/s/s)
  dReal global_erp;		  // global error reduction parameter
  dReal global_cfm;		  // global costraint force mixing parameter
};*)

  TdxWorld = record //(TdBase)
    firstbody : PdxBody;		// body linked list
    firstjoint : TdJointID;	// joint linked list
    nb,nj : integer;			  // number of bodies and joints in lists
    gravity : TdVector3;		// gravity vector (m/s/s)
    global_erp : TdReal;		// global error reduction parameter
    global_cfm : TdReal;		// global costraint force mixing parameter
  end;


(*  typedef struct dJointFeedback {
  dVector3 f1;       // force that joint applies to body 1
  dVector3 t1;       // torque that joint applies to body 1
  dVector3 f2;       // force that joint applies to body 2
  dVector3 t2;       // torque that joint applies to body 2
} dJointFeedback;)*)

  TdJointFeedback = record
    f1 : TdVector3;       // force that joint applies to body 1
    t1 : TdVector3;       // torque that joint applies to body 1
    f2 : TdVector3;       // force that joint applies to body 2
    t2 : TdVector3;       // torque that joint applies to body 2
  end;

  pTdJointFeedback = ^TdJointFeedback;

(*enum {
  d_ERR_UNKNOWN = 0,		/* unknown error */
  d_ERR_IASSERT,		/* internal assertion failed */
  d_ERR_UASSERT,		/* user assertion failed */
  d_ERR_LCP			/* user assertion failed */
};*)
  TdErrorType =
    (d_ERR_UNKNOWN,
     d_ERR_IASSERT,
     d_ERR_UASSERT,
     d_ERR_LCP { = 3//});


(*/* joint type numbers */

enum {
  dJointTypeNone = 0,		/* or "unknown" */
  dJointTypeBall,
  dJointTypeHinge,
  dJointTypeSlider,
  dJointTypeContact,
  dJointTypeHinge2,
  dJointTypeFixed,
  dJointTypeNull,
  dJointTypeAMotor
};*)

  TdJointTypeNumbers =
    (dJointTypeNone,		// or "unknown"
    dJointTypeBall,
    dJointTypeHinge,
    dJointTypeSlider,
    dJointTypeContact,
    dJointTypeHinge2,
    dJointTypeFixed,
    dJointTypeNull,
    dJointTypeAMotor,
    dJointTypePlane2D);

  TdAngularMotorModeNumbers =
    (dAMotorUser,
     dAMotorEuler);


// joint flags
(*enum {
  // if this flag is set, the joint was allocated in a joint group
  dJOINT_INGROUP = 1,

  // if this flag is set, the joint was attached with arguments (0,body).
  // our convention is to treat all attaches as (body,0), i.e. so node[0].body
  // is always nonzero, so this flag records the fact that the arguments were
  // swapped.
  dJOINT_REVERSE = 2,

  // if this flag is set, the joint can not have just one body attached to it,
  // it must have either zero or two bodies attached.
  dJOINT_TWOBODIES = 4
};*)
  //TJointFlag = (

(* Change: New Type added, syntax enforcement *)
  type
    TJointFlag = Integer;

(* These consts now have defined types *)
  const
    dJOINT_INGROUP: TJointFlag = 1;
    dJOINT_REVERSE: TJointFlag = 2;
    dJOINT_TWOBODIES: TJointFlag = 4;

  // Space constants
  const
    TYPE_SIMPLE = $bad;
    TYPE_HASH = $babe;


(*enum {
  dContactMu2		= 0x001,
  dContactFDir1		= 0x002,
  dContactBounce	= 0x004,
  dContactSoftERP	= 0x008,
  dContactSoftCFM	= 0x010,
  dContactMotion1	= 0x020,
  dContactMotion2	= 0x040,
  dContactSlip1		= 0x080,
  dContactSlip2		= 0x100,

  dContactApprox0	= 0x0000,
  dContactApprox1_1	= 0x1000,
  dContactApprox1_2	= 0x2000,
  dContactApprox1	= 0x3000
};*)
//  TdContactType = (
(* Change: New Type added, syntax enforcement *)
  type
    TdContactType = Integer;

(* These consts now have defined types *)
  const
    dContactMu2: TdContactType		    = $0001;
    dContactFDir1: TdContactType	  	= $0002;
    dContactBounce: TdContactType   	= $0004;
    dContactSoftERP: TdContactType  	= $0008;
    dContactSoftCFM: TdContactType  	= $0010;
    dContactMotion1: TdContactType  	= $0020;
    dContactMotion2: TdContactType	  = $0040;
    dContactSlip1: TdContactType		  = $0080;
    dContactSlip2: TdContactType		  = $0100;

    dContactApprox0: TdContactType	  = $00000;
    dContactApprox1_1: TdContactType	= $1000;
    dContactApprox1_2: TdContactType	= $2000;
    dContactApprox1: TdContactType	  = $3000;


(*  typedef struct dSurfaceParameters {
  /* must always be defined */
  int mode;
  dReal mu;

  /* only defined if the corresponding flag is set in mode */
  dReal mu2;
  dReal bounce;
  dReal bounce_vel;
  dReal soft_erp;
  dReal soft_cfm;
  dReal motion1,motion2;
  dReal slip1,slip2;
} dSurfaceParameters;*)

type
  TdSurfaceParameters = record
    // must always be defined
    mode : cardinal;
    mu : TdReal;

    // only defined if the corresponding flag is set in mode
    mu2,
    bounce,
    bounce_vel,
    soft_erp,
    soft_cfm,
    motion1,motion2,
    slip1,slip2 : TdReal
  end;

(*typedef struct dContactGeom {
  dVector3 pos;
  dVector3 normal;
  dReal depth;
  dGeomID g1,g2;
} dContactGeom;*)

  PdxGeom = ^TdxGeom;

  TdContactGeom = record
    pos : TdVector3;
    normal : TdVector3;
    depth : TdReal;
    g1,g2 : PdxGeom;
  end;

  PdContactGeom = ^TdContactGeom;

  (*struct dContact {
  dSurfaceParameters surface;
  dContactGeom geom;
  dVector3 fdir1;
};*)
  TdContact = record
    surface : TdSurfaceParameters;
    geom : TdContactGeom;
    fdir1 : TdVector3;
  end;

  // Collission callback structure
  TdNearCallback = procedure(data : pointer; o1, o2 : PdxGeom); cdecl;

  TdColliderFn = function(o1, o2 : PdxGeom; flags : Integer;
                  contact : PdContactGeom; skip : Integer) : Integer; cdecl;
  TdGetColliderFnFn = function(num : Integer) : TdColliderFn; cdecl;
  TdGetAABBFn = procedure(g : PdxGeom; var aabb : TdAABB); cdecl;
  TdGeomDtorFn = procedure(o : PdxGeom); cdecl;
  TdAABBTestFn = function(o1, o2 : PdxGeom; const aabb2 : TdAABB) : Integer; cdecl;

(*typedef struct dGeomClass {
  int bytes;
  dGetColliderFnFn *collider;
  dGetAABBFn *aabb;
  dAABBTestFn *aabb_test;
  dGeomDtorFn *dtor;
} dGeomClass;*)

  TdGeomClass = record
    bytes : integer;                 // extra storage size
    collider : TdGetColliderFnFn;    // collider function
    aabb : TdGetAABBFn;       // bounding box function
    aabb_test : TdAABBTestFn; // aabb tester, can be 0 for none
    dtor : TdGeomDtorFn;      // destructor, can be 0 for none
  end;

  PdGeomClass = ^TdGeomClass;

(*struct dxGeomClass {
 dGetColliderFnFn *collider;
  dGetAABBFn *aabb;
  dAABBTestFn *aabb_test;
  dGeomDtorFn *dtor;
  int num;             // class number
  int size;            // total size of object, including extra data area
};*)

  TdxGeomClass = record
    collider : TdGetColliderFnFn;    // collider function
    aabb : TdGetAABBFn;              // bounding box function
    aabb_test : TdAABBTestFn;        // aabb tester, can be 0 for none
    dtor  : TdGeomDtorFn;             // destructor, can be 0 for none
    num   : integer;             // class number
    size  : integer;            // total size of object, including extra data area
  end;

  PdxGeomClass = ^TdxGeomClass;

  (*// position vector and rotation matrix for geometry objects that are not
  // connected to bodies.

  struct dxPosR {
    dVector3 pos;
    dMatrix3 R;
  };*)

  TdxPosR = record
    pos : TdVector3;
    R : TdMatrix3;
  end;

  (*struct dxSpace : public dBase {
  int type;			// don't want to use RTTI
  virtual void destroy()=0;
  virtual void add (dGeomID)=0;
  virtual void remove (dGeomID)=0;
  virtual void collide (void *data, dNearCallback *callback)=0;
  virtual int query (dGeomID)=0;
};*)
  TdxSpace = record
    _Garbage : integer;
// * Explaining _Garbage *
// That member first appears in the TdxSpace record.  This is
// because the dxSpace struct declares its functions as virtual.  Since the
// class (a struct in C++ is considered a public class) now contains virtual
// functions (functions in dxBase are not virtual), it needs a vtable to know
// what functions to call in what class.  Visual C++ places the pointer to the
// vtable at offset 0, which is what your _Garbage field caters for.
//
// Steve 'Sly' Williams

    SpaceType : integer;
  end;

  PdxSpace = ^TdxSpace;


(*struct dxTriMeshData{
	Model BVTree;
	MeshInterface Mesh;

	dxTriMeshData();
	~dxTriMeshData();

	void Build(const void* Vertices, int VertexStide, int VertexCount, const void* Indices, int IndexCount, int TriStride);
};*)
  TdxTriMeshData = record
    unknown : byte; //
  end;

  PdxTriMeshData = ^TdxTriMeshData;

(*//simple space - reports all n^2 object intersections
struct dxSimpleSpace : public dxSpace {
  dGeomID first;
  void destroy();
  void add (dGeomID);
  void remove (dGeomID);
  void collide (void *data, dNearCallback *callback);
  int query (dGeomID);
};*)

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxSimpleSpace = record
    _Garbage : integer; // I don't know where this comes from!
    SpaceType : integer;
    First : PdxGeom;
  end;

  PdxSimpleSpace = ^TdxSimpleSpace;

(*//currently the space 'container' is just a list of the geoms in the space.
struct dxHashSpace : public dxSpace {
  dxGeom *first;
  int global_minlevel;	// smallest hash table level to put AABBs in
  int global_maxlevel;	// objects that need a level larger than this will be
			// put in a "big objects" list instead of a hash table
  void destroy();
  void add (dGeomID);
  void remove (dGeomID);
  void collide (void *data, dNearCallback *callback);
  int query (dGeomID);
};*)

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxHashSpace = record
    _Garbage : integer; // I don't know where this comes from!
    SpaceType : integer;
    First : PdxGeom;
    global_minlevel : integer;
    global_maxlevel : integer;
  end;

  PdxHashSpace = ^TdxHashSpace;

  (*typedef struct dGeomSpaceData {
  dGeomID next;
} dGeomSpaceData; *)

  TdGeomSpaceData = record
    next : PdxGeom;
  end;

  (*// common data for all geometry objects. the class-specific data area follows
  // this structure. pos and R will either point to a separately allocated
  // buffer (if body is 0 - pos points to the dxPosR object) or to the pos and
  // R of the body (if body nonzero).
  struct dxGeom {		// a dGeomID is a pointer to this
    dxGeomClass *_class;	// class of this object
    void *data;		// user data pointer
    dBodyID body;		// dynamics body associated with this object (if any)
    dReal *pos;		// pointer to object's position vector
    dReal *R;		// pointer to object's rotation matrix
    dSpaceID spaceid;	// the space this object is in
    dGeomSpaceData space;	// reserved for use by space this object is in
    dReal *space_aabb;	// ptr to aabb array held by dSpaceCollide() fn
    // class-specific data follows here, with proper alignment.
  };*)

  TdxGeom = record // a dGeomID is a pointer to this
    _class : PdxGeomClass;	// class of this object

    {$ifdef cSINGLE}
    Padding : array [0..4] of byte;
    {$else}
    Padding : array [0..2] of pointer;
    {$endif}

    data : pointer;		// user data pointer
    Body : PdxBody ;		// dynamics body associated with this object (if any)
    Pos : PdVector3;		// pointer to object's position vector
    R : PdMatrix3;		// pointer to object's rotation matrix
    spaceid : PdxSpace;	// the space this object is in
    space : TdGeomSpaceData ;	// reserved for use by space this object is in
    space_aabb : PdReal;	// ptr to aabb array held by dSpaceCollide() fn
    // class-specific data follows here, with proper alignment.
  end;

  TGeomList = class(TList)
  private
    function GetItems(i: integer): PdxGeom;
    procedure SetItems(i: integer; const Value: PdxGeom);
  public
    property Items[i : integer] : PdxGeom read GetItems write SetItems; default;
    procedure DeleteAllGeoms(DeleteDataAsObject : boolean=false);
  end;
(*
/* standard joint parameter names. why are these here? - because we don't want
 * to include all the joint function definitions in joint.cpp. hmmmm.
 * MSVC complains if we call D_ALL_PARAM_NAMES_X with a blank second argument,
 * which is why we have the D_ALL_PARAM_NAMES macro as well. please copy and
 * paste between these two.
 */

#define D_ALL_PARAM_NAMES(start) \
  /* parameters for limits and motors */ \
  dParamLoStop = start, \
  dParamHiStop, \
  dParamVel, \
  dParamFMax, \
  dParamFudgeFactor, \
  dParamBounce, \
  dParamCFM, \
  dParamStopERP, \
  dParamStopCFM, \
  /* parameters for suspension */ \
  dParamSuspensionERP, \
  dParamSuspensionCFM,

#define D_ALL_PARAM_NAMES_X(start,x) \
  /* parameters for limits and motors */ \
  dParamLoStop ## x = start, \
  dParamHiStop ## x, \
  dParamVel ## x, \
  dParamFMax ## x, \
  dParamFudgeFactor ## x, \
  dParamBounce ## x, \
  dParamCFM ## x, \
  dParamStopERP ## x, \
  dParamStopCFM ## x, \
  /* parameters for suspension */ \
  dParamSuspensionERP ## x, \
  dParamSuspensionCFM ## x,

enum {
  D_ALL_PARAM_NAMES(0)
  D_ALL_PARAM_NAMES_X(0x100,2)
  D_ALL_PARAM_NAMES_X(0x200,3)

  /* add a multiple of this constant to the basic parameter numbers to get
   * the parameters for the second, third etc axes.
   */
  dParamGroup=0x100
};*)

//  TJointParams = (
    // parameters for limits and motors
(* Change: New Type added, sintax enforcement *)
  type
    TJointParams = Integer;

(* These consts now have defined types *)
  const
    _priv_dParamLoStop  = 0;
    _priv_dParamLoStop2 = $100;
    _priv_dParamLoStop3 = $200;
  const
    dParamLoStop: TJointParams = _priv_dParamLoStop;
    dParamHiStop: TJointParams = _priv_dParamLoStop + 1;
    dParamVel: TJointParams = _priv_dParamLoStop + 2;
    dParamFMax: TJointParams = _priv_dParamLoStop + 3;
    dParamFudgeFactor: TJointParams = _priv_dParamLoStop + 4;
    dParamBounce: TJointParams = _priv_dParamLoStop + 5;
    dParamCFM: TJointParams = _priv_dParamLoStop + 6;
    dParamStopERP: TJointParams = _priv_dParamLoStop + 7;
    dParamStopCFM: TJointParams = _priv_dParamLoStop + 8;
    // parameters for suspension
    dParamSuspensionERP: TJointParams = _priv_dParamLoStop + 9;
    dParamSuspensionCFM: TJointParams = _priv_dParamLoStop + 10;

    // SECOND AXEL
    // parameters for limits and motors
    dParamLoStop2: TJointParams = _priv_dParamLoStop2;
    dParamHiStop2: TJointParams = _priv_dParamLoStop2 + 1;
    dParamVel2: TJointParams = _priv_dParamLoStop2 + 2;
    dParamFMax2: TJointParams = _priv_dParamLoStop2 + 3;
    dParamFudgeFactor2: TJointParams = _priv_dParamLoStop2 + 4;
    dParamBounce2: TJointParams = _priv_dParamLoStop2 + 5;
    dParamCFM2: TJointParams = _priv_dParamLoStop2 + 6;
    dParamStopERP2: TJointParams = _priv_dParamLoStop2 + 7;
    dParamStopCFM2: TJointParams = _priv_dParamLoStop2 + 8;
    // parameters for suspension
    dParamSuspensionERP2: TJointParams = _priv_dParamLoStop2 + 9;
    dParamSuspensionCFM2: TJointParams = _priv_dParamLoStop2 + 10;

    // THIRD AXEL
    // parameters for limits and motors
    dParamLoStop3: TJointParams = _priv_dParamLoStop3;
    dParamHiStop3: TJointParams = _priv_dParamLoStop3 + 1;
    dParamVel3: TJointParams = _priv_dParamLoStop3 + 2;
    dParamFMax3: TJointParams = _priv_dParamLoStop3 + 3;
    dParamFudgeFactor3: TJointParams = _priv_dParamLoStop3 + 4;
    dParamBounce3: TJointParams = _priv_dParamLoStop3 + 5;
    dParamCFM3: TJointParams = _priv_dParamLoStop3 + 6;
    dParamStopERP3: TJointParams = _priv_dParamLoStop3 + 7;
    dParamStopCFM3: TJointParams = _priv_dParamLoStop3 + 8;
    // parameters for suspension
    dParamSuspensionERP3: TJointParams = _priv_dParamLoStop3 + 9;
    dParamSuspensionCFM3: TJointParams = _priv_dParamLoStop3 + 10;

    // AGAIN!
    // dParamGroup * 2 + dParamBounce = dParamBounce2
    dParamGroup: TJointParams = $100;

  type
    TdRealHugeArray = array[0..65535] of TdReal;
    PdRealHugeArray = ^TdRealHugeArray;
  
  //{$ifdef cTRILIST}
  type
    // Tri-list collider
    TdIntegerArray = array[0..65535] of Integer;
    PdIntegerArray = ^TdIntegerArray;

    TdVector3Array = array[0..65535] of TdVector3;
    PdVector3Array = ^TdVector3Array;
  //{$endif}

{ TODO :
// How does one import integers?
dBoxClass
dCCylinderClass
dGeomTransformClass
dSphereClass
dPlaneClass
...

// Functions
dBoxBox
dDebug
dError
dMessage
dFactorCholesky
dFactorLDLT
dPlaneSpace
dInfinityValue
dInvertPDMatrix
dIsPositiveDefinite
dLDLTAddTL
dLDLTRemove
dRemoveRowCol
dSetDebugHandler
dSetErrorHandler
dSetMessageHandler
dSetZero
dSolveCholesky
dSolveLDLT}

  // ***************************************************************************
  // ***************************************************************************
  // ***** WORLD

  //----- dWorld -----
  function dWorldCreate: PdxWorld; cdecl; external ODEDLL;
  procedure dWorldDestroy(const World: PdxWorld); cdecl; external ODEDLL;
  function dWorldGetCFM(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  function dWorldGetERP(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldGetGravity(const World: PdxWorld; gravity: TdVector3); cdecl; external ODEDLL;
  procedure dWorldImpulseToForce(const World: PdxWorld; const stepsize, ix, iy, iz: TdReal; force: TdVector3); cdecl; external ODEDLL;
  procedure dWorldSetCFM(const World: PdxWorld; cfm: TdReal); cdecl; external ODEDLL;
  procedure dWorldSetERP(const World: PdxWorld; erp: TdReal); cdecl; external ODEDLL;
  procedure dWorldSetGravity(const World: PdxWorld; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dWorldStep(const World: PdxWorld; const stepsize: TdReal); cdecl; external ODEDLL;
  // Stepfast
  procedure dWorldStepFast1(const World: PdxWorld; const stepsize: TdReal; const iterations: Integer); cdecl; external ODEDLL;
  procedure dWorldSetAutoEnableDepthSF1(const World: PdxWorld; autodepth: Integer); cdecl; external ODEDLL;
  function dWorldGetAutoEnableDepthSF1(const World: PdxWorld): Integer; cdecl; external ODEDLL;
  procedure dCloseODE; cdecl; external ODEDLL;


  //----- dBody -----
  procedure dBodyAddForce(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddForceAtPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddForceAtRelPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddRelForce(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddRelForceAtPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddRelForceAtRelPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddRelTorque(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddTorque(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external ODEDLL;

  function dBodyCreate(const World: PdxWorld): PdxBody; cdecl; external ODEDLL;
  procedure dBodyDestroy(const Body: PdxBody); cdecl; external ODEDLL;
  procedure dBodyDisable(const Body: PdxBody); cdecl; external ODEDLL;
  procedure dBodyEnable(const Body: PdxBody); cdecl; external ODEDLL;
  function dBodyGetAngularVel(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  procedure dBodyGetFiniteRotationAxis(const Body: PdxBody; result: TdVector3); cdecl; external ODEDLL;
  function dBodyGetFiniteRotationMode(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  function dBodyGetForce(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  function dBodyGetGravityMode(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  function dBodyGetJoint(const Body: PdxBody; const index: Integer): TdJointID; cdecl; external ODEDLL;
  function dBodyGetLinearVel(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  procedure dBodyGetMass(const Body: PdxBody; var mass: TdMass); cdecl; external ODEDLL;
  function dBodyGetNumJoints(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  procedure dBodyGetPointVel(const Body: PdxBody; px, py, pz: TdReal; result: TdVector3); cdecl; external ODEDLL;
  procedure dBodyGetPosRelPoint(const Body: PdxBody; px, py, pz: TdReal; result: TdVector3); cdecl; external ODEDLL;
  function dBodyGetPosition(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  function dBodyGetQuaternion(const Body: PdxBody): PdQuaternion; cdecl; external ODEDLL;
  procedure dBodyGetRelPointPos(const Body: PdxBody; px, py, pz: TdReal; result: TdVector3); cdecl; external ODEDLL;
  procedure dBodyGetRelPointVel(const Body: PdxBody; px, py, pz: TdReal; result: TdVector3); cdecl; external ODEDLL;
  function dBodyGetRotation(const Body: PdxBody): PdMatrix3; cdecl; external ODEDLL;
  function dBodyGetTorque(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  function dBodyIsEnabled(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  procedure dBodySetAngularVel(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetFiniteRotationAxis(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetFiniteRotationMode(const Body: PdxBody; const mode: Integer); cdecl; external ODEDLL;
  procedure dBodySetForce(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetGravityMode(const Body: PdxBody; const mode: Integer); cdecl; external ODEDLL;
  procedure dBodySetLinearVel(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetMass(const Body: PdxBody; const mass: TdMass); cdecl; external ODEDLL;
  procedure dBodySetPosition(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;

  procedure dBodySetQuaternion(const Body: PdxBody; const q: TdQuaternion); cdecl; external ODEDLL;
  procedure dBodySetRotation(const Body: PdxBody; const R: TdMatrix3); cdecl; external ODEDLL;
  procedure dBodySetTorque(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodyVectorFromWorld(const Body: PdxBody; px, py, pz: TdReal; result: TdVector3); cdecl; external ODEDLL;
  procedure dBodyVectorToWorld(const Body: PdxBody; px, py, pz: TdReal; result: TdVector3); cdecl; external ODEDLL;
  procedure dBodySetData (const Body: PdxBody; data : pointer); cdecl; external ODEDLL;
  function dBodyGetData (const Body: PdxBody) : pointer; cdecl; external ODEDLL;

  //----- dJoint -----
  procedure dJointAttach(const dJointID : TdJointID; const body1, body2: PdxBody); cdecl; external ODEDLL;
  function dJointCreateAMotor(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  function dJointCreateBall(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  function dJointCreateContact(const World : PdxWorld; dJointGroupID : TdJointGroupID; const dContact: TdContact): TdJointID; cdecl; external ODEDLL;
  function dJointCreateFixed(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  function dJointCreateHinge(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  function dJointCreateHinge2(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  function dJointCreateSlider(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  function dJointCreateUniversal(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointDestroy(const dJointID : TdJointID); cdecl; external ODEDLL;
  function dJointGetAMotorAngle(const dJointID : TdJointID; const anum: Integer): TdReal; cdecl; external ODEDLL;
  function dJointGetAMotorAngleRate(const dJointID : TdJointID; const anum: Integer): TdReal; cdecl; external ODEDLL;
  procedure dJointGetAMotorAxis(const dJointID : TdJointID; const anum: Integer; result: TdVector3); cdecl; external ODEDLL;
  function dJointGetAMotorAxisRel(const dJointID : TdJointID; const anum: Integer): Integer; cdecl; external ODEDLL;
  function dJointGetAMotorMode(const dJointID : TdJointID): Integer; cdecl; external ODEDLL;
  function dJointGetAMotorNumAxes(const dJointID : TdJointID): Integer; cdecl; external ODEDLL;
  function dJointGetAMotorParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  procedure dJointGetBallAnchor(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  function dJointGetBody(const dJointID : TdJointID; const index: Integer): PdxBody; cdecl; external ODEDLL;
  procedure dJointGetHinge2Anchor(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  function dJointGetHinge2Angle1(const dJointID : TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetHinge2Angle1Rate(const dJointID : TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetHinge2Angle2Rate(const dJointID : TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointGetHinge2Axis1(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetHinge2Axis2(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  function dJointGetHinge2Param(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  procedure dJointGetHingeAnchor(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  function dJointGetHingeAngle(const dJointID : TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetHingeAngleRate(const dJointID : TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointGetHingeAxis(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  function dJointGetHingeParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  procedure dJointGetSliderAxis(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  function dJointGetSliderParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  function dJointGetSliderPosition(const dJointID : TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetSliderPositionRate(const dJointID : TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetType(const dJointID : TdJointID): Integer; cdecl; external ODEDLL;
  procedure dJointGetUniversalAnchor(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetUniversalAxis1(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetUniversalAxis2(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  function dJointGroupCreate(const max_size: Integer): TdJointGroupID; cdecl; external ODEDLL;
  procedure dJointGroupDestroy(const dJointGroupID : TdJointGroupID); cdecl; external ODEDLL;
  procedure dJointGroupEmpty(const dJointGroupID : TdJointGroupID); cdecl; external ODEDLL;
  procedure dJointSetAMotorAngle(const dJointID : TdJointID; const anum: Integer; const angle: TdReal); cdecl; external ODEDLL;
  procedure dJointSetAMotorAxis(const dJointID : TdJointID; const anum, rel: Integer; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetAMotorMode(const dJointID : TdJointID; const mode: TdAngularMotorModeNumbers); cdecl; external ODEDLL;
  procedure dJointSetAMotorNumAxes(const dJointID : TdJointID; const num: Integer); cdecl; external ODEDLL;
  procedure dJointSetAMotorParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  procedure dJointSetBallAnchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetFixed(const dJointID : TdJointID); cdecl; external ODEDLL;
  procedure dJointSetHinge2Anchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetHinge2Axis1(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetHinge2Axis2(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetHinge2Param(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  procedure dJointSetHingeAnchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetHingeAxis(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetHingeParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  procedure dJointSetSliderAxis(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetSliderParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  procedure dJointSetUniversalAnchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetUniversalAxis1(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetUniversalAxis2(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  function dJointGetData (const dJointID : TdJointID) : pointer; cdecl; external ODEDLL;
  procedure dJointSetData (const dJointID : TdJointID; data : Pointer); cdecl; external ODEDLL;
  procedure dJointGetBallAnchor2(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetHingeAnchor2(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetHinge2Anchor2(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetUniversalAnchor2(const dJointID : TdJointID; result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetUniversalParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetUniversalParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;


  // New "callback" routines for feedback of joints
  procedure dJointSetFeedback (const dJointID : TdJointID; Feedback : PTdJointFeedback); cdecl; external ODEDLL;
  function dJointGetFeedback (const dJointID : TdJointID) : PTdJointFeedback; cdecl; external ODEDLL;

  // dJointTypePlane2D

  {$ifdef cPLANE2D}
  // JointPlane2D isn't currently defined in the single dll
  function dJointCreatePlane2D(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetPlane2DXParam(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl; external ODEDLL;
  procedure dJointSetPlane2DYParam(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl; external ODEDLL;
  procedure dJointSetPlane2DAngleParam(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl; external ODEDLL;
  {$endif}

  //----- dGeom -----
  procedure dGeomBoxGetLengths(const Geom : PdxGeom; result: TdVector3); cdecl; external ODEDLL;
  procedure dGeomBoxSetLengths(const Geom : PdxGeom; const lx, ly, lz: TdReal); cdecl; external ODEDLL;
  procedure dGeomCCylinderGetParams(const Geom : PdxGeom; var radius, length: TdReal); cdecl; external ODEDLL;
  procedure dGeomCCylinderSetParams(const Geom : PdxGeom; const radius, length: TdReal); cdecl; external ODEDLL;
  procedure dGeomDestroy(const Geom : PdxGeom); cdecl; external ODEDLL;
  procedure dGeomGetAABB(const Geom : PdxGeom; aabb: TdAABB); cdecl; external ODEDLL;
  function dGeomGetBody(const Geom : PdxGeom): PdxBody; cdecl; external ODEDLL;
  function dGeomGetClass(const Geom : PdxGeom): Integer; cdecl; external ODEDLL;
  function dGeomGetPosition(const Geom : PdxGeom): PdVector3; cdecl; external ODEDLL;
  function dGeomGetRotation(const Geom : PdxGeom): PdMatrix3; cdecl; external ODEDLL;
  function dGeomGetSpace(const Geom : PdxGeom): PdxSpace; cdecl; external ODEDLL;

  // Deprecated!
  //function dGeomGetSpaceAABB(const Geom : PdxGeom): TdReal; cdecl; external ODEDLL;

  // Deprecated!
  //procedure dGeomGroupAdd (const GeomGroup, Geom : PdxGeom); cdecl; external ODEDLL;
  //function dGeomGroupGetGeom(const Geom : PdxGeom; const i: Integer): PdxGeom; cdecl; external ODEDLL;
  //function dGeomGroupGetNumGeoms(const Geom : PdxGeom): Integer; cdecl; external ODEDLL;
  //procedure dGeomGroupRemove(const group, x: PdxGeom); cdecl; external ODEDLL;
  procedure dGeomPlaneGetParams(const Geom : PdxGeom; result: TdVector4); cdecl; external ODEDLL;
  procedure dGeomPlaneSetParams (const Geom : PdxGeom; const a, b, c, d: TdReal); cdecl; external ODEDLL;
  procedure dGeomSetBody(const Geom : PdxGeom; Body: PdxBody); cdecl; external ODEDLL;
  procedure dGeomSetPosition(const Geom : PdxGeom; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dGeomSetRotation(const Geom : PdxGeom; const R: TdMatrix3); cdecl; external ODEDLL;
  function dGeomSphereGetRadius(const Geom : PdxGeom): TdReal; cdecl; external ODEDLL;
  procedure dGeomSphereSetRadius(const Geom : PdxGeom; const radius: TdReal); cdecl; external ODEDLL;
  function dGeomTransformGetCleanup(const Geom : PdxGeom): Integer; cdecl; external ODEDLL;
  function dGeomTransformGetGeom(const Geom : PdxGeom): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomTransformSetCleanup(const Geom : PdxGeom; const mode: Integer); cdecl; external ODEDLL;
  procedure dGeomTransformSetGeom(const Geom, obj: PdxGeom); cdecl; external ODEDLL;
  procedure dGeomSetData (const Geom : PdxGeom; data : pointer); cdecl; external ODEDLL;
  function dGeomGetData (const Geom : PdxGeom) : pointer; cdecl; external ODEDLL;
  procedure dGeomTransformSetInfo (const Geom : PdxGeom; mode : integer); cdecl; external ODEDLL;
  function dGeomTransformGetInfo (const Geom : PdxGeom) : integer; cdecl; external ODEDLL;
  procedure dGeomGetQuaternion(const Geom : PdxGeom; var result: TdQuaternion); cdecl; external ODEDLL;
  procedure dGeomSetQuaternion(const Geom : PdxGeom; const TdQuaternion); cdecl; external ODEDLL;

  function dGeomIsSpace (const Geom : PdxGeom) : integer; cdecl; external ODEDLL;
  procedure dGeomSetCategoryBits (const Geom : PdxGeom; bits : Cardinal); cdecl; external ODEDLL;
  procedure dGeomSetCollideBits (const Geom : PdxGeom; bits : Cardinal); cdecl; external ODEDLL;
  function dGeomGetCategoryBits (const Geom : PdxGeom) : cardinal; cdecl; external ODEDLL;
  function dGeomGetCollideBits (const Geom : PdxGeom) : cardinal; cdecl; external ODEDLL;
  procedure dGeomEnable (const Geom : PdxGeom); cdecl; external ODEDLL;
  procedure dGeomDisable (const Geom : PdxGeom); cdecl; external ODEDLL;
  function dGeomIsEnabled (const Geom : PdxGeom) : integer; cdecl; external ODEDLL;

  function dGeomSpherePointDepth (const Geom : PdxGeom; const x,y,z : TdReal) : TdReal; cdecl; external ODEDLL;
  function dGeomBoxPointDepth (const Geom : PdxGeom; const x,y,z : TdReal) : TdReal; cdecl; external ODEDLL;
  function dGeomPlanePointDepth (const Geom : PdxGeom; const x,y,z : TdReal) : TdReal; cdecl; external ODEDLL;
  function dGeomCCylinderPointDepth (const Geom : PdxGeom; const x,y,z : TdReal) : TdReal; cdecl; external ODEDLL;

  // A strange fix, so the class ids can be updated
  // ***************
  // Deprecated
  //function dCreateGeomGroup(const Space : PdxSpace): PdxGeom; cdecl;
  function dCreateSphere(const Space : PdxSpace; const radius: TdReal): PdxGeom; cdecl;
  function dCreateBox(const Space : PdxSpace; const lx, ly, lz: TdReal): PdxGeom; cdecl;
  function dCreatePlane(const Space : PdxSpace; const a, b, c, d: TdReal): PdxGeom; cdecl;
  function dCreateCCylinder(const Space : PdxSpace; const radius, length: TdReal): PdxGeom; cdecl;
  function dCreateCylinder(const Space : PdxSpace; r, lz : TdReal) : PdxGeom; cdecl;
  function dCreateCone(const Space : PdxSpace; radius, length: TdReal): PdxGeom; cdecl;
  function dCreateTerrain(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer): PdxGeom; cdecl;
  function dCreateRay(const Space : PdxSpace; length: TdReal) : PdxGeom; cdecl;
  function dCreateGeomTransform(const Space : PdxSpace): PdxGeom; cdecl;

  // Deprecated
  //function EXT_dCreateGeomGroup(const Space : PdxSpace): PdxGeom; cdecl; external ODEDLL name 'dCreateGeomGroup';
  function EXT_dCreateSphere(const Space : PdxSpace; const radius: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreateSphere';
  function EXT_dCreateBox(const Space : PdxSpace; const lx, ly, lz: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreateBox';
  function EXT_dCreatePlane(const Space : PdxSpace; const a, b, c, d: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreatePlane';
  function EXT_dCreateCCylinder(const Space : PdxSpace; const radius, length: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreateCCylinder';
  function EXT_dCreateCylinder(const Space : PdxSpace; r, lz : TdReal) : PdxGeom; cdecl; external ODEDLL name 'dCreateCylinder';
  function EXT_dCreateCone(const Space : PdxSpace; radius, length: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreateCone';
  function EXT_dCreateTerrain(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer): PdxGeom; cdecl; external ODEDLL name 'dCreateTerrain';
  function EXT_dCreateRay(const Space : PdxSpace; length : TdReal) : PdxGeom; cdecl; external ODEDLL name 'dCreateRay';
  function EXT_dCreateGeomTransform(const Space : PdxSpace): PdxGeom; cdecl; external ODEDLL name 'dCreateGeomTransform';
  // ***************

  // dCone
  procedure dGeomConeSetParams(const Geom: PdxGeom; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dGeomConeGetParams(const Geom: PdxGeom; var radius, length: TdReal); cdecl; external ODEDLL;
  function dGeomConePointDepth(const Geom: PdxGeom; const x, y, z: TdReal): TdReal; cdecl; external ODEDLL;

  // dTerrain
  function dGeomTerrainPointDepth(const Geom: PdxGeom; const x, y, z: TdReal): TdReal; cdecl; external ODEDLL;

  // dCylinder (not a capped cylinder).
  procedure dGeomCylinderSetParams (const Geom : PdxGeom; radius, length : TdReal); cdecl; external ODEDLL;
  procedure dGeomCylinderGetParams (const Geom : PdxGeom; var radius, length : TdReal); cdecl; external ODEDLL;

  //dRay
  procedure dGeomRaySetLength(const Geom : PdxGeom; length: TdReal); cdecl; external ODEDLL;
  function dGeomRayGetLength(const Geom : PdxGeom) : TdReal; cdecl; external ODEDLL;
  procedure dGeomRaySetClosestHit(const Geom : PdxGeom; closestHit: Integer); cdecl; external ODEDLL;
  function dGeomRayGetClosestHit(const Geom : PdxGeom) : Integer; cdecl; external ODEDLL;
  procedure dGeomRaySet(const Geom : PdxGeom; px, py, pz, dx, dy, dz: TdReal); cdecl; external ODEDLL;
  procedure dGeomRayGet(const Geom : PdxGeom; var start, dir: TdVector3); cdecl; external ODEDLL;

  function dCreateGeomClass(const classptr : TdGeomClass) : Integer; cdecl; external ODEDLL;
  function dGeomGetClassData(o : PdxGeom) : Pointer; cdecl; external ODEDLL;
  function dCreateGeom (classnum : Integer) : PdxGeom; cdecl; external ODEDLL;    

  //----- dTrilistCollider -----
  {$ifdef cTRILIST}
  function dCreateTriMesh(const Space : PdxSpace; Data: PdxTriMeshData; Callback, ArrayCallback, RayCallback: Pointer): PdxGeom; cdecl;
  function EXT_dCreateTriMesh(const Space : PdxSpace; Data: PdxTriMeshData; Callback, ArrayCallback, RayCallback: Pointer): PdxGeom; cdecl; external ODEDLL name 'dCreateTriMesh';
  procedure dGeomTriMeshDataBuildSimple(g: PdxTriMeshData; const Vertices: PdVector3Array; VertexCount: Integer; const Indices: PdIntegerArray; IndexCount: Integer); cdecl; external ODEDLL;
  procedure dGeomTriMeshDataBuildDouble(g: PdxTriMeshData; const Vertices: PdVector3Array; VertexStride, VertexCount: Integer; const Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl; external ODEDLL;
  procedure dGeomTriMeshDataBuildSingle(g: PdxTriMeshData; const Vertices: PdVector3Array; VertexStride, VertexCount: Integer; const Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl; external ODEDLL;
  function dGeomTriMeshDataCreate: PdxTriMeshData; cdecl; external ODEDLL;
  procedure dGeomTriMeshDataDestroy(g: PdxTriMeshData); cdecl; external ODEDLL;
  procedure dGeomTriMeshGetTriangle(g: PdxGeom; Index: Integer; v0, v1, v2: PdVector3); cdecl; external ODEDLL;
  procedure dGeomTriMeshGetPoint(g: PdxGeom; Index: Integer; u, v: TdReal; result: TdVector3); cdecl; external ODEDLL;
  procedure dGeomTriMeshClearTCCache(g: PdxGeom); cdecl; external ODEDLL;
  procedure dGeomTriMeshEnableTC(g: PdxGeom; geomClass, enable: Integer); cdecl; external ODEDLL;
  function dGeomTriMeshIsTCEnabled(g: PdxGeom; geomClass: Integer): Integer; cdecl; external ODEDLL;
  function dGeomTriMeshGetArrayCallback(g: PdxGeom): Pointer; cdecl; external ODEDLL;
  function dGeomTriMeshGetCallback(g: PdxGeom): Pointer; cdecl; external ODEDLL;
  function dGeomTriMeshGetRayCallback(g: PdxGeom): Pointer; cdecl; external ODEDLL;
  procedure dGeomTriMeshSetArrayCallback(g: PdxGeom; ArrayCallback: Pointer); cdecl; external ODEDLL;
  procedure dGeomTriMeshSetCallback(g: PdxGeom; Callback: Pointer); cdecl; external ODEDLL;
  procedure dGeomTriMeshSetRayCallback(g: PdxGeom; RayCallback: Pointer); cdecl; external ODEDLL;
  procedure GeomTriMeshSetData(g: PdxGeom; Data: PdxTriMeshData); cdecl; external ODEDLL;
  {$endif}

  //----- dSpace -----
  procedure dSpaceAdd(const Space : PdxSpace; const Geom : PdxGeom); cdecl; external ODEDLL;
  procedure dSpaceDestroy(const Space: PdxSpace); cdecl; external ODEDLL;
  procedure dSpaceClean (const Space : PdxSpace); cdecl; external ODEDLL;
  function dSpaceQuery (const Space : PdxSpace; const Geom : PdxGeom): Integer; cdecl; external ODEDLL;
  procedure dSpaceRemove(const Space : PdxSpace; const Geom : PdxGeom); cdecl; external ODEDLL;
  function dSimpleSpaceCreate(Space : PdxSpace): PdxSpace; cdecl; external ODEDLL;
  function dHashSpaceCreate(Space : PdxSpace): PdxSpace; cdecl; external ODEDLL;
  function dQuadTreeSpaceCreate(Space : PdxSpace; Center, Extents : TdVector3; Depth : Integer): PdxSpace; cdecl; external ODEDLL;
  procedure dHashSpaceSetLevels(const Space: PdxSpace; const minlevel, maxlevel: Integer); cdecl; external ODEDLL;
  procedure dInfiniteAABB(geom : PdxGeom; var aabb : TdAABB); cdecl; external ODEDLL;
  function dSpaceGetNumGeoms (const Space: PdxSpace) : integer; cdecl; external ODEDLL;
  function dSpaceGetGeom (const Space: PdxSpace; const i: Integer) : PdxGeom; cdecl; external ODEDLL;


  procedure dSpaceSetCleanup (space : PdxSpace; const mode : integer); cdecl; external ODEDLL;
  function dSpaceGetCleanup(Space : PdxSpace): integer; cdecl; external ODEDLL;

  //----- dMass -----
  procedure dMassAdd(var a,b : TdMass); cdecl; external ODEDLL;
  procedure dMassAdjust(var m: TdMass; newmass: TdReal); cdecl; external ODEDLL;
  procedure dMassRotate(var m: TdMass; R: TdMatrix3); cdecl; external ODEDLL;
  procedure dMassSetBox(var m: TdMass; density, lx, ly, lz: TdReal); cdecl; external ODEDLL;
  procedure dMassSetBoxTotal(var m: TdMass; total_mass, lx, ly, lz: TdReal); cdecl; external ODEDLL;
  procedure dMassSetCylinder(var m: TdMass; density: TdReal; direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dMassSetCylinderTotal(var m: TdMass; total_mass: TdReal; direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dMassSetCappedCylinder(var m: TdMass; density: TdReal; direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dMassSetCappedCylinderTotal(var m: TdMass; total_mass: TdReal; direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dMassSetParameters(var m: TdMass; themass, cgx, cgy, cgz, I11, I22, I33, I12, I13, I23: TdReal); cdecl; external ODEDLL;
  procedure dMassSetSphere(var m: TdMass; density, radius: TdReal); cdecl; external ODEDLL;
  procedure dMassSetSphereTotal(var m: TdMass; total_mass, radius: TdReal); cdecl; external ODEDLL;
  procedure dMassSetZero(var m: TdMass); cdecl; external ODEDLL;
  procedure dMassTranslate(var m: TdMass; x, y, z: TdReal); cdecl; external ODEDLL;

  //----- Rotation.h -----
  procedure dQFromAxisAndAngle (var q : TdQuaternion; const ax, ay ,az, angle : TdReal); cdecl; external ODEDLL;
  procedure dRFromAxisAndAngle (var R : TdMatrix3; const ax, ay ,az, angle : TdReal); cdecl; external ODEDLL;
  procedure dRSetIdentity (var R : TdMatrix3); cdecl; external ODEDLL;
  procedure dQSetIdentity (var Q : TdQuaternion); cdecl; external ODEDLL;
  procedure dRFromEulerAngles (var R : TdMatrix3; const phi, theta, psi : TdReal); cdecl; external ODEDLL;
  procedure dRFrom2Axes (var R: TdMatrix3; const ax, ay, az, bx, by, bz : TdReal); cdecl; external ODEDLL;
  procedure dRFromZAxis (var R: TdMatrix3; const ax, ay, az : TdReal); cdecl; external ODEDLL;


  procedure dMultiply0 (const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl; external ODEDLL;
  procedure dMultiply1 (const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl; external ODEDLL;
  procedure dMultiply2 (const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl; external ODEDLL;
  procedure dQMultiply0 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external ODEDLL;
  procedure dQMultiply1 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external ODEDLL;
  procedure dQMultiply2 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external ODEDLL;
  procedure dQMultiply3 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external ODEDLL;
  procedure dQtoR (const q : TdQuaternion; var R : TdMatrix3); cdecl; external ODEDLL;
  procedure dRtoQ (const R : TdMatrix3; var q : TdQuaternion); cdecl; external ODEDLL;
  procedure dWtoDQ (const w : TdVector3; const q: TdQuaternion; var dq : TdVector4); cdecl; external ODEDLL;

  //----- Math -----
  procedure dNormalize3 (var a : TdVector3); cdecl; external ODEDLL;
  procedure dNormalize4 (var a : TdVector4); cdecl; external ODEDLL;

  //----- Misc -----
  procedure dClosestLineSegmentPoints (const a1, a2, b1, b2 : TdVector3; var cp1, cp2 : TdVector3); cdecl; external ODEDLL;

  function dBoxTouchesBox (const _p1 : TdVector3; const R1 : TdMatrix3;
                    const side1 : TdVector3; const _p2 : TdVector3;
                    const R2 : TdMatrix3; const side2 : TdVector3) : integer; cdecl; external ODEDLL;

  function dMaxDifference (A, B : PdReal; n, m : integer) : TdReal; cdecl; external ODEDLL;
  procedure dMakeRandomVector(var n1 : TdVector3; a : integer; f : TdReal); cdecl; external ODEDLL;
  function dAreConnected (a, b : PdxBody) : integer; cdecl; external ODEDLL;
  function dAreConnectedExcluding (a, b : PdxBody; joint_type : TdJointTypeNumbers) : integer; cdecl; external ODEDLL;

  function dCollide (o1, o2 : PdxGeom; flags : integer; var Contact : TdContactGeom; Skip : integer) : integer; cdecl; external ODEDLL;
  procedure dSpaceCollide (const Space : PdxSpace; data : pointer; callback : TdNearCallback); cdecl; external ODEDLL;
  procedure dSpaceCollide2 (o1, o2 : PdxGeom; data : pointer; callback : TdNearCallback); cdecl; external ODEDLL;
  procedure dMakeRandomMatrix (A : PdRealArray; n, m : integer; range :  TdReal); cdecl; external ODEDLL;
  procedure dClearUpperTriangle (A : PdRealArray; n : integer); cdecl; external ODEDLL;
  function dMaxDifferenceLowerTriangle (A : PdRealArray;  var B : TdReal;  n : integer) : TdReal; cdecl; external ODEDLL;

  function dRandGetSeed: Cardinal; cdecl; external ODEDLL;
  procedure dRandSetSeed (const s: Cardinal); cdecl; external ODEDLL;
  function dRandInt (const n: Integer): Integer; cdecl; external ODEDLL;
  function dRandReal: TdReal; cdecl; external ODEDLL;
  // return 1 if the random number generator is working.
  function dTestRand: Integer; cdecl; external ODEDLL;

  procedure dTestMatrixComparison; cdecl; external ODEDLL;
  procedure dTestSolveLCP; cdecl; external ODEDLL;

  // How can this be imported? I can't find it in the source!
  //function dInfinityValue : TdReal; cdecl; external ODEDLL;

  //----- Recreated -----
  function dDot (a, b : TdVector3) : TdReal; overload;
  function dDot (a, b : PdVector3) : TdReal; overload;

  function dDOT14(a,b : TdRealArray) : TdReal; overload;
  function dDOT14(a,b : PdRealArray) : TdReal; overload;

  procedure dMULTIPLY0_333(var A : TdMatrix3; const B,C : TdMatrix3);
  procedure dMULTIPLY0_331(var A : TdVector3; const B : TdMatrix3; const C : TdVector3);

  function Vector3ScalarMul(a : TdVector3; Scalar : TdReal) : TdVector3;
  function Vector3ADD(a, b : TdVector3) : TdVector3;
  function Vector3SUB(a, b : TdVector3) : TdVector3;
  function Vector3Length(a : TdVector3) : TdReal;
  function Vector3Cross(V1, V2 : TdVector3) : TdVector3;
  function Vector3Make(x,y,z : TdReal) : TdVector3;

  procedure DisableStillBodies(World : PdxWorld; Threshold : TdReal=0.0001);

  procedure VerifyDelphiODE(Body : PdxBody; Geom : PdxGeom);

var
  // These must be set up, so I catch the first time a user creates a new
  // object and ask what the class number became. Very fancy... They should
  // be exported from the dll, but how does one export integers from dlls?
  dSphereClass : integer=-1;
  dBoxClass : integer=-1;
  dCCylinderClass : integer=-1;
  dGeomTransformClass : integer=-1;
  dPlaneClass : integer=-1;
  // Deprecated
  //dGeomGroupClass : integer=-1;
  dCylinderClass : integer=-1;
  dRayClass : integer=-1;
  dGeomTransformGroupClass : integer=-1;
  dTriMeshClass : integer=-1;
  dConeClass : integer=-1;
  dTerrainClass : integer=-1;

  DisabledDebugGeom : boolean = False ;

{$IFDEF cODEDebugEnabled}
var
   ODEDebugGeomList: TGeomList;
{$ENDIF}

implementation

var
  WasStillBefore, WasStillBeforeOld : TList;

const
  // to be used as descriptive indices
  IndexX = 0;
  IndexY = 1;
  IndexZ = 2;
  IndexW = 3;

{ TBodyList }

procedure TBodyList.DeleteAllBodies;
var
  i : integer;
begin
  for i := 0 to Count-1 do
    dBodyDestroy(Items[i]);

  Clear;
end;

function TBodyList.GetItems(i: integer): PdxBody;
begin
  result := Get(i);
end;

procedure TBodyList.SetItems(i: integer; const Value: PdxBody);
begin
  Put(i, Value);
end;

{ TGeomList }

procedure TGeomList.DeleteAllGeoms(DeleteDataAsObject : boolean=false);
var
  i : integer;
begin
  for i := 0 to Count-1 do
  begin
    if DeleteDataAsObject and (Items[i].data<>nil) then
      TObject(Items[i].data).Free;

    dGeomDestroy(Items[i]);
  end;

  Clear;
end;

function TGeomList.GetItems(i: integer): PdxGeom;
begin
  result := Get(i);
end;

procedure TGeomList.SetItems(i: integer; const Value: PdxGeom);
begin
  Put(i, Value);
end;

//----- Recreated -----

function dDot (a, b : PdVector3) : TdReal;
begin
  Assert(Assigned(a),'a not assigned!');
  Assert(Assigned(b),'b not assigned!');
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]);
end;

function dDot (a, b : TdVector3) : TdReal;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]);
end;

// #define dDOT(a,b)   ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2])
// #define dDOT14(a,b) ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8])
// #define dDOT41(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[1] + (a)[8]*(b)[2])
// #define dDOT44(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[4] + (a)[8]*(b)[8])

function dDOT14(a,b : TdRealArray) : TdReal; overload;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8]);
end;

function dDOT14(a,b : PdRealArray) : TdReal; overload;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8]);
end;

procedure dMULTIPLY0_331(var A : TdVector3; const B : TdMatrix3; const C : TdVector3);
{var
  v : PdVector3;}
begin
  // #define dMULTIPLY0_331(A,B,C) dMULTIPLYOP0_331(A,=,B,C)

  //  #define dMULTIPLYOP0_331(A,op,B,C) \
  //    (A)[0] op dDOT((B),(C)); \
  //    (A)[1] op dDOT((B+4),(C)); \
  //    (A)[2] op dDOT((B+8),(C));


  A[0] := dDOT(PdVector3(@(B[0]))^, C);

  A[1] := dDOT(PdVector3(@(B[4]))^, C);
  A[2] := dDOT(PdVector3(@(B[8]))^, C);//}
end;

procedure dMULTIPLY0_333(var A : TdMatrix3; const B,C : TdMatrix3);
begin
  // #define dMULTIPLY0_333(A,B,C) dMULTIPLYOP0_333(A,=,B,C)
  // #define dMULTIPLYOP0_333(A,op,B,C) \
  //   (A)[0] op dDOT14((B),(C)); \
  //   (A)[1] op dDOT14((B),(C+1)); \
  //   (A)[2] op dDOT14((B),(C+2)); \
  //   (A)[4] op dDOT14((B+4),(C)); \
  //   (A)[5] op dDOT14((B+4),(C+1)); \
  //   (A)[6] op dDOT14((B+4),(C+2)); \
  //   (A)[8] op dDOT14((B+8),(C)); \
  //   (A)[9] op dDOT14((B+8),(C+1)); \
  //   (A)[10] op dDOT14((B+8),(C+2));

  A[0] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[0])));
  A[1] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[1])));
  A[2] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[2])));

  A[4] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[0])));
  A[5] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[1])));
  A[6] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[2])));

  A[8] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[0])));
  A[9] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[1])));
  A[10] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[2])));
end;

function Vector3ScalarMul(a : TdVector3; Scalar : TdReal) : TdVector3;
begin
  result[0] := a[0]*Scalar;
  result[1] := a[1]*Scalar;
  result[2] := a[2]*Scalar;
end;

function Vector3ADD(a, b : TdVector3) : TdVector3;
begin
  result[0] := a[0]+b[0];
  result[1] := a[1]+b[1];
  result[2] := a[2]+b[2];
end;

function Vector3SUB(a, b : TdVector3) : TdVector3;
begin
  result[0] := a[0]-b[0];
  result[1] := a[1]-b[1];
  result[2] := a[2]-b[2];
end;

function Vector3Length(a : TdVector3) : TdReal;
begin
  result := sqrt(sqr(a[0])+sqr(a[1])+sqr(a[2]));
end;

function Vector3Cross(V1, V2 : TdVector3) : TdVector3;
begin
   Result[IndexX]:=V1[IndexY] * V2[IndexZ] - V1[IndexZ] * V2[IndexY];
   Result[IndexY]:=V1[IndexZ] * V2[IndexX] - V1[IndexX] * V2[IndexZ];
   Result[IndexZ]:=V1[IndexX] * V2[IndexY] - V1[IndexY] * V2[IndexX];
end;

function Vector3Make(x,y,z : TdReal) : TdVector3;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

// Deprecated
{function dCreateGeomGroup(const Space : PdxSpace): PdxGeom; cdecl;
begin
  result := EXT_dCreateGeomGroup(Space);

  if dGeomGroupClass=-1 then
    dGeomGroupClass := dGeomGetClass(result);
end;//}

function dCreateSphere(const Space : PdxSpace; const radius: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreateSphere(Space, radius);

  if dSphereClass=-1 then
    dSphereClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateBox(const Space : PdxSpace; const lx, ly, lz: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreateBox(Space, lx, ly, lz);

  if dBoxClass=-1 then
    dBoxClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreatePlane(const Space : PdxSpace; const a, b, c, d: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreatePlane(Space, a, b, c, d);

  if dPlaneClass=-1 then
    dPlaneClass := dGeomGetClass(result);
end;

function dCreateCCylinder(const Space : PdxSpace; const radius, length: TdReal): PdxGeom;
begin
  result := EXT_dCreateCCylinder(Space, radius, length);

  if dCCylinderClass=-1 then
    dCCylinderClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateCone(const Space : PdxSpace; radius, length: TdReal): PdxGeom; cdecl;
begin
  result := EXT_dCreateCone(Space, radius, length);

  if dConeClass=-1 then
    dConeClass := dGeomGetClass(result);

{$IFDEF cODEDEDUGENABLED}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateTerrain(const Space: PdxSpace; pHeights: PdRealHugeArray; vLength: TdReal; nNumNodesPerSide: Integer): PdxGeom; cdecl;
begin
  result := EXT_dCreateTerrain(Space, pHeights, vLength, nNumNodesPerSide);

  if dTerrainClass=-1 then
    dTerrainClass := dGeomGetClass(result);

{$IFDEF cODEDEDUGENABLED}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateGeomTransform(const Space : PdxSpace): PdxGeom; cdecl;
begin
  result := EXT_dCreateGeomTransform(Space);

  if dGeomTransformClass=-1 then
    dGeomTransformClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateCylinder(const Space : PdxSpace; r, lz : TdReal) : PdxGeom; cdecl;
begin
  result := EXT_dCreateCylinder(Space, r, lz);

  if dCylinderClass=-1 then
    dCylinderClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

function dCreateRay(const Space : PdxSpace; length : TdReal) : PdxGeom; cdecl;
begin
  result := EXT_dCreateRay(Space, length);

  if dRayClass=-1 then
    dRayClass := dGeomGetClass(result);
{$IFDEF cODEDebugEnabled}
   If Not DisabledDebugGeom Then
      Begin
      if Not Assigned(ODEDebugGeomList) then
         ODEDebugGeomList := TGeomList.create();
      ODEDebugGeomList.Add(result);
      End ;
{$ENDIF}
end;

{$ifdef cTRILIST}
function dCreateTriMesh(const Space : PdxSpace; Data: PdxTriMeshData; Callback, ArrayCallback, RayCallback: Pointer): PdxGeom; cdecl;

begin
  result := EXT_dCreateTriMesh(Space, Data, Callback, ArrayCallback, RayCallback);

  if dTriMeshClass=-1 then
    dTriMeshClass := dGeomGetClass(result);
end;
{$endif}


{function dCreateGeomTransformGroup (const Space : PdxSpace) : PdxGeom; cdecl;
begin
  result := EXT_dCreateGeomTransformGroup(Space);

  if dGeomTransformGroupClass=-1 then
    dGeomTransformGroupClass := dGeomGetClass(result);
end;//}

procedure DisableStillBodies(World : PdxWorld; Threshold : TdReal=0.0001);
var
  Body : PdxBody;
  TempList : TList;
begin
  if not Assigned(WasStillBefore) then
  begin
    WasStillBefore := TList.Create;
    WasStillBeforeOld := TList.Create;
  end;

  Body := World.FirstBody;

  WasStillBefore.Clear;

  // We can't disable bodies just as soon as they're still - that could disable
  // bodies that are just slowed down or titering on an edge. If they've been
  // still for two frames, we consider them truly still. 
  while Assigned(Body) do
  begin
    if dBodyIsEnabled(Body)=1 then
    begin
      // Is the body still?
      if (abs(Body.lvel[0])<Threshold) and (abs(Body.lvel[1])<Threshold) and (abs(Body.lvel[2])<Threshold) and
         (abs(Body.avel[0])<Threshold) and (abs(Body.avel[1])<Threshold) and (abs(Body.avel[2])<Threshold) then
      begin
        if WasStillBeforeOld.IndexOf(Body)<>-1 then
          dBodyDisable(Body)
        else
          WasStillBefore.Add(Body);
      end;
    end;

    Body := PdxBody(Body.BaseObject.next);
  end;

  TempList := WasStillBeforeOld;
  WasStillBeforeOld := WasStillBefore;
  WasStillBefore := TempList;
end;

procedure VerifyDelphiODE(Body : PdxBody; Geom : PdxGeom);
var
  m : TdMass;
begin
  // Verify Body
  dBodySetData(Body, pointer(-1));
  Assert(dBodyGetData(Body)=pointer(-1), 'Body test 1 fails');
  Assert(Body.BaseObject.userdata=pointer(-1), 'Body test 2 fails');

  dBodyGetMass(Body, m);

  Assert(Body.mass.mass = m.mass, 'Body test 3 fails');

  // Verify Geom
  dGeomSetData(Geom, pointer(-1));
  Assert(dGeomGetData(Geom)=pointer(-1), 'Geom test 1 fails');
  Assert(dGeomGetBody(Geom)=Geom.Body, 'Geom test 2 fails');
  Assert(Geom.Data=pointer(-1), 'Geom test 3 fails');
end;
end.

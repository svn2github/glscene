{
  GLODEManager - An ODE Manager for GLScene

  Where can I find ... ?
  GLScene              (http://glscene.org)
  Open Dynamics Engine (http://opende.sourceforge.org)
  DelphiODE            (http://www.cambrianlabs.com/Mattias/DelphiODE)

  Note:
  This code is still being developed so any part of it may change at anytime.

  History:

    09/06/03 - SG - Added OnCollision event for ODE Objects and Behaviours.
    08/06/03 - SG - Added rolling friction (experimental).
    06/06/03 - SG - Added cylinder element (experimental).
    04/06/03 - SG - Changes to structures, added TGLODEDynamicBehaviour.
    30/05/03 - SG - Added capsule element and plane object,
                    Fixed problems with Collision callback method.
    29/05/03 - SG - Better GetCollisionSurface code (thanks to Mattias Fagerlund).
    28/05/03 - SG - Some fixes to ODE Elements (thanks to Mattias Fagerlund).
                    Added TGLODEDummy.CalibrateCenterOfMass
    01/03/03 - SG - Creation.
}

unit GLODEManager;

interface

uses
  Classes,ODEImport,ODEGL,GLScene,GLMisc,Geometry,GLTexture,
  OpenGL12,XOpenGL,SysUtils,GLObjects,XCollection,Contnrs;

type

  TODEElements = class;
  TODEBaseElement = class;

  TODECollisionEvent = procedure (Sender : TObject; Object1, Object2 : TObject;
                                  var Contact:TdContact;
                                  var HandleCollision:Boolean) of object;

  TODEObjectCollisionEvent = procedure (Sender : TObject; Object2 : TObject;
                                        Contact:TdContact) of object;

  TODECollisionSurfaceMode = (csmMu2,csmFDir1,csmBounce,csmSoftERP,csmSoftCFM,
                              csmMotion1,csmMotion2,csmSlip1,csmSlip2);
  TSurfaceModes = set of TODECollisionSurfaceMode;

  {
  TGLODEManager:
  ----------
  This is the main component that houses the links to ODE.
  }
  TGLODEManager = class (TComponent)
    private
      FWorld             : PdxWorld;
      FSpace             : PdxSpace;
      FContactGroup,
      FJointGroup        : TdJointGroupID;
      FGravity           : TGLCoordinates;
      FOnCollision       : TODECollisionEvent;
      FContactJointCount,
      FNumContactJoints  : integer;
      FDynamicObjectRegister : TObjectList;
      FRFContactList     : TList; // Rolling friction list
      procedure SetGravity(value:TGLCoordinates);
      procedure GravityChange(Sender:TObject);
    protected
      {: Calculate the contact between 2 objects based on their Collision 
         Surfaces. }
      procedure CalcContact(Object1, Object2 : TObject; var Contact:TdContact);
      {: ODE collision callback function. Passed through the nearCallback 
         global procedure }
      procedure Collision(g1,g2:PdxGeom);
      {: Register an ODE object for auto updating. Used for aligning dynamic
         objects and behaviours to their ODE couterparts after the ODE World
         steps. }
      procedure RegisterObject(aObject:TObject);
      {: Removes an object from the auto updating list. The object will no
         longer update after the ODE World is steps. }
      procedure UnregisterObject(aObject:TObject);
    public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
      {: Steps the ODE world, then aligns any registered objects to it's ODE
         counterpart, and then the rolling friction is calculated. }
      procedure Step(deltaTime:double);
      property World : PdxWorld read FWorld;
      property Space : PdxSpace read FSpace;
      property NumContactJoints : integer read FNumContactJoints;
      property JointGroup : TdJointGroupID read FJointGroup;
    published
      {: The world gravity vector. By default this is a zero length vector
         (no gravity). }
      property Gravity     : TGLCoordinates read FGravity write SetGravity;
      {: This event occurs after the contact information has been filled out
         by the CalcContact procedure and before the contact joint is created.
         The HandleCollision parameter can be set to determine if a collision
         is added the Contact Joints. Any 'last minute' to the collisions
         behaviour can be made through the contact parameter. }
      property OnCollision : TODECollisionEvent read FOnCollision write FOnCollision;
  end;

  {
  TODECollisionSurface:
  ---------------------
  This class is used to describe how an ODE object should behave when it
  collides with another object. This data is used by the TGLODEManager.CalcContact
  procedure, averaging the values of the CollisionSurface to produce an ODE
  contact surface that is (hopefully) accurate. Any fine tuning can be done in
  the TGLODEManager.OnCollision event which is fired after the TGLODEManager.CalcContact
  procedure.
  }
  TODECollisionSurface = class (TPersistent)
    private
      FSurfaceParams : TdSurfaceParameters;
      FRFCoeff   : Single;
      FRFEnabled : Boolean;

      function GetSurfaceMode : TSurfaceModes;
      function GetMu : double;
      function GetMu2 : double;
      function GetBounce : double;
      function GetBounce_Vel : double;
      function GetSoftERP : double;
      function GetSoftCFM : double;
      function GetMotion1 : double;
      function GetMotion2 : double;
      function GetSlip1 : double;
      function GetSlip2 : double;

      procedure SetSurfaceMode(value:TSurfaceModes);
      procedure SetMu(value : double);
      procedure SetMu2(value : double);
      procedure SetBounce(value : double);
      procedure SetBounce_Vel(value : double);
      procedure SetSoftERP(value : double);
      procedure SetSoftCFM(value : double);
      procedure SetMotion1(value : double);
      procedure SetMotion2(value : double);
      procedure SetSlip1(value : double);
      procedure SetSlip2(value : double);
    public
      constructor Create;
    published
      property RollingFrictionCoeff : Single read FRFCoeff write FRFCoeff;
      property RollingFrictionEnabled : Boolean read FRFEnabled write FRFEnabled;
      property SurfaceMode : TSurfaceModes read GetSurfaceMode write SetSurfaceMode;
      property Mu : double read GetMu write SetMu;
      property Mu2 : double read GetMu2 write SetMu2;
      property Bounce : double read GetBounce write SetBounce;
      property Bounce_Vel : double read GetBounce_Vel write SetBounce_Vel;
      property SoftERP : double read GetSoftERP write SetSoftERP;
      property SoftCFM : double read GetSoftCFM write SetSoftCFM;
      property Motion1 : double read GetMotion1 write SetMotion1;
      property Motion2 : double read GetMotion2 write SetMotion2;
      property Slip1 : double read GetSlip1 write SetSlip1;
      property Slip2 : double read GetSlip2 write SetSlip2;
  end;

  TODEElementClass = class of TODEBaseElement;

  {
  TGLODEBaseObject:
  -----------------
  This object provides the base level links to the GLScene object
  heirachy and the GLODEManager. It contains all common properties
  that GLScene <--> ODE objects will require.
  }
  TGLODEBaseObject = class(TGLBaseSceneObject)
    private
      FManager       : TGLODEManager;
      FCollisionSurface : TODECollisionSurface;
      FVisibleAtRunTime : Boolean;
      FOnCollision : TODEObjectCollisionEvent;
      procedure SetSurface(value:TODECollisionSurface);
      procedure SetManager(Value:TGLODEManager);
    protected
      procedure SetVisibleAtRunTime(Value:Boolean);
      {: The initialize and deinitialize procedures are used to create and
         destroy the ODE components. }
      procedure Initialize; virtual;
      procedure Deinitialize; virtual;
    public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      procedure StructureChanged; override;
      procedure NotifyChange(Sender:TObject); override;
    published
      {: The link to the GLODEManager, which houses the ODE collision Space
         and dynamic World pointers. }
      property Manager : TGLODEManager read FManager write SetManager;
      {: The collision surface describes how this object should react in a 
         collision. When it collides with another object the surfaces from
         both objects are mixed to produce the ODE contact parameters, which
         are used by ODE to handle collisions. }
      property Surface : TODECollisionSurface read FCollisionSurface write SetSurface;
      //: Enable this property to have the objects render at run-time.
      property VisibleAtRunTime : Boolean read FVisibleAtRunTime write SetVisibleAtRuntime;
      {: Object level collision event. Object2 is the object being collided 
         with and the contact parameter provides information on the collision. }
      property OnCollision : TODEObjectCollisionEvent read FOnCollision write FOnCollision;

      property ObjectsSorting;
      property VisibilityCulling;
      property Direction;
      property PitchAngle;
      property Position;
      property RollAngle;
      property ShowAxes;
      property TransformationMode;
      property TurnAngle;
      property Up;
      property Visible;
      property OnProgress;
      property Name;
  end;

  {
  TGLODEStaticObject:
  -------------------
  This object provides decendant classes for static ODE implementations
  which have a geom for collisions but no body or mass for motion.
  }
  TGLODEStaticObject = class (TGLODEBaseObject)
    private
      FGeom : PdxGeom;
    protected
      procedure Deinitialize; override;
    public
      property Geom : PdxGeom read FGeom;
  end;

  {
  TGLODEPlane:
  ------------
  The ODE plane geom implementation. Use the direction and position to set
  up the plane (just like a normal GLScene plane).
  }
  TGLODEPlane = class (TGLODEStaticObject)
    private
      procedure AlignODEPlane;
    protected
      procedure Initialize; override;
    public
      constructor Create(AOwner:TComponent); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      procedure NotifyChange(Sender:TObject); override;
      procedure StructureChanged; override;
  end;

  {
  TGLODEDynamicObject:
  --------------------
  This object provides decendant classes for dynamic ODE implementations.
  }
  TGLODEDynamicObject = class (TGLODEBaseObject)
    private
      FBody       : PdxBody;
      FMass       : TdMass;
      FRealignODE : Boolean;
      procedure AlignBodyToMatrix(Mat: TMatrix);
      procedure SetMass(const value:TdMass);
      function GetMass : TdMass;
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
    public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      procedure StructureChanged; override;

      property Body : PdxBody read FBody;
      property Mass : TdMass read GetMass write SetMass;
  end;

  {
  TGLODEDummy:
  ------------
  This is the main object of GLODEManager. It is basically a composite
  object built from it's child elements. To add elements at run-time
  use the AddNewElement function.
  }
  TGLODEDummy = class (TGLODEDynamicObject)
    private
      FElements : TODEElements;
      FColor : TGLColor;
      procedure SetColor(const Value: TGLColor);
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
    public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      procedure StructureChanged; override;

      function AddNewElement(AChild:TODEElementClass):TODEBaseElement;
      procedure AlignObject;
      function CalculateMass : TdMass;
      procedure CalibrateCenterOfMass;
    published
      property Color : TGLColor read FColor write SetColor;
      property Elements : TODEElements read FElements;
  end;

  {
  TGLODEBaseBehaviour:
  ----------------
  Basis structures for GLScene behaviour style implementations.
  }
  TGLODEBaseBehaviour = class (TGLBehaviour)
    private
      FManager : TGLODEManager;
      FSurface : TODECollisionSurface;
      FOnCollision : TODEObjectCollisionEvent;
      procedure SetManager(Value : TGLODEManager);
      procedure SetSurface(value:TODECollisionSurface);
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      procedure Initialize; virtual;
      procedure Deinitialize; virtual;
    published
      property Manager : TGLODEManager read FManager write SetManager;
      property Surface : TODECollisionSurface read FSurface write SetSurface;
      property OnCollision : TODEObjectCollisionEvent read FOnCollision write FOnCollision;
  end;

  {
  TGLODEDynamicBehaviour:
  -----------------------
  GLScene behaviour style implementation of the GLODEDummy, used for linking
  a GLScene object to ODE control through GLScene behaviours.
  }
  TGLODEDynamicBehaviour = class (TGLODEBaseBehaviour)
    private
      FBody : PdxBody;
      FMass : TdMass;
      FElements : TODEElements;
      procedure SetMass(const Value : TdMass);
      function GetMass : TdMass;
      procedure AlignBodyToMatrix(Mat: TMatrix);
      function GetAbsoluteMatrix : TMatrix;
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      class function FriendlyName : String; override;
      procedure Initialize; override;
      procedure Deinitialize; override;
      function AddNewElement(AChild:TODEElementClass):TODEBaseElement; dynamic;
      procedure AlignObject;
      function CalculateMass : TdMass;
      procedure CalibrateCenterOfMass;
      property AbsoluteMatrix : TMatrix read GetAbsoluteMatrix;
      property Body : PdxBody read FBody;
      property Mass : TdMass read GetMass write SetMass;
    published
      property Elements : TODEElements read FElements;
  end;

  {
  TODEElements:
  -------------
  This is the list class that stores the ODEElements for GLODEDummy
  and GLODEDynamicBehaviour objects. When Remove or Delete is called the
  element is destroyed before being removed from the list.
  }
  TODEElements = class(TPersistent)
    private
      FOwner : TPersistent;
      FList  : TList;
      function GetItem(index: integer): TODEBaseElement;
    public
      constructor Create(AOwner:TPersistent);
      destructor Destroy; override;
      procedure Initialize;
      procedure Deinitialize;
      function Add(item:TODEBaseElement) : integer;
      function Count : integer;
      procedure Delete(index:integer);
      procedure Remove(item:TODEBaseElement);
      property Owner : TPersistent read FOwner;
      property Items[index:integer]:TODEBaseElement read GetItem; default;
  end;

  {
  TODEBaseElement:
  ----------------
  This class is the basis for all ODEElements. It provides common
  ODE properties like Mass, Density and a Geom to decended classes.
  To orient the object inside the GLODEDummy use Position, Direction
  and Up or use Matrix. The ODE component of the object will be
  aligned to any changes made to these properties.
  }
  TODEBaseElement = class (TPersistent)
    private
      FOwner : TODEElements;
      FMass  : TdMass;
      FDensity : single;
      FGeomTransform,
      FGeomElement   : PdxGeom;
      FPosition,
      FDirection,
      FUp        : TGLCoordinates;
      FLocalMatrix : TMatrix;
      FRealignODE : Boolean;
      FInitialized : Boolean;
      procedure AlignGeomToMatrix(Mat:TMatrix);
      procedure SetDensity(const Value: single);
      procedure SetMatrix(const Value: TMatrix);
      function GetMatrix: TMatrix;
      procedure NotifyChange(Sender:TObject);
    protected
      procedure RebuildMatrix;
      procedure Initialize; virtual;
      procedure Deinitialize; virtual;
      function CalculateMass : TdMass; virtual;
      procedure ODERebuild; virtual;
    public
      constructor Create(AOwner:TODEElements); virtual;
      destructor Destroy; override;
      procedure BuildList(var rci : TRenderContextInfo); virtual;
      function AbsoluteMatrix:TMatrix;
      function AbsolutePosition:TAffineVector;
      property Matrix : TMatrix read GetMatrix write SetMatrix;
      property GeomTransform : PdxGeom read FGeomTransform;
      property Geom : PdxGeom read FGeomElement;
    published
      property Owner : TODEElements read FOwner;
      property Density : single read FDensity write SetDensity;
      property Position : TGLCoordinates read FPosition;
      property Direction : TGLCoordinates read FDirection;
      property Up : TGLCoordinates read FUp;
  end;

  {
  TODEElementBox:
  ---------------
  ODE box implementation.
  }
  TODEElementBox = class (TODEBaseElement)
    private
      FBoxWidth,
      FBoxHeight,
      FBoxDepth : single;
      function GetBoxWidth  : single;
      function GetBoxHeight : single;
      function GetBoxDepth  : single;
      procedure SetBoxWidth(const Value: single);
      procedure SetBoxHeight(const Value: single);
      procedure SetBoxDepth(const Value: single);
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
    public
      constructor Create(AOwner:TODEElements); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
    published
      property BoxWidth : single read GetBoxWidth write SetBoxWidth;
      property BoxHeight : single read GetBoxHeight write SetBoxHeight;
      property BoxDepth : single read GetBoxDepth write SetBoxDepth;
  end;

  {
  TODEElementSphere:
  ------------------
  ODE sphere implementation.
  }
  TODEElementSphere = class (TODEBaseElement)
    private
      FRadius : Single;
      function GetRadius : single;
      procedure SetRadius(const Value: single);
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
    public
      constructor Create(AOwner:TODEElements); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
    published
      property Radius : single read GetRadius write SetRadius;
  end;

  {
  TODEElementCapsule:
  -------------------
  ODE capped cylinder implementation.
  }
  TODEElementCapsule = class (TODEBaseElement)
    private
      FRadius,
      FLength : single;
      function GetRadius : single;
      function GetLength : single;
      procedure SetRadius(const Value: single);
      procedure SetLength(const Value: single);
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
    public
      constructor Create(AOwner:TODEElements); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
    published
      property Radius : single read GetRadius write SetRadius;
      property Length : single read GetLength write SetLength;
  end;

  {
  TODEElementCylinder:
  --------------------
  ODE cylinder implementation.
  }
  TODEElementCylinder = class (TODEBaseElement)
    private
      FRadius,
      FLength : single;
      function GetRadius : single;
      function GetLength : single;
      procedure SetRadius(const Value: single);
      procedure SetLength(const Value: single);
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
    public
      constructor Create(AOwner:TODEElements); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
    published
      property Radius : single read GetRadius write SetRadius;
      property Length : single read GetLength write SetLength;
  end;

  TODEBaseJoint = class;

  {
  TODEJoints:
  -----------
  This is the list class that stores the ODE Joints.
  }
  TODEJoints = class(TPersistent)
    private
      FOwner : TPersistent;
      FList  : TList;
      function GetItem(index: integer): TODEBaseJoint;
    public
      constructor Create(AOwner:TPersistent);
      destructor Destroy; override;
      procedure Initialize;
      procedure Deinitialize;
      function Add(item:TODEBaseJoint) : integer;
      function Count : integer;
      procedure Delete(index:integer);
      procedure Remove(item:TODEBaseJoint);
      property Owner : TPersistent read FOwner;
      property Items[index:integer]:TODEBaseJoint read GetItem; default;
  end;

  TJointGetParamsProc = function (const dJointID : TdJointID; const parameter: TJointParams): TdReal;
  TJointSetParamsProc = procedure (const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal);
  {
  TODEJointParams:
  ----------------
  This is the list class that stores the ODE Joints.
  }
  TODEJointParams = class
    private
      FOwner : TODEBaseJoint;
      FJointGetParamsProc : TJointGetParamsProc;
      FJointSetParamsProc : TJointSetParamsProc;

      function GetLoStop : TdReal;
      function GetHiStop : TdReal;
      function GetVel : TdReal;
      function GetFMax : TdReal;
      function GetFudgeFactor : TdReal;
      function GetBounce : TdReal;
      function GetCFM : TdReal;
      function GetStopERP : TdReal;
      function GetStopCFM : TdReal;
      function GetSuspensionERP : TdReal;
      function GetSuspensionCFM : TdReal;

      function GetLoStop2 : TdReal;
      function GetHiStop2 : TdReal;
      function GetVel2 : TdReal;
      function GetFMax2 : TdReal;
      function GetFudgeFactor2 : TdReal;
      function GetBounce2 : TdReal;
      function GetCFM2 : TdReal;
      function GetStopERP2 : TdReal;
      function GetStopCFM2 : TdReal;
      function GetSuspensionERP2 : TdReal;
      function GetSuspensionCFM2 : TdReal;

      function GetLoStop3 : TdReal;
      function GetHiStop3 : TdReal;
      function GetVel3 : TdReal;
      function GetFMax3 : TdReal;
      function GetFudgeFactor3 : TdReal;
      function GetBounce3 : TdReal;
      function GetCFM3 : TdReal;
      function GetStopERP3 : TdReal;
      function GetStopCFM3 : TdReal;
      function GetSuspensionERP3 : TdReal;
      function GetSuspensionCFM3 : TdReal;

      procedure SetLoStop(const Value : TdReal);
      procedure SetHiStop(const Value : TdReal);
      procedure SetVel(const Value : TdReal);
      procedure SetFMax(const Value : TdReal);
      procedure SetFudgeFactor(const Value : TdReal);
      procedure SetBounce(const Value : TdReal);
      procedure SetCFM(const Value : TdReal);
      procedure SetStopERP(const Value : TdReal);
      procedure SetStopCFM(const Value : TdReal);
      procedure SetSuspensionERP(const Value : TdReal);
      procedure SetSuspensionCFM(const Value : TdReal);

      procedure SetLoStop2(const Value : TdReal);
      procedure SetHiStop2(const Value : TdReal);
      procedure SetVel2(const Value : TdReal);
      procedure SetFMax2(const Value : TdReal);
      procedure SetFudgeFactor2(const Value : TdReal);
      procedure SetBounce2(const Value : TdReal);
      procedure SetCFM2(const Value : TdReal);
      procedure SetStopERP2(const Value : TdReal);
      procedure SetStopCFM2(const Value : TdReal);
      procedure SetSuspensionERP2(const Value : TdReal);
      procedure SetSuspensionCFM2(const Value : TdReal);

      procedure SetLoStop3(const Value : TdReal);
      procedure SetHiStop3(const Value : TdReal);
      procedure SetVel3(const Value : TdReal);
      procedure SetFMax3(const Value : TdReal);
      procedure SetFudgeFactor3(const Value : TdReal);
      procedure SetBounce3(const Value : TdReal);
      procedure SetCFM3(const Value : TdReal);
      procedure SetStopERP3(const Value : TdReal);
      procedure SetStopCFM3(const Value : TdReal);
      procedure SetSuspensionERP3(const Value : TdReal);
      procedure SetSuspensionCFM3(const Value : TdReal);
    public
      property Owner : TODEBaseJoint read FOwner;
    published
      property LoStop : TdReal read GetLoStop write SetLoStop;
      property HiStop : TdReal read GetHiStop write SetHiStop;
      property Vel : TdReal read GetVel write SetVel;
      property FMax : TdReal read GetFMax write SetFMax;
      property FudgeFactor : TdReal read GetFudgeFactor write SetFudgeFactor;
      property Bounce : TdReal read GetBounce write SetBounce;
      property CFM : TdReal read GetCFM write SetCFM;
      property StopERP : TdReal read GetStopERP write SetStopERP;
      property StopCFM : TdReal read GetStopCFM write SetStopCFM;
      property SuspensionERP : TdReal read GetSuspensionERP write SetSuspensionERP;
      property SuspensionCFM : TdReal read GetSuspensionCFM write SetSuspensionCFM;

      property LoStop2 : TdReal read GetLoStop2 write SetLoStop2;
      property HiStop2 : TdReal read GetHiStop2 write SetHiStop2;
      property Vel2 : TdReal read GetVel2 write SetVel2;
      property FMax2 : TdReal read GetFMax2 write SetFMax2;
      property FudgeFactor2 : TdReal read GetFudgeFactor2 write SetFudgeFactor2;
      property Bounce2 : TdReal read GetBounce2 write SetBounce2;
      property CFM2 : TdReal read GetCFM2 write SetCFM2;
      property StopERP2 : TdReal read GetStopERP2 write SetStopERP2;
      property StopCFM2 : TdReal read GetStopCFM2 write SetStopCFM2;
      property SuspensionERP2 : TdReal read GetSuspensionERP2 write SetSuspensionERP2;
      property SuspensionCFM2 : TdReal read GetSuspensionCFM2 write SetSuspensionCFM2;

      property LoStop3 : TdReal read GetLoStop3 write SetLoStop3;
      property HiStop3 : TdReal read GetHiStop3 write SetHiStop3;
      property Vel3 : TdReal read GetVel3 write SetVel3;
      property FMax3 : TdReal read GetFMax3 write SetFMax3;
      property FudgeFactor3 : TdReal read GetFudgeFactor3 write SetFudgeFactor3;
      property Bounce3 : TdReal read GetBounce3 write SetBounce3;
      property CFM3 : TdReal read GetCFM3 write SetCFM3;
      property StopERP3 : TdReal read GetStopERP3 write SetStopERP3;
      property StopCFM3 : TdReal read GetStopCFM3 write SetStopCFM3;
      property SuspensionERP3 : TdReal read GetSuspensionERP3 write SetSuspensionERP3;
      property SuspensionCFM3 : TdReal read GetSuspensionCFM3 write SetSuspensionCFM3;
  end;

  {
  TODEBaseJoint:
  --------------
  Base structures for ODE Joints.
  }
  TODEBaseJoint = class
    private
      FJointID : TdJointID;
      FBody1,
      FBody2   :  PdxBody;
      FManager : TGLODEManager;
      FJointParams : TODEJointParams;
    protected
      procedure Initialize; virtual;
      procedure Deinitialize; virtual;
    public
      property JointID : TdJointID read FJointID;
      property Body1 : PdxBody read FBody1;
      property Body2 : PdxBody read FBody2;
    published
      property JointParams : TODEJointParams read FJointParams;
  end;

  {
  TODEJointHinge:
  ---------------
  ODE hinge joint implementation.
  }
  TODEJointHinge = class (TODEBaseJoint)
    private

    protected
      procedure Initialize; override;
    public

  end;

procedure nearCallBack(Data:Pointer; o1,o2:PdxGeom); cdecl;
procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------------------------------------------------------
// Global Procedures
// ------------------------------------------------------------------

// nearCallBack
//
procedure nearCallBack(Data:Pointer; o1,o2:PdxGeom); cdecl;
begin
  TGLODEManager(Data).Collision(o1,o2);
end;

// Register
//
procedure Register;
begin
  RegisterClasses([TGLODEManager, TGLODEDummy, TGLODEPlane]);
  RegisterComponents('GLScene',[TGLODEManager]);
end;


// ------------------------------------------------------------------
// TGLODEManager Methods
// ------------------------------------------------------------------

// Create
//
constructor TGLODEManager.Create(AOwner:TComponent);
begin
  FGravity:=TGLCoordinates.CreateInitialized(Self, NullHmgPoint, csVector);
  FGravity.OnNotifyChange:=GravityChange;

  inherited Create(AOwner);

  FWorld:=dWorldCreate;
  FSpace:=dHashSpaceCreate(nil);
  FContactGroup:=dJointGroupCreate(100);
  FJointGroup:=dJointGroupCreate(100);
  FDynamicObjectRegister:=TObjectList.Create;
  FDynamicObjectRegister.OwnsObjects:=False;
  FRFContactList:=TList.Create;

  GravityChange(nil);
  dWorldSetCFM(FWorld,1e-5);
end;

// Destroy
//
destructor TGLODEManager.Destroy;
begin
  FDynamicObjectRegister.Clear;
  FDynamicObjectRegister.Free;
  FGravity.Free;
  FRFContactList.Free;
  dJointGroupDestroy(FContactGroup);
  dJointGroupDestroy(FJointGroup);
  dSpaceDestroy(FSpace);
  dWorldDestroy(FWorld);
  inherited Destroy;
end;

// RegisterObject
//
procedure TGLODEManager.RegisterObject(aObject: TObject);
begin
  FDynamicObjectRegister.Add(aObject);
end;

// UnregisterObject
//
procedure TGLODEManager.UnregisterObject(aObject: TObject);
begin
  FDynamicObjectRegister.Remove(aObject);
end;

// SetGravity
//
procedure TGLODEManager.SetGravity(value:TGLCoordinates);
begin
  FGravity.SetPoint(value.DirectX,value.DirectY,value.DirectZ);
end;

// GravityChange
//
procedure TGLODEManager.GravityChange(Sender:TObject);
begin
  dWorldSetGravity(FWorld,FGravity.X,FGravity.Y,FGravity.Z);
end;

// CalculateContact
//
procedure TGLODEManager.CalcContact(Object1, Object2 : TObject; var Contact:TdContact);
var
  Surface1, Surface2 : TODECollisionSurface;
  Body1, Body2 : PdxBody;
begin
  Surface1:=nil; Surface2:=nil;
  if Object1 is TGLODEBaseObject then
    Surface1:=TGLODEBaseObject(Object1).Surface;
  if Object1 is TGLODEDynamicBehaviour then
    Surface1:=TGLODEDynamicBehaviour(Object1).Surface;
  if Object2 is TGLODEBaseObject then
    Surface2:=TGLODEBaseObject(Object2).Surface;
  if Object2 is TGLODEDynamicBehaviour then
    Surface2:=TGLODEDynamicBehaviour(Object2).Surface;

  if not (Assigned(Surface1) and Assigned(Surface2)) then
    exit;

  with contact.surface do begin
    // Average the involved contact information and assign it to the contact.
    // Better methods for contact calculation will be looked into in the future.
    mode:=Surface1.FSurfaceParams.mode or Surface2.FSurfaceParams.mode;
    mu:=(Surface1.Mu+Surface2.Mu)*0.5;
    mu2:=(Surface1.Mu2+Surface2.Mu2)*0.5;
    bounce:=(Surface1.Bounce+Surface2.Bounce)*0.5;
    bounce_vel:=(Surface1.Bounce_Vel+Surface2.Bounce_Vel)*0.5;
    soft_erp:=(Surface1.SoftERP+Surface2.SoftERP)*0.5;
    soft_cfm:=(Surface1.SoftCFM+Surface2.SoftCFM)*0.5;
    motion1:=(Surface1.Motion1+Surface2.Motion1)*0.5;
    motion2:=(Surface1.Motion2+Surface2.Motion2)*0.5;
    slip1:=(Surface1.Slip1+Surface2.Slip1)*0.5;
    slip2:=(Surface1.Slip2+Surface2.Slip2)*0.5;
  end;

  // Rolling friction
  Body1:=nil; Body2:=nil;
  if Object1 is TGLODEDynamicObject then
    Body1:=TGLODEDynamicObject(Object1).Body;
  if Object1 is TGLODEDynamicBehaviour then
    Body1:=TGLODEDynamicBehaviour(Object1).Body;
  if Object2 is TGLODEDynamicObject then
    Body2:=TGLODEDynamicObject(Object2).Body;
  if Object2 is TGLODEDynamicBehaviour then
    Body2:=TGLODEDynamicBehaviour(Object2).Body;
  if (Surface1.RollingFrictionEnabled) and Assigned(Body1) then
    FRFContactList.Add(Object1);
  if (Surface2.RollingFrictionEnabled) and Assigned(Body2) then
    FRFContactList.Add(Object2);
end;

// Collision
//
procedure TGLODEManager.Collision(g1,g2:PdxGeom);
var
  i : integer;
  Obj1, Obj2 : Pointer;
  b1, b2 : PdxBody;
  numc : integer;
  contact : array of TdContact;
  geomcontact : TdContactGeom;
  Joint : TdJointID;
  HandleCollision : Boolean;
begin
  Obj1:=dGeomGetData(g1);
  Obj2:=dGeomGetData(g2);
  b1:=dGeomGetBody(g1);
  b2:=dGeomGetBody(g2);

  if Assigned(b1) and Assigned(b2) then
    if dAreConnected(b1,b2)=1 then
      exit;

  // Get the collisions
  numc:=dCollide(g1,g2,1,geomcontact,sizeof(TdContact));

  // Set up the initial contact info
  SetLength(contact,numc);
  for i:=0 to numc-1 do begin
    contact[i].geom:=geomcontact;
  end;

  for i:=0 to numc-1 do begin
    HandleCollision:=True;

    // Increment the number of contact joints this step
    FContactJointCount:=FContactJointCount+1;
    if Assigned(Obj1) and Assigned(Obj2) then begin
      // Calculate the contact based on Obj1 and Obj2 surface info
      CalcContact(Obj1,Obj2,Contact[i]);
      if Assigned(FOnCollision) then begin
        // Fire the Scene level OnCollision event for last minute 
        // customization to the contact before the contact joint 
        // is created
        FOnCollision(Self,Obj1,Obj2,Contact[i],HandleCollision);
      end;
      // Fire the OnCollision event for each object 
      if TObject(Obj1) is TGLODEBaseObject then
        if Assigned(TGLODEBaseObject(Obj1).OnCollision) then
          TGLODEBaseObject(Obj1).OnCollision(Self,Obj2,Contact[i]);
      if TObject(Obj2) is TGLODEBaseObject then
        if Assigned(TGLODEBaseObject(Obj2).OnCollision) then
          TGLODEBaseObject(Obj2).OnCollision(Self,Obj1,Contact[i]);
      if TObject(Obj1) is TGLODEBaseBehaviour then
        if Assigned(TGLODEBaseBehaviour(Obj1).OnCollision) then
          TGLODEBaseBehaviour(Obj1).OnCollision(Self,Obj2,Contact[i]);
      if TObject(Obj2) is TGLODEBaseBehaviour then
        if Assigned(TGLODEBaseObject(Obj2).OnCollision) then
          TGLODEBaseBehaviour(Obj2).OnCollision(Self,Obj1,Contact[i]);
    end else begin
      // Default surface values
      contact[i].surface.mu:=1000;
    end;
    if HandleCollision then begin
      Joint:=dJointCreateContact(FWorld,FContactGroup,contact[i]);
      dJointAttach(Joint,b1,b2);
    end;
  end;
end;

// Step
//
procedure TGLODEManager.Step(deltaTime:double);
var
  i : Integer;
  vec   : PdVector3;
  body  : PdxBody;
  Coeff : Single;
begin
  // Run ODE collisions and step the scene
  dSpaceCollide(FSpace,Self,nearCallback);
  dWorldStep(FWorld,deltaTime);
  dJointGroupEmpty(FContactGroup);

  // Align dynamic objects to their ODE bodies
  for i:=0 to FDynamicObjectRegister.Count-1 do begin
    if FDynamicObjectRegister[i] is TGLODEDummy then
      TGLODEDummy(FDynamicObjectRegister[i]).AlignObject;
    if FDynamicObjectRegister[i] is TGLODEDynamicBehaviour then
      TGLODEDynamicBehaviour(FDynamicObjectRegister[i]).AlignObject;
  end;

  // Process rolling friction
  Coeff:=0;
  body:=nil;
  while FRFContactList.Count>0 do begin
    if TObject(FRFContactList[0]) is TGLODEDummy then begin
      Body:=TGLODEDummy(FRFContactList[0]).Body;
      Coeff:=1-(TGLODEDummy(FRFContactList[0]).Surface.RollingFrictionCoeff/
                TGLODEDummy(FRFContactList[0]).Mass.Mass);
    end else
    if TObject(FRFContactList[0]) is TGLODEDynamicBehaviour then begin
      Body:=TGLODEDynamicBehaviour(FRFContactList[0]).Body;
      Coeff:=1-(TGLODEDynamicBehaviour(FRFContactList[0]).Surface.RollingFrictionCoeff/
                TGLODEDynamicBehaviour(FRFContactList[0]).Mass.Mass);
    end;
    vec:=dBodyGetAngularVel(body);
    dBodySetAngularVel(body,vec[0]*Coeff,vec[1]*Coeff,vec[2]*Coeff);
    FRFContactList.Delete(0);
  end;
end;


// ------------------------------------------------------------------
// TODECollisionSurface Methods
// ------------------------------------------------------------------

// Create
//
constructor TODECollisionSurface.Create;
begin
  inherited Create;
  Mu:=1000;
  RollingFrictionEnabled:=False;
  RollingFrictionCoeff:=0.001;    // Larger Coeff = more friction
end;

// GetSurfaceMode
//
function TODECollisionSurface.GetSurfaceMode:TSurfaceModes;
var
  ASurfaceModes : TSurfaceModes;
begin
  ASurfaceModes := [];
  if (FSurfaceParams.Mode and dContactSlip2)<>0 then
    ASurfaceModes:=ASurfaceModes+[csmSlip2];
  if (FSurfaceParams.Mode and dContactSlip1)<>0 then
    ASurfaceModes:=ASurfaceModes+[csmSlip1];
  if (FSurfaceParams.Mode and dContactMotion2)<>0 then
    ASurfaceModes:=ASurfaceModes+[csmMotion2];
  if (FSurfaceParams.Mode and dContactMotion1)<>0 then
    ASurfaceModes:=ASurfaceModes+[csmMotion1];
  if (FSurfaceParams.Mode and dContactSoftCFM)<>0 then
    ASurfaceModes:=ASurfaceModes+[csmSoftCFM];
  if (FSurfaceParams.Mode and dContactSoftERP)<>0 then
    ASurfaceModes:=ASurfaceModes+[csmSoftERP];
  if (FSurfaceParams.Mode and dContactBounce)<>0 then
    ASurfaceModes:=ASurfaceModes+[csmBounce];
  if (FSurfaceParams.Mode and dContactFDir1)<>0 then 
    ASurfaceModes:=ASurfaceModes+[csmFDir1];
  if (FSurfaceParams.Mode and dContactMu2)<>0 then
    ASurfaceModes:=ASurfaceModes+[csmMu2];
  result:=ASurfaceModes;
end;

// SetSurfaceMode
//
procedure TODECollisionSurface.SetSurfaceMode(value:TSurfaceModes);
var
  AMode : Byte;
begin
  AMode := 0;
  if csmSlip2 in value then
    AMode:=AMode or dContactSlip2;
  if csmSlip1 in value then
    AMode:=AMode or dContactSlip1;
  if csmMotion2 in value then
    AMode:=AMode or dContactMotion2;
  if csmMotion1 in value then
    AMode:=AMode or dContactMotion1;
  if csmSoftCFM in value then
    AMode:=AMode or dContactSoftCFM;
  if csmSoftERP in value then
    AMode:=AMode or dContactSoftERP;
  if csmBounce in value then
    AMode:=AMode or dContactBounce;
  if csmFDir1 in value then
    AMode:=AMode or dContactFDir1;
  if csmMu2 in value then
    AMode:=AMode or dContactMu2;
  FSurfaceParams.Mode:=AMode;
end;

// CollisionSurface Property methods
//
function TODECollisionSurface.GetMu : double;
begin
  result:=FSurfaceParams.Mu;
end;

function TODECollisionSurface.GetMu2 : double;
begin
  result:=FSurfaceParams.Mu2;
end;

function TODECollisionSurface.GetBounce : double;
begin
  result:=FSurfaceParams.Bounce;
end;

function TODECollisionSurface.GetBounce_Vel : double;
begin
  result:=FSurfaceParams.Bounce_Vel;
end;

function TODECollisionSurface.GetSoftERP : double;
begin
  result:=FSurfaceParams.soft_erp;
end;

function TODECollisionSurface.GetSoftCFM : double;
begin
  result:=FSurfaceParams.soft_cfm;
end;

function TODECollisionSurface.GetMotion1 : double;
begin
  result:=FSurfaceParams.Motion1;
end;

function TODECollisionSurface.GetMotion2 : double;
begin
  result:=FSurfaceParams.Motion2;
end;

function TODECollisionSurface.GetSlip1 : double;
begin
  result:=FSurfaceParams.Slip1;
end;

function TODECollisionSurface.GetSlip2 : double;
begin
  result:=FSurfaceParams.Slip2;
end;

procedure TODECollisionSurface.SetMu(value : double);
begin
  FSurfaceParams.Mu:=value;
end;

procedure TODECollisionSurface.SetMu2(value : double);
begin
  FSurfaceParams.Mu2:=value;
end;

procedure TODECollisionSurface.SetBounce(value : double);
begin
  FSurfaceParams.Bounce:=value;
end;

procedure TODECollisionSurface.SetBounce_Vel(value : double);
begin
  FSurfaceParams.Bounce_Vel:=value;
end;

procedure TODECollisionSurface.SetSoftERP(value : double);
begin
  FSurfaceParams.soft_erp:=value;
end;

procedure TODECollisionSurface.SetSoftCFM(value : double);
begin
  FSurfaceParams.soft_cfm:=value;
end;

procedure TODECollisionSurface.SetMotion1(value : double);
begin
  FSurfaceParams.Motion1:=value;
end;

procedure TODECollisionSurface.SetMotion2(value : double);
begin
  FSurfaceParams.Motion2:=value;
end;

procedure TODECollisionSurface.SetSlip1(value : double);
begin
  FSurfaceParams.Slip1:=value;
end;

procedure TODECollisionSurface.SetSlip2(value : double);
begin
  FSurfaceParams.Slip2:=value;
end;


// ------------------------------------------------------------------
// TGLODEBaseObject
// ------------------------------------------------------------------

// BuildList
//
procedure TGLODEBaseObject.BuildList(var rci: TRenderContextInfo);
begin
  //
end;

// Create
//
constructor TGLODEBaseObject.Create(AOwner: TComponent);
begin
  inherited;
  FCollisionSurface:=TODECollisionSurface.Create;
  FVisibleAtRuntime:=False;
end;

// Destroy
//
destructor TGLODEBaseObject.Destroy;
begin
  FCollisionSurface.Free;
  inherited;
end;

// Initialize
//
procedure TGLODEBaseObject.Initialize;
begin
  //
end;

// Deinitialize
//
procedure TGLODEBaseObject.Deinitialize;
begin
  //
end;

// SetVisibleAtRunTime
//
procedure TGLODEBaseObject.SetVisibleAtRunTime(Value: Boolean);
begin
  FVisibleAtRunTime:=Value;
  StructureChanged;
end;

// NotifyChange
//
procedure TGLODEBaseObject.NotifyChange(Sender: TObject);
begin
  inherited;
end;

// SetManager
//
procedure TGLODEBaseObject.SetManager(Value: TGLODEManager);
begin
  if FManager<>Value then begin
    if Assigned(FManager) then begin
      Deinitialize;
    end;
    FManager:=Value;
    if Assigned(FManager) then begin
      Initialize;
    end;
  end;
end;

// SetSurface
//
procedure TGLODEBaseObject.SetSurface(value: TODECollisionSurface);
begin
  FCollisionSurface.Assign(value);
end;

// StructureChanged
procedure TGLODEBaseObject.StructureChanged;
begin
  //
  inherited;
end;


// ------------------------------------------------------------------
// TGLODEDynamicObject
// ------------------------------------------------------------------

// BuildList
//
procedure TGLODEDynamicObject.BuildList(var rci: TRenderContextInfo);
begin
  inherited;
end;

// Create
//
constructor TGLODEDynamicObject.Create(AOwner: TComponent);
begin
  inherited;
  //
end;

// Destroy
//
destructor TGLODEDynamicObject.Destroy;
begin
  //This causes access violations, why?
  //FBody.Free;
  inherited;
end;

// Deinitialize
//
procedure TGLODEDynamicObject.Deinitialize;
begin
  //
  inherited;
end;

// Initialize
//
procedure TGLODEDynamicObject.Initialize;
begin
  inherited;
  //
end;

// StructureChanged
//
procedure TGLODEDynamicObject.StructureChanged;
begin
  AlignBodyToMatrix(AbsoluteMatrix);
  inherited;
end;

// AlignBodyToMatrix
//
procedure TGLODEDynamicObject.AlignBodyToMatrix(Mat: TMatrix);
var
  R : TdMatrix3;
begin
  if not Assigned(FBody) then exit;
  dBodySetPosition(FBody,Mat[3][0],Mat[3][1],Mat[3][2]);
  R[0]:=Mat[0][0]; R[1]:=Mat[0][1]; R[2]:= Mat[0][2]; R[3]:= 0;
  R[4]:=Mat[1][0]; R[5]:=Mat[1][1]; R[6]:= Mat[1][2]; R[7]:= 0;
  R[8]:=Mat[2][0]; R[9]:=Mat[2][1]; R[10]:=Mat[2][2]; R[11]:=0;
  dBodySetRotation(FBody,R);
  FRealignODE:=False;
end;

// GetMass
//
function TGLODEDynamicObject.GetMass: TdMass;
begin
  dBodyGetMass(FBody,@FMass);
  Result:=FMass;
end;

// SetMass
//
procedure TGLODEDynamicObject.SetMass(const value: TdMass);
begin
  FMass:=value;
  dBodySetMass(FBody,FMass);
end;


// ------------------------------------------------------------------
// TGLODEDummy
// ------------------------------------------------------------------

// AddNewElement
//
function TGLODEDummy.AddNewElement(AChild: TODEElementClass): TODEBaseElement;
begin
  Result:=nil;
  if not Assigned(Manager) then exit;
  Result:=AChild.Create(FElements);
  FElements.Add(Result);
  Result.Initialize;
  dBodySetMass(FBody,CalculateMass);
end;

// AlignObject
//
procedure TGLODEDummy.AlignObject;
var
  pos : PdVector3;
  R : PdMatrix3;
  m : TMatrix;
begin
  pos:=dBodyGetPosition(Body);
  R:=dBodyGetRotation(Body);
  ODERToGLSceneMatrix(m,R^,pos^);
  Matrix:=m;
end;

// BuildList
//
procedure TGLODEDummy.BuildList(var rci: TRenderContextInfo);
var
  i : integer;
begin
  if not ((csDesigning in ComponentState) or (FVisibleAtRunTime)) then
    exit;

  inherited;

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);

  // Line stipple (why so slow?)
  //glEnable(GL_LINE_STIPPLE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //glLineStipple(1, $CCCC);

  glLineWidth(1);
  ResetGLMaterialColors;
  glColorMaterial(GL_FRONT, GL_EMISSION);
  glEnable(GL_COLOR_MATERIAL);
  glColor4fv(FColor.AsAddress);

  for i:=0 to FElements.Count-1 do begin
    FElements.Items[i].BuildList(rci);
  end;

  glPopAttrib;
end;

// Create
//
constructor TGLODEDummy.Create(AOwner: TComponent);
begin
  inherited;
  FElements:=TODEElements.Create(Self);
  FColor:=TGLColor.Create(self);
end;

// Destroy
//
destructor TGLODEDummy.Destroy;
begin
  Deinitialize;
  FColor.Free;
  FElements.Free;
  inherited;
end;

// Deinitialize
//
procedure TGLODEDummy.Deinitialize;
begin
  FElements.Deinitialize;
  FManager.UnregisterObject(self);
  inherited;
end;

// Initialize
//
procedure TGLODEDummy.Initialize;
begin
  inherited;
  FBody:=dBodyCreate(Manager.World);
  AlignBodyToMatrix(AbsoluteMatrix);
  dMassSetZero(FMass);
  FElements.Initialize;
  dBodySetMass(FBody,CalculateMass);
  Manager.RegisterObject(self);
end;

// CalculateMass
//
function TGLODEDummy.CalculateMass: TdMass;
var
  i : integer;
  m : TdMass;
begin
  dMassSetZero(FMass);
  for i:=0 to Elements.Count-1 do begin
    m:=Elements[i].CalculateMass;
    dMassAdd(FMass,m);
  end;
  Result:=FMass;
end;

// SetColor
//
procedure TGLODEDummy.SetColor(const Value: TGLColor);
begin
  FColor.Assign(Value);
  StructureChanged;
end;

// StructureChanged
//
procedure TGLODEDummy.StructureChanged;
begin
  if Assigned(FBody) then
    dBodySetMass(FBody,CalculateMass);
  inherited;
end;

// CalibrateCenterOfMass
//
procedure TGLODEDummy.CalibrateCenterOfMass;
var
  pos : TAffineVector;
  i : integer;
begin
  SetAffineVector(pos,FMass.c[0],FMass.c[1],FMass.c[2]);
  NegateVector(pos);
  for i:=0 to FElements.Count-1 do
    FElements[i].Position.Translate(pos);
end;


// ------------------------------------------------------------------
// TGLODEBaseBehaviour Methods
// ------------------------------------------------------------------

// Create
//
constructor TGLODEBaseBehaviour.Create(AOwner : TXCollection);
begin
  inherited;
  FSurface:=TODECollisionSurface.Create;
end;

// Destroy
//
destructor TGLODEBaseBehaviour.Destroy;
begin
  Deinitialize;
  FSurface.Free;
  inherited;
end;

// Initialize
//
procedure TGLODEBaseBehaviour.Initialize;
begin
  //
end;

// Deinitialize
//
procedure TGLODEBaseBehaviour.Deinitialize;
begin
  //
end;

// SetManager
//
procedure TGLODEBaseBehaviour.SetManager(Value : TGLODEManager);
begin
  if FManager<>Value then begin
    if Assigned(FManager) then begin
      Deinitialize;
    end;
    FManager:=Value;
    if Assigned(FManager) then begin
      Initialize;
    end;
  end;
end;

// SetSurface
//
procedure TGLODEBaseBehaviour.SetSurface(value: TODECollisionSurface);
begin
  FSurface.Assign(value);
end;


// ------------------------------------------------------------------
// TGLODEDynamicBehaviour
// ------------------------------------------------------------------

// Create
//
constructor TGLODEDynamicBehaviour.Create(AOwner : TXCollection);
begin
  inherited;
  FElements:=TODEElements.Create(Self);
end;

// Destroy
//
destructor TGLODEDynamicBehaviour.Destroy;
begin
  Deinitialize;
  FElements.Free;
  inherited;
end;

// FriendlyName
//
class function TGLODEDynamicBehaviour.FriendlyName: String;
begin
  Result:='ODE Dynamic';
end;

// Initialize
//
procedure TGLODEDynamicBehaviour.Initialize;
begin
  FBody:=dBodyCreate(Manager.World);
  AlignBodyToMatrix(TGLBaseSceneObject(Owner.Owner).AbsoluteMatrix);
  dMassSetZero(FMass);
  FElements.Initialize;
  CalculateMass;
  dBodySetMass(FBody,FMass);
  FManager.RegisterObject(self);
end;

// Deinitialize
//
procedure TGLODEDynamicBehaviour.Deinitialize;
begin
  FElements.Deinitialize;
  FManager.UnregisterObject(self);
end;

// AddNewElement
//
function TGLODEDynamicBehaviour.AddNewElement(AChild:TODEElementClass):TODEBaseElement;
begin
  Result:=nil;
  if not Assigned(Manager) then exit;
  Result:=AChild.Create(FElements);
  FElements.Add(Result);
  Result.Initialize;
  dBodySetMass(FBody,CalculateMass);
end;

// AlignObject
//
procedure TGLODEDynamicBehaviour.AlignObject;
var
  pos : PdVector3;
  R : PdMatrix3;
  m : TMatrix;
begin
  pos:=dBodyGetPosition(Body);
  R:=dBodyGetRotation(Body);
  ODERToGLSceneMatrix(m,R^,pos^);
  TGLBaseSceneObject(Owner.Owner).Matrix:=m;
end;

// AlignBodyToMatrix
//
procedure TGLODEDynamicBehaviour.AlignBodyToMatrix(Mat:TMatrix);
var
  R : TdMatrix3;
begin
  if not Assigned(FBody) then exit;
  dBodySetPosition(FBody,Mat[3][0],Mat[3][1],Mat[3][2]);
  R[0]:=Mat[0][0]; R[1]:=Mat[0][1]; R[2]:= Mat[0][2]; R[3]:= 0;
  R[4]:=Mat[1][0]; R[5]:=Mat[1][1]; R[6]:= Mat[1][2]; R[7]:= 0;
  R[8]:=Mat[2][0]; R[9]:=Mat[2][1]; R[10]:=Mat[2][2]; R[11]:=0;
  dBodySetRotation(FBody,R);
end;

function TGLODEDynamicBehaviour.GetAbsoluteMatrix;
begin
  Result:=TGLBaseSceneObject(Owner.Owner).AbsoluteMatrix;
end;

// CalculateMass
//
function TGLODEDynamicBehaviour.CalculateMass : TdMass;
var
  i : integer;
  m : TdMass;
begin
  dMassSetZero(FMass);
  for i:=0 to Elements.Count-1 do begin
    m:=Elements[i].CalculateMass;
    dMassAdd(FMass,m);
  end;
  Result:=FMass;
end;

// CalibrateCenterOfMass
//
procedure TGLODEDynamicBehaviour.CalibrateCenterOfMass;
var
  pos : TAffineVector;
  i : integer;
begin
  SetAffineVector(pos,FMass.c[0],FMass.c[1],FMass.c[2]);
  NegateVector(pos);
  for i:=0 to FElements.Count-1 do
    FElements[i].Position.Translate(pos);
end;

// GetMass
//
function TGLODEDynamicBehaviour.GetMass: TdMass;
begin
  dBodyGetMass(FBody,@FMass);
  Result:=FMass;
end;

// SetMass
//
procedure TGLODEDynamicBehaviour.SetMass(const value: TdMass);
begin
  FMass:=value;
  dBodySetMass(FBody,FMass);
end;


// ------------------------------------------------------------------
// TODEElements Methods
// ------------------------------------------------------------------

// Add
//
function TODEElements.Add(item: TODEBaseElement): integer;
begin
  result:=FList.Add(item);
end;

// Create
//
constructor TODEElements.Create(AOwner:TPersistent);
begin
  inherited Create;
  FOwner:=AOwner;
  FList:=TList.Create;
end;

// Delete
//
procedure TODEElements.Delete(index: integer);
begin
  if Assigned(Items[index]) then
    Items[index].Free;
  FList.Delete(index);
end;

// Count
//
function TODEElements.Count: integer;
begin
  Result:=FList.Count;
end;

// Destroy
//
destructor TODEElements.Destroy;
begin
  FList.Free;
  inherited;
end;

// Initialize
//
procedure TODEElements.Initialize;
var
  i : integer;
begin
  for i:=0 to FList.Count-1 do
    Items[i].Initialize;
end;

// Deintialize
//
procedure TODEElements.Deinitialize;
var
  i : integer;
begin
  for i:=0 to FList.Count-1 do
    Items[i].Deinitialize;
end;

// GetItem
//
function TODEElements.GetItem(index: integer): TODEBaseElement;
begin
  Result:=TODEBaseElement(FList.Items[index]);
end;

// Remove
//
procedure TODEElements.Remove(item: TODEBaseElement);
begin
  item.Free;
  FList.Remove(item);
end;


// ------------------------------------------------------------------
// TODEBaseElement
// ------------------------------------------------------------------

// AbsoluteMatrix
function TODEBaseElement.AbsoluteMatrix: TMatrix;
var
  Mat : TMatrix;
begin
  Mat:=IdentityHMGMatrix;
  if FOwner.Owner is TGLODEDummy then
    Mat:=TGLODEDummy(FOwner.Owner).AbsoluteMatrix;
  if FOwner.Owner is TGLODEDynamicBehaviour then
    Mat:=TGLODEDynamicBehaviour(FOwner.Owner).AbsoluteMatrix;
  Result:=MatrixMultiply(Mat,FLocalMatrix);
end;

// AbsolutePosition
function TODEBaseElement.AbsolutePosition: TAffineVector;
begin

end;

// AlignGeomToMatrix
//
procedure TODEBaseElement.AlignGeomToMatrix(Mat: TMatrix);
var
  R : TdMatrix3;
begin
  if not Assigned(FGeomElement) then exit;
  R[0]:=Mat[0][0]; R[1]:=Mat[0][1]; R[2]:= Mat[0][2]; R[3]:= 0;
  R[4]:=Mat[1][0]; R[5]:=Mat[1][1]; R[6]:= Mat[1][2]; R[7]:= 0;
  R[8]:=Mat[2][0]; R[9]:=Mat[2][1]; R[10]:=Mat[2][2]; R[11]:=0;
  dGeomSetPosition(Geom,Mat[3][0],Mat[3][1],Mat[3][2]);
  dGeomSetRotation(Geom,R);
  FRealignODE:=False;
end;

// BuildList
//
procedure TODEBaseElement.BuildList(var rci: TRenderContextInfo);
begin
  // Override this procedure with element drawing OpenGL code
end;

// Create
//
constructor TODEBaseElement.Create(AOwner: TODEElements);
begin
  inherited Create;
  FOwner:=AOwner;
  FPosition:=TGLCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FPosition.OnNotifyChange:=NotifyChange;
  FDirection:=TGLCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FDirection.OnNotifyChange:=NotifyChange;
  FUp:=TGLCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FUp.OnNotifyChange:=NotifyChange;
  FDensity:=1;
end;

// Destroy
//
destructor TODEBaseElement.Destroy;
begin
  FPosition.Free;
  FDirection.Free;
  FUp.Free;
  inherited;
end;

// Initialize
//
procedure TODEBaseElement.Initialize;
var
  Manager : TGLODEManager;
  Body : PdxBody;
begin
  Manager:=nil;
  Body:=nil;
  if FOwner.Owner is TGLODEDummy then begin
    Manager:=TGLODEDummy(FOwner.Owner).Manager;
    Body:=TGLODEDummy(FOwner.Owner).Body;
  end;
  if FOwner.Owner is TGLODEDynamicBehaviour then begin
    Manager:=TGLODEDynamicBehaviour(FOwner.Owner).Manager;
    Body:=TGLODEDynamicBehaviour(FOwner.Owner).Body;
  end;
  if not (Assigned(Manager) and Assigned(Body)) then exit;

  FGeomTransform:=dCreateGeomTransform(Manager.Space);
  dGeomSetBody(FGeomTransform,Body);
  dGeomTransformSetCleanup(FGeomTransform,1);
  dGeomTransformSetGeom(FGeomTransform,FGeomElement);
  dGeomSetData(FGeomTransform,FOwner.Owner);
  FInitialized:=True;
end;

// Deinitialize
//
procedure TODEBaseElement.Deinitialize;
begin
  //
end;

// CalculateMass
//
function TODEBaseElement.CalculateMass: TdMass;
var
  R : TdMatrix3;
begin
  R[0]:=FLocalMatrix[0][0]; R[1]:=FLocalMatrix[0][1]; R[2]:= FLocalMatrix[0][2]; R[3]:= 0;
  R[4]:=FLocalMatrix[1][0]; R[5]:=FLocalMatrix[1][1]; R[6]:= FLocalMatrix[1][2]; R[7]:= 0;
  R[8]:=FLocalMatrix[2][0]; R[9]:=FLocalMatrix[2][1]; R[10]:=FLocalMatrix[2][2]; R[11]:=0;
  dMassRotate(FMass,R);
  dMassTranslate(FMass,FLocalMatrix[3][0],FLocalMatrix[3][1],FLocalMatrix[3][2]);
  result:=FMass;
end;

// NotifyChange
//
procedure TODEBaseElement.NotifyChange(Sender: TObject);
begin
  RebuildMatrix;
  ODERebuild;
end;

// GetMatrix
//
function TODEBaseElement.GetMatrix: TMatrix;
begin
  result:=FLocalMatrix;
end;

// RebuildMatrix
//
procedure TODEBaseElement.RebuildMatrix;
begin
  VectorCrossProduct(FUp.AsVector,FDirection.AsVector,FLocalMatrix[0]);
  SetVector(FLocalMatrix[1],FUp.AsVector);
  SetVector(FLocalMatrix[2],FDirection.AsVector);
  SetVector(FLocalMatrix[3],FPosition.AsVector);
end;

// SetDensity
//
procedure TODEBaseElement.SetDensity(const Value: single);
begin
  FDensity:=Value;
end;

// SetMatrix
//
procedure TODEBaseElement.SetMatrix(const Value: TMatrix);
begin
  FLocalMatrix := Value;
  ODERebuild;
end;

// ODERebuild
//
procedure TODEBaseElement.ODERebuild;
begin
  AlignGeomToMatrix(FLocalMatrix);
  CalculateMass;
end;


// ------------------------------------------------------------------
// TODEElementBox
// ------------------------------------------------------------------

// BuildList
//
procedure TODEElementBox.BuildList(var rci : TRenderContextInfo);
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  glBegin(GL_LINE_LOOP);
    glVertex3f(-FBoxWidth/2,-FBoxHeight/2,-FBoxDepth/2);
    glVertex3f(-FBoxWidth/2,FBoxHeight/2,-FBoxDepth/2);
    glVertex3f(-FBoxWidth/2,FBoxHeight/2,FBoxDepth/2);
    glVertex3f(-FBoxWidth/2,-FBoxHeight/2,FBoxDepth/2);
  glEnd;

  glBegin(GL_LINE_LOOP);
    glVertex3f(FBoxWidth/2,FBoxHeight/2,FBoxDepth/2);
    glVertex3f(FBoxWidth/2,-FBoxHeight/2,FBoxDepth/2);
    glVertex3f(FBoxWidth/2,-FBoxHeight/2,-FBoxDepth/2);
    glVertex3f(FBoxWidth/2,FBoxHeight/2,-FBoxDepth/2);
  glEnd;

  glBegin(GL_LINES);
    glVertex3f(-FBoxWidth/2,FBoxHeight/2,-FBoxDepth/2);
    glVertex3f(FBoxWidth/2,FBoxHeight/2,-FBoxDepth/2);
    glVertex3f(-FBoxWidth/2,-FBoxHeight/2,FBoxDepth/2);
    glVertex3f(FBoxWidth/2,-FBoxHeight/2,FBoxDepth/2);
    glVertex3f(-FBoxWidth/2,-FBoxHeight/2,-FBoxDepth/2);
    glVertex3f(FBoxWidth/2,-FBoxHeight/2,-FBoxDepth/2);
    glVertex3f(-FBoxWidth/2,FBoxHeight/2,FBoxDepth/2);
    glVertex3f(FBoxWidth/2,FBoxHeight/2,FBoxDepth/2);
  glEnd;

  glPopMatrix;
end;

// Create
//
constructor TODEElementBox.Create(AOwner: TODEElements);
begin
  inherited;
  BoxWidth:=1;
  BoxHeight:=1;
  BoxDepth:=1;
end;

// Initialize
//
procedure TODEElementBox.Initialize;
begin
  FGeomElement:=dCreateBox(nil,FBoxWidth,FBoxHeight,FBoxDepth);
  inherited;
end;

// Deinitialization
//
procedure TODEElementBox.Deinitialize;
begin
  inherited;
end;

// CalculateMass
//
function TODEElementBox.CalculateMass: TdMass;
begin
  dMassSetBox(FMass,FDensity,BoxWidth,BoxHeight,BoxDepth);
  result:=inherited CalculateMass;
end;

// GetBoxWidth
//
function TODEElementBox.GetBoxWidth: single;
var
  vec : TdVector3;
begin
  if Assigned(FGeomTransform) then begin
    dGeomBoxGetLengths(Geom,vec);
    FBoxWidth:=vec[0];
  end;
  result:=FBoxWidth;
end;

// GetBoxHeight
//
function TODEElementBox.GetBoxHeight: single;
var
  vec : TdVector3;
begin
  if Assigned(FGeomTransform) then begin
    dGeomBoxGetLengths(Geom,vec);
    FBoxHeight:=vec[1];
  end;
  result:=FBoxHeight;
end;

// GetBoxDepth
//
function TODEElementBox.GetBoxDepth: single;
var
  vec : TdVector3;
begin
  if Assigned(FGeomTransform) then begin
    dGeomBoxGetLengths(Geom,vec);
    FBoxDepth:=vec[2];
  end;
  result:=FBoxDepth;
end;

// ODERebuild
//
procedure TODEElementBox.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomBoxSetLengths(Geom,FBoxWidth,FBoxHeight,FBoxDepth);
  inherited;
end;

// SetBoxWidth
//
procedure TODEElementBox.SetBoxWidth(const Value: single);
begin
  FBoxWidth:=Value;
  ODERebuild;
end;

// SetBoxHeight
//
procedure TODEElementBox.SetBoxHeight(const Value: single);
begin
  FBoxHeight:=Value;
  ODERebuild;
end;

// SetBoxDepth
//
procedure TODEElementBox.SetBoxDepth(const Value: single);
begin
  FBoxDepth:=Value;
  ODERebuild;
end;


// ------------------------------------------------------------------
// TODEElementSphere
// ------------------------------------------------------------------

// BuildList
//
procedure TODEElementSphere.BuildList(var rci : TRenderContextInfo);
var
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH : Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
  FTop, FBottom, FStart, FStop : Single;
  I, J, FSlices, FStacks: Integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);
  glScalef(Radius, Radius, Radius);

  FTop:=90;
  FBottom:=-90;
  FStart:=0;
  FStop:=360;
  FSlices:=16;
  FStacks:=16;

  AngTop:=DegToRad(FTop);
  AngBottom:=DegToRad(FBottom);
  AngStart:=DegToRad(FStart);
  AngStop:=DegToRad(FStop);
  StepH:=(AngStop - AngStart) / FSlices;
  StepV:=(AngTop - AngBottom) / FStacks;

  Phi:=AngTop;
  Phi2:=Phi-StepV;
  for J:=0 to FStacks-1 do begin
    Theta:=AngStart;
    SinCos(Phi, SinP, CosP);
    SinCos(Phi2, SinP2, CosP2);

    glBegin(GL_LINE_LOOP);
    for i:=0 to FSlices do begin
      SinCos(Theta, SinT, CosT);
      glVertex3f(CosP*SinT,SinP,CosP*CosT);
      Theta:=Theta+StepH;
    end;
    glEnd;
    Phi:=Phi2;
    Phi2:=Phi2 - StepV;
  end;

  Phi:=AngTop;
  Phi2:=Phi-StepV;
  for J:=0 to FStacks-1 do begin
    Theta:=AngStart;
    SinCos(Phi, SinP, CosP);
    SinCos(Phi2, SinP2, CosP2);

    glBegin(GL_LINE_LOOP);
    for i:=0 to FSlices do begin
      SinCos(Theta, SinT, CosT);
      glVertex3f(SinP,CosP*SinT,CosP*CosT);
      Theta:=Theta+StepH;
    end;
    glEnd;
    Phi:=Phi2;
    Phi2:=Phi2 - StepV;
  end;

  glPopMatrix;
end;

// Create
//
constructor TODEElementSphere.Create(AOwner: TODEElements);
begin
  inherited;
  FRadius:=0.5;
end;

// Initialize
//
procedure TODEElementSphere.Initialize;
begin
  FGeomElement:=dCreateSphere(nil,FRadius);
  inherited;
end;

// Deinitialization
//
procedure TODEElementSphere.Deinitialize;
begin
  inherited;
end;

// CalculateMass
//
function TODEElementSphere.CalculateMass: TdMass;
begin
  dMassSetSphere(FMass,FDensity,Radius);
  result:=inherited CalculateMass;
end;

// GetRadius
//
function TODEElementSphere.GetRadius: single;
begin
  if Assigned(FGeomElement) then
    FRadius:=dGeomSphereGetRadius(FGeomElement);
  result:=FRadius;
end;

// ODERebuild
//
procedure TODEElementSphere.ODERebuild;
begin
  if Assigned(Geom) then begin
    dGeomSphereSetRadius(Geom,FRadius);
  end;
  inherited;
end;

// SetRadius
//
procedure TODEElementSphere.SetRadius(const Value: single);
begin
  FRadius:=Value;
  ODERebuild;
end;

// ------------------------------------------------------------------
// TODEElementCapsule
// ------------------------------------------------------------------

// BuildList
//
procedure TODEElementCapsule.BuildList(var rci : TRenderContextInfo);
var
  i,j,
  Stacks,Slices : integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  Stacks:=8;
  Slices:=16;

  // Middle horizontal circles
  for j:=0 to Stacks-1 do begin
    glBegin(GL_LINE_LOOP);
      for i:=0 to Slices-1 do
        glVertex3f(FRadius*sin(2*i*PI/Slices),FRadius*cos(2*i*PI/Slices),-FLength/2+FLength*j/(Stacks-1));
    glEnd;
  end;

  // Middle vertical lines
  glBegin(GL_LINES);
    for i:=0 to (Slices div 2)-1 do begin
      glVertex3f(FRadius*sin(2*i*PI/Slices),FRadius*cos(2*i*PI/Slices),-FLength/2);
      glVertex3f(FRadius*sin(2*i*PI/Slices),FRadius*cos(2*i*PI/Slices),FLength/2);
      glVertex3f(-FRadius*sin(2*i*PI/Slices),-FRadius*cos(2*i*PI/Slices),-FLength/2);
      glVertex3f(-FRadius*sin(2*i*PI/Slices),-FRadius*cos(2*i*PI/Slices),FLength/2);
    end;
  glEnd;

  // Cap XZ half-circles
  glPushMatrix;
  for j:=0 to (Slices div 2)-1 do begin
    // Top
    glBegin(GL_LINE_STRIP);
      for i:=0 to Slices do
        glVertex3f(FRadius*cos(i*PI/Slices),0,FRadius*sin(i*PI/Slices)+FLength/2);
    glEnd;

    // Bottom
    glBegin(GL_LINE_STRIP);
      for i:=0 to Slices do
        glVertex3f(FRadius*cos(i*PI/Slices),0,-(FRadius*sin(i*PI/Slices)+FLength/2));
    glEnd;
    glRotatef(360/Slices,0,0,1);
  end;
  glPopMatrix;
  glPopMatrix;
end;

// Create
//
constructor TODEElementCapsule.Create(AOwner: TODEElements);
begin
  inherited;
  FRadius:=0.5;
  FLength:=1;
end;

// Initialize
//
procedure TODEElementCapsule.Initialize;
begin
  FGeomElement:=dCreateCCylinder(nil,FRadius,FLength);
  inherited;
end;

// Deinitialization
//
procedure TODEElementCapsule.Deinitialize;
begin
  inherited;
end;

// CalculateMass
//
function TODEElementCapsule.CalculateMass: TdMass;
begin
  dMassSetCappedCylinder(FMass,FDensity,3,FRadius,FLength);
  result:=inherited CalculateMass;
end;

// GetRadius
//
function TODEElementCapsule.GetRadius: single;
var
  rad, len : double;
begin
  if Assigned(FGeomElement) then begin
    dGeomCCylinderGetParams(Geom,rad,len);
    FRadius:=rad;
  end;
  result:=FRadius;
end;

// GetLength
//
function TODEElementCapsule.GetLength: single;
var
  rad, len : double;
begin
  if Assigned(FGeomElement) then begin
    dGeomCCylinderGetParams(Geom,rad,len);
    FLength:=len;
  end;
  result:=FLength;
end;

// Rebuild
//
procedure TODEElementCapsule.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomCCylinderSetParams(Geom,FRadius,FLength);
  inherited;
end;

// SetRadius
//
procedure TODEElementCapsule.SetRadius(const Value: single);
begin
  FRadius:=Value;
  ODERebuild;
end;

// SetLength
//
procedure TODEElementCapsule.SetLength(const Value: single);
begin
  FLength:=Value;
  ODERebuild;
end;


// ------------------------------------------------------------------
// TODEElementCylinder
// ------------------------------------------------------------------

// BuildList
//
procedure TODEElementCylinder.BuildList(var rci : TRenderContextInfo);
var
  i,j,
  Stacks,Slices : integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  Stacks:=8;
  Slices:=16;

  // Middle horizontal circles
  for j:=0 to Stacks-1 do begin
    glBegin(GL_LINE_LOOP);
      for i:=0 to Slices-1 do
        glVertex3f(FRadius*sin(2*i*PI/Slices),FRadius*cos(2*i*PI/Slices),-FLength/2+FLength*j/(Stacks-1));
    glEnd;
  end;

  // Middle vertical lines
  glBegin(GL_LINES);
    for i:=0 to (Slices div 2)-1 do begin
      glVertex3f(FRadius*sin(2*i*PI/Slices),FRadius*cos(2*i*PI/Slices),-FLength/2);
      glVertex3f(FRadius*sin(2*i*PI/Slices),FRadius*cos(2*i*PI/Slices),FLength/2);
      glVertex3f(-FRadius*sin(2*i*PI/Slices),-FRadius*cos(2*i*PI/Slices),-FLength/2);
      glVertex3f(-FRadius*sin(2*i*PI/Slices),-FRadius*cos(2*i*PI/Slices),FLength/2);
    end;
  glEnd;

  // Caps
  glPushMatrix;
  for j:=0 to (Slices div 2)-1 do begin
    glBegin(GL_LINES);
      glVertex3f(-FRadius,0,FLength/2);
      glVertex3f(FRadius,0,FLength/2);
      glVertex3f(-FRadius,0,-FLength/2);
      glVertex3f(FRadius,0,-FLength/2);
    glEnd;
    glRotatef(360/Slices,0,0,1);
  end;
  glPopMatrix;

  glPopMatrix;
end;

// Create
//
constructor TODEElementCylinder.Create(AOwner: TODEElements);
begin
  inherited;
  FRadius:=0.5;
  FLength:=1;
end;

// Initialize
//
procedure TODEElementCylinder.Initialize;
begin
  FGeomElement:=dCreateCylinder(nil,FRadius,FLength);
  inherited;
end;

// Deinitialization
//
procedure TODEElementCylinder.Deinitialize;
begin
  inherited;
end;

// CalculateMass
//
function TODEElementCylinder.CalculateMass: TdMass;
begin
  dMassSetBox(FMass,FDensity,2*FRadius,FLength,2*FRadius);
  result:=inherited CalculateMass;
end;

// GetRadius
//
function TODEElementCylinder.GetRadius: single;
var
  rad, len : double;
begin
  if Assigned(FGeomElement) then begin
    dGeomCylinderGetParams(Geom,rad,len);
    FRadius:=rad;
  end;
  result:=FRadius;
end;

// GetLength
//
function TODEElementCylinder.GetLength: single;
var
  rad, len : double;
begin
  if Assigned(FGeomElement) then begin
    dGeomCylinderGetParams(Geom,rad,len);
    FLength:=len;
  end;
  result:=FLength;
end;

// Rebuild
//
procedure TODEElementCylinder.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomCylinderSetParams(Geom,FRadius,FLength);
  inherited;
end;

// SetRadius
//
procedure TODEElementCylinder.SetRadius(const Value: single);
begin
  FRadius:=Value;
  ODERebuild;
end;

// SetLength
//
procedure TODEElementCylinder.SetLength(const Value: single);
begin
  FLength:=Value;
  ODERebuild;
end;


// ------------------------------------------------------------------
// TGLODEStaticObject
// ------------------------------------------------------------------

// Deinitiailize
//
procedure TGLODEStaticObject.Deinitialize;
begin
  if Assigned(FGeom) then
    dGeomDestroy(FGeom);
  inherited;
end;


// ------------------------------------------------------------------
// TGLODEPlane
// ------------------------------------------------------------------

procedure TGLODEPlane.AlignODEPlane;
var
  Pos, Dir : TVector;
  d : single;
begin
  Dir := AbsoluteDirection;
  Pos := AbsolutePosition;
  d := (Dir[0]*Pos[0]+Dir[1]*Pos[1]+Dir[2]*Pos[2]);
  dGeomPlaneSetParams(Geom,Dir[0],Dir[1],Dir[2],d);
end;

constructor TGLODEPlane.Create(AOwner: TComponent);
begin
  inherited;
  Direction.SetVector(0,1,0);
end;

procedure TGLODEPlane.Initialize;
begin
  inherited;
  FGeom:=dCreatePlane(FManager.Space,0,1,0,0);
  dGeomSetData(FGeom,Self);
  AlignODEPlane;
end;

procedure TGLODEPlane.BuildList(var rci: TRenderContextInfo);
begin
  inherited;
end;

procedure TGLODEPlane.StructureChanged;
begin
  inherited;
end;

procedure TGLODEPlane.NotifyChange(Sender: TObject);
begin
  inherited;
  AlignODEPlane;
end;


// ------------------------------------------------------------------
// TODEJoints Methods
// ------------------------------------------------------------------

// Add
//
function TODEJoints.Add(item: TODEBaseJoint): integer;
begin
  result:=FList.Add(item);
end;

// Create
//
constructor TODEJoints.Create(AOwner:TPersistent);
begin
  inherited Create;
  FOwner:=AOwner;
  FList:=TList.Create;
end;

// Delete
//
procedure TODEJoints.Delete(index: integer);
begin
  if Assigned(Items[index]) then
    Items[index].Free;
  FList.Delete(index);
end;

// Count
//
function TODEJoints.Count: integer;
begin
  Result:=FList.Count;
end;

// Destroy
//
destructor TODEJoints.Destroy;
begin
  FList.Free;
  inherited;
end;

// Initialize
//
procedure TODEJoints.Initialize;
var
  i : integer;
begin
  for i:=0 to FList.Count-1 do
    Items[i].Initialize;
end;

// Deintialize
//
procedure TODEJoints.Deinitialize;
var
  i : integer;
begin
  for i:=0 to FList.Count-1 do
    Items[i].Deinitialize;
end;

// GetItem
//
function TODEJoints.GetItem(index: integer): TODEBaseJoint;
begin
  Result:=TODEBaseJoint(FList.Items[index]);
end;

// Remove
//
procedure TODEJoints.Remove(item: TODEBaseJoint);
begin
  item.Free;
  FList.Remove(item);
end;


// ------------------------------------------------------------------
// TODEJointParams
// ------------------------------------------------------------------

function TODEJointParams.GetBounce: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamBounce);
end;

function TODEJointParams.GetCFM: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamCFM);
end;

function TODEJointParams.GetFMax: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamFMax);
end;

function TODEJointParams.GetFudgeFactor: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamFudgeFactor);
end;

function TODEJointParams.GetHiStop: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamHiStop);
end;

function TODEJointParams.GetLoStop: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamLoStop);
end;

function TODEJointParams.GetStopCFM: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamStopCFM);
end;

function TODEJointParams.GetStopERP: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamStopERP);
end;

function TODEJointParams.GetSuspensionCFM: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamSuspensionCFM);
end;

function TODEJointParams.GetSuspensionERP: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamSuspensionERP);
end;

function TODEJointParams.GetVel: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamVel);
end;

function TODEJointParams.GetBounce2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamBounce2);
end;

function TODEJointParams.GetCFM2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamCFM2);
end;

function TODEJointParams.GetFMax2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamFMax2);
end;

function TODEJointParams.GetFudgeFactor2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamFudgeFactor2);
end;

function TODEJointParams.GetHiStop2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamHiStop2);
end;

function TODEJointParams.GetLoStop2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamLoStop2);
end;

function TODEJointParams.GetStopCFM2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamStopCFM2);
end;

function TODEJointParams.GetStopERP2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamStopERP2);
end;

function TODEJointParams.GetSuspensionCFM2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamSuspensionCFM2);
end;

function TODEJointParams.GetSuspensionERP2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamSuspensionERP2);
end;

function TODEJointParams.GetVel2: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamVel2);
end;

function TODEJointParams.GetBounce3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamBounce3);
end;

function TODEJointParams.GetCFM3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamCFM3);
end;

function TODEJointParams.GetFMax3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamFMax3);
end;

function TODEJointParams.GetFudgeFactor3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamFudgeFactor3);
end;

function TODEJointParams.GetHiStop3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamHiStop3);
end;

function TODEJointParams.GetLoStop3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamLoStop3);
end;

function TODEJointParams.GetStopCFM3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamStopCFM3);
end;

function TODEJointParams.GetStopERP3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamStopERP3);
end;

function TODEJointParams.GetSuspensionCFM3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamSuspensionCFM3);
end;

function TODEJointParams.GetSuspensionERP3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamSuspensionERP3);
end;

function TODEJointParams.GetVel3: TdReal;
begin
  Result:=FJointGetParamsProc(Owner.JointID, dParamVel3);
end;

procedure TODEJointParams.SetBounce(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamBounce, Value);
end;

procedure TODEJointParams.SetCFM(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamCFM, Value);
end;

procedure TODEJointParams.SetFMax(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamFMax, Value);
end;

procedure TODEJointParams.SetFudgeFactor(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamFudgeFactor, Value);
end;

procedure TODEJointParams.SetHiStop(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamHiStop, Value);
end;

procedure TODEJointParams.SetLoStop(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamLoStop, Value);
end;

procedure TODEJointParams.SetStopCFM(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamStopCFM, Value);
end;

procedure TODEJointParams.SetStopERP(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamStopERP, Value);
end;

procedure TODEJointParams.SetSuspensionCFM(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamSuspensionCFM, Value);
end;

procedure TODEJointParams.SetSuspensionERP(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamSuspensionERP, Value);
end;

procedure TODEJointParams.SetVel(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamVel, Value);
end;

procedure TODEJointParams.SetBounce2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamBounce2, Value);
end;

procedure TODEJointParams.SetCFM2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamCFM2, Value);
end;

procedure TODEJointParams.SetFMax2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamFMax2, Value);
end;

procedure TODEJointParams.SetFudgeFactor2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamFudgeFactor2, Value);
end;

procedure TODEJointParams.SetHiStop2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamHiStop2, Value);
end;

procedure TODEJointParams.SetLoStop2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamLoStop2, Value);
end;

procedure TODEJointParams.SetStopCFM2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamStopCFM2, Value);
end;

procedure TODEJointParams.SetStopERP2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamStopERP2, Value);
end;

procedure TODEJointParams.SetSuspensionCFM2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamSuspensionCFM2, Value);
end;

procedure TODEJointParams.SetSuspensionERP2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamSuspensionERP2, Value);
end;

procedure TODEJointParams.SetVel2(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamVel2, Value);
end;

procedure TODEJointParams.SetBounce3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamBounce3, Value);
end;

procedure TODEJointParams.SetCFM3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamCFM3, Value);
end;

procedure TODEJointParams.SetFMax3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamFMax3, Value);
end;

procedure TODEJointParams.SetFudgeFactor3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamFudgeFactor3, Value);
end;

procedure TODEJointParams.SetHiStop3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamHiStop3, Value);
end;

procedure TODEJointParams.SetLoStop3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamLoStop3, Value);
end;

procedure TODEJointParams.SetStopCFM3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamStopCFM3, Value);
end;

procedure TODEJointParams.SetStopERP3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamStopERP3, Value);
end;

procedure TODEJointParams.SetSuspensionCFM3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamSuspensionCFM3, Value);
end;

procedure TODEJointParams.SetSuspensionERP3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamSuspensionERP3, Value);
end;

procedure TODEJointParams.SetVel3(const Value: TdReal);
begin
  FJointSetParamsProc(Owner.JointID, dParamVel3, Value);
end;


// ------------------------------------------------------------------
// TODEBaseJoint
// ------------------------------------------------------------------

// Initialize
//
procedure TODEBaseJoint.Initialize;
begin
   //
end;

// Deinitialize
//
procedure TODEBaseJoint.Deinitialize;
begin

end;


// ------------------------------------------------------------------
// TODEJointHinge
// ------------------------------------------------------------------

// Initialize
//
procedure TODEJointHinge.Initialize;
begin
  inherited;
  dJointCreateHinge(FManager.World, FManager.JointGroup);
  JointParams.FJointGetParamsProc:=@dJointGetHingeParam;
  JointParams.FJointSetParamsProc:=@dJointSetHingeParam;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterXCollectionItemClass(TGLODEDynamicBehaviour);

end.

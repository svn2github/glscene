// GLODEManager
{: An ODE Manager for GLScene.<p>

  Where can I find ... ?
  GLScene              (http://glscene.org)
  Open Dynamics Engine (http://opende.sourceforge.org)
  DelphiODE            (http://www.cambrianlabs.com/Mattias/DelphiODE)

  Notes:
  This code is still being developed so any part of it may change at anytime.
  To install use the GLS_ODE?.dpk in the GLScene/Delphi? folder.

  History:<ul>
    <li>30/07/03 - SG - Split terrain collider into GLODECustomColliders unit.
    <li>25/07/03 - SG - Fixed Manager property persistence, other minor changes.
    <li>24/07/03 - SG - ReadFromFiler and WriteToFiler routines added,
                        improved object and joint initialization system.
                        Manager properties not persitent in joints and behaviours.
    <li>26/06/03 - EG - Replaced TObjectList with TPersistentObjectList,
                        dropped Contnrs dependency (D5 compatibility)
    <li>23/06/03 - SG - Added GLODETerrainCollider, an implementation from DelphiODE
                        terrain demo (buggy caused assertion error in GLHeightData.pas).
    <li>13/06/03 - SG - Added more joints.
    <li>11/06/03 - SG - Base joint classes implemented and added hinge joint.
    <li>09/06/03 - SG - Added OnCollision event for ODE Objects and Behaviours.
    <li>08/06/03 - SG - Added rolling friction (experimental).
    <li>06/06/03 - SG - Added cylinder element (experimental).
    <li>04/06/03 - SG - Changes to structures, added TGLODEDynamicBehaviour.
    <li>30/05/03 - SG - Added capsule element and plane object,
                        Fixed problems with Collision callback method.
    <li>29/05/03 - SG - Better GetCollisionSurface code (thanks to Mattias Fagerlund).
    <li>28/05/03 - SG - Some fixes to ODE Elements (thanks to Mattias Fagerlund).
                        Added TGLODEDummy.CalibrateCenterOfMass
    <li>01/03/03 - SG - Creation.
  </ul>
}

unit GLODEManager;

interface

uses
  Classes, ODEImport, ODEGL, GLScene, GLMisc, Geometry, GLTexture, OpenGL1x,
  XOpenGL, SysUtils, GLObjects, XCollection, PersistentClasses;

type

  TODECollisionEvent = procedure (Sender : TObject; Object1, Object2 : TObject;
                                  var Contact:TdContact;
                                  var HandleCollision:Boolean) of object;

  TODEObjectCollisionEvent = procedure (Sender : TObject; Object2 : TObject;
                                        Contact:TdContact) of object;

  TODECollisionSurfaceMode = (csmMu2,csmFDir1,csmBounce,csmSoftERP,csmSoftCFM,
                              csmMotion1,csmMotion2,csmSlip1,csmSlip2);
  TSurfaceModes = set of TODECollisionSurfaceMode;

  TODEElements = class;
  TODEBaseElement = class;
  TODEBaseJoint = class;

  {
  TGLODEManager:
  ----------
  This is the main component that houses the links to ODE.
  }
  TGLODEManager = class (TComponent)
    private
      FWorld             : PdxWorld;
      FSpace             : PdxSpace;
      FContactGroup      : TdJointGroupID;
      FGravity           : TGLCoordinates;
      FOnCollision       : TODECollisionEvent;
      FContactJointCount,
      FNumContactJoints  : integer;
      FDynamicObjectRegister,
      FJointRegister : TPersistentObjectList;
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
         longer update after the ODE World steps. }
      procedure UnregisterObject(aObject:TObject);

      procedure RegisterJoint(aJoint : TODEBaseJoint);
      procedure UnregisterJoint(aJoint : TODEBaseJoint);

      property DynamicObjectRegister : TPersistentObjectList read FDynamicObjectRegister;
      property JointRegister : TPersistentObjectList read FJointRegister;
    public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
      {: Steps the ODE world, then aligns any registered objects to it's ODE
         counterpart, and then the rolling friction is calculated. }
      procedure Step(deltaTime:double);
      property World : PdxWorld read FWorld;
      property Space : PdxSpace read FSpace;
      property NumContactJoints : integer read FNumContactJoints;
    published
      {: The world gravity vector. By default this is a zero length vector
         (no gravity). }
      property Gravity     : TGLCoordinates read FGravity write SetGravity;
      {: This event occurs after the contact information has been filled out
         by the CalcContact procedure and before the contact joint is created.
         The HandleCollision parameter can be set to determine if a collision
         is added the Contact Joints. Any 'last minute' changes to the 
         collisions behaviour can be made through the contact parameter. }
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
  TODECollisionSurface = class (TPersistentObject)
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
      constructor Create; override;
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
      FInitialized : Boolean;
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
      procedure StructureChanged; override;
      procedure NotifyChange(Sender:TObject); override;
      property Initialized : Boolean read FInitialized;
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
      property TurnAngle;
      property Up;
      property Visible;
      property OnProgress;
      property Name;
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
    public
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
      procedure DefineProperties(Filer: TFiler); override;
      procedure WriteElements(stream : TStream);
      procedure ReadElements(stream : TStream);
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
      FManagerName : String;
      FSurface : TODECollisionSurface;
      FOnCollision : TODEObjectCollisionEvent;
      FInitialized : Boolean;
      procedure SetManager(Value : TGLODEManager);
      procedure SetSurface(value:TODECollisionSurface);
    protected
      procedure Initialize; virtual;
      procedure Deinitialize; virtual;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure Loaded; override;
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      property Initialized : Boolean read FInitialized;
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
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      class function FriendlyName : String; override;
      class function UniqueItem : Boolean; override;
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
  and GLODEDynamicBehaviour objects.
  }
  TODEElements = class(TXCollection)
    private
      function GetElement(index : integer) : TODEBaseElement;
    public
      class function ItemsClass : TXCollectionItemClass; override;
      procedure Initialize;
      procedure Deinitialize;
      property Element[index : integer] : TODEBaseElement read GetElement;
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
  TODEBaseElement = class (TXCollectionItem)
    private
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
      procedure RebuildVectors;
      procedure Initialize; virtual;
      procedure Deinitialize; virtual;
      function CalculateMass : TdMass; virtual;
      procedure ODERebuild; virtual;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      procedure BuildList(var rci : TRenderContextInfo); virtual;
      function AbsoluteMatrix:TMatrix;
      function AbsolutePosition:TAffineVector;
      property Matrix : TMatrix read GetMatrix write SetMatrix;
      property GeomTransform : PdxGeom read FGeomTransform;
      property Geom : PdxGeom read FGeomElement;
      property Initialized : Boolean read FInitialized;
    published
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
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
    public
      constructor Create(AOwner : TXCollection); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;
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
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
    public
      constructor Create(AOwner : TXCollection); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;
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
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
    public
      constructor Create(AOwner : TXCollection); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;
    published
      property Radius : single read GetRadius write SetRadius;
      property Length : single read GetLength write SetLength;
  end;

  {
  TODEElementCylinder:
  --------------------
  ODE cylinder implementation.
  }
  { Commented out because of instability
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
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
    public
      constructor Create(AOwner:TODEElements); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;
    published
      property Radius : single read GetRadius write SetRadius;
      property Length : single read GetLength write SetLength;
  end; //}

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
      procedure SetGeom(Val : PdxGeom);
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
      procedure NotifyChange(Sender:TObject); override;
  end;

  {
  TGLODEJoints:
  -----------
  This is the list class that stores the ODE Joints.
  }
  TODEJoints = class(TXCollection)
    private
      function GetJoint(index: integer): TODEBaseJoint;
    public
      class function ItemsClass : TXCollectionItemClass; override;
      procedure Initialize;
      procedure Deinitialize;
      property Joint[index:integer] : TODEBaseJoint read GetJoint; default;
  end;

  TGLODEJointList = class(TComponent)
    private
      FJoints : TODEJoints;
    protected
      procedure DefineProperties(Filer: TFiler); override;
      procedure WriteJoints(stream : TStream);
      procedure ReadJoints(stream : TStream);
      procedure Loaded; override;
    public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
    published
      property Joints : TODEJoints read FJoints;
  end;

  TODEObjectName = String;

  {
  TODEBaseJoint:
  --------------
  Base structures for ODE Joints.
  }
  TODEBaseJoint = class (TXCollectionItem)
    private
      FJointID : TdJointID;
      FObject1,
      FObject2 : TObject;
      FManager : TGLODEManager;
      FManagerName : String;
      FAnchor,
      FAxis,
      FAxis2   : TGLCoordinates;
      FInitialized : Boolean;
      procedure AnchorChange(Sender : TObject);
      procedure AxisChange(Sender : TObject);
      procedure Axis2Change(Sender : TObject);
      procedure SetManager(Value : TGLODEManager);
    protected
      procedure Initialize; virtual;
      procedure Deinitialize; virtual;
      procedure SetAnchor(Value : TAffineVector); virtual;
      procedure SetAxis(Value : TAffineVector); virtual;
      procedure SetAxis2(Value : TAffineVector); virtual;
      property Anchor : TGLCoordinates read FAnchor;
      property Axis : TGLCoordinates read FAxis;
      property Axis2 : TGLCoordinates read FAxis2;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure Loaded; override;
    public
      constructor Create(aOwner : TXCollection); override;
      destructor Destroy; override;
      procedure Attach(Obj1, Obj2 : TObject);
      property JointID : TdJointID read FJointID;
      property Object1 : TObject read FObject1;
      property Object2 : TObject read FObject2;
      property Initialized : Boolean read FInitialized;
    published
      property Manager : TGLODEManager read FManager write SetManager;
  end;

  {
  TODEJointHinge:
  ---------------
  ODE hinge joint implementation.
  }
  TODEJointHinge = class (TODEBaseJoint)
    protected
      procedure Initialize; override;
      procedure SetAnchor(Value : TAffineVector); override;
      procedure SetAxis(Value : TAffineVector); override;
    public
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
    published
      property Anchor;
      property Axis;
  end;

  {
  TODEJointBall:
  --------------
  ODE ball joint implementation.
  }
  TODEJointBall = class (TODEBaseJoint)
    protected
      procedure Initialize; override;
      procedure SetAnchor(Value : TAffineVector); override;
    public
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
    published
      property Anchor;
  end;

  {
  TODEJointSlider:
  ----------------
  ODE slider joint implementation.
  }
  TODEJointSlider = class (TODEBaseJoint)
    protected
      procedure Initialize; override;
      procedure SetAxis(Value : TAffineVector); override;
    public
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
    published
      property Axis;
  end;

  {
  TODEJointFixed:
  ---------------
  ODE fixed joint implementation.
  }
  TODEJointFixed = class (TODEBaseJoint)
    protected
      procedure Initialize; override;
    public
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
  end;

  {
  TODEJointHinge2:
  ---------------
  ODE hinge2 joint implementation.
  }
  TODEJointHinge2 = class (TODEBaseJoint)
    protected
      procedure Initialize; override;
      procedure SetAnchor(Value : TAffineVector); override;
      procedure SetAxis(Value : TAffineVector); override;
      procedure SetAxis2(Value : TAffineVector); override;
    public
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
    published
      property Anchor;
      property Axis;
      property Axis2;
  end;

  {
  TODEJointUniversal:
  -------------------
  ODE universal joint implementation.
  }
  TODEJointUniversal = class (TODEBaseJoint)
    protected
      procedure Initialize; override;
      procedure SetAnchor(Value : TAffineVector); override;
      procedure SetAxis(Value : TAffineVector); override;
      procedure SetAxis2(Value : TAffineVector); override;
    public
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
    published
      property Anchor;
      property Axis;
      property Axis2;
  end;


procedure nearCallBack(Data:Pointer; o1,o2:PdxGeom); cdecl;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------------------------------------------------------
// Misc Procedures
// ------------------------------------------------------------------

// nearCallBack
//
procedure nearCallBack(Data:Pointer; o1,o2:PdxGeom); cdecl;
begin
  TGLODEManager(Data).Collision(o1,o2);
end;

// GetBodyFromODEObject
//
function GetBodyFromODEObject(Obj:TObject):PdxBody;
begin
  Result:=nil;
  if Assigned(Obj) then begin
    if Obj is TGLODEDynamicObject then
      Result:=TGLODEDynamicObject(Obj).Body;
    if Obj is TGLODEDynamicBehaviour then
      Result:=TGLODEDynamicBehaviour(Obj).Body;
  end;
end;

// GetSurfaceFromODEObject
//
function GetSurfaceFromODEObject(Obj:TObject):TODECollisionSurface;
begin
  Result:=nil;
  if Assigned(Obj) then begin
    if Obj is TGLODEBaseObject then
      Result:=TGLODEBaseObject(Obj).Surface;
    if Obj is TGLODEBaseBehaviour then
      Result:=TGLODEBaseBehaviour(Obj).Surface;
  end;
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
  FDynamicObjectRegister:=TPersistentObjectList.Create;
  FJointRegister:=TPersistentObjectList.Create;
  FRFContactList:=TList.Create;

  dWorldSetCFM(FWorld,1e-5);

  RegisterManager(Self);
end;

// Destroy
//
destructor TGLODEManager.Destroy;
begin
  FDynamicObjectRegister.Free;
  FJointRegister.Free;
  FGravity.Free;
  FRFContactList.Free;
  dJointGroupEmpty(FContactGroup);
  dJointGroupDestroy(FContactGroup);
  dSpaceDestroy(FSpace);
  dWorldDestroy(FWorld);
  DeregisterManager(Self);
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

// RegisterJoint
//
procedure TGLODEManager.RegisterJoint(aJoint : TODEBaseJoint);
begin
  FJointRegister.Add(aJoint);
end;

// UnregisterJoint
//
procedure TGLODEManager.UnregisterJoint(aJoint : TODEBaseJoint);
begin
  FJointRegister.Remove(aJoint);
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
  Surface1:=GetSurfaceFromODEObject(Object1);
  Surface2:=GetSurfaceFromODEObject(Object2);
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
  Body1:=GetBodyFromODEObject(Object1);
  Body2:=GetBodyFromODEObject(Object2);
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

// Create
//
constructor TGLODEBaseObject.Create(AOwner: TComponent);
begin
  inherited;
  FCollisionSurface:=TODECollisionSurface.Create;
  FVisibleAtRuntime:=False;
  FInitialized:=False;
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
  FInitialized:=True;
end;

// Deinitialize
//
procedure TGLODEBaseObject.Deinitialize;
begin
  FInitialized:=False;
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
  dBodyGetMass(FBody,FMass);
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
    TODEBaseElement(FElements.Items[i]).BuildList(rci);
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
  FElements.Free;
  FColor.Free;
  inherited;
end;

// Initialize
//
procedure TGLODEDummy.Initialize;
begin
  if (not Assigned(Manager)) or Assigned(FBody) or (FInitialized) then
    exit;

  FBody:=dBodyCreate(Manager.World);
  AlignBodyToMatrix(AbsoluteMatrix);
  dMassSetZero(FMass);
  FElements.Initialize;
  dBodySetMass(FBody,CalculateMass);
  Manager.RegisterObject(self);
  
  inherited;
end;

// Deinitialize
//
procedure TGLODEDummy.Deinitialize;
begin
  FElements.Deinitialize;
  if Assigned(FManager) then
    FManager.UnregisterObject(self);
  
  inherited;
end;

// DefineProperties
//
procedure TGLODEDummy.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ODEElementsData',
                             ReadElements, WriteElements,
                             (Assigned(FElements) and (FElements.Count>0)));
end;

// WriteElements
//
procedure TGLODEDummy.WriteElements(stream : TStream);
var
  writer : TWriter;
begin
  writer:=TWriter.Create(stream, 16384);
  try
    Elements.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

// ReadElements
//
procedure TGLODEDummy.ReadElements(stream : TStream);
var
  reader : TReader;
begin
  reader:=TReader.Create(stream, 16384);
  try
    Elements.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
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
    m:=TODEBaseElement(Elements[i]).CalculateMass;
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
    TODEBaseElement(FElements[i]).Position.Translate(pos);
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
  FInitialized:=False;
end;

// Destroy
//
destructor TGLODEBaseBehaviour.Destroy;
begin
  FSurface.Free;
  inherited;
end;

// Initialize
//
procedure TGLODEBaseBehaviour.Initialize;
begin
  FInitialized:=True;
end;

// Deinitialize
//
procedure TGLODEBaseBehaviour.Deinitialize;
begin
  FInitialized:=False;
end;

// WriteToFiler
//
procedure TGLODEBaseBehaviour.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else WriteString('');
  end;
end;

// ReadFromFiler
//
procedure TGLODEBaseBehaviour.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    FManagerName:=ReadString;
  end;
end;

// Loaded
//
procedure TGLODEBaseBehaviour.Loaded;
var
  mng : TComponent;
begin
  inherited;
  if FManagerName<>'' then begin
    mng:=FindManager(TGLODEManager, FManagerName);
    if Assigned(mng) then
      Manager:=TGLODEManager(mng);
    FManagerName:='';
  end
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
  if (not Assigned(Manager)) or Assigned(FBody) or (FInitialized) then
    exit;

  FBody:=dBodyCreate(Manager.World);
  AlignBodyToMatrix(TGLBaseSceneObject(Owner.Owner).AbsoluteMatrix);
  dMassSetZero(FMass);
  FElements.Initialize;
  CalculateMass;
  dBodySetMass(FBody,FMass);
  Manager.RegisterObject(self);
  
  inherited;
end;

// Deinitialize
//
procedure TGLODEDynamicBehaviour.Deinitialize;
begin
  FElements.Deinitialize;
  if Assigned(Manager) then
    Manager.UnregisterObject(self);

  inherited;
end;

// WriteToFiler
//
procedure TGLODEDynamicBehaviour.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    FElements.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TGLODEDynamicBehaviour.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    FElements.ReadFromFiler(reader);
  end;
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
    m:=TODEBaseElement(Elements[i]).CalculateMass;
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
    TODEBaseElement(FElements[i]).Position.Translate(pos);
end;

// GetMass
//
function TGLODEDynamicBehaviour.GetMass: TdMass;
begin
  dBodyGetMass(FBody,FMass);
  Result:=FMass;
end;

// SetMass
//
procedure TGLODEDynamicBehaviour.SetMass(const value: TdMass);
begin
  FMass:=value;
  dBodySetMass(FBody,FMass);
end;

class function TGLODEDynamicBehaviour.UniqueItem : Boolean;
begin
  Result:=True;
end;


// ------------------------------------------------------------------
// TODEElements Methods
// ------------------------------------------------------------------

// GetElement
//
function TODEElements.GetElement(index : integer) : TODEBaseElement;
begin
  result:=TODEBaseElement(Items[index]);
end;

// ItemsClass
//
class function TODEElements.ItemsClass : TXCollectionItemClass;
begin
  Result:=TODEBaseElement;
end;

// Initialize
//
procedure TODEElements.Initialize;
var
  i : integer;
begin
  for i:=0 to Count-1 do
    TODEBaseElement(Items[i]).Initialize;
end;

// Deintialize
//
procedure TODEElements.Deinitialize;
var
  i : integer;
begin
  for i:=0 to Count-1 do
    TODEBaseElement(Items[i]).Deinitialize;
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
  if Owner.Owner is TGLODEDummy then
    Mat:=TGLODEDummy(Owner.Owner).AbsoluteMatrix;
  if Owner.Owner is TGLODEDynamicBehaviour then
    Mat:=TGLODEDynamicBehaviour(Owner.Owner).AbsoluteMatrix;
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
constructor TODEBaseElement.Create(AOwner : TXCollection);
begin
  inherited;
  FPosition:=TGLCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FPosition.OnNotifyChange:=NotifyChange;
  FDirection:=TGLCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FDirection.OnNotifyChange:=NotifyChange;
  FUp:=TGLCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FUp.OnNotifyChange:=NotifyChange;
  FDensity:=1;
  FInitialized:=False;
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
  if Owner.Owner is TGLODEDummy then begin
    Manager:=TGLODEDummy(Owner.Owner).Manager;
    Body:=TGLODEDummy(Owner.Owner).Body;
  end;
  if Owner.Owner is TGLODEDynamicBehaviour then begin
    Manager:=TGLODEDynamicBehaviour(Owner.Owner).Manager;
    Body:=TGLODEDynamicBehaviour(Owner.Owner).Body;
  end;
  if not (Assigned(Manager) and Assigned(Body)) then exit;

  FGeomTransform:=dCreateGeomTransform(Manager.Space);
  dGeomSetBody(FGeomTransform,Body);
  dGeomTransformSetCleanup(FGeomTransform,1);
  dGeomTransformSetGeom(FGeomTransform,FGeomElement);
  dGeomSetData(FGeomTransform,Owner.Owner);
  FInitialized:=True;
end;

// Deinitialize
//
procedure TODEBaseElement.Deinitialize;
begin
  FInitialized:=False;
end;

// WriteToFiler
//
procedure TODEBaseElement.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    FPosition.WriteToFiler(writer);
    FDirection.WriteToFiler(writer);
    FUp.WriteToFiler(writer);
    WriteSingle(Density);
  end;
end;

// ReadFromFiler
//
procedure TODEBaseElement.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    FPosition.ReadFromFiler(reader);
    FDirection.ReadFromFiler(reader);
    FUp.ReadFromFiler(reader);
    Density:=ReadSingle;
  end;
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

// RebuildVectors
//
procedure TODEBaseElement.RebuildVectors;
begin
  FUp.SetVector(FLocalMatrix[1][0],FLocalMatrix[1][1],FLocalMatrix[1][2]);
  FDirection.SetVector(FLocalMatrix[2][0],FLocalMatrix[2][1],FLocalMatrix[2][2]);
  FPosition.SetPoint(FLocalMatrix[3][0],FLocalMatrix[3][1],FLocalMatrix[3][2]);
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
  RebuildVectors;
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
constructor TODEElementBox.Create(AOwner : TXCollection);
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
  if FInitialized then exit;

  FGeomElement:=dCreateBox(nil,FBoxWidth,FBoxHeight,FBoxDepth);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementBox.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    WriteSingle(BoxWidth);
    WriteSingle(BoxHeight);
    WriteSingle(BoxDepth);
  end;
end;

// ReadFromFiler
//
procedure TODEElementBox.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    BoxWidth:=ReadSingle;
    BoxHeight:=ReadSingle;
    BoxDepth:=ReadSingle;
  end;
end;

// FriendlyName
//
class function TODEElementBox.FriendlyName : String;
begin
  Result:='Box';
end;

// FriendlyDescription
//
class function TODEElementBox.FriendlyDescription : String;
begin
  Result:='The ODE box element implementation';
end;

// ItemCategory
//
class function TODEElementBox.ItemCategory : String;
begin
  Result:='Primitives';
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
constructor TODEElementSphere.Create(AOwner : TXCollection);
begin
  inherited;
  FRadius:=0.5;
end;

// Initialize
//
procedure TODEElementSphere.Initialize;
begin
  if FInitialized then exit;

  FGeomElement:=dCreateSphere(nil,FRadius);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementSphere.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    WriteSingle(Radius);
  end;
end;

// ReadFromFiler
//
procedure TODEElementSphere.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Radius:=ReadSingle;
  end;
end;

// FriendlyName
//
class function TODEElementSphere.FriendlyName : String;
begin
  Result:='Sphere';
end;

// FriendlyDescription
//
class function TODEElementSphere.FriendlyDescription : String;
begin
  Result:='The ODE sphere element implementation';
end;

// ItemCategory
//
class function TODEElementSphere.ItemCategory : String;
begin
  Result:='Primitives';
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
constructor TODEElementCapsule.Create(AOwner : TXCollection);
begin
  inherited;
  FRadius:=0.5;
  FLength:=1;
end;

// Initialize
//
procedure TODEElementCapsule.Initialize;
begin
  if FInitialized then exit;
  
  FGeomElement:=dCreateCCylinder(nil,FRadius,FLength);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementCapsule.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    WriteSingle(Radius);
    WriteSingle(Length);
  end;
end;

// ReadFromFiler
//
procedure TODEElementCapsule.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Radius:=ReadSingle;
    Length:=ReadSingle;
  end;
end;

// FriendlyName
//
class function TODEElementCapsule.FriendlyName : String;
begin
  Result:='Capsule';
end;

// FriendlyDescription
//
class function TODEElementCapsule.FriendlyDescription : String;
begin
  Result:='The ODE capped cylinder element implementation';
end;

// ItemCategory
//
class function TODEElementCapsule.ItemCategory : String;
begin
  Result:='Primitives';
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
{
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
  if FInitialized then exit;

  FGeomElement:=dCreateCylinder(nil,FRadius,FLength);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementCylinder.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    WriteSingle(Radius);
    WriteSingle(Length);
  end;
end;

// ReadFromFiler
//
procedure TODEElementCylinder.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Radius:=ReadSingle;
    Length:=ReadSingle;
  end;
end;

// FriendlyName
//
class function TODEElementCylinder.FriendlyName : String;
begin
  Result:='Cylinder';
end;

// FriendlyDescription
//
class function TODEElementCylinder.FriendlyDescription : String;
begin
  Result:='The ODE cylinder element implementation';
end;

// ItemCategory
//
class function TODEElementCylinder.ItemCategory : String;
begin
  Result:='Primitives';
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
//}


// ------------------------------------------------------------------
// TGLODEStaticObject
// ------------------------------------------------------------------

// SetGeom
//
procedure TGLODEStaticObject.SetGeom(val : PdxGeom);
begin
  FGeom:=val;
end;


// ------------------------------------------------------------------
// TGLODEPlane
// ------------------------------------------------------------------

// AlignODEPlane
//
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

// Initialize
//
procedure TGLODEPlane.Initialize;
begin
  if not Assigned(Manager) then exit;
  FGeom:=dCreatePlane(Manager.Space,0,1,0,0);
  dGeomSetData(FGeom,Self);
  AlignODEPlane;
end;

// NotifyChange
//
procedure TGLODEPlane.NotifyChange(Sender: TObject);
begin
  inherited;
  AlignODEPlane;
end;


// ------------------------------------------------------------------
// TODEJoints Methods
// ------------------------------------------------------------------

// ItemsClass
//
class function TODEJoints.ItemsClass : TXCollectionItemClass;
begin
  Result:=TODEBaseJoint;
end;

// Initialize
//
procedure TODEJoints.Initialize;
var
  i : integer;
begin
  for i:=0 to Count-1 do
    Joint[i].Initialize;
end;

// Deintialize
//
procedure TODEJoints.Deinitialize;
var
  i : integer;
begin
  for i:=0 to Count-1 do
    Joint[i].Deinitialize;
end;

// GetJoint
//
function TODEJoints.GetJoint(index: integer): TODEBaseJoint;
begin
  Result:=TODEBaseJoint(Items[index]);
end;


// ------------------------------------------------------------------
// TGLODEJointList
// ------------------------------------------------------------------

// Create
//
constructor TGLODEJointList.Create(AOwner: TComponent);
begin
  inherited;
  FJoints:=TODEJoints.Create(Self);
end;

// Destroy
//
destructor TGLODEJointList.Destroy;
begin
  FJoints.Free;
  inherited;
end;

// DefineProperties
//
procedure TGLODEJointList.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ODEJointsData',
                             ReadJoints, WriteJoints,
                             (Assigned(FJoints) and (FJoints.Count>0)));
end;

// WriteJoints
//
procedure TGLODEJointList.WriteJoints(stream : TStream);
var
  writer : TWriter;
begin
  writer:=TWriter.Create(stream, 16384);
  try
    Joints.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

// ReadJoints
//
procedure TGLODEJointList.ReadJoints(stream : TStream);
var
  reader : TReader;
begin
  reader:=TReader.Create(stream, 16384);
  try
    Joints.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

// Loaded
//
procedure TGLODEJointList.Loaded;
var
  i : integer;
begin
  inherited;
  for i:=0 to FJoints.Count-1 do
    FJoints[i].Loaded;
end;


// ------------------------------------------------------------------
// TODEBaseJoint
// ------------------------------------------------------------------

// Create
//
constructor TODEBaseJoint.Create(AOwner : TXCollection);
begin
  inherited;
  FAnchor:=TGLCoordinates.CreateInitialized(Self, NullHMGPoint, csPoint);
  FAnchor.OnNotifyChange:=AnchorChange;
  FAxis:=TGLCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FAxis.OnNotifyChange:=AxisChange;
  FAxis2:=TGLCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FAxis2.OnNotifyChange:=Axis2Change;
  FInitialized:=False;
end;

// Destroy
destructor TODEBaseJoint.Destroy;
begin
  FAnchor.Free;
  FAxis.Free;
  FAxis2.Free;
  inherited;
end;

// Initialize
//
procedure TODEBaseJoint.Initialize;
begin
  FManager.RegisterJoint(Self);
  FInitialized:=True;
end;

// Deinitialize
//
procedure TODEBaseJoint.Deinitialize;
begin
  if FJointID<>0 then
    dJointDestroy(FJointID);
  FManager.UnregisterJoint(Self);
  FInitialized:=False;
end;

// WriteToFiler
//
procedure TODEBaseJoint.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else WriteString('');
    FAnchor.WriteToFiler(writer);
    FAxis.WriteToFiler(writer);
    FAxis2.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TODEBaseJoint.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    FManagerName:=ReadString;
    FAnchor.ReadFromFiler(reader);
    FAxis.ReadFromFiler(reader);
    FAxis2.ReadFromFiler(reader);
  end;
end;

// Loaded
//
procedure TODEBaseJoint.Loaded;
var
  mng : TComponent;
begin
  inherited;
  if FManagerName<>'' then begin
    mng:=FindManager(TGLODEManager, FManagerName);
    if Assigned(mng) then
      Manager:=TGLODEManager(mng);
    FManagerName:='';
  end
end;

// AnchorChange
//
procedure TODEBaseJoint.AnchorChange(Sender : TObject);
begin
  if FJointID<>0 then
    SetAnchor(FAnchor.AsAffineVector);
end;

// AxisChange
//
procedure TODEBaseJoint.AxisChange(Sender : TObject);
begin
  if FJointID<>0 then
    SetAxis(FAxis.AsAffineVector);
end;

// Axis2Change
//
procedure TODEBaseJoint.Axis2Change(Sender : TObject);
begin
  if FJointID<>0 then
    SetAxis2(FAxis2.AsAffineVector);
end;

// Attach
//
procedure TODEBaseJoint.Attach(Obj1, Obj2: TObject);
var
  Body1, Body2 : PdxBody;
begin
  if FJointID=0 then exit;

  Body1:=GetBodyFromODEObject(Obj1);
  Body2:=GetBodyFromODEObject(Obj2);
  if Assigned(Body1) or Assigned(Body2) then begin
    dJointAttach(FJointID,Body1,Body2);
  end;
end;

// SetAnchor
//
procedure TODEBaseJoint.SetAnchor(Value: TAffineVector);
begin
  // virtual
end;

// SetAxis
//
procedure TODEBaseJoint.SetAxis(Value: TAffineVector);
begin
  // virtual
end;

// SetAxis2
//
procedure TODEBaseJoint.SetAxis2(Value: TAffineVector);
begin
  // virtual
end;

// SetManager
//
procedure TODEBaseJoint.SetManager(Value: TGLODEManager);
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


// ------------------------------------------------------------------
// TODEJointHinge
// ------------------------------------------------------------------

// Initialize
//
procedure TODEJointHinge.Initialize;
begin
  if (not Assigned(FManager)) or (FInitialized) then exit;
  if FJointID=0 then
    FJointID:=dJointCreateHinge(FManager.World,0);
  inherited;
end;

// SetAnchor
//
procedure TODEJointHinge.SetAnchor(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetHingeAnchor(FJointID,Value[0],Value[1],Value[2]);
end;

// SetAxis
//
procedure TODEJointHinge.SetAxis(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetHingeAxis(FJointID,Value[0],Value[1],Value[2]);
end;

// FriendlyName
//
class function TODEJointHinge.FriendlyName : String;
begin
  Result:='Hinge';
end;

// FriendlyDescription
//
class function TODEJointHinge.FriendlyDescription : String;
begin
  Result:='ODE Hinge joint implementation';
end;


// ------------------------------------------------------------------
// TODEJointBall
// ------------------------------------------------------------------

// Initialize
//
procedure TODEJointBall.Initialize;
begin
  if (not Assigned(FManager)) or (FInitialized) then exit;
  if FJointID=0 then
    FJointID:=dJointCreateBall(FManager.World,0);
  inherited;
end;

// SetAnchor
//
procedure TODEJointBall.SetAnchor(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetBallAnchor(FJointID,Value[0],Value[1],Value[2]);
end;

// FriendlyName
//
class function TODEJointBall.FriendlyName : String;
begin
  Result:='Ball';
end;

// FriendlyDescription
//
class function TODEJointBall.FriendlyDescription : String;
begin
  Result:='ODE Ball joint implementation';
end;


// ------------------------------------------------------------------
// TODEJointSlider
// ------------------------------------------------------------------

// Initialize
//
procedure TODEJointSlider.Initialize;
begin
  if (not Assigned(FManager)) or (FInitialized) then exit;
  if FJointID=0 then
    FJointID:=dJointCreateSlider(FManager.World,0);
  inherited;
end;

// SetAxis
//
procedure TODEJointSlider.SetAxis(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetSliderAxis(FJointID,Value[0],Value[1],Value[2]);
end;

// FriendlyName
//
class function TODEJointSlider.FriendlyName : String;
begin
  Result:='Slider';
end;

// FriendlyDescription
//
class function TODEJointSlider.FriendlyDescription : String;
begin
  Result:='ODE Slider joint implementation';
end;


// ------------------------------------------------------------------
// TODEJointFixed
// ------------------------------------------------------------------

// Initialize
//
procedure TODEJointFixed.Initialize;
begin
  if (not Assigned(FManager)) or (FInitialized) then exit;
  if FJointID=0 then
    FJointID:=dJointCreateFixed(FManager.World,0);
  inherited;
end;

// FriendlyName
//
class function TODEJointFixed.FriendlyName : String;
begin
  Result:='Fixed';
end;

// FriendlyDescription
//
class function TODEJointFixed.FriendlyDescription : String;
begin
  Result:='ODE Fixed joint implementation';
end;


// ------------------------------------------------------------------
// TODEJointHinge2
// ------------------------------------------------------------------

// Initialize
//
procedure TODEJointHinge2.Initialize;
begin
  if (not Assigned(FManager)) or (FInitialized) then exit;
  if FJointID=0 then
    FJointID:=dJointCreateHinge2(FManager.World,0);
  inherited;
end;

// SetAnchor
//
procedure TODEJointHinge2.SetAnchor(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetHinge2Anchor(FJointID,Value[0],Value[1],Value[2]);
end;

// SetAxis
//
procedure TODEJointHinge2.SetAxis(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetHinge2Axis1(FJointID,Value[0],Value[1],Value[2]);
end;

// SetAxis2
//
procedure TODEJointHinge2.SetAxis2(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetHinge2Axis2(FJointID,Value[0],Value[1],Value[2]);
end;

// FriendlyName
//
class function TODEJointHinge2.FriendlyName : String;
begin
  Result:='Hinge2';
end;

// FriendlyDescription
//
class function TODEJointHinge2.FriendlyDescription : String;
begin
  Result:='ODE Double Axis Hinge joint implementation';
end;


// ------------------------------------------------------------------
// TODEJointUniversal
// ------------------------------------------------------------------

// Initialize
//
procedure TODEJointUniversal.Initialize;
begin
  if (not Assigned(FManager)) or (FInitialized) then exit;
  if FJointID=0 then
    FJointID:=dJointCreateUniversal(FManager.World,0);
  inherited;
end;

// SetAnchor
//
procedure TODEJointUniversal.SetAnchor(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetUniversalAnchor(FJointID,Value[0],Value[1],Value[2]);
end;

// SetAxis
//
procedure TODEJointUniversal.SetAxis(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetUniversalAxis1(FJointID,Value[0],Value[1],Value[2]);
end;

// SetAxis2
//
procedure TODEJointUniversal.SetAxis2(Value : TAffineVector);
begin
  if FJointID<>0 then
    dJointSetUniversalAxis2(FJointID,Value[0],Value[1],Value[2]);
end;

// FriendlyName
//
class function TODEJointUniversal.FriendlyName : String;
begin
  Result:='Universal';
end;

// FriendlyDescription
//
class function TODEJointUniversal.FriendlyDescription : String;
begin
  Result:='ODE Universal joint implementation';
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterXCollectionItemClass(TGLODEDynamicBehaviour);
  RegisterXCollectionItemClass(TODEElementBox);
  RegisterXCollectionItemClass(TODEElementSphere);
  RegisterXCollectionItemClass(TODEElementCapsule);
  //RegisterXCollectionItemClass(TODEElementCylinder);

  RegisterXCollectionItemClass(TODEJointHinge);
  RegisterXCollectionItemClass(TODEJointBall);
  RegisterXCollectionItemClass(TODEJointSlider);
  RegisterXCollectionItemClass(TODEJointFixed);
  RegisterXCollectionItemClass(TODEJointHinge2);
  RegisterXCollectionItemClass(TODEJointUniversal);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  UnregisterXCollectionItemClass(TGLODEDynamicBehaviour);
  UnregisterXCollectionItemClass(TODEElementBox);
  UnregisterXCollectionItemClass(TODEElementSphere);
  UnregisterXCollectionItemClass(TODEElementCapsule);
  //UnregisterXCollectionItemClass(TODEElementCylinder);

  UnregisterXCollectionItemClass(TODEJointHinge);
  UnregisterXCollectionItemClass(TODEJointBall);
  UnregisterXCollectionItemClass(TODEJointSlider);
  UnregisterXCollectionItemClass(TODEJointFixed);
  UnregisterXCollectionItemClass(TODEJointHinge2);
  UnregisterXCollectionItemClass(TODEJointUniversal);

end.

// GLODEManager
{: An ODE Manager for GLScene.<p>

  Where can I find ... ?<ul>
    <li>GLScene              (http://glscene.org)
    <li>Open Dynamics Engine (http://opende.sourceforge.org)
    <li>DelphiODE            (http://www.cambrianlabs.com/Mattias/DelphiODE)
  </ul>

  Notes:
  This code is still being developed so any part of it may change at anytime.
  To install use the GLS_ODE?.dpk in the GLScene/Delphi? folder.<p>

  History:<ul>
    <li>03/05/04 - SG - Tri-mesh and various fixes/enhancements.
    <li>23/04/04 - SG - Fixes for object registration,
                        Exception raised now if ODE fails to initialize at run-time.
    <li>21/04/04 - SG - Changed to dynamic linking DelphiODE,
                        Design-time no longer makes any DelphiODE calls.
    <li>15/04/04 - SG - Added OnCustomCollision event to TGLODEManager.
    <li>14/04/04 - SG - Minor DelphiODE compatibility changes.
    <li>30/03/04 - SG - Joint objects are now fully persistent.
    <li>05/03/04 - SG - SetSurfaceMode fix (Alex)
    <li>25/02/04 - SG - Added the GLODEStaticBehaviour.
    <li>24/02/04 - SG - Added the static GLODETerrain collider.
    <li>23/02/04 - SG - Fix for design to real time gravity persistence.
                        Added cone, cylinder and tri-mesh elements.
                        Other various fixes/enhancements.
    <li>28/01/04 - SG - Added TGLODEStaticDummy. Fixed Element alignment code.
                        Other minor fixes/changes.
    <li>13/11/03 - SG - Fixed bug with destroying geoms, manager now forces
                        registered objects to Deinitialize.
                        Fixed up some comments.
    <li>12/11/03 - SG - Fixed bug with TGLODEManager.Collision
    <li>01/09/03 - SG - Changed all relevant floating point types to TdReal,
                        Changed Read/Write Single/Double to Read/Write Float.
    <li>19/08/03 - SG - Added GetBodyFromGLSceneObject (Dan Bartlett),
                        Added StepFast and FastIterations to GLODEManager.
    <li>11/08/03 - SG - Added some force/torque methods to dynamic objects.
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
  Classes, dynode, dynodegl, GLScene, GLMisc, VectorGeometry, GLTexture, OpenGL1x,
  XOpenGL, SysUtils, GLObjects, XCollection, PersistentClasses, VectorLists;

type

  TODECustomCollisionEvent = procedure (Geom1, Geom2 : PdxGeom) of object;

  TODECollisionEvent = procedure (Sender : TObject; Object1, Object2 : TObject;
                                  var Contact:TdContact;
                                  var HandleCollision:Boolean) of object;

  TODEObjectCollisionEvent = procedure (Sender : TObject; Object2 : TObject;
                                        Contact:TdContact) of object;

  TODECollisionSurfaceMode = (csmMu2,csmFDir1,csmBounce,csmSoftERP,csmSoftCFM,
                              csmMotion1,csmMotion2,csmSlip1,csmSlip2);
  TSurfaceModes = set of TODECollisionSurfaceMode;

  TODESolverMethod = (osmDefault, osmStepFast, osmQuickStep);

  TODEElements = class;
  TODEBaseElement = class;
  TODEBaseJoint = class;

  // TGLODEManager
  //
  {: This is the main component that houses the links to ODE. }
  TGLODEManager = class (TComponent)
    private
      FWorld             : PdxWorld;
      FSpace             : PdxSpace;
      FContactGroup      : TdJointGroupID;
      FGravity           : TGLCoordinates;
      FOnCollision       : TODECollisionEvent;
      FOnCustomCollision : TODECustomCollisionEvent;
      FContactJointCount,
      FNumContactJoints  : integer;
      FDynamicObjectRegister,
      FJointRegister : TPersistentObjectList;
      FRFContactList     : TList; // Rolling friction list
      FIterations : Integer;
      FSolver : TODESolverMethod;
      procedure SetGravity(value:TGLCoordinates);
      procedure GravityChange(Sender:TObject);
      procedure SetIterations(const val : Integer);
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

      procedure Loaded; override;

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
      {: Use this event to override the collision handling procedure with your
         own custom collision handling code. }
      property OnCustomCollision : TODECustomCollisionEvent read FOnCustomCollision write FOnCustomCollision;
      //: ODE solver properties
      property Solver : TODESolverMethod read FSolver write FSolver;
      property Iterations : Integer read FIterations write SetIterations;
  end;

  // TODECollisionSurface
  //
  {: Describes how an ODE object should behave when it collides with another object.<p>
     This class is used to describe how an ODE object should behave when it
     collides with another object. This data is used by the TGLODEManager.CalcContact
     procedure, averaging the values of the CollisionSurface to produce an ODE
     contact surface that is (hopefully) accurate. Any fine tuning can be done in
     the TGLODEManager.OnCollision event which is fired after the TGLODEManager.CalcContact
     procedure. }
  TODECollisionSurface = class (TPersistentObject)
    private
      FSurfaceParams : TdSurfaceParameters;
      FRFCoeff   : Single;
      FRFEnabled : Boolean;

      function GetSurfaceMode : TSurfaceModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoftERP : TdReal;
      function GetSoftCFM : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;

      procedure SetSurfaceMode(value:TSurfaceModes);
      procedure SetMu(value : TdReal);
      procedure SetMu2(value : TdReal);
      procedure SetBounce(value : TdReal);
      procedure SetBounce_Vel(value : TdReal);
      procedure SetSoftERP(value : TdReal);
      procedure SetSoftCFM(value : TdReal);
      procedure SetMotion1(value : TdReal);
      procedure SetMotion2(value : TdReal);
      procedure SetSlip1(value : TdReal);
      procedure SetSlip2(value : TdReal);
    public
      constructor Create; override;
    published
      property RollingFrictionCoeff : Single read FRFCoeff write FRFCoeff;
      property RollingFrictionEnabled : Boolean read FRFEnabled write FRFEnabled;
      property SurfaceMode : TSurfaceModes read GetSurfaceMode write SetSurfaceMode;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Bounce : TdReal read GetBounce write SetBounce;
      property Bounce_Vel : TdReal read GetBounce_Vel write SetBounce_Vel;
      property SoftERP : TdReal read GetSoftERP write SetSoftERP;
      property SoftCFM : TdReal read GetSoftCFM write SetSoftCFM;
      property Motion1 : TdReal read GetMotion1 write SetMotion1;
      property Motion2 : TdReal read GetMotion2 write SetMotion2;
      property Slip1 : TdReal read GetSlip1 write SetSlip1;
      property Slip2 : TdReal read GetSlip2 write SetSlip2;
  end;

  TODEElementClass = class of TODEBaseElement;

  // TGLODEBaseObject
  //
  {: Links GLScene object and ODE manager.<p>
     This object provides the base level links to the GLScene object
     heirachy and the GLODEManager. It contains all common properties
     that GLScene &lt;--&gt; ODE objects will require. }
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
      procedure Reinitialize;
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

  // TGLODEDynamicObject
  //
  {: Provides decendant classes for dynamic ODE implementations. }
  TGLODEDynamicObject = class (TGLODEBaseObject)
    private
      FBody       : PdxBody;
      FMass       : TdMass;
      FRealignODE,
      FEnabled    : Boolean;
      procedure AlignBodyToMatrix(Mat: TMatrix);
      procedure SetMass(const value:TdMass);
      function GetMass : TdMass;
      procedure SetEnabled(const Value : Boolean);
      function GetEnabled : Boolean;
    protected
      procedure Initialize; override;
    public
      constructor Create(AOwner : TComponent); override;
      procedure StructureChanged; override;

      procedure AddForce(Force : TAffineVector);
      procedure AddForceAtPos(Force, Pos : TAffineVector);
      procedure AddForceAtRelPos(Force, Pos : TAffineVector);
      procedure AddRelForce(Force : TAffineVector);
      procedure AddRelForceAtPos(Force, Pos : TAffineVector);
      procedure AddRelForceAtRelPos(Force, Pos : TAffineVector);
      procedure AddTorque(Torque : TAffineVector);
      procedure AddRelTorque(Torque : TAffineVector);

      property Body : PdxBody read FBody;
      property Mass : TdMass read GetMass write SetMass;
      property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

  // TGLODEDummy
  //
  {: The main object of GLODEManager. It is basically a composite object
     built from it's child elements. To add elements at run-time use the 
     AddNewElement function. }
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

  // TGLODEBaseBehaviour
  //
  {: Basis structures for GLScene behaviour style implementations. }
  TGLODEBaseBehaviour = class (TGLBehaviour)
    private
      FManager : TGLODEManager;
      FManagerName : String;
      FSurface : TODECollisionSurface;
      FOnCollision : TODEObjectCollisionEvent;
      FInitialized : Boolean;
      FOwnerBaseSceneObject : TGLBaseSceneObject;
      procedure SetManager(Value : TGLODEManager);
      procedure SetSurface(value:TODECollisionSurface);
      function GetAbsoluteMatrix : TMatrix;
    protected
      procedure Initialize; virtual;
      procedure Deinitialize; virtual;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure Loaded; override;
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      procedure Reinitialize;
      property Initialized : Boolean read FInitialized;
      property AbsoluteMatrix : TMatrix read GetAbsoluteMatrix;
    published
      property Manager : TGLODEManager read FManager write SetManager;
      property Surface : TODECollisionSurface read FSurface write SetSurface;
      property OnCollision : TODEObjectCollisionEvent read FOnCollision write FOnCollision;
  end;

  // TGLODEDynamicBehaviour
  //
  {: GLScene behaviour style implementation of the GLODEDummy, used for
     linking a GLScene object to ODE control through GLScene behaviours. }
  TGLODEDynamicBehaviour = class (TGLODEBaseBehaviour)
    private
      FBody : PdxBody;
      FMass : TdMass;
      FElements : TODEElements;
      FEnabled : Boolean;
      procedure SetMass(const Value : TdMass);
      function GetMass : TdMass;
      procedure AlignBodyToMatrix(Mat: TMatrix);
      procedure SetEnabled(const Value : Boolean);
      function GetEnabled : Boolean;
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

      procedure AddForce(Force : TAffineVector);
      procedure AddForceAtPos(Force, Pos : TAffineVector);
      procedure AddForceAtRelPos(Force, Pos : TAffineVector);
      procedure AddRelForce(Force : TAffineVector);
      procedure AddRelForceAtPos(Force, Pos : TAffineVector);
      procedure AddRelForceAtRelPos(Force, Pos : TAffineVector);
      procedure AddTorque(Torque : TAffineVector);
      procedure AddRelTorque(Torque : TAffineVector);

      property Body : PdxBody read FBody;
      property Mass : TdMass read GetMass write SetMass;
    published
      property Elements : TODEElements read FElements;
      property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

  // TGLODEStaticBehaviour
  //
  {: Static ODE object binding through a behaviour. }
  TGLODEStaticBehaviour = class (TGLODEBaseBehaviour)
    private
      FElements : TODEElements;
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure AlignElementsToMatrix(Mat:TMatrix);
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      class function FriendlyName : String; override;
      class function UniqueItem : Boolean; override;
      function AddNewElement(AChild:TODEElementClass):TODEBaseElement; dynamic;
    published
      property Elements : TODEElements read FElements;
  end;

  // TODEElements
  //
  {: This is the list class that stores the ODEElements for GLODEDummy
     and GLODEDynamicBehaviour objects. }
  TODEElements = class(TXCollection)
    private
      function GetElement(index : integer) : TODEBaseElement;
    public
      destructor Destroy; override;
      class function ItemsClass : TXCollectionItemClass; override;
      procedure Initialize;
      procedure Deinitialize;
      property Element[index : integer] : TODEBaseElement read GetElement;
  end;

  // TODEBaseElement
  //
  {: This class is the basis for all ODEElements. It provides common ODE 
     properties like Mass, Density and a Geom to decended classes. To
     orient the object inside the GLODEDummy use Position, Direction and 
     Up or use Matrix. The ODE component of the object will be aligned to 
     any changes made to these properties. }
  TODEBaseElement = class (TXCollectionItem)
    private
      FMass  : TdMass;
      FDensity : TdReal;
      FGeomTransform,
      FGeomElement   : PdxGeom;
      FPosition,
      FDirection,
      FUp        : TGLCoordinates;
      FLocalMatrix : TMatrix;
      FRealignODE : Boolean;
      FInitialized : Boolean;
      FDynamic : Boolean;
      procedure AlignGeomElementToMatrix(Mat:TMatrix);
      procedure SetDensity(const Value: TdReal);
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
      function IsODEInitialized : Boolean;
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
      property Density : TdReal read FDensity write SetDensity;
      property Position : TGLCoordinates read FPosition;
      property Direction : TGLCoordinates read FDirection;
      property Up : TGLCoordinates read FUp;
  end;

  // TODEElementBox
  //
  {: ODE box implementation. }
  TODEElementBox = class (TODEBaseElement)
    private
      FBoxWidth,
      FBoxHeight,
      FBoxDepth : TdReal;
      function GetBoxWidth  : TdReal;
      function GetBoxHeight : TdReal;
      function GetBoxDepth  : TdReal;
      procedure SetBoxWidth(const Value: TdReal);
      procedure SetBoxHeight(const Value: TdReal);
      procedure SetBoxDepth(const Value: TdReal);
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
      property BoxWidth : TdReal read GetBoxWidth write SetBoxWidth;
      property BoxHeight : TdReal read GetBoxHeight write SetBoxHeight;
      property BoxDepth : TdReal read GetBoxDepth write SetBoxDepth;
  end;

  // TODEElementSphere
  //
  {: ODE sphere implementation. }
  TODEElementSphere = class (TODEBaseElement)
    private
      FRadius : TdReal;
      function GetRadius : TdReal;
      procedure SetRadius(const Value: TdReal);
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
      property Radius : TdReal read GetRadius write SetRadius;
  end;

  // TODEElementCapsule
  //
  {: ODE capped cylinder implementation. }
  TODEElementCapsule = class (TODEBaseElement)
    private
      FRadius,
      FLength : TdReal;
      function GetRadius : TdReal;
      function GetLength : TdReal;
      procedure SetRadius(const Value: TdReal);
      procedure SetLength(const Value: TdReal);
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
      property Radius : TdReal read GetRadius write SetRadius;
      property Length : TdReal read GetLength write SetLength;
  end;

  // TODEElementCylinder
  //
  {: ODE cylinder implementation. }
  TODEElementCylinder = class (TODEBaseElement)
    private
      FRadius,
      FLength : TdReal;
      function GetRadius : TdReal;
      function GetLength : TdReal;
      procedure SetRadius(const Value: TdReal);
      procedure SetLength(const Value: TdReal);
    protected
      procedure Initialize; override;
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
    public
      constructor Create(AOwner:TXCollection); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;
    published
      property Radius : TdReal read GetRadius write SetRadius;
      property Length : TdReal read GetLength write SetLength;
  end;

  // TODEElementCone
  //
  {: ODE cone implementation. }
  TODEElementCone = class (TODEBaseElement)
    private
      FRadius,
      FLength : TdReal;
      function GetRadius : TdReal;
      function GetLength : TdReal;
      procedure SetRadius(const Value: TdReal);
      procedure SetLength(const Value: TdReal);
    protected
      procedure Initialize; override;
      function CalculateMass : TdMass; override;
      procedure ODERebuild; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
    public
      constructor Create(AOwner:TXCollection); override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;
    published
      property Radius : TdReal read GetRadius write SetRadius;
      property Length : TdReal read GetLength write SetLength;
  end;

  // TODEElementTriMesh
  //
  {: ODE tri-mesh implementation. }
  TODEElementTriMesh = class (TODEBaseElement)
    private
      FTriMeshData : PdxTriMeshData;
      FVertices : TAffineVectorList;
      FIndices : TIntegerList;
    protected
      procedure Initialize; override;
      procedure Deinitialize; override;
      function CalculateMass : TdMass; override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure SetVertices(const Value : TAffineVectorList);
      procedure SetIndices(const Value : TIntegerList);
    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;
      procedure RefreshTriMeshData;

      property Vertices : TAffineVectorList read FVertices write SetVertices;
      property Indices : TIntegerList read FIndices write SetIndices;
  end;

  // TGLODEStaticObject
  //
  {: This object provides decendant classes for static ODE implementations
     which have a geom for collisions but no body or mass for motion. }
  TGLODEStaticObject = class (TGLODEBaseObject)
    private
      FGeom : PdxGeom;

    protected
      procedure Deinitialize; override;
      procedure SetGeom(const Value : PdxGeom);

    public
      property Geom : PdxGeom read FGeom;
  end;

  // TGLODEPlane
  //
  {: The ODE plane geom implementation. Use the direction and position to set
     up the plane (just like a normal GLScene plane). }
  TGLODEPlane = class (TGLODEStaticObject)
    private
      procedure AlignODEPlane;
    protected
      procedure Initialize; override;
    public
      procedure NotifyChange(Sender:TObject); override;
  end;

  // TGLODETerrain
  //
  {: ODE terrain collider implementation. Data is the terrain height data
     array. The Length property defines the dimensions of the terrain and
     NumNodesPerSide sets the height data dimensions. The Terrain collider
     has several limitations: It can't be moved, the data must be square
     and positive Z is assumed to be up. Also, the terrain data is repeated
     indefinately. }
  TGLODETerrain = class (TGLODEStaticObject)
    private
      FData : PdRealHugeArray;
      FLength : TdReal;
      FNumNodesPerSide : Integer;

    protected
      procedure Initialize; override;
      procedure SetLength(const Value : TdReal);
      procedure SetNumNodesPerSide(const Value : Integer);
      procedure SetData(index : Integer; const Value : TdReal);
      procedure SetRaster(x,y : Integer; const Value : TdReal);
      function GetData(index : Integer) : TdReal;
      function GetRaster(x,y : Integer) : TdReal;

    public
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;

      procedure AssignData(aSource : PdRealHugeArray; aNumNodesPerSide : Integer);

      property Data[index : Integer] : TdReal read GetData write SetData;
      property Raster[x,y : Integer] : TdReal read GetRaster write SetRaster;

    published
      property Length : TdReal read FLength write SetLength;
      property NumNodesPerSide : Integer read FNumNodesPerSide write SetNumNodesPerSide;
  end;

  // TGLODEStaticDummy
  //
  //: A static version of the TGLODEDummy.
  TGLODEStaticDummy = class (TGLODEBaseObject)
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
      procedure AlignElementsToMatrix(Mat:TMatrix);
    public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
      procedure BuildList(var rci : TRenderContextInfo); override;
      procedure StructureChanged; override;
      procedure NotifyChange(Sender:TObject); override;

      function AddNewElement(AChild:TODEElementClass):TODEBaseElement;
    published
      property Color : TGLColor read FColor write SetColor;
      property Elements : TODEElements read FElements;
  end;

  // TGLODEJoints
  //
  {: An XCollection decendant for ODE Joints. }
  TODEJoints = class(TXCollection)
    private
      function GetJoint(index: integer): TODEBaseJoint;
    public
      class function ItemsClass : TXCollectionItemClass; override;
      procedure Initialize;
      procedure Deinitialize;
      property Joint[index:integer] : TODEBaseJoint read GetJoint; default;
  end;

  // TGLODEJointList
  //
  {: Component front-end for storing ODE Joints. }
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

  // TODEBaseJoint
  //
  {: Base structures for ODE Joints. }
  TODEBaseJoint = class (TXCollectionItem)
    private
      FJointID : TdJointID;
      FObject1,
      FObject2 : TGLBaseSceneObject;
      FManager : TGLODEManager;
      FObject1Name,
      FObject2Name,
      FManagerName : String;
      FAnchor,
      FAxis,
      FAxis2   : TGLCoordinates;
      FInitialized : Boolean;
      procedure AnchorChange(Sender : TObject);
      procedure AxisChange(Sender : TObject);
      procedure Axis2Change(Sender : TObject);
      procedure SetManager(const Value : TGLODEManager);
      procedure SetObject1(const Value : TGLBaseSceneObject);
      procedure SetObject2(const Value : TGLBaseSceneObject);
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
      function IsODEInitialized : Boolean;
    public
      constructor Create(aOwner : TXCollection); override;
      destructor Destroy; override;
      procedure Attach(Obj1, Obj2 : TGLBaseSceneObject);
      property JointID : TdJointID read FJointID;
      property Initialized : Boolean read FInitialized;
    published
      property Manager : TGLODEManager read FManager write SetManager;
      property Object1 : TGLBaseSceneObject read FObject1 write SetObject1;
      property Object2 : TGLBaseSceneObject read FObject2 write SetObject2;
  end;

  // TODEJointHinge
  //
  {: ODE hinge joint implementation. }
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

  // TODEJointBall
  //
  {: ODE ball joint implementation. }
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

  // TODEJointSlider
  //
  {: ODE slider joint implementation. }
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

  // TODEJointFixed
  //
  {: ODE fixed joint implementation. }
  TODEJointFixed = class (TODEBaseJoint)
    protected
      procedure Initialize; override;
    public
      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
  end;

  // TODEJointHinge2
  //
  {: ODE hinge2 joint implementation. }
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

  // TODEJointUniversal
  //
  {: ODE universal joint implementation. }
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


{: ODE nearCallBack, throws near callback to the collision procedure
   of the ODE manager linked by the Data pointer. }
procedure nearCallBack(Data:Pointer; o1,o2:PdxGeom); cdecl;
{: Helper functions for extracting data from objects with different
   inheritance. }
function GetBodyFromODEObject(Obj:TObject):PdxBody;
function GetSurfaceFromODEObject(Obj:TObject):TODECollisionSurface;
function GetBodyFromGLSceneObject(Obj:TGLBaseSceneObject):PdxBody;

// GLODEObject register methods (used for joint object persistence)
procedure RegisterGLODEObject(aGLODEObject : TGLBaseSceneObject);
procedure UnregisterGLODEObject(aGLODEObject : TGLBaseSceneObject);
function GetGLODEObject(aObjectName : String) : TGLBaseSceneObject;

var
  vGLODEObjectRegister : TList;

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

// IsDynamic
//
function IsDynamic(Obj:TObject) : Boolean;
begin
  Result:=   (Obj is TGLODEDynamicObject)
          or (Obj is TGLODEDynamicBehaviour);
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

// GetBodyFromGLSceneObject
//
function GetBodyFromGLSceneObject(Obj:TGLBaseSceneObject):PdxBody;
var
  GLOB:TGLODEDynamicBehaviour;
begin
  Result:=nil;
  if Assigned(Obj) then begin
    if Obj is TGLODEDynamicObject then
      Result:=TGLODEDynamicObject(Obj).Body
    else begin
      GLOB:=TGLODEDynamicBehaviour(Obj.Behaviours.GetByClass(TGLODEDynamicBehaviour));
      if GLOB<>nil then
        Result:=GLOB.Body;
    end;
  end;
end;

// IsGLODEObject
//
function IsGLODEObject(Obj:TGLBaseSceneObject):Boolean;
var
  GLOB:TGLODEDynamicBehaviour;
begin
  Result:=False;
  if Assigned(Obj) then begin
    if Obj is TGLODEDynamicObject then
      Result:=True
    else begin
      GLOB:=TGLODEDynamicBehaviour(Obj.Behaviours.GetByClass(TGLODEDynamicBehaviour));
      if Assigned(GLOB) then
        Result:=True;
    end;
  end;
end;

// RegisterGLODEObject
//
procedure RegisterGLODEObject(aGLODEObject : TGLBaseSceneObject);
begin
  if vGLODEObjectRegister.IndexOf(aGLODEObject) = -1 then
    vGLODEObjectRegister.Add(aGLODEObject);
end;

// UnregisterGLODEObject
//
procedure UnregisterGLODEObject(aGLODEObject : TGLBaseSceneObject);
begin
  vGLODEObjectRegister.Remove(aGLODEObject);
end;

// GetGLODEObject
//
function GetGLODEObject(aObjectName : String) : TGLBaseSceneObject;
var
  i : Integer;
begin
  Result:=nil;
  for i:=0 to vGLODEObjectRegister.Count-1 do
    if TGLBaseSceneObject(vGLODEObjectRegister[i]).GetNamePath = aObjectName then begin
      Result:=vGLODEObjectRegister[i];
      Exit;
    end;
end;


// ------------------------------------------------------------------
// TGLODEManager Methods
// ------------------------------------------------------------------

// Create
//
constructor TGLODEManager.Create(AOwner:TComponent);
begin
  inherited;

  FDynamicObjectRegister:=TPersistentObjectList.Create;
  FJointRegister:=TPersistentObjectList.Create;
  FRFContactList:=TList.Create;

  FGravity:=TGLCoordinates.CreateInitialized(Self, NullHmgPoint, csVector);
  FGravity.OnNotifyChange:=GravityChange;

  FSolver:=osmDefault;
  FIterations:=3;

  if not (csDesigning in ComponentState) then begin
    if not InitODE('') then
      raise Exception.Create('ODE failed to initialize.');
    FWorld:=dWorldCreate;
    FSpace:=dHashSpaceCreate(nil);
    dWorldSetCFM(FWorld,1e-5);
    dWorldSetQuickStepNumIterations(FWorld, FIterations);
    FContactGroup:=dJointGroupCreate(100);
  end;

  RegisterManager(Self);
end;

// Destroy
//
destructor TGLODEManager.Destroy;
begin
  // Unregister everything
  while FDynamicObjectRegister.Count>0 do begin
    if FDynamicObjectRegister[0] is TGLODEBaseObject then
      TGLODEBaseObject(FDynamicObjectRegister[0]).Manager:=nil
    else if FDynamicObjectRegister[0] is TGLODEBaseBehaviour then
      TGLODEBaseBehaviour(FDynamicObjectRegister[0]).Manager:=nil
    else FDynamicObjectRegister.Delete(0);
  end;

  // Clean up everything
  FDynamicObjectRegister.Free;
  FJointRegister.Free;
  FGravity.Free;
  FRFContactList.Free;

  if Assigned(FWorld) then begin
    dJointGroupEmpty(FContactGroup);
    dJointGroupDestroy(FContactGroup);
    dSpaceDestroy(FSpace);
    dWorldDestroy(FWorld);
    CloseODE;
  end;

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

// Loaded
//
procedure TGLODEManager.Loaded;
begin
  GravityChange(Self);
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
  if Assigned(FWorld) then
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
  // Check for custom collision handling event
  if Assigned(FOnCustomCollision) then begin
    FOnCustomCollision(g1,g2);
    exit;
  end;

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
        if Assigned(TGLODEBaseObject(Obj1).FOnCollision) then
          TGLODEBaseObject(Obj1).FOnCollision(Self,Obj2,Contact[i]);
      if TObject(Obj2) is TGLODEBaseObject then
        if Assigned(TGLODEBaseObject(Obj2).FOnCollision) then
          TGLODEBaseObject(Obj2).FOnCollision(Self,Obj1,Contact[i]);
      if TObject(Obj1) is TGLODEBaseBehaviour then
        if Assigned(TGLODEBaseBehaviour(Obj1).FOnCollision) then
          TGLODEBaseBehaviour(Obj1).FOnCollision(Self,Obj2,Contact[i]);
      if TObject(Obj2) is TGLODEBaseBehaviour then
        if Assigned(TGLODEBaseBehaviour(Obj2).FOnCollision) then
          TGLODEBaseBehaviour(Obj2).FOnCollision(Self,Obj1,Contact[i]);
    end else begin
      // Default surface values
      contact[i].surface.mu:=1000;
    end;
    if HandleCollision then begin
      // Create and assign the contact joint
      Joint:=dJointCreateContact(FWorld,FContactGroup,@contact[i]);
      dJointAttach(Joint,b1,b2);
      // Increment the number of contact joints this step
      FContactJointCount:=FContactJointCount+1;
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
  if not Assigned(World) then exit;

  // Reset the contact joint counter
  FContactJointCount:=0;

  // Run ODE collisions and step the scene
  dSpaceCollide(FSpace,Self,nearCallback);
  case FSolver of
    osmDefault   : dWorldStep(FWorld, deltaTime);
    osmStepFast  : dWorldStepFast1(FWorld, deltaTime, FIterations);
    osmQuickStep : dWorldQuickStep(FWorld, deltaTime);
  end;
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

procedure TGLODEManager.SetIterations(const val : Integer);
begin
  FIterations:=val;
  if Assigned(FWorld) then
    dWorldSetQuickStepNumIterations(FWorld, FIterations);
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
  AMode : Integer;
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
function TODECollisionSurface.GetMu : TdReal;
begin
  result:=FSurfaceParams.Mu;
end;

function TODECollisionSurface.GetMu2 : TdReal;
begin
  result:=FSurfaceParams.Mu2;
end;

function TODECollisionSurface.GetBounce : TdReal;
begin
  result:=FSurfaceParams.Bounce;
end;

function TODECollisionSurface.GetBounce_Vel : TdReal;
begin
  result:=FSurfaceParams.Bounce_Vel;
end;

function TODECollisionSurface.GetSoftERP : TdReal;
begin
  result:=FSurfaceParams.soft_erp;
end;

function TODECollisionSurface.GetSoftCFM : TdReal;
begin
  result:=FSurfaceParams.soft_cfm;
end;

function TODECollisionSurface.GetMotion1 : TdReal;
begin
  result:=FSurfaceParams.Motion1;
end;

function TODECollisionSurface.GetMotion2 : TdReal;
begin
  result:=FSurfaceParams.Motion2;
end;

function TODECollisionSurface.GetSlip1 : TdReal;
begin
  result:=FSurfaceParams.Slip1;
end;

function TODECollisionSurface.GetSlip2 : TdReal;
begin
  result:=FSurfaceParams.Slip2;
end;

procedure TODECollisionSurface.SetMu(value : TdReal);
begin
  FSurfaceParams.Mu:=value;
end;

procedure TODECollisionSurface.SetMu2(value : TdReal);
begin
  FSurfaceParams.Mu2:=value;
end;

procedure TODECollisionSurface.SetBounce(value : TdReal);
begin
  FSurfaceParams.Bounce:=value;
end;

procedure TODECollisionSurface.SetBounce_Vel(value : TdReal);
begin
  FSurfaceParams.Bounce_Vel:=value;
end;

procedure TODECollisionSurface.SetSoftERP(value : TdReal);
begin
  FSurfaceParams.soft_erp:=value;
end;

procedure TODECollisionSurface.SetSoftCFM(value : TdReal);
begin
  FSurfaceParams.soft_cfm:=value;
end;

procedure TODECollisionSurface.SetMotion1(value : TdReal);
begin
  FSurfaceParams.Motion1:=value;
end;

procedure TODECollisionSurface.SetMotion2(value : TdReal);
begin
  FSurfaceParams.Motion2:=value;
end;

procedure TODECollisionSurface.SetSlip1(value : TdReal);
begin
  FSurfaceParams.Slip1:=value;
end;

procedure TODECollisionSurface.SetSlip2(value : TdReal);
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
  RegisterGLODEObject(Self);
end;

// Destroy
//
destructor TGLODEBaseObject.Destroy;
begin
  Deinitialize;
  UnregisterGLODEObject(Self);
  FCollisionSurface.Free;
  inherited;
end;

// Initialize
//
procedure TGLODEBaseObject.Initialize;
begin
  Manager.RegisterObject(self);
  FInitialized:=True;
end;

// Deinitialize
//
procedure TGLODEBaseObject.Deinitialize;
begin
  if Assigned(FManager) then
    FManager.UnregisterObject(self);
  FInitialized:=False;
end;

// Reinitialize
//
procedure TGLODEBaseObject.Reinitialize;
begin
  if Initialized then
    Deinitialize;
  Initialize;
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
    if Assigned(FManager) and not (csDesigning in ComponentState) then
      Deinitialize;
    FManager:=Value;
    if Assigned(FManager) and not (csDesigning in ComponentState) then
      Initialize;
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

// Create
//
constructor TGLODEDynamicObject.Create(AOwner : TComponent);
begin
  inherited;
  FEnabled:=True;
end;

// StructureChanged
//
procedure TGLODEDynamicObject.StructureChanged;
begin
  AlignBodyToMatrix(AbsoluteMatrix);
  inherited;
end;

// Initialize
//
procedure TGLODEDynamicObject.Initialize;
begin
  Enabled:=FEnabled;
  inherited;
end;

// AlignBodyToMatrix
//
procedure TGLODEDynamicObject.AlignBodyToMatrix(Mat: TMatrix);
var
  R : TdMatrix3;
begin
  if not Assigned(FBody) then exit;
  R[0]:=Mat[0][0]; R[1]:=Mat[1][0]; R[2]:= Mat[2][0]; R[3]:= 0;
  R[4]:=Mat[0][1]; R[5]:=Mat[1][1]; R[6]:= Mat[2][1]; R[7]:= 0;
  R[8]:=Mat[0][2]; R[9]:=Mat[1][2]; R[10]:=Mat[2][2]; R[11]:=0;
  dBodySetRotation(FBody,R);
  dBodySetPosition(FBody,Mat[3][0],Mat[3][1],Mat[3][2]);
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
  dBodySetMass(FBody,@FMass);
end;

// SetEnabled
//
procedure TGLODEDynamicObject.SetEnabled(const Value : Boolean);
begin
  if Assigned(FBody) then
    if Value <> Enabled then begin
      FEnabled:=Value;
      if Assigned(FBody) then begin
        if FEnabled then dBodyEnable(FBody)
        else dBodyDisable(FBody);
      end;
    end;
end;

// GetEnabled
//
function TGLODEDynamicObject.GetEnabled : Boolean;
begin
  if Assigned(FBody) then
    FEnabled:=(dBodyIsEnabled(FBody)=1);
  Result:=FEnabled;
end;

// AddForce
//
procedure TGLODEDynamicObject.AddForce(Force : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForce(FBody,Force[0],Force[1],Force[2]);
end;

// AddlForceAtPos
//
procedure TGLODEDynamicObject.AddForceAtPos(Force, Pos : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtPos(FBody,Force[0],Force[1],Force[2],Pos[0],Pos[1],Pos[2]);
end;

// AddForceAtRelPos
//
procedure TGLODEDynamicObject.AddForceAtRelPos(Force, Pos : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtRelPos(FBody,Force[0],Force[1],Force[2],Pos[0],Pos[1],Pos[2]);
end;

// AddRelForce
//
procedure TGLODEDynamicObject.AddRelForce(Force : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelForce(FBody,Force[0],Force[1],Force[2]);
end;

// AddRelForceAtPos
//
procedure TGLODEDynamicObject.AddRelForceAtPos(Force, Pos : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtPos(FBody,Force[0],Force[1],Force[2],Pos[0],Pos[1],Pos[2]);
end;

// AddRelForceAtRelPos
//
procedure TGLODEDynamicObject.AddRelForceAtRelPos(Force, Pos : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelForceAtRelPos(FBody,Force[0],Force[1],Force[2],Pos[0],Pos[1],Pos[2]);
end;

// AddTorque
//
procedure TGLODEDynamicObject.AddTorque(Torque : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddTorque(FBody,Torque[0],Torque[1],Torque[2]);
end;

// AddRelTorque
//
procedure TGLODEDynamicObject.AddRelTorque(Torque : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelTorque(FBody,Torque[0],Torque[1],Torque[2]);
end;


// ------------------------------------------------------------------
// TGLODEDummy
// ------------------------------------------------------------------

// AddNewElement
//
function TGLODEDummy.AddNewElement(AChild: TODEElementClass): TODEBaseElement;
var
  calcmass : TdMass;
begin
  Result:=AChild.Create(FElements);
  FElements.Add(Result);
  if FInitialized then begin
    Result.Initialize;
    calcmass:=CalculateMass;
    dBodySetMass(FBody,@calcmass);
  end;
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
  rci.GLStates.ResetGLMaterialColors;
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
  inherited;
  FElements.Free;
  FColor.Free;
end;

// Initialize
//
procedure TGLODEDummy.Initialize;
var
  calcmass : TdMass;
begin
  if (not Assigned(Manager)) or Assigned(FBody) or (FInitialized) then exit;
  if not Assigned(Manager.World) then exit;

  FBody:=dBodyCreate(Manager.World);
  AlignBodyToMatrix(AbsoluteMatrix);
  dMassSetZero(FMass);
  FElements.Initialize;
  calcmass:=CalculateMass;
  dBodySetMass(FBody,@calcmass);

  inherited;
end;

// Deinitialize
//
procedure TGLODEDummy.Deinitialize;
begin
  if not FInitialized then exit;
  FElements.Deinitialize;
  if Assigned(FBody) then begin
    dBodyDestroy(FBody);
    FBody:=nil;
  end;
  dMassSetZero(FMass);

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
var
  calcmass : TdMass;
begin
  if Assigned(FBody) then begin
    calcmass:=CalculateMass;
    dBodySetMass(FBody,@calcmass);
  end;
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
  FOwnerBaseSceneObject:=OwnerBaseSceneObject;
  if Assigned(FOwnerBaseSceneObject) then
    RegisterGLODEObject(OwnerBaseSceneObject);
end;

// Destroy
//
destructor TGLODEBaseBehaviour.Destroy;
begin
  Deinitialize;
  FSurface.Free;
  // This is a dodgy way to do it but at least
  // it doesn't crash ;)
  if Assigned(FOwnerBaseSceneObject) then
    UnregisterGLODEObject(FOwnerBaseSceneObject);
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

// Reinitialize
//
procedure TGLODEBaseBehaviour.Reinitialize;
begin
  if Initialized then
    Deinitialize;
  Initialize;
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
    if Assigned(FManager) and not (csDesigning in TComponent(Owner.Owner).ComponentState) then
      Deinitialize;
    FManager:=Value;
    if Assigned(FManager) and not (csDesigning in TComponent(Owner.Owner).ComponentState) then
      Initialize;
  end;
end;

// SetSurface
//
procedure TGLODEBaseBehaviour.SetSurface(value: TODECollisionSurface);
begin
  FSurface.Assign(value);
end;

// GetAbsoluteMatrix
//
function TGLODEBaseBehaviour.GetAbsoluteMatrix;
begin
  Result:=IdentityHMGMatrix;
  if Assigned(Owner.Owner) then
    if Owner.Owner is TGLBaseSceneObject then
      Result:=TGLBaseSceneObject(Owner.Owner).AbsoluteMatrix;
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
  FEnabled:=True;
end;

// Destroy
//
destructor TGLODEDynamicBehaviour.Destroy;
begin
  inherited;
  FElements.Free;
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
  if (not Assigned(Manager)) or Assigned(FBody) or (FInitialized) then exit;
  if not Assigned(Manager.World) then exit;

  FBody:=dBodyCreate(Manager.World);
  AlignBodyToMatrix(OwnerBaseSceneObject.AbsoluteMatrix);
  dMassSetZero(FMass);
  FElements.Initialize;
  CalculateMass;
  dBodySetMass(FBody,@FMass);
  Manager.RegisterObject(self);

  Enabled:=FEnabled;

  inherited;
end;

// Deinitialize
//
procedure TGLODEDynamicBehaviour.Deinitialize;
begin
  if not FInitialized then exit;
  FElements.Deinitialize;
  if Assigned(FBody) then begin
    dBodyDestroy(FBody);
    FBody:=nil;
  end;
  dMassSetZero(FMass);
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
var
  calcmass : TdMass;
begin
  Result:=nil;
  if not Assigned(Manager) then exit;
  Result:=AChild.Create(FElements);
  FElements.Add(Result);
  Result.Initialize;
  calcmass:=CalculateMass;
  dBodySetMass(FBody,@calcmass);
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
  if OwnerBaseSceneObject.Parent is TGLBaseSceneObject then
    m:=MatrixMultiply(m, OwnerBaseSceneObject.Parent.InvAbsoluteMatrix);
  OwnerBaseSceneObject.Matrix:=m;
end;

// AlignBodyToMatrix
//
procedure TGLODEDynamicBehaviour.AlignBodyToMatrix(Mat:TMatrix);
var
  R : TdMatrix3;
begin
  if not Assigned(FBody) then exit;
  R[0]:=Mat[0][0]; R[1]:=Mat[1][0]; R[2]:= Mat[2][0]; R[3]:= 0;
  R[4]:=Mat[0][1]; R[5]:=Mat[1][1]; R[6]:= Mat[2][1]; R[7]:= 0;
  R[8]:=Mat[0][2]; R[9]:=Mat[1][2]; R[10]:=Mat[2][2]; R[11]:=0;
  dBodySetRotation(FBody,R);
  dBodySetPosition(FBody,Mat[3][0],Mat[3][1],Mat[3][2]);
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
  dBodySetMass(FBody,@FMass);
end;

// UniqueItem
//
class function TGLODEDynamicBehaviour.UniqueItem : Boolean;
begin
  Result:=True;
end;

// SetEnabled
//
procedure TGLODEDynamicBehaviour.SetEnabled(const Value : Boolean);
begin
  if Assigned(FBody) then
    if Value <> Enabled then begin
      FEnabled:=Value;
      if Assigned(FBody) then begin
        if FEnabled then dBodyEnable(FBody)
        else dBodyDisable(FBody);
      end;
    end;
end;

// GetEnabled
//
function TGLODEDynamicBehaviour.GetEnabled : Boolean;
begin
  if Assigned(FBody) then
    FEnabled:=(dBodyIsEnabled(FBody)=1);
  Result:=FEnabled;
end;

// AddForce
//
procedure TGLODEDynamicBehaviour.AddForce(Force : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForce(FBody,Force[0],Force[1],Force[2]);
end;

// AddlForceAtPos
//
procedure TGLODEDynamicBehaviour.AddForceAtPos(Force, Pos : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtPos(FBody,Force[0],Force[1],Force[2],Pos[0],Pos[1],Pos[2]);
end;

// AddForceAtRelPos
//
procedure TGLODEDynamicBehaviour.AddForceAtRelPos(Force, Pos : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtRelPos(FBody,Force[0],Force[1],Force[2],Pos[0],Pos[1],Pos[2]);
end;

// AddRelForce
//
procedure TGLODEDynamicBehaviour.AddRelForce(Force : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelForce(FBody,Force[0],Force[1],Force[2]);
end;

// AddRelForceAtPos
//
procedure TGLODEDynamicBehaviour.AddRelForceAtPos(Force, Pos : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtPos(FBody,Force[0],Force[1],Force[2],Pos[0],Pos[1],Pos[2]);
end;

// AddRelForceAtRelPos
//
procedure TGLODEDynamicBehaviour.AddRelForceAtRelPos(Force, Pos : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelForceAtRelPos(FBody,Force[0],Force[1],Force[2],Pos[0],Pos[1],Pos[2]);
end;

// AddTorque
//
procedure TGLODEDynamicBehaviour.AddTorque(Torque : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddTorque(FBody,Torque[0],Torque[1],Torque[2]);
end;

// AddRelTorque
//
procedure TGLODEDynamicBehaviour.AddRelTorque(Torque : TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelTorque(FBody,Torque[0],Torque[1],Torque[2]);
end;


// ------------------------------------------------------------------
// TGLODEStaticBehaviour
// ------------------------------------------------------------------

// Create
//
constructor TGLODEStaticBehaviour.Create(AOwner : TXCollection);
begin
  inherited;
  FElements:=TODEElements.Create(Self);
end;

// Destroy
//
destructor TGLODEStaticBehaviour.Destroy;
begin
  inherited;
  FElements.Free;
end;

// FriendlyName
//
class function TGLODEStaticBehaviour.FriendlyName: String;
begin
  Result:='ODE Static';
end;

// UniqueItem
//
class function TGLODEStaticBehaviour.UniqueItem : Boolean;
begin
  Result:=True;
end;

// Initialize
//
procedure TGLODEStaticBehaviour.Initialize;
begin
  if (not Assigned(Manager)) or (FInitialized) then exit;
  if not Assigned(Manager.Space) then exit;

  FElements.Initialize;
  Manager.RegisterObject(self);

  inherited;
end;

// Deinitialize
//
procedure TGLODEStaticBehaviour.Deinitialize;
begin
  if not FInitialized then exit;
  FElements.Deinitialize;
  if Assigned(FManager) then
    FManager.UnregisterObject(self);

  inherited;
end;

// WriteToFiler
//
procedure TGLODEStaticBehaviour.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    FElements.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TGLODEStaticBehaviour.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    FElements.ReadFromFiler(reader);
  end;
end;

// AddNewElement
//
function TGLODEStaticBehaviour.AddNewElement(AChild:TODEElementClass):TODEBaseElement;
begin
  Result:=nil;
  if not Assigned(Manager) then exit;
  Result:=AChild.Create(FElements);
  FElements.Add(Result);
  Result.Initialize;
end;

// AlignElementsToMatrix
//
procedure TGLODEStaticBehaviour.AlignElementsToMatrix(Mat:TMatrix);
var
  i : Integer;
begin
  if not FInitialized then exit;

  for i:=0 to FElements.Count-1 do
    TODEBaseElement(FElements[i]).AlignGeomElementToMatrix(TODEBaseElement(FElements[i]).AbsoluteMatrix);
end;


// ------------------------------------------------------------------
// TODEElements Methods
// ------------------------------------------------------------------

// Destroy
//
destructor TODEElements.Destroy;
begin
  Deinitialize;
  inherited;
end;

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
  if Owner.Owner is TGLODEBaseObject then
    Mat:=TGLODEBaseObject(Owner.Owner).AbsoluteMatrix;
  if Owner.Owner is TGLODEBaseBehaviour then
    Mat:=TGLODEBaseBehaviour(Owner.Owner).AbsoluteMatrix;
  Result:=MatrixMultiply(Mat,FLocalMatrix);
end;

// AbsolutePosition
function TODEBaseElement.AbsolutePosition: TAffineVector;
begin
  Result:=AffineVectorMake(AbsoluteMatrix[3]);
end;

// AlignGeomElementToMatrix
//
procedure TODEBaseElement.AlignGeomElementToMatrix(Mat: TMatrix);
var
  R : TdMatrix3;
begin
  if not Assigned(FGeomElement) then exit;
  dGeomSetPosition(FGeomElement,Mat[3][0],Mat[3][1],Mat[3][2]);
  R[0]:=Mat[0][0]; R[1]:=Mat[1][0]; R[2]:= Mat[2][0]; R[3]:= 0;
  R[4]:=Mat[0][1]; R[5]:=Mat[1][1]; R[6]:= Mat[2][1]; R[7]:= 0;
  R[8]:=Mat[0][2]; R[9]:=Mat[1][2]; R[10]:=Mat[2][2]; R[11]:=0;
  dGeomSetRotation(FGeomElement,R);
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
  FDynamic:=IsDynamic(Owner.Owner);
  FLocalMatrix:=IdentityHMGMatrix;
end;

// Destroy
//
destructor TODEBaseElement.Destroy;
begin
  if FInitialized then Deinitialize;
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

  if (Owner.Owner is TGLODEBaseObject) then
    Manager:=TGLODEBaseObject(Owner.Owner).Manager;
  if Owner.Owner is TGLODEBaseBehaviour then
    Manager:=TGLODEBaseBehaviour(Owner.Owner).Manager;
  if not Assigned(Manager) then exit;

  if FDynamic then begin
    if (Owner.Owner is TGLODEDummy) then
      Body:=TGLODEDummy(Owner.Owner).Body;
    if Owner.Owner is TGLODEDynamicBehaviour then
      Body:=TGLODEDynamicBehaviour(Owner.Owner).Body;
    if not Assigned(Body) then exit;
  end;

  if not Assigned(Manager.World) then exit;

  if FDynamic then begin
    FGeomTransform:=dCreateGeomTransform(Manager.Space);
    dGeomSetBody(FGeomTransform,Body);
    dGeomTransformSetCleanup(FGeomTransform,0);
    dGeomTransformSetGeom(FGeomTransform,FGeomElement);
    dGeomSetData(FGeomTransform,Owner.Owner);
    AlignGeomElementToMatrix(FLocalMatrix);
  end else begin
    dSpaceAdd(Manager.Space, FGeomElement);
    dGeomSetData(FGeomElement,Owner.Owner);
    AlignGeomElementToMatrix(AbsoluteMatrix);
  end;

  FInitialized:=True;
end;

// Deinitialize
//
procedure TODEBaseElement.Deinitialize;
begin
  if not FInitialized then exit;
  if Assigned(FGeomTransform) then begin
    dGeomDestroy(FGeomTransform);
    FGeomTransform:=nil;
  end;
  if Assigned(FGeomElement) then begin
    dGeomDestroy(FGeomElement);
    FGeomElement:=nil;
  end;
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
    WriteFloat(Density);
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
    Density:=ReadFloat;
  end;
end;

// IsODEInitialized
//
function TODEBaseElement.IsODEInitialized : Boolean;
var
  Manager : TGLODEManager;
begin
  Result:=False;
  Manager:=nil;
  if (Owner.Owner is TGLODEBaseObject) then
    Manager:=TGLODEBaseObject(Owner.Owner).Manager;
  if Owner.Owner is TGLODEBaseBehaviour then
    Manager:=TGLODEBaseBehaviour(Owner.Owner).Manager;
  if not Assigned(Manager) then exit;
  Result:=Assigned(Manager.Space);
end;

// CalculateMass
//
function TODEBaseElement.CalculateMass: TdMass;
var
  R : TdMatrix3;
begin
  R[0]:=FLocalMatrix[0][0]; R[1]:=FLocalMatrix[1][0]; R[2]:= FLocalMatrix[2][0]; R[3]:= 0;
  R[4]:=FLocalMatrix[0][1]; R[5]:=FLocalMatrix[1][1]; R[6]:= FLocalMatrix[2][1]; R[7]:= 0;
  R[8]:=FLocalMatrix[0][2]; R[9]:=FLocalMatrix[1][2]; R[10]:=FLocalMatrix[2][2]; R[11]:=0;
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
procedure TODEBaseElement.SetDensity(const Value: TdReal);
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
  if Initialized then begin
    if FDynamic then begin
      CalculateMass;
      AlignGeomElementToMatrix(FLocalMatrix);
    end else
      AlignGeomElementToMatrix(AbsoluteMatrix);
  end;
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
  if not IsODEInitialized then exit;

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
    WriteFloat(BoxWidth);
    WriteFloat(BoxHeight);
    WriteFloat(BoxDepth);
  end;
end;

// ReadFromFiler
//
procedure TODEElementBox.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    BoxWidth:=ReadFloat;
    BoxHeight:=ReadFloat;
    BoxDepth:=ReadFloat;
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
function TODEElementBox.GetBoxWidth: TdReal;
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
function TODEElementBox.GetBoxHeight: TdReal;
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
function TODEElementBox.GetBoxDepth: TdReal;
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
procedure TODEElementBox.SetBoxWidth(const Value: TdReal);
begin
  FBoxWidth:=Value;
  ODERebuild;
end;

// SetBoxHeight
//
procedure TODEElementBox.SetBoxHeight(const Value: TdReal);
begin
  FBoxHeight:=Value;
  ODERebuild;
end;

// SetBoxDepth
//
procedure TODEElementBox.SetBoxDepth(const Value: TdReal);
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
  if not IsODEInitialized then exit;

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
    WriteFloat(Radius);
  end;
end;

// ReadFromFiler
//
procedure TODEElementSphere.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Radius:=ReadFloat;
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
function TODEElementSphere.GetRadius: TdReal;
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
procedure TODEElementSphere.SetRadius(const Value: TdReal);
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
  if not IsODEInitialized then exit;

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
    WriteFloat(Radius);
    WriteFloat(Length);
  end;
end;

// ReadFromFiler
//
procedure TODEElementCapsule.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Radius:=ReadFloat;
    Length:=ReadFloat;
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
function TODEElementCapsule.GetRadius: TdReal;
var
  rad, len : TdReal;
begin
  if Assigned(FGeomElement) then begin
    dGeomCCylinderGetParams(Geom,rad,len);
    FRadius:=rad;
  end;
  result:=FRadius;
end;

// GetLength
//
function TODEElementCapsule.GetLength: TdReal;
var
  rad, len : TdReal;
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
procedure TODEElementCapsule.SetRadius(const Value: TdReal);
begin
  FRadius:=Value;
  ODERebuild;
end;

// SetLength
//
procedure TODEElementCapsule.SetLength(const Value: TdReal);
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
        glVertex3f(FRadius*sin(2*i*PI/Slices),-FLength/2+FLength*j/(Stacks-1),FRadius*cos(2*i*PI/Slices));
    glEnd;
  end;

  // Middle vertical lines
  glBegin(GL_LINES);
    for i:=0 to (Slices div 2)-1 do begin
      glVertex3f(FRadius*sin(2*i*PI/Slices),-FLength/2,FRadius*cos(2*i*PI/Slices));
      glVertex3f(FRadius*sin(2*i*PI/Slices),FLength/2,FRadius*cos(2*i*PI/Slices));
      glVertex3f(-FRadius*sin(2*i*PI/Slices),-FLength/2,-FRadius*cos(2*i*PI/Slices));
      glVertex3f(-FRadius*sin(2*i*PI/Slices),FLength/2,-FRadius*cos(2*i*PI/Slices));
    end;
  glEnd;

  // Caps
  glPushMatrix;
  for j:=0 to (Slices div 2)-1 do begin
    glBegin(GL_LINES);
      glVertex3f(-FRadius,FLength/2,0);
      glVertex3f(FRadius,FLength/2,0);
      glVertex3f(-FRadius,-FLength/2,0);
      glVertex3f(FRadius,-FLength/2,0);
    glEnd;
    glRotatef(360/Slices,0,1,0);
  end;
  glPopMatrix;

  glPopMatrix;
end;

// Create
//
constructor TODEElementCylinder.Create(AOwner: TXCollection);
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
  if not IsODEInitialized then exit;

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
    WriteFloat(Radius);
    WriteFloat(Length);
  end;
end;

// ReadFromFiler
//
procedure TODEElementCylinder.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Radius:=ReadFloat;
    Length:=ReadFloat;
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
  dMassSetCylinder(FMass,FDensity,3,FRadius,FLength);
  result:=inherited CalculateMass;
end;

// GetRadius
//
function TODEElementCylinder.GetRadius: TdReal;
var
  rad, len : TdReal;
begin
  if Assigned(FGeomElement) then begin
    dGeomCylinderGetParams(Geom,rad,len);
    FRadius:=rad;
  end;
  result:=FRadius;
end;

// GetLength
//
function TODEElementCylinder.GetLength: TdReal;
var
  rad, len : TdReal;
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
procedure TODEElementCylinder.SetRadius(const Value: TdReal);
begin
  FRadius:=Value;
  ODERebuild;
end;

// SetLength
//
procedure TODEElementCylinder.SetLength(const Value: TdReal);
begin
  FLength:=Value;
  ODERebuild;
end;


// ------------------------------------------------------------------
// TODEElementCone
// ------------------------------------------------------------------

// BuildList
//
procedure TODEElementCone.BuildList(var rci : TRenderContextInfo);
var
  i,j,
  //Stacks,
  Slices : integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  //Stacks:=8;
  Slices:=16;

  // Middle horizontal circles
  {for j:=0 to Stacks-1 do begin
    glBegin(GL_LINE_LOOP);
      for i:=0 to Slices-1 do
        glVertex3f(FRadius*sin(2*i*PI/Slices),FRadius*cos(2*i*PI/Slices),-FLength/2+FLength*j/(Stacks-1));
    glEnd;
  end;//}

  // Middle vertical lines
  glBegin(GL_LINES);
    for i:=0 to (Slices div 2)-1 do begin
      glVertex3f(FRadius*sin(2*i*PI/Slices),FRadius*cos(2*i*PI/Slices),-FLength/2);
      glVertex3f(0,0,FLength/2);
      glVertex3f(-FRadius*sin(2*i*PI/Slices),-FRadius*cos(2*i*PI/Slices),-FLength/2);
      glVertex3f(0,0,FLength/2);
    end;
  glEnd;

  // Cap
  glPushMatrix;
  for j:=0 to (Slices div 2)-1 do begin
    glBegin(GL_LINES);
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
constructor TODEElementCone.Create(AOwner: TXCollection);
begin
  inherited;
  FRadius:=0.5;
  FLength:=1;
end;

// Initialize
//
procedure TODEElementCone.Initialize;
begin
  if FInitialized then exit;
  if not IsODEInitialized then exit;

  FGeomElement:=dCreateCone(nil,FRadius,FLength);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementCone.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    WriteFloat(Radius);
    WriteFloat(Length);
  end;
end;

// ReadFromFiler
//
procedure TODEElementCone.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Radius:=ReadFloat;
    Length:=ReadFloat;
  end;
end;

// FriendlyName
//
class function TODEElementCone.FriendlyName : String;
begin
  Result:='Cone';
end;

// FriendlyDescription
//
class function TODEElementCone.FriendlyDescription : String;
begin
  Result:='The ODE cone element implementation';
end;

// ItemCategory
//
class function TODEElementCone.ItemCategory : String;
begin
  Result:='Primitives';
end;

// CalculateMass
//
function TODEElementCone.CalculateMass: TdMass;

  procedure dMassSetCone(var m : TdMass; const density, radius, length : Single);
  var
    ms, Rsqr, Lsqr,
    Ixx, Iyy, Izz : Single;
  begin
    // Calculate Mass
    Rsqr:=radius*radius;
    Lsqr:=length*length;
    ms:=Pi*Rsqr*length*density/3;

    // Calculate Mass Moments of Inertia about the Centroid
    Ixx:=0.15*ms*Rsqr+0.0375*ms*Lsqr;
    Iyy:=0.15*ms*Rsqr+0.0375*ms*Lsqr;
    Izz:=0.3*ms*Rsqr;

    // Set the ODE Mass parameters
    with m do begin
      mass:=ms;
      c[0]:=0; c[1]:=0; c[2]:=0.25*length;
      I[0]:=Ixx; I[1]:=0;   I[2]:=0;    I[4]:=0;
      I[4]:=0;   I[5]:=Iyy; I[6]:=0;    I[7]:=0;
      I[8]:=0;   I[9]:=0;   I[10]:=Izz; I[11]:=0;
    end;
  end;

begin
  dMassSetCone(FMass,FDensity,FRadius,FLength);
  result:=inherited CalculateMass;
end;

// GetRadius
//
function TODEElementCone.GetRadius: TdReal;
var
  rad, len : TdReal;
begin
  if Assigned(FGeomElement) then begin
    dGeomConeGetParams(Geom,rad,len);
    FRadius:=rad;
  end;
  result:=FRadius;
end;

// GetLength
//
function TODEElementCone.GetLength: TdReal;
var
  rad, len : TdReal;
begin
  if Assigned(FGeomElement) then begin
    dGeomConeGetParams(Geom,rad,len);
    FLength:=len;
  end;
  result:=FLength;
end;

// Rebuild
//
procedure TODEElementCone.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomConeSetParams(Geom,FRadius,FLength);
  inherited;
end;

// SetRadius
//
procedure TODEElementCone.SetRadius(const Value: TdReal);
begin
  FRadius:=Value;
  ODERebuild;
end;

// SetLength
//
procedure TODEElementCone.SetLength(const Value: TdReal);
begin
  FLength:=Value;
  ODERebuild;
end;


// ------------------------------------------------------------------
// TODEElementTriMesh
// ------------------------------------------------------------------

// Create
//
constructor TODEElementTriMesh.Create(AOwner : TXCollection);
begin
  inherited;
  FVertices:=TAffineVectorList.Create;
  FIndices:=TIntegerList.Create;
end;

// Destroy
//
destructor TODEElementTriMesh.Destroy;
begin
  FVertices.Free;
  FIndices.Free;
  inherited;
end;

// Initialize
//
procedure TODEElementTriMesh.Initialize;
begin
  if not IsODEInitialized then exit;
  if FInitialized or not ((FVertices.Count>0) and (FIndices.Count>0)) then exit;

  FTriMeshData:=dGeomTriMeshDataCreate;
  dGeomTriMeshDataBuildSingle(FTriMeshData, @FVertices.List[0],
                              3*SizeOf(Single), FVertices.Count,
                              @FIndices.List[0], FIndices.Count,
                              3*SizeOf(Integer));
  FGeomElement:=dCreateTriMesh(nil, FTriMeshData, nil, nil, nil);

  inherited;
end;

// Deinitialize
//
procedure TODEElementTriMesh.Deinitialize;
begin
  if not FInitialized then exit;
  if Assigned(FTriMeshData) then
    dGeomTriMeshDataDestroy(FTriMeshData);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementTriMesh.WriteToFiler(writer : TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
  end;
end;

// ReadFromFiler
//
procedure TODEElementTriMesh.ReadFromFiler(reader : TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
  end;
end;

// FriendlyName
//
class function TODEElementTriMesh.FriendlyName : String;
begin
  Result:='Tri-Mesh';
end;

// FriendlyDescription
//
class function TODEElementTriMesh.FriendlyDescription : String;
begin
  Result:='The ODE tri-mesh element implementation';
end;

// ItemCategory
//
class function TODEElementTriMesh.ItemCategory : String;
begin
  Result:='Meshes';
end;

// CalculateMass
//
function TODEElementTriMesh.CalculateMass: TdMass;
var
  r : Single;
  min, max : TAffineVector;
begin
  if Vertices.Count>0 then begin
    Vertices.GetExtents(min,max);
    r:=MaxFloat(VectorLength(min), VectorLength(Max));
  end else
    r:=1;
  dMassSetSphere(FMass,FDensity,r);
  result:=inherited CalculateMass;
end;

// SetVertices
//
procedure TODEElementTriMesh.SetVertices(const Value : TAffineVectorList);
begin
  FVertices.Assign(Value);
  RefreshTriMeshData;
end;

// SetIndices
//
procedure TODEElementTriMesh.SetIndices(const Value : TIntegerList);
begin
  FIndices.Assign(Value);
  RefreshTriMeshData;
end;

// RefreshTriMeshData
//
procedure TODEElementTriMesh.RefreshTriMeshData;
begin
  if FInitialized then
    Deinitialize;
  Initialize;
end;


// ------------------------------------------------------------------
// TGLODEStaticObject
// ------------------------------------------------------------------

// Deinitialize
//
procedure TGLODEStaticObject.Deinitialize;
begin
  if not FInitialized then exit;
  dGeomDestroy(FGeom);
  inherited;
end;

// SetGeom
//
procedure TGLODEStaticObject.SetGeom(const Value : PdxGeom);
begin
  FGeom:=Value;
end;


// ------------------------------------------------------------------
// TGLODEPlane
// ------------------------------------------------------------------

// AlignODEPlane
//
procedure TGLODEPlane.AlignODEPlane;
var
  Pos, Dir : TVector;
  d : TdReal;
begin
  if Assigned(Geom) then begin
    Dir := AbsoluteDirection;
    Pos := AbsolutePosition;
    d := (Dir[0]*Pos[0]+Dir[1]*Pos[1]+Dir[2]*Pos[2]);
    dGeomPlaneSetParams(Geom,Dir[0],Dir[1],Dir[2],d);
  end;
end;

// Initialize
//
procedure TGLODEPlane.Initialize;
begin
  if FInitialized or not Assigned(Manager) then exit;
  if not Assigned(Manager.Space) then exit;
  FGeom:=dCreatePlane(Manager.Space,0,1,0,0);
  dGeomSetData(FGeom,Self);
  AlignODEPlane;
  inherited;
end;

// NotifyChange
//
procedure TGLODEPlane.NotifyChange(Sender: TObject);
begin
  inherited;
  AlignODEPlane;
end;


// ------------------------------------------------------------------
// TGLODETerrain
// ------------------------------------------------------------------

// Create
//
constructor TGLODETerrain.Create(aOwner: TComponent);
begin
  inherited;
  NumNodesPerSide:=0;
end;

// Destroy
//
destructor TGLODETerrain.Destroy;
begin
  if Assigned(FData) then FreeMem(FData);
  inherited;
end;

// AssignData
//
procedure TGLODETerrain.AssignData(aSource : PdRealHugeArray; aNumNodesPerSide : Integer);
begin
  NumNodesPerSide:=aNumNodesPerSide;
  System.Move(aSource^,FData^,NumNodesPerSide*NumNodesPerSide*SizeOf(TdReal));
end;

// GetData
//
function TGLODETerrain.GetData(index: Integer): TdReal;
begin
  Assert((index>NumNodesPerSide*NumNodesPerSide-1) and (index<0),'Invalid data request.');
  Result:=FData[index];
end;

// GetRaster
//
function TGLODETerrain.GetRaster(x, y: Integer): TdReal;
begin
  Result:=GetData(x+y*NumNodesPerSide);
end;

// Initialize
//
procedure TGLODETerrain.Initialize;
begin
  if FInitialized or (not Assigned(Manager)) then exit;
  if not IsODEInitialized then exit;
  if NumNodesPerSide<=0 then exit;

  FGeom:=dCreateTerrainY(Manager.Space, FData, Length, NumNodesPerSide, 1, 1);

  inherited;
end;

// SetData
//
procedure TGLODETerrain.SetData(index: Integer; const Value: TdReal);
begin
  Assert((index>=0) and (index<NumNodesPerSide*NumNodesPerSide),'Invalid data request.');
  FData[index]:=Value;
end;

// SetLength
//
procedure TGLODETerrain.SetLength(const Value: TdReal);
begin
  if Value<>FLength then begin
    FLength:=Value;
    Reinitialize;
  end;
end;

// SetNumNodesPerSide
//
procedure TGLODETerrain.SetNumNodesPerSide(const Value: Integer);
begin
  if Value<>FNumNodesPerSide then begin
    FNumNodesPerSide:=Value;
    if Assigned(FData) then FreeMem(FData);
    GetMem(FData, FNumNodesPerSide*FNumNodesPerSide*SizeOf(TdReal));
  end;
end;

// SetRaster
//
procedure TGLODETerrain.SetRaster(x, y: Integer; const Value: TdReal);
begin
  SetData(x+y*NumNodesPerSide, Value);
end;


// ------------------------------------------------------------------
// TGLODEStaticDummy
// ------------------------------------------------------------------

// AddNewElement
//
function TGLODEStaticDummy.AddNewElement(AChild: TODEElementClass): TODEBaseElement;
begin
  Result:=AChild.Create(FElements);
  FElements.Add(Result);
  if FInitialized then
    Result.Initialize;
end;

// AlignElementsToMatrix
//
procedure TGLODEStaticDummy.AlignElementsToMatrix(Mat:TMatrix);
var
  i : Integer;
begin
  if not FInitialized then exit;

  for i:=0 to FElements.Count-1 do
    TODEBaseElement(FElements[i]).AlignGeomElementToMatrix(TODEBaseElement(FElements[i]).AbsoluteMatrix);
end;

// BuildList
//
procedure TGLODEStaticDummy.BuildList(var rci: TRenderContextInfo);
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
  rci.GLStates.ResetGLMaterialColors;
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
constructor TGLODEStaticDummy.Create(AOwner: TComponent);
begin
  inherited;
  FElements:=TODEElements.Create(Self);
  FColor:=TGLColor.Create(self);
end;

// Destroy
//
destructor TGLODEStaticDummy.Destroy;
begin
  inherited;
  FElements.Free;
  FColor.Free;
end;

// Initialize
//
procedure TGLODEStaticDummy.Initialize;
begin
  if (not Assigned(Manager)) or (FInitialized) then exit;
  if not Assigned(Manager.Space) then exit;

  FElements.Initialize;
  Manager.RegisterObject(self);

  inherited;
end;

// Deinitialize
//
procedure TGLODEStaticDummy.Deinitialize;
begin
  if not FInitialized then exit;
  FElements.Deinitialize;
  if Assigned(FManager) then
    FManager.UnregisterObject(self);

  inherited;
end;

// DefineProperties
//
procedure TGLODEStaticDummy.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ODEElementsData',
                             ReadElements, WriteElements,
                             (Assigned(FElements) and (FElements.Count>0)));
end;

// WriteElements
//
procedure TGLODEStaticDummy.WriteElements(stream : TStream);
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
procedure TGLODEStaticDummy.ReadElements(stream : TStream);
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

// StructureChanged
//
procedure TGLODEStaticDummy.StructureChanged;
begin
  AlignElementsToMatrix(AbsoluteMatrix);
  inherited;
end;

// NotifyChange
//
procedure TGLODEStaticDummy.NotifyChange(Sender:TObject);
begin
  AlignElementsToMatrix(AbsoluteMatrix);
  inherited;
end;

// SetColor
//
procedure TGLODEStaticDummy.SetColor(const Value: TGLColor);
begin
  FColor.Assign(Value);
  StructureChanged;
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
  FJointID:=0;
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
  Deinitialize;
  FAnchor.Free;
  FAxis.Free;
  FAxis2.Free;
  inherited;
end;

// Initialize
//
procedure TODEBaseJoint.Initialize;
begin
  if not IsODEInitialized then exit;
  Attach(FObject1, FObject2);
  FManager.RegisterJoint(Self);
  FInitialized:=True;
end;

// Deinitialize
//
procedure TODEBaseJoint.Deinitialize;
begin
  if not Initialized then exit;
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
    if Assigned(FObject1) then
      WriteString(FObject1.GetNamePath)
    else WriteString('');
    if Assigned(FObject2) then
      WriteString(FObject2.GetNamePath)
    else WriteString('');
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
    FObject1Name:=ReadString;
    FObject2Name:=ReadString;
  end;
end;

// Loaded
//
procedure TODEBaseJoint.Loaded;
var
  mng : TComponent;
  obj : TGLBaseSceneObject;
begin
  inherited;
  if FManagerName<>'' then begin
    mng:=FindManager(TGLODEManager, FManagerName);
    if Assigned(mng) then
      Manager:=TGLODEManager(mng);
    FManagerName:='';
  end;
  if FObject1Name<>'' then begin
    obj:=GetGLODEObject(FObject1Name);
    if Assigned(obj) then
      Object1:=obj;
    FObject1Name:='';
  end;
  if FObject2Name<>'' then begin
    obj:=GetGLODEObject(FObject2Name);
    if Assigned(obj) then
      Object2:=obj;
    FObject2Name:='';
  end;

  AnchorChange(Self);
  AxisChange(Self);
  Axis2Change(Self);
end;

function TODEBaseJoint.IsODEInitialized : Boolean;
begin
  Result:=False;
  if not Assigned(Manager) then exit;
  Result:=Assigned(Manager.World);
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
procedure TODEBaseJoint.Attach(Obj1, Obj2: TGLBaseSceneObject);
var
  Body1, Body2 : PdxBody;
begin
  if (FJointID=0) or not FInitialized then exit;

  FObject1:=Obj1;
  FObject2:=Obj2;

  Body1:=GetBodyFromGLSceneObject(Obj1);
  Body2:=GetBodyFromGLSceneObject(Obj2);
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
procedure TODEBaseJoint.SetManager(const Value: TGLODEManager);
begin
  if FManager<>Value then begin
    if Assigned(FManager) then
      if not (csDesigning in FManager.ComponentState) then
        Deinitialize;
    FManager:=Value;
    if Assigned(FManager) then
      if not (csDesigning in FManager.ComponentState) then
        Initialize;
  end;
end;

// SetObject1
//
procedure TODEBaseJoint.SetObject1(const Value: TGLBaseSceneObject);
begin
  if FObject1<>Value then begin
    if Assigned(Value) then
      if IsGLODEObject(Value) then
        FObject1:=Value
      else
        FObject1:=nil;
    Attach(FObject1, FObject2);
  end;
end;

// SetObject2
//
procedure TODEBaseJoint.SetObject2(const Value: TGLBaseSceneObject);
begin
  if FObject2<>Value then begin
    if Assigned(Value) then
      if IsGLODEObject(Value) then
        FObject2:=Value
      else
        FObject2:=nil;
    Attach(FObject1, FObject2);
  end;
end;


// ------------------------------------------------------------------
// TODEJointHinge
// ------------------------------------------------------------------

// Initialize
//
procedure TODEJointHinge.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then exit;
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
  if (not IsODEInitialized) or (FInitialized) then exit;
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
  if (not IsODEInitialized) or (FInitialized) then exit;
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
  if (not IsODEInitialized) or (FInitialized) then exit;
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
  if (not IsODEInitialized) or (FInitialized) then exit;
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
  if (not IsODEInitialized) or (FInitialized) then exit;
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

  vGLODEObjectRegister:=TList.Create;

  RegisterXCollectionItemClass(TGLODEDynamicBehaviour);
  RegisterXCollectionItemClass(TGLODEStaticBehaviour);
  RegisterXCollectionItemClass(TODEElementBox);
  RegisterXCollectionItemClass(TODEElementSphere);
  RegisterXCollectionItemClass(TODEElementCapsule);
  RegisterXCollectionItemClass(TODEElementCylinder);
  RegisterXCollectionItemClass(TODEElementCone);
  RegisterXCollectionItemClass(TODEElementTriMesh);

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

  vGLODEObjectRegister.Free;

  UnregisterXCollectionItemClass(TGLODEDynamicBehaviour);
  UnregisterXCollectionItemClass(TGLODEStaticBehaviour);
  UnregisterXCollectionItemClass(TODEElementBox);
  UnregisterXCollectionItemClass(TODEElementSphere);
  UnregisterXCollectionItemClass(TODEElementCapsule);
  UnregisterXCollectionItemClass(TODEElementCylinder);
  UnregisterXCollectionItemClass(TODEElementCone);
  UnregisterXCollectionItemClass(TODEElementTriMesh);

  UnregisterXCollectionItemClass(TODEJointHinge);
  UnregisterXCollectionItemClass(TODEJointBall);
  UnregisterXCollectionItemClass(TODEJointSlider);
  UnregisterXCollectionItemClass(TODEJointFixed);
  UnregisterXCollectionItemClass(TODEJointHinge2);
  UnregisterXCollectionItemClass(TODEJointUniversal);

end.
//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VerletClasses<p>

   Base Verlet modelling/simulation classes.<p>
   This unit is generic, GLScene-specific sub-classes are in GLVerletClasses.<p>

	<b>History : </b><font size=-1><ul>
      <li>24/06/03 - MF - Added force kickbacks for integration with external
                          physics. Needs to be split into force+torque!
      <li>19/06/03 - MF - Added TVerletGlobalConstraint.SatisfyConstraintForEdge
                          and implemented for TVCSphere and TVCCapsule 
      <li>19/06/03 - MF - Added friction to TVCCylinder
      <li>19/06/03 - MF - Added surface normals to all colliders - surface
                          normal is identical to Normalize(Movement)!
      <li>18/06/03 - MF - Moved FrictionRatio to TVerletGlobalFrictionConstraint
      <li>18/06/03 - EG - Updated TVCCapsule
      <li>18/06/03 - MF - Updated TVCFloor to use a normal and a point
      <li>18/06/03 - MF - Added TVCCapsule
      <li>17/06/03 - MF - Added TVFAirResistance
      <li>17/06/03 - MF - Added TVCCube collider
      <li>16/06/03 - MF - Fixed TVFSpring.SetRestlengthToCurrent
      <li>24/07/02 - EG - Added TVCCylinder
      <li>18/07/02 - EG - Improved forces & constraints
      <li>23/06/02 - EG - Stricter encapsulation, fixed some leaks,
                          Various optimizations (+25%)
      <li>21/06/02 - EG - Creation (original code by Mattias Fagerlund)
   </ul>
}
unit VerletClasses;

interface

uses Classes, Geometry, SysUtils, VectorLists;

const
   G_DRAG = 0.005;
   cDEFAULT_CONSTRAINT_FRICTION = 0.6;

type

   TVerletAssembly = class;

   // TVerletNode
   //
   TVerletNode = class
      private
			{ Private Declarations }
         FLocation, FOldLocation : TAffineVector;
         FForce : TAffineVector;
         FOwner : TVerletAssembly;
         FWeight, FInvWeight : Single;
         FRadius : Single;
         FNailedDown : Boolean;
         FFriction: single;

		protected
			{ Protected Declarations }
         procedure SetWeight(const value : Single);

         procedure Updated; virtual;

         function GetSpeed: TAffineVector;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); virtual;
         destructor Destroy; override;

         procedure ApplyFriction(const friction, penetrationDepth : Single;
                                 const surfaceNormal : TAffineVector);
         procedure OldApplyFriction(const friction, penetrationDepth : Single);

         procedure Verlet(const deltaTime, newTime : Double); virtual;

         procedure Initialize; dynamic;

         function DistanceToNode(const node : TVerletNode) : Single;
         function GetMovement : TAffineVector;

         property Owner : TVerletAssembly read FOwner;
         property Location : TAffineVector read FLocation write FLocation;
         property OldLocation : TAffineVector read FOldLocation write FOldLocation;
         property Radius : Single read FRadius write FRadius;
         property Force : TAffineVector read FForce write FForce;
         property NailedDown : Boolean read FNailedDown write FNailedDown;
         property Weight : Single read FWeight write SetWeight;
         property InvWeight : Single read FInvWeight;
         property Speed : TAffineVector read GetSpeed;
         property Friction : single read FFriction write FFriction;
   end;

   TVerletNodeClass = class of TVerletNode;

   // TVerletNodeList
   //
   TVerletNodeList = class(TList)
      private
			{ Private Declarations }
         function GetItems(i : Integer): TVerletNode;
         procedure SetItems(i : Integer; const value : TVerletNode);

      public
			{ Public Declarations }
         property Items[i : Integer] : TVerletNode read GetItems write SetItems; default;
   end;

   // TVerletConstraint
   //
   TVerletConstraint = class
      private
			{ Private Declarations }
         FOwner : TVerletAssembly;
         FEnabled : Boolean;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); virtual;
         destructor Destroy; override;

         procedure SatisfyConstraint(const iteration, maxIterations : Integer); virtual; abstract;

         //: Notifies removal of a node
         procedure RemoveNode(aNode : TVerletNode); virtual; abstract;

         procedure BeforeIterations; virtual;

         property Owner : TVerletAssembly read FOwner;
         property Enabled : Boolean read FEnabled write FEnabled;
   end;

   // TVerletDualConstraint
   //
   TVerletDualConstraint = class (TVerletConstraint)
      private
			{ Private Declarations }
         FNodeA, FNodeB : TVerletNode;

      public
			{ Public Declarations }
         procedure RemoveNode(aNode : TVerletNode); override;

         {: Reference to NodeA. }
         property NodeA : TVerletNode read FNodeA write FNodeA;
         {: Reference to NodeB. }
         property NodeB : TVerletNode read FNodeB write FNodeB;
   end;

   // TVerletGroupConstraint
   //
   TVerletGroupConstraint = class (TVerletConstraint)
      private
			{ Private Declarations }
         FNodes : TVerletNodeList;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); override;
         destructor Destroy; override;

         procedure RemoveNode(aNode : TVerletNode); override;

         property Nodes : TVerletNodeList read FNodes;
   end;

   TVerletEdgeList = class;

   // TVerletEdge
   // Verlet edges simulate rigid collission edges
   TVerletEdge = class
      private
			{ Private Declarations }
         FNodeA: TVerletNode;
         FNodeB: TVerletNode;

      public
			{ Public Declarations }
         property NodeA : TVerletNode read FNodeA write FNodeA;
         property NodeB : TVerletNode read FNodeB write FNodeB;

         constructor Create(aNodeA, aNodeB : TVerletNode);
   end;

   TVerletEdgeList = class(TList)
      private
			{ Private Declarations }
         function GetItems(i: integer): TVerletEdge;
         procedure SetItems(i: integer; const Value: TVerletEdge);

      public
			{ Public Declarations }
         property Items[i : integer] : TVerletEdge read GetItems write SetItems; default;
   end;

   // TVerletGlobalConstraint
   //
   TVerletGlobalConstraint = class (TVerletConstraint)
      private
			{ Private Declarations }
         FKickbackForce: TAffineVector;

      public
			{ Public Declarations }
         procedure RemoveNode(aNode : TVerletNode); override;
         procedure BeforeIterations; override;

         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                        const iteration, maxIterations : Integer); virtual; abstract;
         procedure SatisfyConstraintForEdge(aEdge : TVerletEdge;
                        const iteration, maxIterations : Integer); virtual;

         property KickbackForce : TAffineVector read FKickbackForce write FKickbackForce;

         procedure AddForce(Force : TAffineVector);
         procedure AddForceAt(Force : TAffineVector);
   end;

   // TVerletGlobalFrictionConstraint
   //
   TVerletGlobalFrictionConstraint = class (TVerletGlobalConstraint)
      private
			{ Private Declarations }
         FFrictionRatio: single;

      public
			{ Public Declarations }
         property FrictionRatio : single read FFrictionRatio write FFrictionRatio;

         constructor Create(aOwner : TVerletAssembly); override;
   end;

   // TVerletConstraintList
   //
   TVerletConstraintList = class(TList)
      private
			{ Private Declarations }
         function GetItems(i : Integer): TVerletConstraint;
         procedure SetItems(i : Integer; const Value: TVerletConstraint);

      public
			{ Public Declarations }
         property Items[i : Integer] : TVerletConstraint read GetItems write SetItems; default;
   end;

   // TVerletForce
   //
   {: Generic verlet force. }
   TVerletForce = class
      private
			{ Private Declarations }
         FOwner : TVerletAssembly;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); virtual;
         destructor Destroy; override;

         //: Implementation should add force to force resultant for all relevant nodes
         procedure AddForce; virtual; abstract;

         //: Notifies removal of a node
         procedure RemoveNode(aNode : TVerletNode); virtual; abstract;

         property Owner : TVerletAssembly read FOwner;
   end;

   // TVerletDualForce
   //
   {: A verlet force that applies to two specified nodes. }
   TVerletDualForce = class (TVerletForce)
      private
			{ Private Declarations }
         FNodeA, FNodeB : TVerletNode;

      public
			{ Public Declarations }
         procedure RemoveNode(aNode : TVerletNode); override;

         {: Reference to NodeA. }
         property NodeA : TVerletNode read FNodeA write FNodeA;
         {: Reference to NodeB. }
         property NodeB : TVerletNode read FNodeB write FNodeB;
   end;

   // TVerletGroupForce
   //
   {: A verlet force that applies to a specified group of nodes. }
   TVerletGroupForce = class (TVerletForce)
      private
			{ Private Declarations }
         FNodes : TVerletNodeList;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); override;
         destructor Destroy; override;

         procedure RemoveNode(aNode : TVerletNode); override;

         {: Nodes of the force group, referred, NOT owned. }
         property Nodes : TVerletNodeList read FNodes;
   end;

   // TVerletGlobalForce
   //
   {: A global force (applied to all verlet nodes). }
   TVerletGlobalForce = class (TVerletForce)
      private
			{ Private Declarations }

      public
			{ Public Declarations }
         procedure RemoveNode(aNode : TVerletNode); override;

         procedure AddForce; override;
         procedure AddForceToNode(aNode : TVerletNode); virtual; abstract;
   end;

   // TVerletForceList
   //
   TVerletForceList = class (TList)
      private
			{ Private Declarations }
         function GetItems(i : Integer): TVerletForce;
         procedure SetItems(i : Integer; const Value: TVerletForce);

      public
			{ Public Declarations }
         property Items[i : Integer] : TVerletForce read GetItems write SetItems; default;
   end;

   TVCStick = class;
   TVFSpring = class;

   // TVerletAssembly
   //
   TVerletAssembly = class
      private
			{ Private Declarations }
         FIterations : Integer;
         FNodes : TVerletNodeList;
         FConstraints : TVerletConstraintList;
         FForces : TVerletForceList;
         FMaxDeltaTime, FSimTime : Single;
         FDrag : Single;
         FCurrentDeltaTime: single;
         FInvCurrentDeltaTime : single;
         FSolidEdges: TVerletEdgeList;

		protected
			{ Protected Declarations }
         procedure AccumulateForces(const deltaTime, newTime : Double); virtual;
         procedure Verlet(const deltaTime, newTime : Double); virtual;
         procedure SatisfyConstraints(const deltaTime, newTime : Double); virtual;

         function VerletNodeClass : TVerletNodeClass; virtual;

      public
			{ Public Declarations }
         constructor Create; virtual;
         destructor Destroy; override;

         function AddNode(aNode : TVerletNode) : Integer;
         procedure RemoveNode(aNode : TVerletNode);
         function AddConstraint(aConstraint : TVerletConstraint) : Integer;
         procedure RemoveConstraint(aConstraint : TVerletConstraint);
         function AddForce(aForce : TVerletForce) : Integer;
         procedure RemoveForce(aForce : TVerletForce);
         procedure AddSolidEdge(aNodeA, aNodeB : TVerletNode);

         function CreateOwnedNode(const location : TAffineVector;
                                  const aRadius : Single = 0;
                                  const aWeight : Single=1) : TVerletNode;
         function CreateStick(aNodeA, aNodeB : TVerletNode; Slack : single = 0) : TVCStick;
         function CreateSpring(aNodeA, aNodeB : TVerletNode; Strength,
          Dampening : single; Slack : single=0) : TVFSpring;

         procedure Initialize; dynamic;
         function Progress(const deltaTime, newTime : Double) : Integer; virtual;

         function FirstNode : TVerletNode;
         function LastNode : TVerletNode;

         property Drag : Single read FDrag write FDrag;
         property Iterations : Integer read FIterations write FIterations;
         property Nodes : TVerletNodeList read FNodes;
         property Constraints : TVerletConstraintList read FConstraints;

         property SimTime : Single read FSimTime write FSimTime;
         property MaxDeltaTime : Single read FMaxDeltaTime write FMaxDeltaTime;

         property CurrentDeltaTime : single read FCurrentDeltaTime;

         property SolidEdges : TVerletEdgeList read FSolidEdges write FSolidEdges;
   end;

   // TVFGravity
   //
   TVFGravity = class(TVerletGlobalForce)
      private
			{ Private Declarations }
         FGravity : TAffineVector;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); override;

         procedure AddForceToNode(aNode : TVerletNode); override;

         property Gravity : TAffineVector read FGravity write FGravity;
   end;

   // TVFAirResistance
   //
   TVFAirResistance = class(TVerletGlobalForce)
      private
			{ Private Declarations }
         FDragCoeff: single;
         FWindDirection: TAffineVector;
      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); override;

         procedure AddForceToNode(aNode : TVerletNode); override;

         property DragCoeff : single read FDragCoeff write FDragCoeff;
         property WindDirection : TAffineVector read FWindDirection write FWindDirection;
   end;

   // TVFSpring
   //
   TVFSpring = class (TVerletDualForce)
      private
			{ Private Declarations }
         RestLength : Single;
         Strength : Single;
         Damping : Single;
         Slack : Single;

      public
			{ Public Declarations }
         procedure SetRestLengthToCurrent;
         procedure AddForce; override;
   end;

   // TVCFloor
   //
   {: Floor collision constraint.<p>
      The floor is in the XZ plane with a Y+ normal. Highly un-pretty.}
   TVCFloor = class (TVerletGlobalFrictionConstraint)
      private
			{ Private Declarations }
         FBounceRatio : Single;
         FLocation, FNormal : TAffineVector;
    procedure SetNormal(const Value: TAffineVector);

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                        const iteration, maxIterations : Integer); override;

         property BounceRatio : Single read FBounceRatio write FBounceRatio;

         property Location : TAffineVector read FLocation write FLocation;
         property Normal : TAffineVector read FNormal write SetNormal;

         constructor Create(aOwner : TVerletAssembly); override;
   end;

   // TVCStickBase
   //
   TVCStickBase = class (TVerletDualConstraint)
      private
			{ Private Declarations }

		protected
			{ Protected Declarations }
         procedure StickConstraint(Iteration: Integer; MaxIterations : Integer;
                     RestLength : Single; Node0, Node1 : TVerletNode; Slack : Single);
   end;

   // TVCStick
   //
   {: Stick constraint.<p>
      Imposes a fixed distance between two nodes. }
   TVCStick = class (TVCStickBase)
      private
			{ Private Declarations }
         FSlack : Single;
         FRestLength : Single;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); override;

         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;

         procedure SetRestLengthToCurrent;

         property Slack : Single read FSlack write FSlack;
         property RestLength : Single read FRestLength write FRestLength;
   end;

   // TVCSphere
   //
   {: Sphere collision constraint. }
   TVCSphere = class (TVerletGlobalFrictionConstraint)
      private
			{ Private Declarations }
         FLocation : TAffineVector;
         FRadius  : Single;

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         procedure SatisfyConstraintForEdge(aEdge : TVerletEdge;
                        const iteration, maxIterations : Integer); override;

         property Location : TAffineVector read FLocation write FLocation;
         property Radius : Single read FRadius write FRadius;
   end;

   // TVCCylinder
   //
   {: Cylinder collision constraint.<p>
      The cylinder is considered infinite by this constraint. }
   TVCCylinder = class (TVerletGlobalFrictionConstraint)
      private
			{ Private Declarations }
         FBase, FAxis : TAffineVector;
         FRadius, FRadius2  : Single;

      protected
			{ Protected Declarations }
         procedure SetRadius(const val : Single);

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         {: A base point on the cylinder axis.<p>
            Can theoretically be anywhere, however, to reduce floating point
            precision issues, choose it in the area where collision detection
            will occur. }
         property Base : TAffineVector read FBase write FBase;
         {: Cylinder axis vector.<p>
            Must be normalized. }
         property Axis : TAffineVector read FAxis write FAxis;
         {: Cylinder radius. }
         property Radius : Single read FRadius write SetRadius;
   end;

   // TVCCube
   //
   {: Cube collision constraint. }
   TVCCube = class (TVerletGlobalFrictionConstraint)
      private
			{ Private Declarations }
         FLocation : TAffineVector;
         FHalfSides : TAffineVector;
         FSides: TAffineVector;
         FDirection: TAffineVector;
         procedure SetSides(const Value: TAffineVector);

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         // Broken and very slow!
         procedure SatisfyConstraintForEdge(aEdge : TVerletEdge;
                        const iteration, maxIterations : Integer); override;//}

         property Direction : TAffineVector read FDirection write FDirection;
         property Location : TAffineVector read FLocation write FLocation;
         property Sides : TAffineVector read FSides write SetSides;
   end;

   // TVCCapsule
   //
   {: Capsule collision constraint. }
   TVCCapsule = class (TVerletGlobalFrictionConstraint)
      private
			{ Private Declarations }
         FBase : TAffineVector;
         FAxis : TAffineVector;
         FRadius, FRadius2, FLength, FLengthDiv2 : Single;

      protected
			{ Protected Declarations }
         procedure SetAxis(const val : TAffineVector);
         procedure SetRadius(const val : Single);
         procedure SetLength(const val : Single);

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         procedure SatisfyConstraintForEdge(aEdge : TVerletEdge;
                        const iteration, maxIterations : Integer); override;

         property Base : TAffineVector read FBase write FBase;
         property Axis : TAffineVector read FAxis write SetAxis;
         property Radius : single read FRadius write SetRadius;
         property Length : single read FLength write SetLength;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TVerletNode ------------------
// ------------------

// Create
//
constructor TVerletNode.Create(aOwner : TVerletAssembly);
begin
   inherited Create;
   if Assigned(aOwner) then
      aOwner.AddNode(Self);
   FWeight:=1;
   FInvWeight:=1;
   FRadius:=0;
   FFriction:=1;
end;

// Destroy
//
destructor TVerletNode.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.RemoveNode(Self);
   inherited;
end;

// ApplyFriction
//
{ TODO: Improve the friction calculations

  Friction = - NormalForce * FrictionConstant

  To compute the NormalForce, which is the force acting on the normal of the
  collider, we can use the fact that F = m*a.

  m is the weight of the node, a is the acceleration (retardation) caused by the
  collission.

  Acceleration := - PenetrationDepth / Owner.FCurrentDeltaTime;

  The force with which the node has been "stopped" from penetration
  NormalForce := Weight * Acceleration;

  This force should be applied to stopping the movement.
}
procedure TVerletNode.ApplyFriction(const friction, penetrationDepth : Single;
                                    const surfaceNormal : TAffineVector);
var
   frictionMove, move, moveNormal : TAffineVector;
   realFriction : single;
begin
   if (penetrationDepth>0) then begin
       realFriction := friction*FFriction;
       if realFriction>0 then begin
           VectorSubtract(Location, OldLocation, move);
           moveNormal:=VectorScale(surfaceNormal, VectorDotProduct(move, surfaceNormal));
           frictionMove:=VectorSubtract(move, moveNormal);
           if penetrationDepth>Radius then
              ScaleVector(frictionMove, realFriction)
           else ScaleVector(frictionMove, realFriction*Sqrt(penetrationDepth/Radius));
           VectorAdd(OldLocation, frictionMove, FOldLocation);
       end;
   end;
end;

// OldApplyFriction
//
procedure TVerletNode.OldApplyFriction(const friction, penetrationDepth : Single);
var
   frictionMove, move : TAffineVector;
//   pd : Single;
begin
   VectorSubtract(Location, OldLocation, move);
   VectorScale(move, friction*FFriction, frictionMove);
   //pd:=Abs(penetrationDepth);
   //ScaleVector(frictionMove, friction*pd);
   VectorAdd(OldLocation, frictionMove, FOldLocation);
end;

// DistanceToNode
//
function TVerletNode.DistanceToNode(const node : TVerletNode) : Single;
begin
   Result:=VectorDistance(Location, node.Location);
end;

// GetMovement
//
function TVerletNode.GetMovement : TAffineVector;
begin
   Result:=VectorSubtract(Location, OldLocation);
end;

// Initialize
//
procedure TVerletNode.Initialize;
begin
   FOldLocation:=Location;
end;

// SetWeight
//
procedure TVerletNode.SetWeight(const value : Single);
begin
   FWeight:=value;
   if value<>0 then
      FInvWeight:=1/value
   else FInvWeight:=1;
end;

// Verlet
//
procedure TVerletNode.Verlet(const deltaTime, newTime : Double);
var
   newLocation, temp, move, accel : TAffineVector;
begin
   if NailedDown then begin
      FOldLocation:=Location;
   end else begin
      temp:=Location;
      VectorSubtract(Location, OldLocation, move);
      ScaleVector(move, 1-Owner.Drag*deltaTime);

      VectorAdd(Location, move, newLocation);
      VectorScale(Force, Sqr(deltaTime)*FInvWeight, accel);
      AddVector(newLocation, accel);

      FLocation:=newLocation;
      FOldLocation:=temp;
   end;
end;

// Updated
//
procedure TVerletNode.Updated;
begin
   // nothing here, reserved for subclass use
end;

// ------------------
// ------------------ TVerletNodeList ------------------
// ------------------

// GetItems
//
function TVerletNodeList.GetItems(i : Integer) : TVerletNode;
begin
   Result:=Get(i);
end;

// SetItems
//
procedure TVerletNodeList.SetItems(i : Integer; const value : TVerletNode);
begin
   Put(i, value);
end;

function TVerletNode.GetSpeed: TAffineVector;
begin
  result := VectorScale(VectorSubtract(FLocation, FOldLocation), 1/Owner.CurrentDeltaTime);
end;

// ------------------
// ------------------ TVerletConstraint ------------------
// ------------------

// Create
//
procedure TVerletConstraint.BeforeIterations;
begin
  // NADA!
end;

constructor TVerletConstraint.Create(aOwner : TVerletAssembly);
begin
   inherited Create;
   if Assigned(aOwner) then
      aOwner.AddConstraint(Self);
   FEnabled:=True;
end;

// Destroy
//
destructor TVerletConstraint.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.RemoveConstraint(Self);
   inherited;
end;

// ------------------
// ------------------ TVerletDualConstraint ------------------
// ------------------

// RemoveNode
//
procedure TVerletDualConstraint.RemoveNode(aNode : TVerletNode);
begin
   if FNodeA=aNode then
      FNodeA:=nil;
   if FNodeB=aNode then
      FNodeB:=nil;
end;

// ------------------
// ------------------ TVerletGroupConstraint ------------------
// ------------------

// Create
//
constructor TVerletGroupConstraint.Create(aOwner : TVerletAssembly);
begin
   inherited Create(aOwner);
   FNodes:=TVerletNodeList.Create;
end;

// Destroy
//
destructor TVerletGroupConstraint.Destroy;
begin
   FNodes.Free;
   inherited;
end;

// RemoveNode
//
procedure TVerletGroupConstraint.RemoveNode(aNode : TVerletNode);
begin
   FNodes.Remove(aNode);
end;

// ------------------
// ------------------ TVerletGlobalConstraint ------------------
// ------------------

// RemoveNode
//
procedure TVerletGlobalConstraint.AddForce(Force: TAffineVector);
begin
  AddVector(FKickbackForce, Force);
end;

procedure TVerletGlobalConstraint.AddForceAt(Force: TAffineVector);
begin
  // BAD BAD!
  AddVector(FKickbackForce, Force);
end;

procedure TVerletGlobalConstraint.BeforeIterations;
begin
  inherited;
  FKickbackForce := NullVector;
end;

procedure TVerletGlobalConstraint.RemoveNode(aNode : TVerletNode);
begin
   // nothing to do here
end;

// SatisfyConstraint
//
procedure TVerletGlobalConstraint.SatisfyConstraint(const iteration, maxIterations : Integer);
var
   i : Integer;
   node : TVerletNode;
   list : PPointerList;
begin
   list:=Owner.Nodes.List;
   for i:=0 to Owner.Nodes.Count-1 do begin
      node:=TVerletNode(list[i]);
      if not node.NailedDown then
         SatisfyConstraintForNode(node, iteration, maxIterations);
   end;
   for i:=0 to Owner.SolidEdges.Count-1 do begin
         SatisfyConstraintForEdge(Owner.SolidEdges[i], iteration, maxIterations);
   end;
end;

// SatisfyConstraintForEdge
//
procedure TVerletGlobalConstraint.SatisfyConstraintForEdge(
  aEdge: TVerletEdge; const iteration, maxIterations: Integer);
begin
  // Purely virtual, but can't be abstract...
end;

// ------------------
// ------------------ TVerletGlobalFrictionConstraint ------------------
// ------------------

constructor TVerletGlobalFrictionConstraint.Create(
  aOwner: TVerletAssembly);
begin
  inherited;

  FFrictionRatio:=cDEFAULT_CONSTRAINT_FRICTION;
end;

// ------------------
// ------------------ TVerletConstraintList ------------------
// ------------------

// GetItems
//
function TVerletConstraintList.GetItems(i : Integer) : TVerletConstraint;
begin
   Result:=Get(i);
end;

// SetItems
//
procedure TVerletConstraintList.SetItems(i : Integer;
                                         const value : TVerletConstraint);
begin
   Put(i, value);
end;

// ------------------
// ------------------ TVerletForce ------------------
// ------------------

// Create
//
constructor TVerletForce.Create(aOwner : TVerletAssembly);
begin
   inherited Create;
   if Assigned(aOwner) then
      aOwner.AddForce(Self);
end;

// Destroy
//
destructor TVerletForce.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.RemoveForce(Self);
   inherited;
end;

// ------------------
// ------------------ TVerletGroupForce ------------------
// ------------------

// Create
//
constructor TVerletGroupForce.Create(aOwner : TVerletAssembly);
begin
   inherited Create(aOwner);
   FNodes:=TVerletNodeList.Create;
end;

// Destroy
//
destructor TVerletGroupForce.Destroy;
begin
   FNodes.Free;
   inherited;
end;

// RemoveNode
//
procedure TVerletGroupForce.RemoveNode(aNode : TVerletNode);
begin
   FNodes.Remove(aNode);
end;

// ------------------
// ------------------ TVerletGlobalForce ------------------
// ------------------

// RemoveNode
//
procedure TVerletGlobalForce.RemoveNode(aNode : TVerletNode);
begin
   // nothing to do here
end;

// AddForce
//
procedure TVerletGlobalForce.AddForce;
var
   i : Integer;
   node : TVerletNode;
   list : PPointerList;
begin
   list:=Owner.Nodes.List;
   for i:=0 to Owner.Nodes.Count-1 do begin
      node:=TVerletNode(list[i]);
      if not node.NailedDown then
         AddForceToNode(node);
   end;
end;

// ------------------
// ------------------ TVerletDualForce ------------------
// ------------------

// RemoveNode
//
procedure TVerletDualForce.RemoveNode(aNode : TVerletNode);
begin
   if FNodeA=aNode then
      FNodeA:=nil;
   if FNodeB=aNode then
      FNodeB:=nil;
end;

// ------------------
// ------------------ TVerletForceList ------------------
// ------------------

// GetItems
//
function TVerletForceList.GetItems(i : Integer) : TVerletForce;
begin
   Result:=Get(i);
end;

// SetItems
//
procedure TVerletForceList.SetItems(i : Integer; const value : TVerletForce);
begin
   Put(i, value);
end;

// ------------------
// ------------------ TVerletAssembly ------------------
// ------------------

// Create
//
constructor TVerletAssembly.Create;
begin
   inherited;
   FDrag:=0.01;
   FNodes:=TVerletNodeList.Create;
   FConstraints:=TVerletConstraintList.Create;
   FForces:=TVerletForceList.Create;
   FMaxDeltaTime:=0.02;
   FIterations:=3;
   FSolidEdges := TVerletEdgeList.Create;
end;

// Destroy
//
destructor TVerletAssembly.Destroy;
var
   i : Integer;
begin
   // Delete all nodes
   for i:=0 to FNodes.Count-1 do with FNodes[i] do begin
      FOwner:=nil;
      Free;
   end;
   FreeAndNil(FNodes);
   // Delete all constraints
   for i:=0 to FConstraints.Count-1 do with FConstraints[i] do begin
      FOwner:=nil;
      Free;
   end;
   FreeAndNil(FConstraints);
   // Delete all forces
   for i:=0 to FForces.Count-1 do with FForces[i] do begin
      FOwner:=nil;
      Free;
   end;
   FreeAndNil(FForces);

   for i := 0 to FSolidEdges.Count-1 do
    FSolidEdges[i].Free;
   FreeAndNil(FSolidEdges);

   inherited;
end;

// AccumulateForces
//
procedure TVerletAssembly.AccumulateForces(const deltaTime, newTime : Double);
var
   i : Integer;
begin
   // First of all, reset all forces
   for i:=0 to FNodes.Count-1 do
      FNodes[i].FForce:=NullVector;
   // Now, update all forces in the assembly!
   for i:=0 to FForces.Count-1 do
      FForces[i].AddForce;
end;

// AddNode
//
function TVerletAssembly.AddNode(aNode : TVerletNode) : Integer;
begin
   if Assigned(aNode.FOwner) then
      aNode.Owner.FNodes.Remove(aNode);
   Result:=FNodes.Add(aNode);
   aNode.FOwner:=Self;
end;

// RemoveNode
//
procedure TVerletAssembly.RemoveNode(aNode : TVerletNode);
var
   i : Integer;
begin
   if aNode.Owner=Self then begin
      FNodes.Remove(aNode);
      aNode.FOwner:=nil;
      // drop refs in constraints
      for i:=0 to FConstraints.Count-1 do
         FConstraints[i].RemoveNode(aNode);
      // drop refs in forces
      for i:=0 to FForces.Count-1 do
         FForces[i].RemoveNode(aNode);
   end;
end;

// AddConstraint
//
function TVerletAssembly.AddConstraint(aConstraint : TVerletConstraint) : Integer;
begin
   if Assigned(aConstraint.FOwner) then
      aConstraint.Owner.FConstraints.Remove(aConstraint);
   Result:=FConstraints.Add(aConstraint);
   aConstraint.FOwner:=Self;
end;

// RemoveConstraint
//
procedure TVerletAssembly.RemoveConstraint(aConstraint : TVerletConstraint);
begin
   if aConstraint.Owner=Self then begin
      FConstraints.Remove(aConstraint);
      aConstraint.FOwner:=nil;
   end;
end;

// AddForce
//
function TVerletAssembly.AddForce(aForce : TVerletForce) : Integer;
begin
   if Assigned(aForce.FOwner) then
      aForce.Owner.FForces.Remove(aForce);
   Result:=FForces.Add(aForce);
   aForce.FOwner:=Self;
end;

// RemoveForce
//
procedure TVerletAssembly.RemoveForce(aForce : TVerletForce);
begin
   if aForce.Owner=Self then begin
      FForces.Remove(aForce);
      aForce.FOwner:=nil;
   end;
end;

// AddSolidEdge
//
procedure TVerletAssembly.AddSolidEdge(aNodeA, aNodeB: TVerletNode);
var
  VerletEdge : TVerletEdge;
begin
  VerletEdge := TVerletEdge.Create(aNodeA, aNodeB);
  SolidEdges.Add(VerletEdge);
end;

// FirstNode
//
function TVerletAssembly.FirstNode : TVerletNode;
begin
   Assert(FNodes.Count>0, 'There are no nodes in the assembly!');
   Result:=FNodes[0];
end;

// lastNode
//
function TVerletAssembly.LastNode : TVerletNode;
begin
   Assert(FNodes.Count>0, 'There are no nodes in the assembly!');
   Result:=FNodes[FNodes.Count-1];
end;

// CreateOwnedNode
//
function TVerletAssembly.CreateOwnedNode(const location : TAffineVector;
            const aRadius : Single = 0; const aWeight : Single=1) : TVerletNode;
begin
   Result:=VerletNodeClass.Create(self);
   Result.Location:=Location;
   Result.OldLocation:=Location;
   Result.Weight:=aWeight;
   Result.Radius:=aRadius;
end;

// CreateStick
//
function TVerletAssembly.CreateStick(aNodeA, aNodeB : TVerletNode; Slack : single = 0) : TVCStick;
begin
   Result:=TVCStick.Create(Self);
   Result.NodeA:=aNodeA;
   Result.NodeB:=aNodeB;
   Result.SetRestLengthToCurrent;
   Result.Slack := Slack;
end;

// CreateSpring
//
function TVerletAssembly.CreateSpring(aNodeA, aNodeB: TVerletNode;
  Strength, Dampening: single; Slack : single=0): TVFSpring;
begin
   Result:=TVFSpring.Create(Self);
   Result.NodeA:=aNodeA;
   Result.NodeB:=aNodeB;
   Result.Strength := Strength;
   Result.Damping := Dampening;
   Result.SetRestLengthToCurrent;
   Result.Slack := Slack;
end;


// Initialize
//
procedure TVerletAssembly.Initialize;
var
   i : Integer;
begin
   for i:=0 to FNodes.Count-1 do
      FNodes[i].Initialize;
end;

// Progress
//
function TVerletAssembly.Progress(const deltaTime, newTime : Double) : Integer;
var
   i : Integer;
   ticks : Integer;
   myDeltaTime : Single;
begin
   ticks:=0;
   myDeltaTime:=FMaxDeltaTime;
   FCurrentDeltaTime := FMaxDeltaTime;
   FInvCurrentDeltaTime := 1 / FCurrentDeltaTime;

   while FSimTime<newTime do begin
      Inc(ticks);
      FSimTime:=FSimTime+myDeltaTime;
      Verlet(myDeltaTime, FSimTime);
      AccumulateForces(myDeltaTime, FSimTime);
      SatisfyConstraints(myDeltaTime, FSimTime);

      Break; // Why a break here? Looks like a debug instruction left my mistake!
   end;

   Result:=ticks;

   for i:=0 to FNodes.Count-1 do
      FNodes[i].Updated;
end;

// SatisfyConstraints
//
procedure TVerletAssembly.SatisfyConstraints(const deltaTime, newTime : Double);
var
   i, j : Integer;
begin
   for i:=0 to FConstraints.Count-1 do with FConstraints[i] do
     if Enabled then
        BeforeIterations;//}
   for j:=0 to Iterations-1 do
   begin
      for i:=0 to FConstraints.Count-1 do with FConstraints[i] do
         if Enabled then
            SatisfyConstraint(j, Iterations);//}
   end;
end;

// Verlet
//
procedure TVerletAssembly.Verlet(const deltaTime, newTime: Double);
var
   i : Integer;
begin
   for i:=0 to FNodes.Count-1 do
      FNodes[i].Verlet(deltaTime, newTime);
end;

// VerletNodeClass
//
function TVerletAssembly.VerletNodeClass : TVerletNodeClass;
begin
   // This is the typical verlet node of this assembly, others might use
   // different classes. It's later used when calling CreateOwnedNode - so
   // override this method if that's the only difference between the
   // base CreateOwnedNode and your CreateOwnedNode.
   Result:=TVerletNode;
end;

// ------------------
// ------------------ TVFGravity ------------------
// ------------------

// Create
//
constructor TVFGravity.Create(aOwner : TVerletAssembly);
begin
   inherited;
   FGravity[0]:=0;
   FGravity[1]:=-9.81;
   FGravity[2]:=0;
end;

// AddForceToNode
//
procedure TVFGravity.AddForceToNode(aNode : TVerletNode);
begin
   CombineVector(aNode.FForce, Gravity, @aNode.Weight);
end;

// ------------------
// ------------------ TVFSpring ------------------
// ------------------

// AddForce
//
procedure TVFSpring.AddForce;
var
   hTerm, dTerm : Single;
   deltaP, deltaV, force : TAffineVector;
   deltaLength : Single;
begin
   // Avoid div by zero!
   if VectorEquals(NodeA.Location, NodeB.Location) then begin
      if not NodeA.NailedDown then
         NodeA.FLocation[0]:=NodeA.FLocation[0]+0.01
      else NodeB.FLocation[0]:=NodeB.FLocation[0]+0.01;
   end;

   VectorSubtract(NodeA.Location, NodeB.Location, deltaP);
   deltaLength:=VectorLength(deltaP);

   if deltaLength>Slack then
   begin
     hTerm:=(deltaLength - RestLength) * Strength;
     VectorSubtract(NodeA.GetMovement, NodeB.GetMovement, deltaV);

     dTerm:=VectorDotProduct(deltaV, deltaP) * Damping / deltaLength;
     force:=VectorScale(deltaP, 1/deltaLength);
     ScaleVector(force, -(hTerm+dTerm));

     AddVector(NodeA.FForce, force);
     SubtractVector(NodeB.FForce, force);
   end;
end;

// SetRestLengthToCurrent
//
procedure TVFSpring.SetRestLengthToCurrent;
begin
   RestLength:=VectorDistance(NodeA.Location, NodeB.Location);
end;

// ------------------
// ------------------ TVFAirResistance ------------------
// ------------------

procedure TVFAirResistance.AddForceToNode(aNode: TVerletNode);
var
  s, F : TAffineVector;
  sMag : single;
  r : single;
begin
  // CombineVector(aNode.FForce, Gravity, @aNode.Weight);

  // Fd = DragCoefficient * LiquidDensity * Velocity2 * Area / 2
  s := aNode.Speed;
  sMag := VectorLength(s);

  r := aNode.Radius + 1;

  if sMag<> 0 then
  begin
    F := VectorScale(s, - sqr(sMag) * sqr(r) * pi * FDragCoeff);

    aNode.FForce := VectorAdd(aNode.FForce, F);
  end;
end;

constructor TVFAirResistance.Create(aOwner: TVerletAssembly);
begin
  inherited;

  FDragCoeff := 0.001;
  FWindDirection[0] := 0;
  FWindDirection[1] := 0;
  FWindDirection[2] := 0;
end;

// ------------------
// ------------------ TVCFloor ------------------
// ------------------

// SatisfyConstraintForNode
//

constructor TVCFloor.Create(aOwner: TVerletAssembly);
begin
  inherited;
  MakeVector(FNormal, 0, 1, 0);
  MakeVector(FLocation, 0, 0, 0);
end;

procedure TVCFloor.SatisfyConstraintForNode(aNode : TVerletNode;
                                       const iteration, maxIterations : Integer);
var
   penetrationDepth : Single;
   currentPenetrationDepth : single;
   d : TAffineVector;
   move : TAffineVector;
begin
   //currentPenetrationDepth:=floorLevel-(aNode.Location[1]-aNode.Radius);
   currentPenetrationDepth := -PointPlaneDistance(aNode.Location, FLocation, FNormal)-aNode.Radius;

   // Record how far down the node goes
   penetrationDepth:=currentPenetrationDepth;
   // Correct the node location
   if currentPenetrationDepth>0 then
   begin
      Move := VectorScale(FNormal, currentPenetrationDepth);

      if BounceRatio>0 then
      begin
         d:= VectorSubtract(aNode.FLocation, aNode.FOldLocation);

         //aNode.FLocation[1]:=floorLevel+aNode.Radius;

         if FrictionRatio>0 then
            aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);

         // aNode.FOldLocation[1]:=aNode.FLocation[1]+d*BounceRatio;

         aNode.FOldLocation := VectorAdd(aNode.FLocation, VectorScale(d, BounceRatio));
      end else begin
         //aNode.FLocation[1]:=floorLevel+aNode.Radius;
         AddVector(aNode.FLocation, Move);
         if FrictionRatio>0 then
            aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
      end;
   end;
end;

procedure TVCFloor.SetNormal(const Value: TAffineVector);
begin
  FNormal := Value;
  NormalizeVector(FNormal);
end;

// ------------------
// ------------------ TVCStickBase ------------------
// ------------------

// StickConstraint
//
procedure TVCStickBase.StickConstraint(
   Iteration: Integer; MaxIterations : Integer; RestLength : Single;
   Node0, Node1 : TVerletNode; Slack : Single);
var
   delta : TAffineVector;
   f, r : Single;
   deltaLength, diff : Single;
const
   cDefaultDelta : TAffineVector = (0.01, 0, 0);
begin
   VectorSubtract(Node1.Location, Node0.Location, delta);
   deltaLength:=VectorLength(delta);
   // Avoid div by zero!
   if deltaLength<1e-3 then begin
      delta:=cDefaultDelta;
      deltaLength:=0.01;
   end;

   diff:=(deltaLength-RestLength)/deltaLength;

   if Abs(diff)>Slack then begin
      r:=1/(Node0.InvWeight+Node1.InvWeight);
      if diff<0 then
         diff:=(diff+Slack)*r
      else diff:=(diff-Slack)*r;

      // Take into acount the different weights of the nodes!

      if not Node0.NailedDown then begin
         f:=diff*Node0.InvWeight;
         CombineVector(Node0.FLocation, delta, f);
      end;
      if not Node1.NailedDown then begin
         f:=-diff*Node1.InvWeight;
         CombineVector(Node1.FLocation, delta, f);
      end;
   end;
end;

// ------------------
// ------------------ TVCStick ------------------
// ------------------

// Create
//
constructor TVCStick.Create(aOwner : TVerletAssembly);
begin
   inherited;
end;

// SatisfyConstraint
//
procedure TVCStick.SatisfyConstraint(const iteration, maxIterations : Integer);
begin
  Assert((NodeA<>NodeB), 'The nodes are identical - that causes division by zero!');
  StickConstraint(Iteration, MaxIterations, FRestLength, NodeA, NodeB, FSlack);
end;

// SetRestLengthToCurrent
//
procedure TVCStick.SetRestLengthToCurrent;
begin
   FRestLength:=VectorDistance(NodeA.Location, NodeB.Location);
end;

// ------------------
// ------------------ TVCSphere ------------------
// ------------------


// SatisfyConstraintForEdge
//
procedure TVCSphere.SatisfyConstraintForEdge(aEdge: TVerletEdge;
  const iteration, maxIterations: Integer);
var
  closestPoint, move, delta, contactNormal : TAffineVector;
  deltaLength, diff : single;
begin
  // If the edge penetrates the sphere, try pushing the nodes until it no
  // longer does
  closestPoint := PointSegmentClosestPoint(FLocation, aEdge.NodeA.FLocation, aEdge.NodeB.FLocation);

  // Find the distance between the two
  VectorSubtract(closestPoint, Location, delta);

  deltaLength := VectorLength(delta);

  if deltaLength<Radius then  begin
      if deltaLength>0 then begin
         contactNormal := VectorScale(delta, 1/deltaLength);
         aEdge.NodeA.ApplyFriction(FFrictionRatio, Radius-Abs(DeltaLength), contactNormal);
         aEdge.NodeB.ApplyFriction(FFrictionRatio, Radius-Abs(DeltaLength), contactNormal);
      end;

      // Move it outside the sphere!
      diff:=(Radius-deltaLength)/deltaLength;
      VectorScale(delta, diff, move);

      AddVector(aEdge.NodeA.FLocation, move);
      AddVector(aEdge.NodeB.FLocation, move);

      // Add the force to the kickback
      // F = a * m
      // a = move / deltatime
      AddForce(VectorScale(move, -(aEdge.NodeA.FWeight + aEdge.NodeB.FWeight)  * Owner.FInvCurrentDeltaTime));
  end;
end;

// SatisfyConstraintForNode
//
procedure TVCSphere.SatisfyConstraintForNode(aNode : TVerletNode;
  const iteration, maxIterations : Integer);
var
   delta, move, contactNormal : TAffineVector;
   deltaLength, diff : Single;
begin
   // Find the distance between the two
   VectorSubtract(aNode.Location, Location, delta);

   // Is it inside the sphere?
   deltaLength:=VectorLength(delta)-aNode.Radius;
   if Abs(deltaLength)<Radius then begin
      if deltaLength>0 then begin
         contactNormal := VectorScale(delta, 1/deltaLength);
         aNode.ApplyFriction(FFrictionRatio, Radius-Abs(DeltaLength), contactNormal);
      end
      else
        // Slow it down - this part should not be fired
        aNode.OldApplyFriction(FFrictionRatio, Radius-Abs(DeltaLength));

      // Move it outside the sphere!
      diff:=(Radius-deltaLength)/deltaLength;
      VectorScale(delta, diff, move);

      AddVector(aNode.FLocation, move);

      // Add the force to the kickback
      // F = a * m
      // a = move / deltatime
      AddForce(VectorScale(move, -aNode.FWeight * Owner.FInvCurrentDeltaTime));
   end;
end;

// ------------------
// ------------------ TVCCylinder ------------------
// ------------------

// SetRadius
//
procedure TVCCylinder.SetRadius(const val : Single);
begin
   FRadius:=val;
   FRadius2:=Sqr(val);
end;

// SatisfyConstraintForNode
//
procedure TVCCylinder.SatisfyConstraintForNode(aNode : TVerletNode;
                                    const iteration, maxIterations : Integer);
var
   proj, newLocation, move : TAffineVector;
   f, dist2, penetrationDepth : Single;
begin
   // Compute projection of node position on the axis
   f:=PointProject(aNode.Location, Base, axis);
   proj:=VectorCombine(Base, Axis, 1, f);

   // Sqr distance
   dist2:=VectorDistance2(proj, aNode.Location);
   if dist2<FRadius2 then begin
      // move out of the cylinder
      VectorLerp(proj, aNode.Location, FRadius*RSqrt(dist2), newLocation);

      move := VectorSubtract(aNode.FLocation, newLocation);

      penetrationDepth := VectorLength(Move);

      aNode.ApplyFriction(FFrictionRatio, penetrationDepth, VectorScale(move, 1/penetrationDepth));

      aNode.FLocation := newLocation;
   end;
end;

// ------------------
// ------------------ TVCCube ------------------
// ------------------

// BROKEN AND VERY SLOW!
procedure TVCCube.SatisfyConstraintForEdge(aEdge: TVerletEdge;
  const iteration, maxIterations: Integer);
var
  Corners : array[0..7] of TAffineVector;
  EdgeRelative : array[0..1] of TAffineVector;

  shortestMove, contactNormal : TAffineVector;
  shortestDeltaLength : single;

  procedure AddCorner(CornerID : integer; x,y,z : single);
  begin
    x := (x-0.5)*2;
    y := (y-0.5)*2;
    z := (z-0.5)*2;
    MakeVector(Corners[CornerID], FHalfSides[0]*x, FHalfSides[1]*y, FHalfSides[2]*z);
    AddVector(Corners[CornerID], FLocation);
  end;

  procedure TryEdge(Corner0, Corner1 : integer);
  var
    CubeEdgeClosest, aEdgeClosest : TAffineVector;
    CenteraEdge, move : TAffineVector;
    deltaLength : single;
  begin
    SegmentSegmentClosestPoint(
      Corners[Corner0],
      Corners[Corner1],
      aEdge.NodeA.FLocation,
      aEdge.NodeB.FLocation,
      CubeEdgeClosest,
      aEdgeClosest);

    CenteraEdge := VectorSubtract(aEdgeClosest, FLocation);

    if (abs(CenteraEdge[0])<FHalfSides[0]) and
       (abs(CenteraEdge[1])<FHalfSides[1]) and
       (abs(CenteraEdge[2])<FHalfSides[2]) then
    begin
      // The distance to move the edge is the difference between CenterCubeEdge and
      // CenteraEdge
      move := VectorSubtract(CubeEdgeClosest, aEdgeClosest);

      deltaLength := VectorLength(move);

      if (deltaLength>0) and (deltaLength<shortestDeltaLength) then
      begin
        shortestDeltaLength := deltaLength;
        shortestMove := move;
      end;
    end;
  end;
begin
  // DISABLED!
  exit;

  // Early out test
  EdgeRelative[0] := VectorSubtract(aEdge.FNodeA.FLocation, FLocation);
  EdgeRelative[1] := VectorSubtract(aEdge.FNodeB.FLocation, FLocation);

  // If both edges are on the same side of _any_ box side, the edge can't
  // cut the box
  if ((EdgeRelative[0][0]> FHalfSides[0]) and (EdgeRelative[1][0] >FHalfSides[0])) or
     ((EdgeRelative[0][0]<-FHalfSides[0]) and (EdgeRelative[1][0]<-FHalfSides[0])) or

     ((EdgeRelative[0][1]> FHalfSides[1]) and (EdgeRelative[1][1]> FHalfSides[1])) or
     ((EdgeRelative[0][1]<-FHalfSides[1]) and (EdgeRelative[1][1]<-FHalfSides[1])) or

     ((EdgeRelative[0][2]> FHalfSides[2]) and (EdgeRelative[1][2]> FHalfSides[2])) or
     ((EdgeRelative[0][2]<-FHalfSides[2]) and (EdgeRelative[1][2]<-FHalfSides[2])) then
  begin
    exit;
  end;

  // For each cube edge:
  //   find closest positions between CubeEdge and aEdge
  //   if aEdgeClosestPosition within cube then
  //     move nodes until closest position is outside cube
  //     exit
  AddCorner(0, 0, 0, 0);
  AddCorner(1, 1, 0, 0);
  AddCorner(2, 1, 1, 0);
  AddCorner(3, 0, 1, 0);

  AddCorner(4, 0, 0, 1);
  AddCorner(5, 1, 0, 1);
  AddCorner(6, 1, 1, 1);
  AddCorner(7, 0, 1, 1);

  shortestDeltaLength := 10e30;

  TryEdge(0,1);
  TryEdge(1,2);
  TryEdge(2,3);
  TryEdge(3,0);

  TryEdge(4,5);
  TryEdge(5,6);
  TryEdge(6,7);
  TryEdge(7,4);

  TryEdge(0,3);
  TryEdge(1,5);
  TryEdge(2,6);
  TryEdge(3,7);

  if shortestDeltaLength<10e8 then
  begin
     contactNormal := VectorScale(shortestMove, 1/shortestDeltaLength);

     {aEdge.NodeA.ApplyFriction(FFrictionRatio, shortestDeltaLength, contactNormal);
     aEdge.NodeB.ApplyFriction(FFrictionRatio, shortestDeltaLength, contactNormal);//}

     AddVector(aEdge.NodeA.FLocation, shortestMove);
     AddVector(aEdge.NodeB.FLocation, shortestMove);//}
  end;
end;//*)

procedure TVCCube.SatisfyConstraintForNode(aNode: TVerletNode;
  const iteration, maxIterations: Integer);
var
  P, AbsP, dp, ContactNormal, SideSign : TAffineVector;
  SmallestSide : integer;
  procedure TestSide(Side : integer);
  begin
    if (p[Side])<0 then
    begin
      p[Side]:=-FHalfSides[Side]-p[Side];
      SideSign[Side] := -1;
    end
    else
    begin
      p[Side]:=FHalfSides[Side]-p[Side];
      SideSign[Side] := 1;
    end;
  end;
begin
  // TODO: Direction of Cube should be used to rotate the nodes location, as it
  // stands, the cube can only face in one direction.

  P := VectorSubtract(FLocation, aNode.FLocation);

  if (abs(p[0])>FHalfSides[0]) or (abs(p[1])>FHalfSides[1]) or (abs(p[2])>FHalfSides[2]) then
  begin
    exit;
  end;

  TestSide(0);
  TestSide(1);
  TestSide(2);

  AbsP := p;
  AbsVector(AbsP);

  if AbsP[0]<AbsP[1] then
  begin
    if AbsP[0]<AbsP[2] then
    begin
      // 0 is smallest
      SmallestSide := 0;
    end else
    begin
      // 2 is smallest
      SmallestSide := 2;
    end;
  end else
  begin
    if AbsP[1]<AbsP[2] then
    begin
      // 1 is smallest
      SmallestSide := 1;
    end else
    begin
      // 2 is smallest
      SmallestSide := 2;
    end;
  end;

  // Clear dp
  dp[0] := 0;
  dp[1] := 0;
  dp[2] := 0;

  ContactNormal := dp;

  // Only move along the "shortest" axis
  dp[SmallestSide] := P[SmallestSide];
  ContactNormal[SmallestSide] := SideSign[SmallestSide];

  // Slow it down!
  //aNode.OldApplyFriction(0.05, abs(P[SmallestSide]));
  aNode.ApplyFriction(FFrictionRatio, abs(P[SmallestSide]), ContactNormal);

  SubtractVector(aNode.FLocation, dp);
end;

procedure TVCCube.SetSides(const Value: TAffineVector);
begin
  FSides := Value;
  FHalfSides := VectorScale(Sides, 0.5);
end;

// ------------------
// ------------------ TVCCapsule ------------------
// ------------------

{ TVCCapsule }
// SetAxis
//
procedure TVCCapsule.SetAxis(const val : TAffineVector);
begin
   FAxis:=VectorNormalize(val);
end;

// SetLength
//
procedure TVCCapsule.SetLength(const val : Single);
begin
   FLength:=val;
   FLengthDiv2:=val*0.5;
end;

// SetRadius
//
procedure TVCCapsule.SetRadius(const val : Single);
begin
   FRadius:=val;
   FRadius2:=Sqr(val);
end;

// SatisfyConstraintForNode
//
procedure TVCCapsule.SatisfyConstraintForNode(aNode : TVerletNode;
                                              const iteration, maxIterations : Integer);
var
   p, n2, penetrationDepth  : Single;
   closest, v : TAffineVector;
   newLocation, move : TAffineVector;

begin
   // Find the closest point to location on the capsule axis
   p:=ClampValue(PointProject(aNode.Location, FBase, FAxis),
                 -FLengthDiv2, FLengthDiv2);
   closest:=VectorCombine(FBase, FAxis, 1, p);

   // vector from closest to location
   VectorSubtract(aNode.Location, closest, v);

   // should it be altered?
   n2:=VectorNorm(v);

   if n2<FRadius2 then
   begin
      newLocation := VectorCombine(closest, v, 1, Sqrt(FRadius2/n2));

      // Do friction calculations
      move := VectorSubtract(newLocation,aNode.FLocation);
      penetrationDepth := VectorLength(move);

      //aNode.OldApplyFriction(FFrictionRatio, penetrationDepth);
      aNode.ApplyFriction(FFrictionRatio, penetrationDepth, VectorScale(move, 1/penetrationDepth));

      aNode.FLocation:=newLocation;

      AddForce(VectorScale(move, -aNode.FWeight * Owner.FInvCurrentDeltaTime));
   end;
end;


procedure TVCCapsule.SatisfyConstraintForEdge(aEdge: TVerletEdge;
  const iteration, maxIterations: Integer);
var
   sphereLocation, closestPoint, dummy, delta, move, contactNormal : TAffineVector;
   Ax0, Ax1 : TAffineVector;
   deltaLength, diff, penetrationDepth : Single;
begin
  VectorScale(FAxis, FLengthDiv2, Ax0);
  AddVector(Ax0, FBase);
  VectorScale(FAxis, -FLengthDiv2, Ax1);
  AddVector(Ax1, FBase);

   SegmentSegmentClosestPoint(
    aEdge.NodeA.FLocation,
    aEdge.NodeB.FLocation,
    Ax0,
    Ax1,
    dummy,
    sphereLocation);

  // If the edge penetrates the sphere, try pushing the nodes until it no
  // longer does
  closestPoint := PointSegmentClosestPoint(sphereLocation, aEdge.NodeA.FLocation, aEdge.NodeB.FLocation);

  // Find the distance between the two
  VectorSubtract(closestPoint, sphereLocation, delta);

  deltaLength := VectorLength(delta);

  if deltaLength<Radius then  begin
      // Move it outside the sphere!
      diff:=(Radius-deltaLength)/deltaLength;
      VectorScale(delta, diff, move);

      penetrationDepth := VectorLength(move);
      contactNormal := VectorScale(move, 1/penetrationDepth);
      aEdge.NodeA.ApplyFriction(FFrictionRatio, penetrationDepth, contactNormal);
      aEdge.NodeB.ApplyFriction(FFrictionRatio, penetrationDepth, contactNormal);

      AddVector(aEdge.NodeA.FLocation, move);
      AddVector(aEdge.NodeB.FLocation, move);

      AddForce(VectorScale(move, -(aEdge.NodeA.FWeight + aEdge.NodeB.FWeight)  * Owner.FInvCurrentDeltaTime));
  end;

end;
// ------------------
// ------------------ TVerletEdge ------------------
// ------------------

{ TVerletEdge }

constructor TVerletEdge.Create(aNodeA, aNodeB: TVerletNode);
begin
  FNodeA := aNodeA;
  FNodeB := aNodeB;
end;

// ------------------
// ------------------ TVerletEdgeList ------------------
// ------------------

{ TVerletEdgeList }

function TVerletEdgeList.GetItems(i: integer): TVerletEdge;
begin
  result := Get(i);
end;

procedure TVerletEdgeList.SetItems(i: integer; const Value: TVerletEdge);
begin
  put(i, Value);
end;


end.

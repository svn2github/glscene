//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VerletClasses<p>

   Base Verlet modelling/simulation classes.<p>
   This unit is generic, GLScene-specific sub-classes are in GLVerletClasses.<p>

	<b>History : </b><font size=-1><ul>
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

uses Classes, Geometry, SysUtils;

const
   G_DRAG = 0.005;

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

		protected
			{ Protected Declarations }
         procedure SetWeight(const value : Single);

         procedure Updated; virtual;

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

   // TVerletGlobalConstraint
   //
   TVerletGlobalConstraint = class (TVerletConstraint)
      private
			{ Private Declarations }

      public
			{ Public Declarations }
         procedure RemoveNode(aNode : TVerletNode); override;
         
         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                        const iteration, maxIterations : Integer); virtual; abstract;
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

         function CreateOwnedNode(const location : TAffineVector;
                                  const aRadius : Single = 0;
                                  const aWeight : Single=1) : TVerletNode;
         function CreateStick(aNodeA, aNodeB : TVerletNode) : TVCStick;
         function CreateSpring(aNodeA, aNodeB : TVerletNode; Strength, Dampening : single) : TVFSpring;

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

   // TVFSpring
   //
   TVFSpring = class (TVerletDualForce)
      private
			{ Private Declarations }
         RestLength : Single;
         Strength : Single;
         Damping : Single;

      public
			{ Public Declarations }
         procedure SetRestLengthToCurrent;
         procedure AddForce; override;
   end;

   // TVCFloor
   //
   {: Floor collision constraint.<p>
      The floor is in the XZ plane with a Y+ normal. }
   TVCFloor = class (TVerletGlobalConstraint)
      private
			{ Private Declarations }
         FFloorLevel, FBounceRatio, FFrictionRatio : Single;

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                        const iteration, maxIterations : Integer); override;

         property FloorLevel : Single read FFloorLevel write FFloorLevel;
         property BounceRatio : Single read FBounceRatio write FBounceRatio;
         property FrictionRatio : Single read FFrictionRatio write FFrictionRatio;
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
   TVCSphere = class (TVerletGlobalConstraint)
      private
			{ Private Declarations }
         FLocation : TAffineVector;
         FRadius  : Single;

      public
			{ Public Declarations }
         procedure SatisfyConstraintForNode(aNode : TVerletNode;
                           const iteration, maxIterations : Integer); override;

         property Location : TAffineVector read FLocation write FLocation;
         property Radius : Single read FRadius write FRadius;
   end;

   // TVCCylinder
   //
   {: Cylinder collision constraint.<p>
      The cylinder is considered infinite by this constraint. }
   TVCCylinder = class (TVerletGlobalConstraint)
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
procedure TVerletNode.ApplyFriction(const friction, penetrationDepth : Single;
                                    const surfaceNormal : TAffineVector);
var
   frictionMove, move, moveNormal : TAffineVector;
begin
   VectorSubtract(Location, OldLocation, move);
   moveNormal:=VectorScale(surfaceNormal, VectorDotProduct(move, surfaceNormal));
   frictionMove:=VectorSubtract(move, moveNormal);
   if penetrationDepth>Radius then
      ScaleVector(frictionMove, friction)
   else ScaleVector(frictionMove, friction*Sqrt(penetrationDepth/Radius));
   VectorAdd(OldLocation, frictionMove, FOldLocation);
end;

// OldApplyFriction
//
procedure TVerletNode.OldApplyFriction(const friction, penetrationDepth : Single);
var
   frictionMove, move : TAffineVector;
//   pd : Single;
begin
   VectorSubtract(Location, OldLocation, move);
   VectorScale(move, friction, frictionMove);
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

// ------------------
// ------------------ TVerletConstraint ------------------
// ------------------

// Create
//
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
function TVerletAssembly.CreateStick(aNodeA, aNodeB : TVerletNode) : TVCStick;
begin
   Result:=TVCStick.Create(Self);
   Result.NodeA:=aNodeA;
   Result.NodeB:=aNodeB;
   Result.SetRestLengthToCurrent;
end;

// CreateSpring
//
function TVerletAssembly.CreateSpring(aNodeA, aNodeB: TVerletNode;
  Strength, Dampening: single): TVFSpring;
begin
   Result:=TVFSpring.Create(Self);
   Result.NodeA:=aNodeA;
   Result.NodeB:=aNodeB;
   Result.Strength := Strength;
   Result.Damping := Dampening;
   Result.SetRestLengthToCurrent;
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

   while FSimTime<newTime do begin
      Inc(ticks);
      FSimTime:=FSimTime+myDeltaTime;
      AccumulateForces(myDeltaTime, FSimTime);
      Verlet(myDeltaTime, FSimTime);
      SatisfyConstraints(myDeltaTime, FSimTime);
      Break;
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
   for j:=0 to Iterations-1 do
      for i:=0 to FConstraints.Count-1 do with FConstraints[i] do
         if Enabled then
            SatisfyConstraint(j, Iterations);
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

   hTerm:=(deltaLength - RestLength) * Strength;
   VectorSubtract(NodeA.GetMovement, NodeB.GetMovement, deltaV);

   dTerm:=VectorDotProduct(deltaV, deltaP) * Damping / deltaLength;
   force:=VectorScale(deltaP, 1/deltaLength);
   ScaleVector(force, -(hTerm+dTerm));

   AddVector(NodeA.FForce, force);
   SubtractVector(NodeB.FForce, force);
end;

// SetRestLengthToCurrent
//
procedure TVFSpring.SetRestLengthToCurrent;
begin
   RestLength:=VectorDistance(NodeA.Location, NodeB.Location);
end;

// ------------------
// ------------------ TVCFloor ------------------
// ------------------

// SatisfyConstraintForNode
//
procedure TVCFloor.SatisfyConstraintForNode(aNode : TVerletNode;
                                       const iteration, maxIterations : Integer);
var
   penetrationDepth : Single;
   currentPenetrationDepth, d : Single;
begin
   currentPenetrationDepth:=floorLevel-(aNode.Location[1]-aNode.Radius);
   // Record how far down the node goes
   penetrationDepth:=currentPenetrationDepth;
   // Correct the node location
   if currentPenetrationDepth>0 then begin
      if BounceRatio>0 then begin
         d:=aNode.FLocation[1]-aNode.FOldLocation[1];
         aNode.FLocation[1]:=floorLevel+aNode.Radius;
         if FrictionRatio>0 then
            aNode.ApplyFriction(FrictionRatio, penetrationDepth, YVector);
         aNode.FOldLocation[1]:=aNode.FLocation[1]+d*BounceRatio;
      end else begin
         aNode.FLocation[1]:=floorLevel+aNode.Radius;
         if FrictionRatio>0 then
            aNode.ApplyFriction(FrictionRatio, penetrationDepth, YVector);
      end;
   end;
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

// SatisfyConstraintForNode
//
procedure TVCSphere.SatisfyConstraintForNode(aNode : TVerletNode;
                                    const iteration, maxIterations : Integer);
var
   delta, move : TAffineVector;
   deltaLength, diff : Single;
begin
   // Find the distance between the two
   VectorSubtract(aNode.Location, Location, delta);

   // Is it inside the sphere?
   deltaLength:=VectorLength(delta)-aNode.Radius;
   if Abs(deltaLength)<Radius then begin
      // Slow it down!
      aNode.OldApplyFriction(0.05, Radius-Abs(DeltaLength));

      // Move it outside the sphere!
      diff:=(Radius-deltaLength)/deltaLength;
      VectorScale(delta, diff, move);

      AddVector(aNode.FLocation, move);
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
   proj : TAffineVector;
   f, dist2 : Single;
begin
   // Compute projection of node position on the axis
   VectorSubtract(aNode.Location, Base, proj);
   f:=VectorDotProduct(Axis, proj);
   proj:=VectorCombine(Base, Axis, 1, f);

   // Sqr distance
   dist2:=VectorDistance2(proj, aNode.Location);
   if dist2<FRadius2 then begin
      // move out of the cylinder
      VectorLerp(proj, aNode.Location, FRadius*RSqrt(dist2), aNode.FLocation);
   end;
end;
end.

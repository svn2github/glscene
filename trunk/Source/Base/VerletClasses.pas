{: VerletClasses<p>

   Base Verlet modelling/simulation classes.<p>
   This unit is generic, GLScene-specific sub-classes are in GLVerletClasses.<p>

	<b>History : </b><font size=-1><ul>
      <li>23/06/02 - EG - Stricter encapsulation, fixed some leaks,
                          Various optimizations (+25%) 
      <li>21/06/02 - EG - Creation (original code by Mattias Fagerlund)
   </ul>

   TODO: a "universal" force class
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
         FOwner : TVerletAssembly;
         FLocation, FOldLocation : TAffineVector;
         FForce : TAffineVector;
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

         procedure ApplyFriction(const friction, penetrationDepth : Single);
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
         FNodes : TVerletNodeList;
         FEnabled : Boolean;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); virtual;
         destructor Destroy; override;

         procedure SatisfyConstraint(const iteration, maxIterations : Integer); virtual; abstract;

         property Owner : TVerletAssembly read FOwner;
         property Enabled : Boolean read FEnabled write FEnabled;
         property Nodes : TVerletNodeList read FNodes;
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
   TVerletForce = class
      private
			{ Private Declarations }
         FOwner : TVerletAssembly;
         FNodes : TVerletNodeList;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); virtual;
         destructor Destroy; override;

         procedure AddForce; virtual; abstract;

         property Owner : TVerletAssembly read FOwner;
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

         function CreateOwnedNode(const location : TAffineVector;
                                  const radius : Single = 0;
                                  const weight : Single=1) : TVerletNode; dynamic;
         function AddNode(aNode : TVerletNode) : Integer;
         procedure RemoveNode(aNode : TVerletNode);
         function AddConstraint(aConstraint : TVerletConstraint) : Integer;
         procedure RemoveConstraint(aConstraint : TVerletConstraint);
         function AddForce(aForce : TVerletForce) : Integer;
         procedure RemoveForce(aForce : TVerletForce);

         procedure Initialize; dynamic;
         function Progress(const deltaTime, newTime : Double) : Integer; virtual;

         function FirstNode : TVerletNode;
         function LastNode : TVerletNode;

         property Drag : Single read FDrag write FDrag;
         property Iterations : Integer read FIterations write FIterations;
         property Nodes : TVerletNodeList read FNodes;
         property Constraints : TVerletConstraintList read FConstraints;

         property SimTime : Single read FSimTime write FSimTime;
   end;

   // TVFGravity
   //
   TVFGravity = class(TVerletForce)
      private
			{ Private Declarations }
         FGravity : TAffineVector;

      public
			{ Public Declarations }
         constructor Create(aOwner : TVerletAssembly); override;
         destructor Destroy; override;

         procedure AddForce; override;

         property Gravity : TAffineVector read FGravity write FGravity;
   end;

   // TVFSpring
   //
   TVFSpring = class (TVerletForce)
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
   TVCFloor = class (TVerletConstraint)
      private
			{ Private Declarations }
         FFloorLevel : Single;

      public
			{ Public Declarations }
         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;

         property FloorLevel : Single read FFloorLevel write FFloorLevel; 
   end;

   // TVCStickBase
   //
   TVCStickBase = class(TVerletConstraint)
      private
			{ Private Declarations }

		protected
			{ Protected Declarations }
         procedure StickConstraint(Iteration: Integer; MaxIterations : Integer;
                     RestLength : Single; Node0, Node1 : TVerletNode; Slack : Single);
   end;

   // TVCStick
   //
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
   TVCSphere = class (TVerletConstraint)
      private
			{ Private Declarations }
         FLocation : TAffineVector;
         FRadius  : Single;

      public
			{ Public Declarations }
         procedure SatisfyConstraint(const iteration, maxIterations : Integer); override;

         property Location : TAffineVector read FLocation write FLocation;
         property Radius : Single read FRadius write FRadius;
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
procedure TVerletNode.ApplyFriction(const friction, penetrationDepth : Single);
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
   FNodes:=TVerletNodeList.Create;
   if Assigned(aOwner) then
      aOwner.AddConstraint(Self);
   FEnabled:=true;
end;

// Destroy
//
destructor TVerletConstraint.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.RemoveConstraint(Self);
   FNodes.Free;
   inherited;
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
   FNodes:=TVerletNodeList.Create;
   if Assigned(aOwner) then
      aOwner.AddForce(Self);
end;

// Destroy
//
destructor TVerletForce.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.RemoveForce(Self);
   FNodes.Free;
   inherited;
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

// CreateOwnedNode
//
function TVerletAssembly.CreateOwnedNode(const location : TAffineVector;
            const radius : Single = 0; const weight : Single=1) : TVerletNode;
begin
   Result:=VerletNodeClass.Create(self);
   Result.Location:=Location;
   Result.Weight:=Weight;
   Result.Radius:=Radius;
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
         FConstraints[i].FNodes.Remove(aNode);
      // drop refs in forces
      for i:=0 to FForces.Count-1 do
         FForces[i].FNodes.Remove(aNode);
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
   // DON'T USE OUR LIST OF NODES, USE THE LIST OF THE VerletAssembly!
   FNodes.Free;
   FNodes:=aOwner.FNodes;

   FGravity[0]:=0;
   FGravity[1]:=-9.81;
   FGravity[2]:=0;
end;

// Destroy
//
destructor TVFGravity.Destroy;
begin
   // Nodes aren't ours
   FNodes:=nil;
   inherited;
end;

// AddForce
//
procedure TVFGravity.AddForce;
var
   i : Integer;
begin
   inherited;
   for i:=0 to FNodes.Count-1 do with FNodes[i] do begin
      if not NailedDown then
         CombineVector(FForce, Gravity, @Weight);
   end;
end;

// ------------------
// ------------------ TVFGravity ------------------
// ------------------

// AddForce
//
procedure TVFSpring.AddForce;
var
   hTerm, dTerm : Single;
   deltaP, deltaV, force : TAffineVector;
//   errorLength, diff : Single;
   deltaLength : Single;
   node0, node1 : TVerletNode;
begin
   node0:=FNodes[0];
   node1:=FNodes[1];
   // Avoid div by zero!
   if VectorEquals(node0.Location, node1.Location) then begin
      if not node0.NailedDown then
         node0.FLocation[0]:=Node0.FLocation[0]+0.01
      else node1.FLocation[0]:=Node1.FLocation[0]+0.01;
   end;

   VectorSubtract(node0.Location, node1.Location, deltaP);
   deltaLength:=VectorLength(deltaP);

   //Hterm = (dist - spring->restLen) * spring->Ks; // Ks * (dist - rest)
   hTerm:=(deltaLength - RestLength) * Strength;

   //VectorDifference(&p1->v,&p2->v,&deltaV); // Delta Velocity Vector
   VectorSubtract(node0.GetMovement, node1.GetMovement, deltaV);

   //Dterm = (DotProduct(&deltaV,&deltaP) * spring->Kd) / dist; // Damping Term
   dTerm:=VectorDotProduct(deltaV, deltaP) * Damping / deltaLength;

   //ScaleVector(&deltaP,1.0f / dist, &springForce); // Normalize Distance Vector
   //Force:=VectorNormalize(DeltaP);
   force:=VectorScale(deltaP, 1/deltaLength);

   //ScaleVector(&springForce,-(Hterm + Dterm),&springForce); // Calc Force
   ScaleVector(force, -(hTerm+dTerm));

   //VectorSum(&p1->f,&springForce,&p1->f); // Apply to Particle 1
   AddVector(node0.FForce, force);

   //VectorDifference(&p2->f,&springForce,&p2->f); // - Force on Particle 2
   SubtractVector(Node1.FForce, force);
end;

// SetRestLengthToCurrent
//
procedure TVFSpring.SetRestLengthToCurrent;
begin
   Assert((FNodes.Count=2), 'There must be exactly two VerletNodes per each TVCStick object!');
   RestLength:=VectorDistance(FNodes[1].Location, FNodes[0].Location);
end;

// ------------------
// ------------------ TVCFloor ------------------
// ------------------

// SatisfyConstraint
//
procedure TVCFloor.SatisfyConstraint(const iteration, maxIterations : Integer);
var
   i : Integer;
//   overrun : Single;
//   loc : TAffineVector;
   penetrationDepth : Single;
   currentPenetrationDepth : Single;
   planeNormal : TAffineVector;
//   move : TAffineVector;
   node : TVerletNode;
begin
   SeTAffineVector(planeNormal, 1, 0, 1);

   for i:=0 to FNodes.Count-1 do begin
      node:=FNodes[i];
      if not node.NailedDown then begin
         currentPenetrationDepth:=floorLevel-(node.Location[1]-node.Radius);
         // Record how far down the node goes
         penetrationDepth:=currentPenetrationDepth;
         // Correct the node location
         if currentPenetrationDepth>0 then begin
            node.FLocation[1]:=floorLevel+node.Radius;
            node.ApplyFriction(0.01, penetrationDepth);
         end;
      end;
   end;
end;

// ------------------
// ------------------ TVCStickBase ------------------
// ------------------

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
   // Avoid div by zero!
   if VectorEquals(Node0.Location, Node1.Location) then begin
      delta:=cDefaultDelta;
      deltaLength:=0.01;
   end else begin
      VectorSubtract(Node1.Location, Node0.Location, delta);
      deltaLength:=VectorLength(delta);
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
  Assert((FNodes.Count=2), 'There must be exactly two VerletNodes per each TVCStick object!');
  Assert((FNodes[0]<>FNodes[1]), 'The nodes are identical - that causes division by zero!');

  StickConstraint(Iteration, MaxIterations, FRestLength, FNodes[0], FNodes[1], FSlack);
end;

// SetRestLengthToCurrent
//
procedure TVCStick.SetRestLengthToCurrent;
begin
   Assert((FNodes.Count=2), 'There must be exactly two VerletNodes per each TVCStick object!');
   FRestLength:=VectorDistance(FNodes[1].Location, FNodes[0].Location);
end;

// ------------------
// ------------------ TVCSphere ------------------
// ------------------

// SatisfyConstraint
//
procedure TVCSphere.SatisfyConstraint(const iteration, maxIterations : Integer);
var
   i : Integer;
   node : TVerletNode;
   delta, move : TAffineVector;
   deltaLength, diff : Single;
begin
   inherited;
   for i:=0 to FNodes.Count-1 do begin
      node:=FNodes[i];
      if not node.NailedDown then begin
         // Find the distance between the two
         VectorSubtract(Node.Location, Location, delta);

         // Is it inside the sphere?
         deltaLength:=VectorLength(delta)-Node.Radius;
         if Abs(deltaLength)<Radius then begin
           // Slow it down!
           Node.ApplyFriction(0.05, Radius-Abs(DeltaLength));

           // Move it outside the sphere!
           diff:=(Radius-deltaLength)/deltaLength;
           VectorScale(delta, diff, move);

           AddVector(Node.FLocation, move);
         end;
      end;
   end;
end;

end.

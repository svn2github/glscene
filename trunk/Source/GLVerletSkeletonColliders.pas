{: GLVerletSkeletonColliders<p>

   Skeleton colliders for defining and controlling verlet
   constraints.<p>

   <b>History :</b><font size=-1><ul>
     <li>08/10/03 - SG - Creation.
   </ul></font>
}
unit GLVerletSkeletonColliders;

interface

uses
  Classes, PersistentClasses, VectorGeometry, GLVectorFileObjects,
  VerletClasses;

type
  
  // TSCVerletBase
  //
  {: Base verlet skeleton collider class. }
  TSCVerletBase = class(TSkeletonCollider)
    private
      FVerletConstraint : TVerletConstraint;

    public
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;

      {: Warning: The constriants aren't destroyed by the collider.
         Use AddSCVerletConstriantsToVerletWorld to add the constraints
         to a VerletWorld. The VerletWorld will handle the constraint
         cleanup. }
      property VerletConstraint : TVerletConstraint read FVerletConstraint;
  end;

  // TSCVerletSphere
  //
  {: Sphere shaped verlet constraint in a skeleton collider. }
  TSCVerletSphere = class(TSCVerletBase)
    private
      FRadius : Single;

    protected
      procedure SetRadius(const val : Single);

    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AlignCollider; override;

      property Radius : Single read FRadius write SetRadius;
  end;

  // TSCVerletCapsule
  //
  {: Capsule shaped verlet constraint in a skeleton collider. }
  TSCVerletCapsule = class(TSCVerletBase)
    private
      FRadius,
      FLength : Single;

    protected
      procedure SetRadius(const val : Single);
      procedure SetLength(const val : Single);

    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AlignCollider; override;

      property Radius : Single read FRadius write SetRadius;
      property Length : Single read FLength write SetLength;
  end;

{: After loading call this function to add all the constraints in a
   skeleton collider list to a given verlet world. }
procedure AddSCVerletConstriantsToVerletWorld(
  colliders : TSkeletonColliderList; world : TVerletWorld);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ Global methods ------------------
// ------------------

// AddSCVerletConstriantsToVerletWorld
//
procedure AddSCVerletConstriantsToVerletWorld(
  colliders : TSkeletonColliderList; world : TVerletWorld);
var
  i : Integer;
begin
  for i:=0 to colliders.Count-1 do
    if colliders[i] is TSCVerletBase then
      world.AddConstraint(TSCVerletBase(colliders[i]).VerletConstraint);
end;

// ------------------
// ------------------ TSCVerletBase ------------------
// ------------------

// WriteToFiler
//
procedure TSCVerletBase.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
  end;
end;

// ReadFromFiler
//
procedure TSCVerletBase.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do
    // Nothing yet
  else RaiseFilerException(archiveVersion);
end;


// ------------------
// ------------------ TSCVerletSphere ------------------
// ------------------

// Create
//
constructor TSCVerletSphere.Create;
begin
  inherited;
  FVerletConstraint:=TVCSphere.Create(nil);
  Radius:=0.5;
  AlignCollider;
end;

// WriteToFiler
//
procedure TSCVerletSphere.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
  end;
end;

// ReadFromFiler
//
procedure TSCVerletSphere.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do
    FRadius:=ReadFloat
  else RaiseFilerException(archiveVersion);
end;

// AlignCollider
//
procedure TSCVerletSphere.AlignCollider;
begin
  inherited;
  TVCSphere(FVerletConstraint).Location:=AffineVectorMake(GlobalMatrix[3]);
  TVCSphere(FVerletConstraint).Radius:=Radius;
end;

// SetRadius
//
procedure TSCVerletSphere.SetRadius(const val : Single);
begin
  if val<>FRadius then begin
    FRadius:=val;
    TVCSphere(FVerletConstraint).Radius:=FRadius;
  end;
end;


// ------------------
// ------------------ TSCVerletCapsule ------------------
// ------------------

// Create
//
constructor TSCVerletCapsule.Create;
begin
  inherited;
  FVerletConstraint:=TVCCapsule.Create(nil);
  Radius:=0.5;
  Length:=1;
  AlignCollider;
end;

// WriteToFiler
//
procedure TSCVerletCapsule.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
    WriteFloat(FLength);
  end;
end;

// ReadFromFiler
//
procedure TSCVerletCapsule.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do begin
    FRadius:=ReadFloat;
    FLength:=ReadFloat;
  end else RaiseFilerException(archiveVersion);
end;

// AlignCollider
//
procedure TSCVerletCapsule.AlignCollider;
begin
  inherited;
  TVCCapsule(FVerletConstraint).Location:=AffineVectorMake(GlobalMatrix[3]);
  TVCCapsule(FVerletConstraint).Axis:=AffineVectorMake(GlobalMatrix[1]);
  TVCCapsule(FVerletConstraint).Radius:=Radius;
  TVCCapsule(FVerletConstraint).Length:=Length;
end;

// SetRadius
//
procedure TSCVerletCapsule.SetRadius(const val : Single);
begin
  if val<>FRadius then begin
    FRadius:=val;
    TVCCapsule(FVerletConstraint).Radius:=FRadius;
  end;
end;

// SetLength
//
procedure TSCVerletCapsule.SetLength(const val : Single);
begin
  if val<>FLength then begin
    FLength:=val;
    TVCCapsule(FVerletConstraint).Length:=FLength;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterClasses([TSCVerletBase,TSCVerletSphere,TSCVerletCapsule]);

end.

//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{ 
   Classes and functions that make integration between verlets and glscene
   objects easy.
    
}
unit VXS.VerletClasses;

interface

uses
  VXS.VerletTypes, VXS.VectorGeometry, VXS.Scene, VXS.Objects, VXS.VectorTypes;

type
  // TVXVerletNode
  //
  { Specialized verlet node that can be anchored to a GLScene object. If it's
     anchored and has the property "NailedDown" set, it will remain in the same
     relative position to the GLScene object.}
  TVXVerletNode = class(TVerletNode)
  private
    FRelativePosition: TAffineVector;
    FBaseSceneObject: TVXBaseSceneObject;
    procedure SetBaseSceneObject(const Value: TVXBaseSceneObject);
  protected
    procedure SetLocation(const Value: TAffineVector);override;
  public
    procedure Verlet(const vpt : TVerletProgressTimes); override;

    property VKBaseSceneObject : TVXBaseSceneObject read FBaseSceneObject write SetBaseSceneObject;
    property RelativePosition : TAffineVector read FRelativePosition write FRelativePosition;
  end;

  function CreateVCPlaneFromGLPlane(Plane : TVXPlane; VerletWorld : TVXVerletWorld; Offset : single) : TVCFloor;
//-------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------

function CreateVCPlaneFromGLPlane(Plane : TVXPlane; VerletWorld : TVXVerletWorld; Offset : single) : TVCFloor;
begin
  result := TVCFloor.Create(VerletWorld);
  with result do
  begin
    Normal := VectorNormalize(Plane.Direction.AsAffineVector);

    Location := VectorAdd(Plane.Position.AsAffineVector, VectorScale(Normal, Offset));
  end;
end;

{ TVXVerletNode }

procedure TVXVerletNode.SetBaseSceneObject(
  const Value: TVXBaseSceneObject);
begin
  FBaseSceneObject := Value;

  if Assigned(VKBaseSceneObject) and NailedDown then
    FRelativePosition := AffineVectorMake(VKBaseSceneObject.AbsoluteToLocal(VectorMake(FLocation, 1)));
end;

procedure TVXVerletNode.SetLocation(const Value: TAffineVector);
begin
  inherited;
  if Assigned(VKBaseSceneObject) and NailedDown then
    FRelativePosition := VKBaseSceneObject.AbsoluteToLocal(Value);
end;

procedure TVXVerletNode.Verlet(const vpt : TVerletProgressTimes);
begin
  if Assigned(VKBaseSceneObject) and NailedDown then
  begin
    FLocation := VKBaseSceneObject.LocalToAbsolute(FRelativePosition);
  end else
    inherited;
end;
end.

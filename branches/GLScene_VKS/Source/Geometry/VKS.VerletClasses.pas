//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{ 
   Classes and functions that make integration between verlets and glscene
   objects easy.
    
}
unit VKS.VerletClasses;

interface

uses
  VKS.VerletTypes, VKS.VectorGeometry, VKS.Scene, VKS.Objects, VKS.VectorTypes;

type
  // TVKVerletNode
  //
  { Specialized verlet node that can be anchored to a GLScene object. If it's
     anchored and has the property "NailedDown" set, it will remain in the same
     relative position to the GLScene object.}
  TVKVerletNode = class(TVerletNode)
  private
    FRelativePosition: TAffineVector;
    FGLBaseSceneObject: TVKBaseSceneObject;
    procedure SetGLBaseSceneObject(const Value: TVKBaseSceneObject);
  protected
    procedure SetLocation(const Value: TAffineVector);override;
  public
    procedure Verlet(const vpt : TVerletProgressTimes); override;

    property GLBaseSceneObject : TVKBaseSceneObject read FGLBaseSceneObject write SetGLBaseSceneObject;
    property RelativePosition : TAffineVector read FRelativePosition write FRelativePosition;
  end;

  function CreateVCPlaneFromGLPlane(Plane : TVKPlane; VerletWorld : TVerletWorld; Offset : single) : TVCFloor;
//-------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------

function CreateVCPlaneFromGLPlane(Plane : TVKPlane; VerletWorld : TVerletWorld; Offset : single) : TVCFloor;
begin
  result := TVCFloor.Create(VerletWorld);
  with result do
  begin
    Normal := VectorNormalize(Plane.Direction.AsAffineVector);

    Location := VectorAdd(Plane.Position.AsAffineVector, VectorScale(Normal, Offset));
  end;
end;

{ TVKVerletNode }

procedure TVKVerletNode.SetGLBaseSceneObject(
  const Value: TVKBaseSceneObject);
begin
  FGLBaseSceneObject := Value;

  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := AffineVectorMake(GLBaseSceneObject.AbsoluteToLocal(VectorMake(FLocation, 1)));
end;

procedure TVKVerletNode.SetLocation(const Value: TAffineVector);
begin
  inherited;
  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := GLBaseSceneObject.AbsoluteToLocal(Value);
end;

procedure TVKVerletNode.Verlet(const vpt : TVerletProgressTimes);
begin
  if Assigned(GLBaseSceneObject) and NailedDown then
  begin
    FLocation := GLBaseSceneObject.LocalToAbsolute(FRelativePosition);
  end else
    inherited;
end;
end.

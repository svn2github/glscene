//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{ 
   Classes and functions that make integration between verlets and glscene
   objects easy.
    
}
unit GLX.VerletClasses;

interface

uses
  GLX.VerletTypes, GLX.VectorGeometry, GLX.Scene, GLX.Objects, GLX.VectorTypes;

type
  // TGLVerletNode
  //
  { Specialized verlet node that can be anchored to a GLScene object. If it's
     anchored and has the property "NailedDown" set, it will remain in the same
     relative position to the GLScene object.}
  TGLVerletNode = class(TVerletNode)
  private
    FRelativePosition: TAffineVector;
    FBaseSceneObject: TGLBaseSceneObject;
    procedure SetBaseSceneObject(const Value: TGLBaseSceneObject);
  protected
    procedure SetLocation(const Value: TAffineVector);override;
  public
    procedure Verlet(const vpt : TVerletProgressTimes); override;

    property VKBaseSceneObject : TGLBaseSceneObject read FBaseSceneObject write SetBaseSceneObject;
    property RelativePosition : TAffineVector read FRelativePosition write FRelativePosition;
  end;

  function CreateVCPlaneFromGLPlane(Plane : TGLPlane; VerletWorld : TGLVerletWorld; Offset : single) : TVCFloor;
//-------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------

function CreateVCPlaneFromGLPlane(Plane : TGLPlane; VerletWorld : TGLVerletWorld; Offset : single) : TVCFloor;
begin
  result := TVCFloor.Create(VerletWorld);
  with result do
  begin
    Normal := VectorNormalize(Plane.Direction.AsAffineVector);

    Location := VectorAdd(Plane.Position.AsAffineVector, VectorScale(Normal, Offset));
  end;
end;

{ TGLVerletNode }

procedure TGLVerletNode.SetBaseSceneObject(
  const Value: TGLBaseSceneObject);
begin
  FBaseSceneObject := Value;

  if Assigned(VKBaseSceneObject) and NailedDown then
    FRelativePosition := AffineVectorMake(VKBaseSceneObject.AbsoluteToLocal(VectorMake(FLocation, 1)));
end;

procedure TGLVerletNode.SetLocation(const Value: TAffineVector);
begin
  inherited;
  if Assigned(VKBaseSceneObject) and NailedDown then
    FRelativePosition := VKBaseSceneObject.AbsoluteToLocal(Value);
end;

procedure TGLVerletNode.Verlet(const vpt : TVerletProgressTimes);
begin
  if Assigned(VKBaseSceneObject) and NailedDown then
  begin
    FLocation := VKBaseSceneObject.LocalToAbsolute(FRelativePosition);
  end else
    inherited;
end;
end.

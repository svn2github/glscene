unit GLVerletClasses;

interface

uses
  VerletClasses, VectorGeometry, GLScene, GLObjects;

type
  TGLVerletNode = class(TVerletNode)
  private
    FRelativePosition: TAffineVector;
    FGLBaseSceneObject: TGLBaseSceneObject;
    procedure SetGLBaseSceneObject(const Value: TGLBaseSceneObject);
  protected
    procedure SetLocation(const Value: TAffineVector);override;
  public
    procedure Verlet(const deltaTime, newTime : Double); override;

    property GLBaseSceneObject : TGLBaseSceneObject read FGLBaseSceneObject write SetGLBaseSceneObject;
    property RelativePosition : TAffineVector read FRelativePosition write FRelativePosition;
  end;

  function CreateVCPlaneFromGLPlane(Plane : TGLPlane; VerletWorld : TVerletWorld) : TVCFloor;

implementation

function CreateVCPlaneFromGLPlane(Plane : TGLPlane; VerletWorld : TVerletWorld) : TVCFloor;
begin
  result := TVCFloor.Create(VerletWorld);
  with result do
  begin
    Location := VectorAdd(Plane.Position.AsAffineVector, AffineVectorMake(0,0.1,0));
    Normal := Plane.Direction.AsAffineVector;
  end;
end;

{ TGLVerletNode }

procedure TGLVerletNode.SetGLBaseSceneObject(
  const Value: TGLBaseSceneObject);
begin
  FGLBaseSceneObject := Value;

  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := AffineVectorMake(GLBaseSceneObject.AbsoluteToLocal(VectorMake(FLocation, 1)));
end;

procedure TGLVerletNode.SetLocation(const Value: TAffineVector);
begin
  inherited;
  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := GLBaseSceneObject.AbsoluteToLocal(Value);
end;

procedure TGLVerletNode.Verlet(const deltaTime, newTime: Double);
begin
  if Assigned(GLBaseSceneObject) and NailedDown then
  begin
    FLocation := GLBaseSceneObject.LocalToAbsolute(FRelativePosition);
  end else
    inherited;
end;
end.

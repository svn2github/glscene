unit uSimplePhysics;

interface

uses
  GLScene, Contnrs, GLGeometryBB, GLVectorGeometry, GLVectorTypes;

const
  {user data}
  C_PHYS_BULLET = 0; //Пуля
  C_PHYS_DESTROYABLE = 1; //Что-то разрушаемое пулей с параметром жизнь
  C_PHYS_INVINCIBLE = 2; //Неразрушаемое и неосязаемое
  C_PHYS_ASTEROID = 3; //Астероид

  C_BOUNCE_FORCE = 20;

type

  TdfPhysShape = (psSphere, psBox);

  TdfPhys = class
    Enabled: Boolean;
    Obj: TGLBaseSceneObject;
    Shape: TdfPhysShape;
    Mass: Single;

    BSRadius: Single;
    BB: THmgBoundingBox;

    PrevPos: TAffineVector;

    IsStatic: Boolean; //Объект статический.
    UserType: Integer; //Тип объекта, см. выше
    UserGroup: Integer; //Объекты из одной группы не пересекаются

    ForceVec: TAffineVector;
  end;

  TdfCollisionNotify = procedure(o1, o2: TdfPhys) of object;

  TdfSimplePhysics = class
  private
    FOnCollision: TdfCollisionNotify;

    FPhys: TObjectList;

    tmpPhys1, tmpPhys2: TdfPhys;
    dist: Single;
    movedir, tmpVec: TAffineVector;
    procedure UpdateForce(aObj: TdfPhys; deltaTime: Double);
    procedure PrecheckCollisions(o1, o2: TdfPhys);
    procedure CheckCollisions(o1, o2: TdfPhys);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Update(deltaTime: Double);

    function AddPhysToObject(aObj: TGLBaseSceneObject; aShape: TdfPhysShape): TdfPhys;
    procedure RemovePhys(aPhys: TdfPhys);
    procedure Bounce(aPhys1, aPhys2: TdfPhys);

    property OnCollision: TdfCollisionNotify read FOnCollision write FOnCollision;
  end;

function GetFreeUserGroup(): Integer;

var
  dfPhysics: TdfSimplePhysics;

implementation

var
  iLastIndex: Integer = -128;

function GetFreeUserGroup(): Integer;
begin
  Result := iLastIndex;
  Inc(iLastIndex);
end;

{ TdfSimplePhysics }

function TdfSimplePhysics.AddPhysToObject(aObj: TGLBaseSceneObject;
  aShape: TdfPhysShape): TdfPhys;
var
  aPhys: TdfPhys;
begin
  aPhys := TdfPhys.Create();
  aPhys.Shape := aShape;
  aPhys.Obj := aObj;
  aPhys.Enabled := True;
  aPhys.ForceVec := NullVector;
  aPhys.IsStatic := False;
  case aShape of
    psSphere: aPhys.BSRadius := aObj.BoundingSphereRadius;
    psBox: aPhys.BB := aObj.BoundingBox(False);
  end;
  aPhys.PrevPos := AffineVectorMake(aObj.AbsolutePosition);
  FPhys.Add(aPhys);
  Result := aPhys;
end;

procedure TdfSimplePhysics.Bounce(aPhys1, aPhys2: TdfPhys);
begin
  movedir := VectorSubtract(AffineVectorMake(aPhys1.Obj.AbsolutePosition), aPhys1.PrevPos);
  if VectorLength(movedir) >= 0.01 then
  begin
    NormalizeVector(movedir);
    tmpVec := VectorSubtract(AffineVectorMake(aPhys1.Obj.AbsolutePosition),
       AffineVectorMake(aPhys2.Obj.AbsolutePosition));
    NormalizeVector(tmpVec);
    movedir := VectorReflect(movedir, tmpVec);

    RandomPointOnSphere(tmpVec);
    AddVector(movedir, tmpVec);
    NormalizeVector(movedir);
    {Вот здесь нужно приложить определенную силу по вектору movedir}
    aPhys1.ForceVec := VectorScale(movedir, C_BOUNCE_FORCE);
  end;
end;

procedure TdfSimplePhysics.CheckCollisions(o1, o2: TdfPhys);
begin
  if o1.Shape = psSphere then
  begin
    if o2.Shape = psSphere then
    begin
      //sphere - sphere
      dist := VectorDistance(o1.Obj.AbsolutePosition,
        o2.Obj.AbsolutePosition);
      if dist <= (o1.BSRadius + o2.BSRadius) then
      begin
        FOnCollision(o1, o2);
      end;
    end
    else
    begin
      //sphere - box
    end;

  end
  else if o2.Shape = psSphere then
  begin
    //box-sphere
  end
  else
  begin
    //box-box
  end;
       
end;

constructor TdfSimplePhysics.Create;
begin
  inherited;
  FPhys := TObjectList.Create(True);

  tmpPhys1 := nil;
  tmpPhys2 := nil;
end;

destructor TdfSimplePhysics.Destroy;
begin
  FPhys.Free;
  inherited;
end;

procedure TdfSimplePhysics.PrecheckCollisions(o1, o2: TdfPhys);
begin

end;

procedure TdfSimplePhysics.RemovePhys(aPhys: TdfPhys);
begin

end;

procedure TdfSimplePhysics.Update(deltaTime: Double);
var
  i, j: Integer;
begin
  for i := 0 to FPhys.Count - 2 do
  begin
    tmpPhys1 := TdfPhys(FPhys[i]);
    if tmpPhys1.Enabled and not tmpPhys1.IsStatic then
    begin
      for j := i + 1 to FPhys.Count - 1 do
      begin
        tmpPhys2 := TdfPhys(FPhys[j]);
        if tmpPhys2.Enabled and (tmpPhys1.UserGroup <> tmpPhys2.UserGroup) then
        begin
          // Precheck with boundingspheres?
          CheckCollisions(tmpPhys1, tmpPhys2);
        end;
      end;
      UpdateForce(tmpPhys1, deltaTime);
      tmpPhys1.PrevPos := AffineVectorMake(tmpPhys1.Obj.AbsolutePosition);
    end;
  end;
end;

procedure TdfSimplePhysics.UpdateForce(aObj: TdfPhys; deltaTime: Double);
begin
  with aObj do
  begin
    Obj.Position.AddScaledVector(deltaTime * 5, ForceVec);
    ScaleVector(ForceVec, 0.98);
    if VectorLength(ForceVec) <= 0.001 then
      ForceVec := NullVector;
  end;

end;

end.

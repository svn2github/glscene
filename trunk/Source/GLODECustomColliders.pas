// GLODECustomColliders
{: Custom ODE collider implementations.<p>

   The experimental colliders in this unit don't behave themselves
   as yet. Please be patient, they'll have their time.<p>

   <b>Credits : </b><font size=-1><ul>
     <li>Terrain collider code adapted from Mattias Fagerlund's 
         DelphiODE terrain collision demo. 
         Website: http://www.cambrianlabs.com/Mattias/DelphiODE
     <li>Freeform static collider code by Matheus Degiovani.
  </ul>

  <b>History : </b><font size=-1><ul>
    <li>30/07/03 - SG - Creation.
  </ul>
}
unit GLODECustomColliders;

interface

uses
  Classes, GLODEManager, ODEImport, ODEGL, VectorGeometry, GeometryBB,
  VectorLists, GLTerrainRenderer, GLVectorfileObjects;

type
  //TCustomCollider = function(o1, o2 : PdxGeom; flags : Integer;
  //  contact : PdContactGeom; skip : Integer) : Integer of Object; cdecl;

  TCorner = class
    pos : TdVector3;
    Depth : single;
  end;

  TGLODETerrainCollider = class (TGLODEStaticObject)
    private
      FTerrainRenderer : TGLTerrainRenderer;
      FColliderClass : TdGeomClass;
      FClassNum : integer;
      FCornerList : TList;
      FCornerCache : array[0..7] of TCorner;
      function ColliderFormula(x, y : Single) : Single;
      function ColliderFormulaNormal(x, y : Single) : TAffineVector;
    protected
      procedure Initialize; override;
    public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
    published
      property TerrainRenderer : TGLTerrainRenderer read FTerrainRenderer write FTerrainRenderer;
  end;

  TGLODEFreeFormStaticCollider = class (TGLODEStaticObject)
    private
      FGLFreeForm: TGLFreeForm;
      FColliderClass : TdGeomClass;
      FClassNum : integer;
    protected
      procedure Initialize; override;
    public
      constructor Create(AOwner:TComponent); override;
    published
      property GLFreeForm: TGLFreeForm read FGLFreeForm write FGLFreeForm;
  end;

implementation

// -----------------------------------------------------------------------------
// Private functions used by TGLODETerrainCollider
// -----------------------------------------------------------------------------

// TerrainCollideSphere
//
function TerrainCollideSphere(o1, o2 : PdxGeom; flags : Integer;
                              contact : PdContactGeom;
                              skip : Integer) : Integer; cdecl;

  procedure AddContact(x, y, z : Single; var nb : Integer);
  var
    n : TAffineVector;
    zs : Single;
  begin
    if nb>=flags then Exit;
    zs:=TGLODETerrainCollider(dGeomGetData(o1)).ColliderFormula(x,y);
    if z<zs then begin
      contact.pos[0]:=x;
      contact.pos[1]:=y;
      contact.pos[2]:=zs;
      contact.pos[3]:=1;
      n:=TGLODETerrainCollider(dGeomGetData(o1)).ColliderFormulaNormal(x, y);
      contact.normal[0]:=-n[0];
      contact.normal[1]:=-n[1];
      contact.normal[2]:=-n[2];
      contact.normal[3]:=0;
      contact.depth:=zs-z;
      contact.g1:=o1;
      contact.g2:=o2;
      contact:=PdContactGeom(Integer(contact)+skip);
      Inc(nb);
    end;
  end;

var
  pos : PdVector3;
  r, dx, dy, dz : Single;
  i : Integer;
begin
  Result:=0;

  if not Assigned(dGeomGetData(o1)) then exit;
  if not Assigned(TGLODETerrainCollider(dGeomGetData(o1)).TerrainRenderer) then exit;

  // collide o2 (a sphere) against a formula
  pos:=dGeomGetPosition(o2);
  r:=dGeomSphereGetRadius(o2);

  // collide below sphere center
  AddContact(pos[0], pos[1], pos[2]-r, Result);
  // test corona at 0.4 and 0.8 radius
  for i:=0 to 5 do begin
    SinCos(DegToRad(i*60), r*0.4, dy, dx);
    dz:=r-Sqrt(Sqr(r)-Sqr(dx)-Sqr(dy));
    AddContact(pos[0]+dx,
               pos[1]+dy,
               pos[2]-r+dz, Result);
    SinCos(DegToRad(i*60), r*0.8, dy, dx);
    dz:=r-Sqrt(Sqr(r)-Sqr(dx)-Sqr(dy));
    AddContact(pos[0]+dx,
               pos[1]+dy,
               pos[2]-r+dz, Result);
  end;
end;

// CornerSort
//
function CornerSort(Item1, Item2: Pointer): Integer;
var
  c1, c2 : TCorner;
begin
  c1 := TCorner(Item1);
  c2 := TCorner(Item2);

  if c1.Depth>c2.Depth then
    result := -1
  else if c1.Depth=c2.Depth then
    result := 0
  else
    result := 1;
end;

// TerrainCollideBox
//
function TerrainCollideBox(o1, o2 : PdxGeom; flags : Integer;
                           contact : PdContactGeom;
                           skip : Integer) : Integer; cdecl;

var
   pos : PdVector3;
   Body : PdxBody;
   Sides, dPos : TdVector3;
   CornerList : TList;

   procedure AddContact(x, y, z : Single);
   var
      zs : Single;
      Corner : TCorner;
   begin
      zs:=TGLODETerrainCollider(dGeomGetData(o1)).ColliderFormula(x, y);
      if z<zs then
      begin
        // Pick the next free corner
        Corner := TGLODETerrainCollider(dGeomGetData(o1)).FCornerCache[CornerList.Count];

        Corner.pos[0]:=x;
        Corner.pos[1]:=y;
        Corner.pos[2]:=zs;
        Corner.Depth := zs-z;

        CornerList.Add(Corner);
      end;
   end;

  procedure KeepDeepest(var nb : integer);
  var
    i : integer;
    n : TAffineVector;
    Corner : TCorner;
  begin
    CornerList.Sort(CornerSort);

    for i := 0 to CornerList.Count-1 do
    begin
      if nb>=flags then Exit;

      Corner := CornerList[i];

      contact.pos[0]:=Corner.pos[0];
      contact.pos[1]:=Corner.pos[1];
      contact.pos[2]:=Corner.pos[2];
      //contact.pos[3]:=1;
      n:=TGLODETerrainCollider(dGeomGetData(o1)).ColliderFormulaNormal(Corner.pos[0], Corner.pos[1]);
      contact.normal[0]:=-n[0];
      contact.normal[1]:=-n[1];
      contact.normal[2]:=-n[2];
      //contact.normal[3]:=0;
      contact.depth:=Corner.Depth;
      contact.g1:=o1;
      contact.g2:=o2;
      contact:=PdContactGeom(Integer(contact)+skip);
      Inc(nb);
    end;
  end;

begin
  Result:=0;

  if not Assigned(dGeomGetData(o1)) then exit;
  if not Assigned(TGLODETerrainCollider(dGeomGetData(o1)).TerrainRenderer) then exit;

  CornerList:=TGLODETerrainCollider(dGeomGetData(o1)).FCornerList;

  // Make sure the corner list is empty
  CornerList.Clear;

  // collide o2 (a box) against a formula
  pos:=dGeomGetPosition(o2);
  dGeomBoxGetLengths(o2, Sides);

  Body := dGeomGetBody(o2);

  dGeomBoxGetLengths(o2, Sides);

  dBodyVectorToWorld(Body, sides[0]/2,sides[1]/2,sides[2]/2, dPos);
  AddContact(pos[0]+dpos[0],pos[1]+dpos[1],pos[2]+dpos[2]);
  AddContact(pos[0]-dpos[0],pos[1]-dpos[1],pos[2]-dpos[2]);

  dBodyVectorToWorld(Body, sides[0]/2,sides[1]/2, -sides[2]/2, dPos);
  AddContact(pos[0]+dpos[0],pos[1]+dpos[1],pos[2]+dpos[2]);
  AddContact(pos[0]-dpos[0],pos[1]-dpos[1],pos[2]-dpos[2]);

  dBodyVectorToWorld(Body, sides[0]/2,-sides[1]/2, sides[2]/2, dPos);
  AddContact(pos[0]+dpos[0],pos[1]+dpos[1],pos[2]+dpos[2]);
  AddContact(pos[0]-dpos[0],pos[1]-dpos[1],pos[2]-dpos[2]);

  dBodyVectorToWorld(Body, -sides[0]/2,sides[1]/2, sides[2]/2, dPos);
  AddContact(pos[0]+dpos[0],pos[1]+dpos[1],pos[2]+dpos[2]);
  AddContact(pos[0]-dpos[0],pos[1]-dpos[1],pos[2]-dpos[2]);
  KeepDeepest(Result);
  CornerList.Clear;
end;

// GetTerrainColliderFn
//
function GetTerrainColliderFn(num:Integer):TdColliderFn; cdecl;
begin
  if num = dSphereClass then
    Result:=TerrainCollideSphere
  else if num = dBoxClass then
    Result:=TerrainCollideBox
  else
    Result:=nil;
end;


// -----------------------------------------------------------------------------
// TGLODETerrainCollider
// -----------------------------------------------------------------------------

// Create
//
constructor TGLODETerrainCollider.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);

  FColliderClass.bytes:=0;
  FColliderClass.collider:=GetTerrainColliderFn;
  FColliderClass.aabb:=dInfiniteAABB;
  FColliderClass.aabb_test:=nil;
  FColliderClass.dtor:=nil;

  FCornerList:=TList.Create;
  for i := 0 to 7 do
    FCornerCache[i] := TCorner.Create;

end;

destructor TGLODETerrainCollider.Destroy;
var
  i : integer;
begin
  FCornerList.Free;
  for i := 0 to 7 do
    FCornerCache[i].Free;
  inherited;
end;

// Initialize
//
procedure TGLODETerrainCollider.Initialize;
begin
  if not Assigned(Manager) then exit;
  FClassNum:=dCreateGeomClass(FColliderClass);
  SetGeom(dCreateGeom(FClassNum));
  dGeomSetData(Geom,Self);
  dSpaceAdd(Manager.Space,Geom);
end;

// ColliderFormula
//
function TGLODETerrainCollider.ColliderFormula(x, y : Single) : Single;
var
  Pos : TVector;
begin
  Pos[0] := x;
  Pos[1] := y;
  Result := FTerrainRenderer.InterpolatedHeight(Pos);
end;

// ColliderFormulaNormal
//
function TGLODETerrainCollider.ColliderFormulaNormal(x, y : Single) : TAffineVector;
const
  DELTA = 0.2;
begin
  Result:=CalcPlaneNormal(AffineVectorMake(x, y, ColliderFormula(x, y)),
                          AffineVectorMake(x+DELTA, y, ColliderFormula(x+DELTA, y)),
                          AffineVectorMake(x, y+DELTA, ColliderFormula(x, y+DELTA)))//}
end;


// -----------------------------------------------------------------------------
// Private functions used by TGLODEFreeFormStaticCollider
// -----------------------------------------------------------------------------

function ConvertdAABBtoAABB(aabb: TdAABB): TAABB;
begin
  result.min[0]:= aabb[0];
  result.min[1]:= aabb[1];
  result.min[2]:= aabb[2];
  result.max[0]:= aabb[3];
  result.max[1]:= aabb[4];
  result.max[2]:= aabb[5];
end;

procedure NormalizeAABB(var aabb: TAABB);
var
ext: TAffineVector;
begin
  ext[0]:= (aabb.max[0] - aabb.min[0]) / 2;
  ext[1]:= (aabb.max[1] - aabb.min[1]) / 2;
  ext[2]:= (aabb.max[2] - aabb.min[2]) / 2;
  SetAABB(aabb, vectorMake(ext));
end;

type
  TInfoContact = record
    pos, normal: TAffineVector;
    dist: single;
    i1, i2, i3: Integer; //vertex indices
  end;
  TInfoContactArray = array of TInfoContact;

function PointInsideTriangle(v1, v2, v3, p0, normal: TAffineVector): 
boolean;
var
n1, n2, n3: TAffineVector;
d1, d2, d3: single;
begin
      CalcPlaneNormal(v1, v2, p0, n1);
      CalcPlaneNormal(v2, v3, p0, n2);
      CalcPlaneNormal(v3, v1, p0, n3);

      d1:= VectorDotProduct(n1, n2);
      d2:= VectorDotProduct(n2, n3);
      d3:= VectorDotProduct(n3, n1);
      result:= (d1 > 0) and (d2 > 0) and (d3 > 0);
end;

function DistancePointTriangle(v1, v2, v3, p0: TAffineVector;
  pContato: PAffineVector; dir: PAffineVector = nil): single;
var
  normal, {dir,} p1, p2, p3: TAffineVector;
  iPoint: TVector;
  d1, d2, d3: single;
  gotDir: boolean;
begin
  CalcPlaneNormal(v1, v2, v3, normal);

  if not assigned(dir) then begin
    new(dir);
    gotDir:= true;
    dir^:= VectorNegate(normal);
    scaleVector(dir^, pointPlaneDistance(p0, v1, normal));
    NormalizeVector(dir^);
  end else gotDir:= false;

  RayCastPlaneIntersect(vectorMake(p0), vectorMake(dir^), vectorMake(v1), vectorMake(normal), @iPoint);
  pContato^:= affineVectorMake(iPoint);

  if gotDir then dispose(dir);

  if PointInsideTriangle(v1, v2, v3, pContato^, normal) then begin
    result:= VectorDistance(pContato^, p0);
    exit;
  end;

  p1:= PointSegmentClosestPoint(pContato^, v1, v2);
  p2:= PointSegmentClosestPoint(pContato^, v2, v3);
  p3:= PointSegmentClosestPoint(pContato^, v3, v1);

  d1:= VectorLength(VectorSubtract(pContato^, P1));
  d2:= VectorLength(VectorSubtract(pContato^, P2));
  d3:= VectorLength(VectorSubtract(pContato^, P3));

  if (d1 < d2) and (d1 < d3) then begin
    pContato^:= p1;
    result:= VectorDistance(p1, p0);
  end else if d2 < d3 then begin
    pContato^:= p2;
    result:= VectorDistance(p2, p0);
  end else begin
    pContato^:= p3;
    result:= VectorDistance(p3, p0);
  end;
end;

procedure SortContatos(var arr: TInfoContactArray);
var
  i, j, sel, len: integer;
  aux: TInfoContact;
begin
  len:= length(arr);
  for i:= 0 to len -1 do begin
    sel:= i;
    for j:= i+2 to len -1 do
      if arr[j].dist < arr[sel].dist then sel:= j;

    aux:= arr[i];
    arr[i]:= arr[sel];
    arr[sel]:= aux;
  end;
end;

function CriarContatos(triList: TaffineVectorList; centro: TAffineVector; dir: PAffineVector = nil): TInfoContactArray;
var
i, j: integer;
begin
  i:= 0; j:= 0;
  setLength(result, triList.count div 3);
  while i < triList.count -1 do with result[j] do begin
    dist:= DistancePointTriangle(triList[i], triList[i+1], triList[i+2], centro, @pos, dir);
    normal:= CalcPlaneNormal(triList[i], triList[i+1], triList[i+2]);
    i1:= i; i2:= i+1; i3:= i+2;

    i:= i + 3;
    j:= j + 1;
  end;
end;

function StaticFreeFormCollideSphere(o1, o2 : PdxGeom; flags : Integer;
                                      contact : PdContactGeom;
                                      skip : Integer) : Integer; cdecl;
var
  pos, dir: PdVector3;
  r{, len}: single;
  iPoint, iNormal, vDir, vPos, raioDir: TVector;
  freeForm: TGLFreeForm;
begin
  result:= 0;

  if not Assigned(dGeomGetData(o1)) then exit;
  if not Assigned(TGLODEFreeFormStaticCollider(dGeomGetData(o1)).GLFreeForm) then
    exit;

  // collide o2 (a sphere) against a formula
  pos:=dGeomGetPosition(o2);
  r:=dGeomSphereGetRadius(o2);

  freeForm:= TGLODEFreeFormStaticCollider(dGeomGetData(o1)).GLFreeForm;

  dir:= dBodyGetLinearVel(dGeomGetBody(o2));
  vDir:= ConvertdVector3ToVector4f(dir);
  //len:= VectorLength(vDir);
  NormalizeVector(vdir);

  vPos:= ConvertdVector3ToVector4f(pos);

  if freeForm.OctreeSphereSweepIntersect(vPos, vDir, MinSingle, r, @iPoint, @iNormal) then begin
    NormalizeVector(iNormal);
    negateVector(iNormal);
    //iNormal:= VectorMake(0, -5, 0, 0);
    result:= 1;
    contact.pos:= ConvertVector4fTodVector3(iPoint);
    contact.normal:= ConvertVector4fTodVector3(iNormal);

    raioDir:= VectorSubtract(iPoint, vPos);
    NormalizeVector(raioDir);
    ScaleVector(raioDir, r);
    AddVector(raioDir, vPos);

    contact.depth:= vectorDistance(raioDir, iPoint);
    contact.g1:= o1;
    contact.g2:= o2;
  end;
end;

function StaticFreeFormCollideBox(o1, o2 : PdxGeom; flags : Integer;
                                  contact : PdContactGeom;
                                  skip : Integer) : Integer; cdecl;
var
  freeform: TGLFreeForm;
  dAABB: TdAABB;
  aabb: TAABB;
  mat, invMat: TMatrix;
  tris: TAffineVectorList;
  conts: TInfoContactArray;
  pos: PdVector3;
begin
  result:= 0;

  if not Assigned(dGeomGetData(o1)) then exit;
  if not Assigned(TGLODEFreeFormStaticCollider(dGeomGetData(o1)).GLFreeForm) then 
    exit;

  freeForm:= TGLODEFreeFormStaticCollider(dGeomGetData(o1)).GLFreeForm;

  dGeomGetAABB(o2, dAABB);
  aabb:= ConvertdAABBtoAABB(dAABB);
  mat:= GLMatrixFromGeom(o2);
  invMat:= mat;
  InvertMatrix(invmat);

  NormalizeAABB(aabb);

  tris:= TAffineVectorList.Create;
  if freeform.OctreeAABBIntersect(aabb, mat, invMat, tris) then begin
    result:= 1;

    pos:= dGeomGetPosition(o2);
    conts:= CriarContatos(tris, ConvertdVector3ToVector3f(pos));
    SortContatos(conts);

    contact.pos:= ConvertVector3fTodVector3(conts[0].pos);
    contact.normal:= ConvertVector3fTodVector3(vectorNegate(conts[0].normal));
    contact.depth:= conts[0].dist;
    contact.g1:= o1;
    contact.g2:= o2;
  end;
  tris.free;
end;

function GetFreeFormStaticColliderFn(num:Integer):TdColliderFn; cdecl;
begin
  if num = dSphereClass then result:= StaticFreeFormCollideSphere
  else if num = dBoxClass then result:= StaticFreeFormCollideBox
  else result:= nil;
end;

// -----------------------------------------------------------------------------
// TGLODEFreeFormStaticCollider
// -----------------------------------------------------------------------------

// Create
//
constructor TGLODEFreeFormStaticCollider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FColliderClass.bytes:=0;
  FColliderClass.collider:=GetFreeFormStaticColliderFn;
  FColliderClass.aabb:=dInfiniteAABB;
  FColliderClass.aabb_test:=nil;
  FColliderClass.dtor:=nil;
end;

// Initialize
//
procedure TGLODEFreeFormStaticCollider.Initialize;
begin
  if not Assigned(Manager) then exit;
  FClassNum:=dCreateGeomClass(FColliderClass);
  SetGeom(dCreateGeom(FClassNum));
  dGeomSetData(Geom,Self);
  dSpaceAdd(Manager.Space,Geom);
end;

end.
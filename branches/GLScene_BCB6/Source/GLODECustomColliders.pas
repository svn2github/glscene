// GLODECustomColliders
{: Custom ODE collider implementations.<p>

   <b>Credits : </b><font size=-1><ul>
     <li>Terrain collider code adapted from Mattias Fagerlund's 
         DelphiODE terrain collision demo. 
         Website: http://www.cambrianlabs.com/Mattias/DelphiODE
  </ul>

  <b>History : </b><font size=-1><ul>
    <li>23/04/04 - SG - Removed freeform static collider
    <li>29/10/03 - SG - Fix for GLODETerrainCollider (Matheus Degiovani)
    <li>30/07/03 - SG - Creation.
  </ul>
}
unit GLODECustomColliders;

interface

uses
  Classes, GLODEManager, dynode, dynodegl, VectorGeometry, GeometryBB,
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
  if not Assigned(Manager.Space) then exit;
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
  MakePoint(Pos,x,y,0);
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

end.
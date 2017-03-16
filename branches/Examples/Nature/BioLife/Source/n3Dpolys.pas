{HACER QUE TODO TPOINT APUNTE SIEMPRE
A SUS VERTICES Y QUE DEJE DE HACERLO SI
 UN VERTICE SE DESPRENDE}
 {ELIMINAR CAMPO REFERENCES DE TPOINT}
{3D object handling.  this unit is related to uRickGL
  Tentity:  an entity or object
    tface: a face of an entity, each entity has many faces
  Tvertex: a vertex of a face, each face has many vertices.
   By: Ricardo Sarmiento
       delphiman@hotmail.com }
unit n3Dpolys;

interface

uses classes, SysUtils, gl, glu;
{MWMWMWMWMWMWMWMWMWMW    Classes definition  MWMWMWMWMW}
type
  tface = class;
  TGLpoint = class;
  Ttexture = class;

  Tentity = class(Tobject)
{entity or object or 3D mesh}
    Faces: TList;
  {2D polygons list belonging to the mesh}
    Points: TList;
  {list of all the vertices of the entity}
    R, G, B: Byte;
  {default color to use }
    Position: array[1..3] of single;
  {position of entity in space   X,Y,Z}
    rotation: array[1..3] of single;
  {Orientation, Rx,Ry,Rz}
    id: GLuInt;
  {entity's identIfier, optional}
    Texture: Ttexture;
  {pointer to a texture}
    wireframe: byte;
  {0: use points,
   1:use wireframes,
   2: use solid poligons}
    constructor Create;
  {create a zero-polygon entity}
    destructor Destroy; override;
    procedure Load(st: string);
  {load from proprietary file format}
    procedure Save(st: string);
  {not yet implemented}
    procedure SetColor(iR, iG, iB: Byte);
  {change entity´s color}
    procedure Move(X, Y, Z: single);
  {move entity to new position}
    procedure Rotate(Rx, Ry, Rz: single);
   {turn entity to new orientation}
    procedure Redraw;
  {construct the entity using OpenGL commands}
    procedure LoadDXF(st: string);
  {Read DXF file in ST and generate the mesh}
    procedure CalcNormals;
  {calculate normals for each vertex}
    procedure Center; {find the geometric center of the entity
                         and put it at 0,0,0}
    function AddFace: tface;
  {add a new, empty face to the entity}
    function FindPoint(ix, iy, iz: single): TGLpoint;
              {search for a point near ix,iy,iz}
    function CreateTexture: TTexture;
              {create and assign a texture to the entity}
  end; {Tentity} {useful when rotating entity around it´s center}

  tface = class(Tobject) {a face or polygon of an entity}
    Vertices: Tlist; {the mesh´s vertex list}
    r, g, b: Byte; {face color}
    Owner: Tentity; {points to the owner entity}
    ApplyTexture: boolean; {use texturing or not in this face}
    constructor Create(iOwner: Tentity);
              {create the face and assign its owner entity}
    destructor Destroy; override;
    procedure AddVertex(x, y, z, nx, ny, nz: single);
              {add vertex at X,Y,Z with normal nx,ny,nz}
    procedure Redraw;
              {draw face using OpenGL commands}

  {calculate normal given 3 points of the face,
   normally not called directly}
    procedure CalcNormal(uno, dos, tres: integer;
      var inx, iny, inz: single);
    procedure SetColor(ir, ig, ib: Byte);
   {set the desired color of the face}
  end; {tface}

  TGLpoint = class(Tobject) {a colored point in space}
    x, y, z: single; {position x,y,z}
    r, g, b: Byte; {vertex´s color, rgb}
    References: Byte; {number of vertices using the point
                             for color and position}
    Vertices: Tlist;
    constructor Create(ix, iy, iz: single);
  {set position and References to 0}
    destructor Destroy; override;
    procedure SetColor(ir, ig, ib: Byte);
  {set the desired color of the point}
    procedure SetPosition(ix, iy, iz: single);
  {move the point to Set place}
  end; {TGLpoint}

  Tvertex = class(Tobject) {a vertex of a flat polygon}
    nx, ny, nz: single; {normal vector,
                     each vertex has it´s own normal vector,
                     this is useful for certain tricks}
    point: TGLpoint; {points to position and color data}
    Owner: Tface; {points to the face that owns the vertex}
    Tx, Tz: single; {Texture X and Z coordinates}
    constructor Create(iowner: Tface; inx, iny, inz: single);
              {create vertex with its normal}
    procedure SetColor(ir, ig, ib: Byte);
             {set the desired individual color of the vertex}
    procedure SetPosition(ix, iy, iz: single);
             {move individually the vertex to a dIfferent place}
    procedure MakeCopy;
  {create a new Tpoint instance,
  so that the vertex can
  modIfy individualy the color and position}
  end; {Tvertex}

  Ttexture = class(Tobject) {a texture}
    Automatic: boolean;
  {use or not automatic texture coordinates generation}
    AutoXmult: array[0..3] of GLint;
  {multiply values for X coordinate}
    AutoZmult: array[0..3] of GLint;
  {multiply values for Z coordinate}
    AutoGenModeX: GLint;
  {coordinate calculation algorithm to be used: }
    AutoGenModeZ: GLint;
  {GL_object_linear, GL_Eye_linear or GL_Sphere_map}
    WrapSMode: Glint;
    WrapTMode: Glint;
    MagFilter: Glint;
    MinFilter: Glint;
    EnvironmentMode: GLint;
  {GL_decal, GL_modulate or GL_blend}
    EnvBlendColor: array[0..3] of byte;
  {RGBA color if EnvironmentMode is blend}
    Owner: Tentity; {a texture can be owned by an Entity}
    MyList: GLsizei; {number of the display list for this texture}
    constructor Create(iowner: Tentity); {set defaults for all fields}
    destructor destroy; override; {destroy the display list}
    function LoadTexture(st: string): shortint;
  {load a new texture file,
  return 0 if ok,
  negative number if error}
    procedure Redraw;
  {call the display list,
  it´s not really a redraw, it´s part of one}
  end; {Ttexture}

{global constants}
{const
  MinimalDistance = 0.0001;}
    {If two points are this far from each other,
      they can be considered to be in the same position}

var {global variables}
MinimalDistance:Double;
  //  these two variables are used in this unit
  //and in unit UrickGL:
  PutNames: boolean;
  {If true put names to vertex
  and entity primitives when rendering}
  ActualVertexNumber: LongInt;
  {used to mark every vertex
  with a dIfferent name in every entity}

implementation
{MWMWMWMWMW  IMPLEMENTATION of the CLASSES  MWMWMWMWMWMW}

constructor Tentity.create;
begin
  inherited create;
  id := 0;
   MinimalDistance := 0.0001;
  {initiallly this value has no importance}
  Faces := Tlist.create;
  Points := Tlist.create;
  SetColor(128, 128, 128); {use a medium grey color}
  wireframe := 2; {by default use solid polygons for rendering}
end;

destructor Tentity.destroy;
begin
  Points.Free;
  Faces.Free;
  inherited Destroy;
end;

procedure Tentity.Load(st: string);
var
  f: file;
  numFaces,
    NumPoints,
    NumTextures,
    i, j: LongInt;
  IdData: array[0..3] of char;
  Reserved: array[1..20] of Byte;
  ix, iy, iz: single;
  inx, iny, inz: single;
  ir, ig, ib: Byte;
  Point: TGLpoint;
  Face: Tface;
  Vertex: Tvertex;
  PointNum: longint;
  numVertices: byte;
  {this limits the vertex count for each polygon
   to less than 255 vertices, more than enough}
  Version,
    SubVersion: byte;
begin
  assignFile(f, st);
  if not FileExists(st) then
    exit;
  Reset(f, 1);
  BlockRead(f, IdData, sizeof(IdData));
  BlockRead(f, Version, sizeof(version));
  BlockRead(f, SubVersion, sizeof(SubVersion));
  if version = 1 then
  begin
    {first clear old data stored in object}
    Faces.Clear;
    Points.Clear;
    {then, begin to read new data}
    BlockRead(f, ir, sizeof(ir));
    BlockRead(f, ig, sizeof(ig));
    BlockRead(f, ib, sizeof(ib));
    SetColor(ir, ig, ib);
    BlockRead(f, numFaces, sizeof(numFaces));
    BlockRead(f, numPoints, sizeof(numPoints));
    BlockRead(f, numTextures, sizeof(numTextures));
    {not used yet}
    BlockRead(f, Reserved, sizeof(Reserved));
    {for future purposes}
    {read Points}
    i := 0;
    while i < NumPoints do
    begin
      BlockRead(f, ix, sizeof(ix));
      BlockRead(f, iy, sizeof(iy));
      BlockRead(f, iz, sizeof(iz));
      BlockRead(f, ir, sizeof(ir));
      BlockRead(f, ig, sizeof(ig));
      BlockRead(f, ib, sizeof(ib));
      Point := TGLpoint.create(ix, iy, iz);
      Point.SetColor(ir, ig, ib);
      Points.add(point);
      inc(i);
    end;
    {Read faces}
    i := 0;
    while i < NumFaces do
    begin
      BlockRead(f, ir, sizeof(ir));
      BlockRead(f, ig, sizeof(ig));
      BlockRead(f, ib, sizeof(ib));
      BlockRead(f, numVertices, SizeOf(numVertices));
      Face := AddFace;
      Face.SetColor(ir, ig, ib);
      j := 0;
      while j < NumVertices do
      begin
        BlockRead(f, inx, sizeof(inx));
        BlockRead(f, iny, sizeof(iny));
        BlockRead(f, inz, sizeof(inz));
        BlockRead(f, PointNum, sizeof(PointNum));
        Vertex := Tvertex.create(Face, inx, iny, inz);
        Vertex.Point := TGLpoint(Points.items[PointNum]);
        Vertex.Point.Vertices.add(Vertex);
        {the point must have the references to its vertices}
        inc(Vertex.Point.references);
        Face.Vertices.add(vertex);
        inc(j);
      end;
      inc(i);
    end;
    {Read Texture coordinates, not yet implemented}
  end;
  CloseFile(f);
end;

procedure Tentity.Save(st: string);
var
  f: file;
  numFaces,
    NumPoints,
    NumTextures,
    i, j: LongInt;
  IdData: array[0..3] of char;
  Reserved: array[1..20] of Byte;
  Point: TGLpoint;
  Face: Tface;
  Vertex: Tvertex;
  PointNum: longint;
  numVertices: byte;
   {this limits the vertex count for each polygon
    to less than 255 vertices, more than enough}
  Version,
    SubVersion: byte;
begin
  assignFile(f, st);
  ReWrite(f, 1);
  IdData[0] := '3';
  IdData[1] := 'D';
  IdData[2] := 'P';
  IdData[3] := 'F';
   {3DPF: 3D Proprietary Format}
  Version := 1; {this file was stored using algorithm version 1. }
  SubVersion := 0; {this file was stored using algorithm version  .0}
  NumFaces := Faces.count;
  NumPoints := Points.Count;
  NumTextures := 0; {by now no textures are allowed}
  BlockWrite(f, IdData, sizeof(IdData));
  BlockWrite(f, Version, sizeof(Version));
  BlockWrite(f, SubVersion, sizeof(SubVersion));
  BlockWrite(f, r, sizeof(r));
  BlockWrite(f, g, sizeof(g));
  BlockWrite(f, b, sizeof(b));
  BlockWrite(f, NumFaces, sizeof(NumFaces));
  BlockWrite(f, NumPoints, sizeof(NumPoints));
  BlockWrite(f, NumTextures, sizeof(NumTextures));
   {not used yet}
  BlockWrite(f, Reserved, sizeof(Reserved));
   {for future purposes}
  {Write Points}
  i := 0;
  while i < NumPoints do
  begin
    Point := TGLpoint(Points.items[i]);
    with point do
    begin
      BlockWrite(f, x, sizeof(x));
      BlockWrite(f, y, sizeof(y));
      BlockWrite(f, z, sizeof(z));
      BlockWrite(f, r, sizeof(r));
      BlockWrite(f, g, sizeof(g));
      BlockWrite(f, b, sizeof(b));
    end;
    inc(i);
  end;
  {Write faces}
  i := 0;
  while i < NumFaces do
  begin
    Face := Tface(Faces.items[i]);
    with face do
    begin
      NumVertices := Vertices.count;
      BlockWrite(f, r, sizeof(r));
      BlockWrite(f, g, sizeof(g));
      BlockWrite(f, b, sizeof(b));
      BlockWrite(f, NumVertices, SizeOf(NumVertices));
    end;
    j := 0;
    while j < NumVertices do
    begin
      Vertex := Tvertex(Face.vertices.items[j]);
      with Vertex do
      begin
        PointNum := Points.Indexof(Point);
        BlockWrite(f, nx, sizeof(nx));
        BlockWrite(f, ny, sizeof(ny));
        BlockWrite(f, nz, sizeof(nz));
        BlockWrite(f, PointNum, sizeof(PointNum));
      end;
      inc(j);
    end;
    inc(i);
  end;
  {Write Texture coordinates, not yet implemented}
  CloseFile(f);
end;

procedure Tentity.SetColor;
begin
  R := iR;
  G := iG;
  B := iB;
end;

procedure Tentity.Move;
begin
  Position[1] := x;
  Position[2] := y;
  Position[3] := z;
end;

procedure Tentity.Rotate;
begin
  rotation[1] := rx;
  rotation[2] := ry;
  rotation[3] := rz;
end;

procedure Tentity.Redraw;
var
  i, num: integer;
begin
  glMatrixMode(GL_MODELVIEW);
  case wireframe of
    0: GlPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
    1: GlPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    2: GlPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
  glLoadIdentity;
   {reset the translation and Rotation values}
  glTranslatef(Position[1], Position[2], Position[3]);
  {translate, not the object but the coordinate system}
  glRotatef(rotation[1], 1.0, 0.0, 0.0);
  {same, rotate the coordinate system}
  glRotatef(rotation[2], 0.0, 1.0, 0.0);
  glRotatef(rotation[3], 0.0, 0.0, 1.0);
  if Assigned(Texture) then
  {there is a texture assigned}
    Texture.redraw;
  if PutNames then
  {If this is active then each entity has it´s own name}
  begin
    GlLoadName(id);
    GLPassThrough(id);
     {it should be:  GLfloat(id), but it wouldn´t compile}
  end;
  i := 0;
  num := Faces.count;
  while i < num do
  begin
    if PutNames then
    begin
      ActualVertexNumber := i; {from 0 to Faces.count-1}
      ActualVertexNumber := ActualVertexNumber shl 16;
       {a LongInt has 32 bits,
       so this shifts the actual face number to the upper 16 bits}
    end;
    tface(Faces.items[i]).Redraw;
    inc(i);
  end;
end;

procedure Tentity.LoadDXF(st: string);
var
  f: textfile;
  st1, st2: string;
  in_entities: boolean;
  kind, group, err: integer;
  analyzing: boolean;
  x1, x2, y1, y2, z1, z2, x3, y3, z3, x4, y4, z4: single;
  Face: tface;
  procedure DivRectangle;
  begin
    Face := AddFace;
    Face.AddVertex(x4, y4, z4, 0, 0, 1);
    Face.AddVertex(x3, y3, z3, 0, 0, 1);
    Face.AddVertex(x2, y2, z2, 0, 0, 1);
    Face := AddFace;
    Face.AddVertex(x4, y4, z4, 0, 0, 1);
    Face.AddVertex(x2, y2, z2, 0, 0, 1);
    Face.AddVertex(x1, y1, z1, 0, 0, 1);
  end;
begin
  {first clear old data stored in object}
  Faces.Clear;
  Points.Clear;
  {then, begin}
  assignfile(f, st);
  reset(f);
  in_entities := false;
  repeat
    readln(f, st1);
    if st1 = 'ENTITIES' then in_entities := true;
  until in_entities or eof(f);
  analyzing := false;
  kind := 0;
  x1 := 0; x2 := 0; x3 := 0; x4 := 0;
  y1 := 0; y2 := 0; y3 := 0; y4 := 0;
  z1 := 0; z2 := 0; z3 := 0; z4 := 0;
  if in_entities then
    repeat
      readln(f, st1);
      readln(f, st2);
      group := StrToInt(st1);
      case group of
        0: begin
            if analyzing then
            begin
              case kind of
                1: begin
                    if (x4 <> x3) or (y4 <> y3) or (z4 <> z3) then
                      DivRectangle
                    else
                    begin
                      Face := AddFace;
                      Face.AddVertex(x3, y3, z3, 0, 0, 1);
                      Face.AddVertex(x2, y2, z2, 0, 0, 1);
                      Face.AddVertex(x1, y1, z1, 0, 0, 1);
                    end;
                  end;
              end; {case}
              kind := 0;
            end;
            if st2 = '3DFACE' then
              kind := 1; {line}
            if kind > 0 then
              analyzing := true;
          end;
        10: val(st2, x1, err);
        20: val(st2, y1, err);
        30: val(st2, z1, err);
        11: val(st2, x2, err);
        21: val(st2, y2, err);
        31: val(st2, z2, err);
        12: val(st2, x3, err);
        22: val(st2, y3, err);
        32: val(st2, z3, err);
        13: val(st2, x4, err);
        23: val(st2, y4, err);
        33: val(st2, z4, err);
      end; {of case}
    until eof(f);
  closefile(f);
end;

procedure Tentity.CalcNormals;
var
  Face: tface;
  i, j, numFaces, NumVertices: integer;
  inx, iny, inz: single;
begin
  i := 0;
  numFaces := Faces.Count;
  while i < numFaces do
  begin
    j := 0;
    Face := tface(Faces.Items[i]);
    numVertices := Face.Vertices.Count;
    Face.CalcNormal(0, 1, 2, inx, iny, inz);
     {it uses the 1st, 2nd and 3rd vertex of the face}
    while j < numVertices do
    begin
      with Tvertex(Face.Vertices[j]) do
      begin
        nx := inx;
        ny := iny;
        nz := inz;
      end;
      inc(j);
    end;
    inc(i, 1);
  end;
end;

procedure Tentity.Center;
var
  j, NumPoints: integer;
  x, y, z,
    maxx, maxy, maxz,
    minx, miny, minz,
    cx, cy, cz: single;
begin
  maxx := -100000000; maxy := -100000000; maxz := -100000000;
  minx := 100000000; miny := 100000000; minz := 100000000;
  {obtain the farthest vertices}
  NumPoints := Points.Count;
  j := 0;
  while j < NumPoints do
  begin
    x := TGLpoint(Points.items[j]).x;
    y := TGLpoint(Points.items[j]).y;
    z := TGLpoint(Points.items[j]).z;
    if x < minx then minx := x
    else
      if x > maxX then maxX := x;
    if y < miny then miny := y
    else
      if y > maxy then maxy := y;
    if z < minz then minz := z
    else
      if z > maxz then maxz := z;
    inc(j);
  end;
  {calculate the center coordinates}
  cx := minx + (maxx - minx) / 2;
  cy := miny + (maxy - miny) / 2;
  cz := minz + (maxz - minz) / 2;
  {now move the vertices}
  NumPoints := Points.Count;
  j := 0;
  while j < NumPoints do
  begin
    TGLpoint(Points.items[j]).x := TGLpoint(Points.items[j]).x - cx;
    TGLpoint(Points.items[j]).y := TGLpoint(Points.items[j]).y - cy;
    TGLpoint(Points.items[j]).z := TGLpoint(Points.items[j]).z - cz;
    inc(j);
  end;
end;

{add a new face to the entity}

function Tentity.AddFace;
begin
  Result := tface.create(Self);
  Faces.add(result);
  Result.SetColor(r, g, b);
end;

{search for a point near to x,y,z}

function Tentity.FindPoint;
var
  i, NumPoints: integer;
begin
  Result := nil;
  numPoints := Points.count;
  i := 0;
  while i < numPoints do
  begin
    with TGLpoint(points.items[i]) do
      if SQR(x - ix) + SQR(y - iy) + SQR(z - iz) < MinimalDistance
        then
      begin
        Result := TGLpoint(points.items[i]);
        Exit;
      end;
    inc(i);
  end;
end;

function Tentity.CreateTexture;
begin
  Texture := Ttexture.create(self);
  Result := texture;
end;

constructor tface.Create;
begin
  inherited create;
  Vertices := Tlist.create;
  Owner := iOwner;
  if Assigned(Owner.texture) then
    ApplyTexture := true
    {if there is a texture assigned,
    then by default the polygon will be textured too}
  else {the polygon won´t have a texture on it}
    ApplyTexture := false;
  r := 128;
  g := 128;
  b := 128;
end;

destructor tface.Destroy;
begin
  Vertices.free;
  inherited destroy;
end;

procedure tface.AddVertex;
var
  Vertex: Tvertex;
  Point: TGLpoint;
begin
  Vertex := Tvertex.create(Self, nx, ny, nz);
   {the vertex is always created}
  Point := Owner.FindPoint(x, y, z);
   {find a very near point to x,y,z, If found it will be used}
  if (Point = nil) or (point.r <> r)
    or (point.g <> g) or (point.b <> b) then
   {a near, same color point was not found}
  begin
    Point := TGLpoint.Create(x, y, z);
    Point.SetColor(r, g, b);
    Owner.Points.Add(Point);
  end;
  Vertex.Point := Point;
   {reference the point...}
  Point.vertices.add(vertex);
   {...and the point also references the vertex}
  inc(Point.References);
  {now the vertex is referencing the point}
  Vertices.Add(Vertex);
end;

procedure tface.Redraw;
var
  i, num: LongInt;
  manual: boolean;
begin
  if PutNames then {each face has it´s own name}
  begin
    GlLoadName(ActualVertexNumber);
    GlPassThrough(ActualVertexNumber);
     {it should be:
     GLfloat(ActualVertexNumber), but it wouldn´t compile}
  end;
  if not ApplyTexture or not Assigned(owner.texture) then
  begin
    gldisable(GL_texture_2d);
    manual := true;
  end
  else
  begin
    glenable(gl_texture_2d);
    manual := not owner.texture.automatic;
  end;
  glBegin(GL_POLYGON);
  i := 0;
  num := Vertices.count;
  while i < num do
    with Tvertex(Vertices.items[i]) do
    begin
      glnormal3f(nx, ny, nz);
      glColor3ub(point.r, point.g, point.b);
      if ApplyTexture and manual then
        GlTexCoord2F(tx, 1 - tz);
      glvertex3f(point.x, point.y, point.z);
      inc(i);
    end;
  glEnd;
end;

procedure tface.CalcNormal;
var
  longi, vx1, vy1, vz1, vx2, vy2, vz2: single;
begin
  vx1 := Tvertex(Vertices.items[uno]).point.x
    - Tvertex(Vertices.items[dos]).point.x;
  vy1 := Tvertex(Vertices.items[uno]).point.y
    - Tvertex(Vertices.items[dos]).point.y;
  vz1 := Tvertex(Vertices.items[uno]).point.z
    - Tvertex(Vertices.items[dos]).point.z;

  vx2 := Tvertex(Vertices.items[dos]).point.x
    - Tvertex(Vertices.items[tres]).point.x;
  vy2 := Tvertex(Vertices.items[dos]).point.y
    - Tvertex(Vertices.items[tres]).point.y;
  vz2 := Tvertex(Vertices.items[dos]).point.z
    - Tvertex(Vertices.items[tres]).point.z;

  inx := vy1 * vz2 - vz1 * vy2;
  iny := vz1 * vx2 - vx1 * vz2;
  inz := vx1 * vy2 - vy1 * vx2;

  {now reduce the vector to be unitary,  length=1}
  longi := sqrt(inx * inx + iny * iny + inz * inz);
  if longi = 0 then
    longi := 1; {avoid zero division error}
  inx := inx / longi;
  iny := iny / longi;
  inz := inz / longi;
end;

procedure tface.SetColor;
begin
  r := iR;
  g := iG;
  b := iB;
end;

constructor TGLpoint.Create;
begin
  inherited Create;
  References := 0;
  Vertices := Tlist.create;
  SetPosition(ix, iy, iz);
  SetColor(128, 128, 128);
end;

destructor TGLpoint.Destroy;
var
  i: integer;
begin
  for i := 0 to vertices.count - 1 do
    Vertices.Items[i] := nil;
  vertices.free;
  inherited destroy;
end;

 {set the desired color of the point}

procedure TGLpoint.SetColor(ir, ig, ib: Byte);
begin
  r := ir;
  g := ig;
  b := ib;
end;

{move the point to a dIfferent place}

procedure TGLpoint.SetPosition(ix, iy, iz: single);
begin
  x := ix;
  y := iy;
  z := iz;
end;

constructor Tvertex.Create;
begin
  inherited Create;
  nx := inx;
  ny := iny;
  nz := inz;
  Point := nil;
  Owner := iOwner;
  tx := 0;
  tz := 0;
end;

procedure Tvertex.MakeCopy;
var
  NewPoint: TGLpoint;
begin
  if Point.References > 1 then
  {check If the copy is really necesary}
  begin
  {the vertex is sharing the point,
    so let´s create its own point}
    dec(Point.References);
    {this vertex won´t use that point again}
    NewPoint := TGLpoint.Create(Point.x, Point.y, Point.z);
    Owner.Owner.points.add(newPoint);
    with NewPoint do
    begin
      References := 1; {inc the references on the new point}
      r := Point.r;
      g := Point.g;
      b := Point.b;
    end;
    Point := newPoint;
  end; {now we are ready to
  set the individual values of our vertex}
end;

{If it´s necessary, the point will be duplicated
  so that the individual color can be modIfied}

procedure Tvertex.SetColor;
begin
  MakeCopy;
  Point.r := iR;
  Point.g := iG;
  Point.b := iB;
end;

{If it´s necessary, the point will be duplicated
  so that the individual position can be modIfied}

procedure Tvertex.SetPosition;
begin
  MakeCopy;
  Point.x := iX;
  Point.y := iY;
  Point.z := iZ;
end;

constructor TTexture.Create; {set defaults for all fields}
begin
  Owner := iOwner;
  MyList := glgenlists(1); {generate an empty list for this texture}
  WrapSmode := gl_repeat; {tile the texture in X}
  WrapTmode := gl_repeat; {tile the texture in Z}
  MagFilter := gl_nearest; {texture using the nearest texel}
  MinFilter := gl_nearest; {texture using the nearest texel}
  Automatic := False; {by default the texture coordinates
                            have to be calculated by hand}
  AutoGenModeX := gl_object_linear;
  AutoGenModeZ := gl_object_linear;
  AutoXmult[0] := 1; AutoXmult[1] := 0;
  AutoXmult[2] := 0; AutoXmult[3] := 0;
  AutoZmult[0] := 0; AutoZmult[1] := 0;
  AutoZmult[2] := 1; AutoZmult[3] := 0;
  EnvironmentMode := GL_decal; {like the decals in a racing car}
  EnvBlendColor[0] := 0;
  EnvBlendColor[1] := 0;
  EnvBlendColor[2] := 0;
  EnvBlendColor[3] := 255; {alpha is by default 1}
end;

destructor Ttexture.destroy;
begin
  inherited destroy;
  GLdeleteLists(MyList, 1);
  {destroy the display list of the texture}
end;

function TTexture.LoadTexture(st: string): shortint;
const
  MB = 19778;
type
  ptybuff = ^tybuff;
  ptybuffa = ^tybuffa;
  tybuff = array[1..64000] of record
    r: byte;
    g: byte;
    b: byte;
  end;
  tybuffa = array[1..64000] of record
    r: byte;
    g: byte;
    b: byte;
    a: byte;
  end;
var
  f: file;
  Buffer2b: ptybuff;
  Buffer2: pointer;
  Buffer3b: ptybuffa;
  Buffer3: pointer;
  i: integer;
{$A-}
  header: record
    FileType: Word; {always MB}
    size: longint;
    Reserved1,
      Reserved2: word; {reserved for future purposes}
    offset: longint; {offset to image in bytes}
  end; {header}
  BMPInfo: record
    size: longint; {size of BMPinfo in bytes}
    width: longint; {width of the image in pixels}
    height: longint; {height of the image in pixels}
    planes: word; {number of planes (always 1)}
    Colorbits: word;
    {number of bits used to describe color in each pixel}
    compression: longint; {compression used}
    ImageSize: longint; {image size in bytes}
    XpixPerMeter: longint; {pixels per meter in X}
    YpixPerMeter: longint; {pixels per meter in Y}
    ColorUsed: longint; {number of the color used ¿¿¿???}
    Important: longint; {number of "important" colors}
  end; {info}
{$A+}
begin
  if not FileExists(st) then
  begin
    result := -1; {file not found}
    exit;
  end;
  assignfile(f, st);
  reset(f, 1);
  blockread(f, header, sizeof(header));
  blockread(f, BMPinfo, sizeof(BMPinfo));
  if header.FileType <> MB then
  begin
    result := -2; {file type is not BMP}
    exit;
  end;
  header.size := header.size - sizeof(header) - sizeof(BMPinfo);
  getmem(buffer2, header.size);
  getmem(buffer3, header.size * 4 div 3);
  buffer2b := ptybuff(buffer2);
  buffer3b := ptybuffA(buffer3);
  Blockread(f, buffer2^, header.size);
  for i := 1 to header.size div 3 do
  begin
    buffer3b^[i].r := buffer2b^[i].b;
    buffer3b^[i].g := buffer2b^[i].g;
    buffer3b^[i].b := buffer2b^[i].r;
    buffer3b^[i].a := envblendcolor[3];
     {obtain blend alpha from envblendcolor.alpha}
  end;
  closefile(f);
  GlNewList(MyList, gl_compile);
    {OpenGL 1.0 ignores this one}
  glpixelstorei(gl_unpack_alignment, 4);
  glpixelstorei(gl_unpack_row_length, 0);
  glpixelstorei(gl_unpack_skip_rows, 0);
  glpixelstorei(gl_unpack_skip_pixels, 0);
    {for GLteximage2D the parameters are:
        gl_Texture_2d,
        level of detail (0 unless using mipmapped textures)
        components: 3 for RGB, 4 for RGBA  1 for indexed 256 color
        width, height
        border: width of the border, between 0 and 2.
        Format: gl_color_index, GL_RGB, GL_rgbA,
                GL_luminance are the most used
                type of the data for each pixel
                pointer to image data}
  glenable(GL_BLEND);
{  gltexImage2d(gl_texture_2d, 0, 4, BMPinfo.width,
    BMPinfo.height,
    0,
    gl_rgba,
    gl_unsigned_byte, buffer3^);}
  glendlist;
  result := 0; {no error}
end;

{call the display list, it´s not really a redraw, it´s part of one}

procedure TTexture.redraw;
begin
  if Automatic then {automatic texture coordinates generation}
  begin
    gltexgenf(GL_s, gl_texture_gen_mode, AutoGenModeX);
    gltexgenfv(GL_s, gl_object_plane, addr(AutoXmult));
    gltexgenf(GL_t, gl_texture_gen_mode, AutoGenModeZ);
    gltexgenfv(GL_t, gl_object_plane, addr(AutoZmult));
  end;
  gltexparameteri(gl_texture_2d, gl_texture_wrap_s, WrapSmode);
  gltexparameteri(gl_texture_2d, gl_texture_wrap_t, WrapTmode);
  gltexparameteri(gl_texture_2d, gl_texture_mag_filter, MagFilter);
  gltexparameteri(gl_texture_2d, gl_texture_min_filter, MinFilter);
  gltexEnvi(gl_texture_env, gl_texture_env_mode, EnvironmentMode);
  glcalllist(MyList);
end;

initialization
  {initially, the primitives will not be named}
  PutNames := false;
end.

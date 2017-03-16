{this unit has a group of useful routines for use with OpenGL}
{  TsceneGL: a 3D scene,
has a group of entities and a group of lights.
   Tlight: a light, has color, position and orientation.
    by now only the color attributes have any effect.
   T3dMouse: enables a normal mouse
   to interact easily with one 3D object

   By: Ricardo Sarmiento
       delphiman@hotmail.com
}
unit nGL;

interface

uses Windows, Messages, SysUtils, Classes,
  GL, GLU, n3DPolys;

type
  TsceneGL = class(Tobject)
    DC: HDC; {somewhat private}
    HRC: HGLRC; {somewhat private}
    WindowHandle: Thandle;
     {handle to the host window, a Tpanel most times}
    Entities: Tlist; {Availiable entities in the scene}
    Lights: Tlist; {Availiable lights in the scene}
    Active: boolean;
    {true: InitRC has been called already}
    Fperspective: boolean;
    {true: use perspective projection,
    false: use orthogonal projection}
    Angle, DistNear, DistFar: single;
    {values for Field-of-view angle,
    distance to near clipping plane,
    distance to far clipping plane}
    texturing: boolean;
    {true: use textures and texture information,
     false: don´t use textures}
    BackR, BackG, BackB: float;
    {Background color (RGB), between 0.0 and 1.0 }
    fogColor: array[0..3] of glFloat;
    {fog color (RGBA), between 0.0 and 1.0 }
    fogDensity, fogMinDist, fogMaxDist: float;
    fogEnabled: boolean;
    fogtype: GLint;
    constructor create; {creates a new scene}
    destructor destroy; override;
    {don´t call this directly, call free instead}

    {initialization of the Rendering Context,
      receives the handle of the
     display control, frecuently it is a Tpanel}
    procedure InitRC(iHandle: THandle);
    procedure Redraw; {redraw scene}

    {set values for the pixel´s format.
    don´t call this Function directly,
    call instead InitRC.}
    procedure SetDCPixelFormat;

    {Free all resources taken by InitRC.
    don´t call this Function directly}
    procedure ReleaseRC;

    {reflects changes in the width and height
    (ancho,alto) of the display control}
    procedure UpdateArea(width, height: integer);

    {Set the perspective values for
    Field-of-view angle,
    distance to near clipping plane,
    distance to far clipping plane}
    procedure SetPerspective(iAngle, iDistNear, iDistFar: single);
  end; {TsceneGL}

  Tlight = class(Tobject)
    Source: Tvertex;
     {position and orientation of the light,
     not yet implemented}
    Fambient,
      FdIffuse,
      Fspecular: array[0..3] of GLfloat;
    {ambient, dIffuse and specular components}
    Number: LongInt;
    {number of the light, from 1 to 8}
    constructor Create(num: Byte);
    {create a new, white Ambient light.  num goes from 1 to 8}
    destructor Destroy; override;
    {don´t call directly, call instead free}
    procedure Ambient(r, g, b, a: GLfloat);
    {change the Ambient component of the light}
    procedure DIffuse(r, g, b, a: GLfloat);
    {change the dIffuse component of the light}
    procedure Specular(r, g, b, a: GLfloat);
    {change the specular component of the light}
    procedure Redraw;
    {executes the OpenGL code for this light}
  end; {Tlight}

  T3dMouse = class(Tobject)
  {ease manipulation of 3D objects with the mouse}
    Button1, {left button on mouse}
      Button2: boolean; {right button on mouse}
    Mode: integer; {movement mode:
                        1-move x,y-z,
                        2-turn rx,ry-rz
                        3-turn rx,ry-rz+move z.}
    Entity: Tentity;
    {points to the entity to be modIfied, just one at a time}
    Start: array[1..6] of single;
     {x,y,z - rx,ry,rz
     saves the initial position when dragging mouse}
    Scaling: array[1..6] of single;
     {x,y,z - rx,ry,rz
     stores the speed relations between mouse and 3D}
    BlockStatus: array[1..6] of boolean;
    {x,y,z - rx,ry,rz   which movements are blocked}
    constructor Create(iEntity: Tentity);
     {iEntity is the pointer to one entity in the scene}
    procedure Move(x, y: single; Botones: TShIftState);
    {move or turn the entity according to the mode}
    procedure Scale(x, y, z, rx, ry, rz: single);
    {controls the relative speed between the mouse and the object}
    procedure Block(num: integer; valor: boolean);
    {blocks movement and/or rotation on any of 6 axis.}
    procedure FindVertex(x, y: integer;
      Scene: TsceneGL;
      var pt: Tvertex);
       {return a pointer to the vertex which was nearer to the mouse}
       {Scene is the scene to be evaluated,
       pt is the chosen vertex,
       can be nil If none was found under the mouse}
      {x,y are the coordinates of the mouse}
  end; {num goes from 1 to 6,
          valor=true:block,
          valor=false: don´t block movement}

const
  MaxHits = 200;
var
  xxx, yyy, nnn: integer;
  numFound: integer;
  VertexHits: array[1..MaxHits] of integer;

{This function can convert a BMP file into a RGBA file}
{parameters: BMPname: name of the BMP file,
              RGBAname: name of the new file,
             transparent: color that will be treated as transparent}
function ConvertBMP(BMPname: string;
  RGBAname: string;
  Transparent: longint): integer;

{this is to avoid certain problems with borland tools.
  don´t call them yourself}
function MaskExceptions: Pointer;
procedure UnmaskExceptions(OldMask: Pointer);

implementation

constructor TsceneGL.create;
begin
  inherited create;
  Entities := Tlist.create;
  Lights := Tlist.create;
  Active := false;
  Fperspective := true;
  fogEnabled := false;
  Angle := 30; {degrees for Field-of-view angle}
  DistNear := 1; {1 unit of distance to near clipping plane}
  DistFar := 100; {distance to far clipping plane}
  texturing := true; {use textures}
end;

destructor TsceneGL.destroy;
begin
  if Active then
    ReleaseRC;
  Lights.free;
  Entities.free;
  inherited destroy;
end;

procedure TsceneGL.InitRC;
begin
  {set the area where the OpenGL rendering will take place}
  WindowHandle := iHandle;
  DC := GetDC(WindowHandle);
  SetDCPixelFormat;
  HRC := wglCreateContext(DC);
  wglMakeCurrent(DC, HRC);
  { enable depht testing and back face rendering}
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  glEnable(GL_LIGHTING); {enable Lights}
  Active := True;
end;

procedure TsceneGL.SetDCPixelFormat;
var
  nPixelFormat: Integer;
  pfd: TPixelFormatDescriptor;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  with pfd do begin
    nSize := sizeof(pfd); // Size of this structure
    nVersion := 1; // Version number
    dwFlags := PFD_DRAW_TO_WINDOW or
      PFD_SUPPORT_OPENGL or
      PFD_DOUBLEBUFFER; // Flags
    iPixelType := PFD_TYPE_RGBA; // RGBA pixel values
    cColorBits := 24; // 24-bit color
    cDepthBits := 32; // 32-bit depth buffer
    iLayerType := PFD_MAIN_PLANE; // Layer type
  end;
  nPixelFormat := ChoosePixelFormat(DC, @pfd);
  SetPixelFormat(DC, nPixelFormat, @pfd);
  DescribePixelFormat(DC, nPixelFormat,
    sizeof(TPixelFormatDescriptor), pfd);
end;

procedure TsceneGL.ReleaseRC;
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(HRC);
  ReleaseDC(WindowHandle, DC);
  Active := False;
end;

procedure TsceneGL.UpdateArea;
var
  gldAspect: GLdouble;
  ratio, range: GlFloat;
begin
  { redefine the visible volume
  and the viewport when the window´s size is modIfied}
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  if Fperspective then
  begin
    gldAspect := width / height;
    gluPerspective(Angle, // Field-of-view angle
      gldAspect, // Aspect ratio of viewing volume
      DistNear, // Distance to near clipping plane
      DistFar); // Distance to far clipping plane
  end
  else
  begin { Orthogonal projection}
    range := 6;
    if width <= height then
    begin
      ratio := height / width;
      GlOrtho(-range, range,
        -range * ratio,
        range * ratio,
        -range * 4, range * 4);
    end
    else
    begin
      ratio := width / height;
      GlOrtho(-range * ratio,
        range * ratio,
        -range, range,
        -range * 4, range * 4);
    end;
  end;
  glViewport(0, 0, width, height);
end;

procedure TsceneGL.Redraw;
const
  glfMaterialColor: array[0..3] of GLfloat = (0.5, 1.0, 0.5, 1.0);
var
  i, num: integer;
  ps: TPaintStruct;
  pp: Pointer;
begin
  if Active then
  begin
    {initialization}
    pp := MaskExceptions;
    BeginPaint(WindowHandle, ps);
      {place Lights}
    i := 0;
    num := Lights.count;
    while i < num do
    begin
      Tlight(Lights.items[i]).Redraw;
      inc(i);
    end;
    {clear depht and color buffers}
    glclearcolor(backR, backG, backB, 1);
    {specify background color}
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    {Clear the rendering area}
    glenable(gl_color_material);
    {tell OpenGL to pay attention to GlColor orders}
    if fogEnabled then
    begin
      glenable(GL_fog);
      {too slow: glhint(gl_fog_hint, gl_nicest);}
      {good fog, but a little slow}
      glfogi(gl_fog_mode, fogtype);
      glfogfv(gl_fog_color, @fogColor);
      if fogtype <> GL_linear then
      {density doesnt work with linear fog}
        glfogf(gl_fog_density, FogDensity);
      glfogf(GL_fog_start, fogMinDist);
      glfogf(GL_fog_end, fogMaxDist);
    end else
      gldisable(GL_fog);
    if texturing then
    begin
      gldisable(GL_texture_1d);
      glenable(gl_texture_2d);
    end
    else
    begin
      glDisable(GL_texture_1d);
      glDisable(gl_texture_2d);
    end;
    if PutNames then
    begin
      glInitNames;
      {init the name stack,
      not necessary If your objects aren´t named}
      glPushName(0);
      {init the name stack}
    end;
    {draw entities}
    i := 0;
    num := Entities.count;
    while i < num do
    begin
      Tentity(Entities.items[i]).Redraw;
      inc(i);
    end;
    {finalization}
    {swap the rendering buffers so there is no flickering}
    SwapBuffers(DC);
    EndPaint(WindowHandle, ps);
    UnmaskExceptions(pp);
  end;
end;

procedure TsceneGL.SetPerspective;
begin
  Angle := iAngle;
  DistNear := iDistNear;
  DistFar := iDistFar;
end;

 {by default a light is created white }

constructor Tlight.Create;
begin
  inherited create;
  Source := Tvertex.create(nil, 0, 0, 0);
  {light with no orientation...}
  Source.Point := TGLpoint.Create(0, 0, 0);
   {...and placed in the center of the scene}
  Ambient(0.5, 0.5, 0.5, 1);
  DIffuse(0.25, 0.25, 0.25, 1);
  Specular(0.1, 0.1, 0.1, 1);
  case num of
    1: Number := GL_Light0;
    2: Number := GL_Light1;
    3: Number := GL_Light2;
    4: Number := GL_Light3;
    5: Number := GL_Light4;
    6: Number := GL_Light5;
    7: Number := GL_Light6;
    8: Number := GL_Light7;
  end; {case}
end;

destructor Tlight.destroy;
begin
  Source.point.free;
  Source.free;
end;

procedure Tlight.Ambient;
begin
  Fambient[0] := r;
  Fambient[1] := g;
  Fambient[2] := b;
  Fambient[3] := a;
end;

procedure Tlight.DIffuse;
begin
  fDIffuse[0] := r;
  fDIffuse[1] := g;
  fDIffuse[2] := b;
  fDIffuse[3] := a;
end;

procedure Tlight.Specular;
begin
  Fspecular[0] := r;
  Fspecular[1] := g;
  Fspecular[2] := b;
  Fspecular[3] := a;
end;

procedure Tlight.Redraw;
begin
  glLightfv(Number, GL_AMBIENT, @Fambient);
  glLightfv(Number, GL_DIfFUSE, @fDIffuse);
  glLightfv(Number, GL_SPECULAR, @Fspecular);
  glEnable(Number); {enable light number N}
end;

constructor T3dMouse.Create;
var
  i: integer;
begin
  inherited create;
  Entity := iEntity;
  Mode := 3;
  {this is the mode used by defect,
  just because it fits my needs}
  Button1 := false;
  Button2 := false;
  for i := 1 to 6 do
    BlockStatus[i] := false;
end;

procedure T3dMouse.Move;
begin
  if assigned(Entity) then
  begin
    if not Button1 then
    begin
      if ssLeft in botones then
      begin
        if mode = 1 then {X,Y,Z}
        begin
          Start[1] := x - Entity.Position[1] / Scaling[1]; {X}
          Start[2] := y - Entity.Position[2] / Scaling[2]; {Y}
        end;
        if mode in [2, 3] then {2:rx,ry,rz  3:rx,ry,rz,Z}
        begin
          Start[6] := x - Entity.rotation[3] / Scaling[6]; {rx}
          Start[4] := y - Entity.rotation[1] / Scaling[4]; {ry}
        end;
        Button1 := true;
      end;
    end
    else
    begin
      if ssLeft in botones then
      begin
        if mode = 1 then
        begin
          if not BlockStatus[1] then
            Entity.Position[1] := (x - Start[1]) * Scaling[1]; {X}
          if not BlockStatus[2] then
            Entity.Position[2] := (y - Start[2]) * Scaling[2]; {Y}
        end;
        if mode in [2, 3] then
        begin
          if not BlockStatus[4] then
            Entity.rotation[3] := (x - Start[6]) * Scaling[6]; {rx}
          if not BlockStatus[5] then
            Entity.rotation[1] := (y - Start[4]) * Scaling[4]; {ry}
        end;
      end
      else
        Button1 := false;
    end;
    if not Button2 then
    begin
      if ssRight in botones then
      begin
        if mode in [1, 3] then
          Start[3] := y - Entity.Position[3] / Scaling[3]; {z}
        if mode in [2, 3] then
          Start[5] := x - Entity.rotation[2] / Scaling[5]; {rz}
        Button2 := true;
      end;
    end
    else
    begin
      if ssRight in botones then
      begin
        if mode in [1, 3] then
          if not BlockStatus[3] then
            Entity.Position[3] := (y - Start[3]) * Scaling[3]; {Z}
        if mode in [2, 3] then
          if not BlockStatus[6] then
            Entity.rotation[2] := (x - Start[5]) * Scaling[5]; {rz}
      end
      else
        Button2 := false;
    end;
  end;
end;

procedure T3dMouse.Scale;
begin
  Scaling[1] := x;
  Scaling[2] := y;
  Scaling[3] := z;
  Scaling[4] := rx;
  Scaling[5] := ry;
  Scaling[6] := rz;
end;

procedure T3dMouse.Block;
begin
  if num in [1..6] then
    BlockStatus[num] := valor;
end;

procedure T3dMouse.FindVertex;
const
  tambuffer = 8000; {8000 items reserved for the rendering buffer}
  radio = 15; {this is the search radius}
var
  buffer: array[0..tamBuffer] of GLfloat;
  size, i, j, count: integer;
  nx, ny: integer;
  PreviousPutNames: boolean;
  ActualVertex: LongInt;

begin
  PreviousPutNames := PutNames;
   {preserve the previous value of PutNames}
  PutNames := true; {put names on each vertex}
  numFound := 0;
  GlFeedBackBuffer(tamBuffer, GL_2D, @buffer);
  GlRenderMode(GL_feedBack);
  Scene.Redraw;
  size := GlRenderMode(GL_render);
  {now that we got the 2D coordinates of each vertex,
  find which vertex is near}
  i := 0;
  try
    while i < size do
    begin
      if buffer[i] = GL_Pass_Through_token then
     {the G.P.T.T. is a marker to divide the buffer in sections}
        if buffer[i + 1] = Entity.id then
      {this is the entity we are dealing with}
        begin
          inc(i, 2);
        // continue cycling until we find another Object marker:
          while (buffer[i] = GL_Pass_Through_token) do
          begin
            ActualVertex := round(Buffer[i + 1]);
            if buffer[i + 2] = GL_Polygon_Token then
          {this is a polygon, let´s check it out}
            begin
              count := trunc(buffer[i + 3]);
              inc(i, 4);
              for j := 0 to count - 1 do
            {let´s take a look at each vertex of this polygon}
              begin
                nx := round(buffer[i]);
              {x coordinate of a vertex}
                ny := round(buffer[i + 1]);
              {y coordinate of a vertex}
                if (nx + radio > x) and (nx - radio < x)
                  and (ny + radio > y) and (ny - radio < y)
                  and (NumFound < MaxHits) then
                begin
                  inc(numFound);
                  VertexHits[numFound] := ActualVertex + j;
                end;
                inc(i, 2); {x and y}
              end;
            end
            else
              inc(i, 2);
          end;
        end;
      inc(i);
    end;
  except
  end;
  {restore the previous value of PutNames}
  PutNames := PreviousPutNames;
end;

{MWMWMWMWMWMWMWMWMW   END OF CLASS IMPLEMENTATION WMWMWMWMWM}

function MaskExceptions: Pointer;
var
  OldMask: Pointer;
begin
  asm
    fnstcw WORD PTR OldMask;
    mov eax, OldMask;
    or eax, $3f;
    mov WORD PTR OldMask+2,ax;
    fldcw WORD PTR OldMask+2;
  end;
  result := OldMask;
end;

procedure UnmaskExceptions(OldMask: Pointer);
begin
  asm
    fnclex;
    fldcw WORD PTR OldMask;
  end;
end;

function ConvertBMP(BMPname: string;
  RGBAname: string;
  Transparent: longint): integer;
begin
  {this will convert a RGB bmp to a RGBA bmp,
  it is not ready yet}
  result := 0;
end;

end.

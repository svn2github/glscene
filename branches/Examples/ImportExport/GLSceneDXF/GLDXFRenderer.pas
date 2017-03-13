{:   GLDXFRenderer<p>

  OpenGL renderer for DXF objects<p>

  <b>History :</b><font size=-1><ul>
    <li>19/01/03 - DA - 3DFaces seems to be drawing correctly now
    <li>18/01/03 - DA - Unit creation from GLDXFVectorFile,
                        Now drawing clockwise and counter-clockwise polygons
  </ul></font>
}
unit GLDXFRenderer;

interface

uses
  OpenGL,
  TypesDXF,
  FileDXF,
  ObjectsDXF,
   
  GLScene,
  GLVectorTypes,
  GLVectorFileObjects,
  GLTexture,
  OpenGLTokens,
  GLRenderContextInfo;

type

  // TGLDXFRenderer
  //
  {: An OpenGL DXF rendere. }
  TGLDXFRenderer = class
  private
    FHeader: TDXFHeader;
    FEntities: TDXFEntitiesList;

  public
    procedure DrawGLArc(StartAngle, EndAngle, Radius: Double; Position: T3DPoint);
    procedure DrawPoint(Point: TDXFPoint);
    procedure DrawLine(Line: TDXFLine);
    procedure DrawPolyline(Polyline: TDXFPolyline);
    procedure DrawArc(Arc: TDXFArc);
    procedure DrawCircle(Circle: TDXFCircle);
    procedure Draw3DFace(Face: TDXF3DFace);
    procedure DrawInsert(Insert: TDXFInsert);
    procedure DrawSolid(Solid: TDXFSolid);    
    procedure DrawEntity(Entity: TDXFEntity);

    //: render DXF object with Direct Opengl
    procedure Render(Sender: TObject;var rci:TGLRenderContextInfo);

    //: DXF header
    property Header: TDXFHeader read FHeader write FHeader;
    //: list of entities in the DXF file
    property Entities: TDXFEntitiesList read FEntities write FEntities;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses
  OpenGL1x,
  GLVectorGeometry,

  MathsDXF,

  WinProcs, // OutputDebugString()
  SysUtils, 

  Math
  ;

{ TGLDXFRenderer }

// DrawGLArc
//
procedure TGLDXFRenderer.DrawGLArc(StartAngle, EndAngle, Radius: Double; Position: T3DPoint);
var
  a: Double;
begin
  // draw the arc
  glPushMatrix;
    glTranslated(Position.X, Position.Y, Position.Z);
    glBegin(GL_LINE_STRIP);
      a := StartAngle;
      while (a <= EndAngle) do begin
        glVertex2f(Cos(DegToRad(a)) * Radius, Sin(DegToRad(a)) * Radius);
        // stop exactly at the end angle
        if (a < EndAngle) and (a + 1 > EndAngle) then a := EndAngle else a := a + 1;
      end;
    glEnd;
  glPopMatrix;
end;

// DrawPoint
//
procedure TGLDXFRenderer.DrawPoint(Point: TDXFPoint);
begin
  with Point do begin
    glColor3f(Color.X, Color.Y, Color.Z); // color
    glBegin(GL_POINTS);
      glVertex3f(Primary.X, Primary.Y, Primary.Z);
    glEnd;
  end;
end;

// DrawLine
//
procedure TGLDXFRenderer.DrawLine(Line: TDXFLine);
begin
  with Line do begin
    glColor3f(Color.X, Color.Y, Color.Z); // color
    glBegin(GL_LINES);
      glVertex3f(Primary.X, Primary.Y, Primary.Z);
      glVertex3f(EndPoint.X, EndPoint.Y, EndPoint.Z);
    glEnd;
  end;
end;

// DrawPolyline
//
procedure TGLDXFRenderer.DrawPolyline(Polyline: TDXFPolyline);
var
  v: integer;

  procedure DrawPolyElementArc(V1, V2 : TDXFVertex);
  var
    //Ang: Double;
    IncAng, Ang_a, Chord, Rad, ChordAng, Dir: Double;
    StartAngle, EndAngle: Double;
    StartPoint, EndPoint: TAffineVector;
    CP: T3DPoint;
  begin
    // The bulge is the tangent of one fourth the included angle
    // for an arc segment, made negative if the arc goes
    // clockwise from the start point to the endpoint.
    // A bulge of 0 indicates a straight segment, and a bulge
    // of 1 is a semicircle. Thus, Mutiplying the ArcTANgent of the
    // bulge factor by 4 gives the included angle (angle formed by
    // line form the first vertex to the center point to the
    // second vertex)in radians.
    IncAng := 4 * ArcTan(Abs(V1.Bulge));
    // Ang := RadToDeg(IncAng);

    // if not 0 calculate Angle A.  Angle A is the angle formed by the
    // chord of the arc, the invisible line between the two vertices of
    // the defined arc, and the invisible line from a vertex of the arc
    // to the center point of the arc.
    Ang_a := (Pi / 2) - (IncAng / 2);

    // calculate the length of the chord of the polyline arc
    // distance between the two vertex that form the polyline arc
    StartPoint := AffineVectorMake(V1.Primary.X, V1.Primary.Y, V1.Primary.Z);
    EndPoint := AffineVectorMake(V2.Primary.X, V2.Primary.Y, V2.Primary.Z);
    Chord := VectorDistance(StartPoint, EndPoint);

    // if the Include Angle is not 0, a straight Polyline segment,
    // calculate and display Radius of arc, Angle of Chord,
    // center point
    Rad := (Chord / 2) / Cos(Ang_a);
    ChordAng := DegToRad(Angle(V1.Primary, V2.Primary));

    // check to see if Bulge Factor is negative or postive
    // determines which direction the arc was created
    // clockwise or counter clockwise.
    if V1.Bulge > 0 then Dir:= ChordAng + Ang_a else Dir:= ChordAng - Ang_a;

    // finds center point of ARC
    CP := Polar(V1.Primary, Dir, Abs(Rad));

    // Compute the start angle and end angle for drawing
    StartAngle := Angle(CP, V1.Primary);
    EndAngle := Angle(CP, V2.Primary);
    if V1.Bulge < 0 then begin // clockwise
      StartAngle := StartAngle + EndAngle;
      EndAngle := StartAngle - EndAngle;
      StartAngle := StartAngle - EndAngle;
    end;
    if StartAngle > EndAngle then EndAngle := EndAngle + 360;

    // Print debug information
    // display values for the included angle. The included angle is
    // the angle from the center point of the arc to the two vertices
    // defining the start and end points of the arc.
    {
    OutputDebugString(PAnsiChar(
      Format('Inc. angle=%f°,%fR Angle A=%f° Chord length=%f Radius=%f Angle of Chord=%f°, Dir=%f°',
      [Ang, IncAng, RadToDeg(Ang_a), Chord, Rad, RadToDeg(ChordAng), RadToDeg(Dir)])));
    OutputDebugString(PAnsiChar(Format('Center Point of Arc: (%f, %f, %f)',
      [CP[0], CP[1], CP[2]])));
    OutputDebugString(PAnsiChar(Format('StartAngle=%f° EndAngle=%f° Bulge=%f',
      [StartAngle, EndAngle, V1.Bulge])));
    }

    // draw the polyline arc
    DrawGLArc(StartAngle, EndAngle, Rad, CP);

    // draw arc vertices
    {
    glColor3f(0, 200, 0); // color
    DrawGLArc(0, 360, 0.01, V1.Primary);
    glColor3f(0, 200, 100); // color
    DrawGLArc(0, 360, 0.01, V2.Primary);
    }
  end;

  procedure DrawPolyElement(V1, V2 : TDXFVertex);
  begin
    with V1 do begin
      if Bulge = 0 then begin // straight segment
        glBegin(GL_LINES);
        glVertex3f(Primary.X, Primary.Y, Primary.Z); // 1st point
        with V2 do glVertex3f(Primary.X, Primary.Y, Primary.Z); // 2nd point
        glEnd;
      end
      else DrawPolyElementArc(V1, V2); // arc
    end;
  end;

begin
  with Polyline do begin
    // only draw standards polylines at this time
    if Flag and (Ord(pl3DPolygon) + Ord(plPolyface)) = 0 then begin

      glColor3f(Color.X, Color.Y, Color.Z); // color

      // draw segments and arcs
      for v := 0 to Length(PointList) - 2 do
        DrawPolyElement(PointList[v], PointList[v+1]);

      // it is a closed polyline ?
      if Flag and Ord(plClosed) <> 0 then
        DrawPolyElement(PointList[Length(PointList) - 1], PointList[0]);

    end else OutputDebugString(PWideChar('Polyline non standard ignored.'));
  end;
end;

// DrawArc
//
procedure TGLDXFRenderer.DrawArc(Arc: TDXFArc);
var
  ea: integer;
begin
  with Arc do begin
    // some arcs have an end angle lower than the start angle, correct this
    if StartAngle > EndAngle then
      ea := Round(EndAngle + 360)
    else ea := Round(EndAngle);

    glColor3f(Color.X, Color.Y, Color.Z); // color
    DrawGLArc(StartAngle, ea, Radius, Primary);
  end;
end;

// DrawCircle
//
procedure TGLDXFRenderer.DrawCircle(Circle: TDXFCircle);
begin
  with Circle do begin
    glColor3f(Color.X, Color.Y, Color.Z); // color
    DrawGLArc(0, 360, Radius, Primary);
  end;
end;

// Draw3DFace
//
procedure TGLDXFRenderer.Draw3DFace(Face: TDXF3DFace);
begin
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glBegin(GL_POLYGON);
    with Face do begin
      glColor3f(Color.X, Color.Y, Color.Z); // color
      glVertex3f(Primary.X, Primary.Y, Primary.Z);
      glVertex3f(Second.X, Second.Y, Second.Z);
      glVertex3f(Third.X, Third.Y, Third.Z);
      glVertex3f(Fourth.X, Fourth.Y, Fourth.Z);
      {
      OutputDebugString(PAnsiChar(Format('3DFace (%f;%f;%f) (%f;%f;%f) (%f;%f;%f) (%f;%f;%f)',
        [Primary[0], Primary[1], Primary[2], Second[0], Second[1], Second[2],
        Third[0], Third[1], Third[2], Fourth[0], Fourth[1], Fourth[2]])));
      }
    end;
  glEnd;
end;

// DrawInsert
//
procedure TGLDXFRenderer.DrawInsert(Insert: TDXFInsert);
var
  Block : TDXFBlock;
  CntEntity : Integer;
begin
  with Insert do begin
    Block := Blocks.BlockByName[BlockName];

    if Assigned(Block) then begin
      glPushMatrix;
      glTranslated(Primary.X, Primary.Y, Primary.Z);
      if Scale.X <> 0 then glScaled(Scale.X, Scale.Y, Scale.Z);

      with Block.Entities do
        for CntEntity := 0 to Count - 1 do
          DrawEntity(Entity[CntEntity]);
      glPopMatrix;
    end
    else OutputDebugString(PWideChar('Block not found : ' + BlockName));
  end;
end;

// DrawEntity
//
{: Draw an entity
  @param Entity The entity to draw }
procedure TGLDXFRenderer.DrawEntity(Entity: TDXFEntity);
begin
    if Entity is TDXFPolyline then DrawPolyline(TDXFPolyline(Entity)) else
    if Entity is TDXFLine then DrawLine(TDXFLine(Entity)) else
    if Entity is TDXFArc then DrawArc(TDXFArc(Entity)) else
    if Entity is TDXFCircle then DrawCircle(TDXFCircle(Entity)) else
    if Entity is TDXFText then { drawed with opengl objects } else
    if Entity is TDXFSolid then DrawSolid(TDXFSolid(Entity)) else
    if Entity is TDXF3DFace then Draw3DFace(TDXF3DFace(Entity)) else
    if Entity is TDXFInsert then DrawInsert(TDXFInsert(Entity)) else
    if Entity is TDXFPoint then DrawPoint(TDXFPoint(Entity)) else
    OutputDebugString(PWideChar('Ne sait pas afficher ' + Entity.ClassName));
end;

// Render
//
procedure TGLDXFRenderer.Render(Sender: TObject;var rci: TGLRenderContextInfo);
var
  CntEntity : Integer;
begin
  // save attributes before modifications
  glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
  glDisable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);
//  glPushMatrix;

  // rotate all the scene to render in the same way as DXF
  glRotated(180, 0, 1, 0);

  // draw entities
  with Entities do
    for CntEntity := 0 to Count - 1 do
      DrawEntity(Entity[CntEntity]);

  // restore attributes
//  glPopMatrix;
  glPopAttrib;
end;

// DrawSolid
//
procedure TGLDXFRenderer.DrawSolid(Solid: TDXFSolid);
var
  poly: PGLUtesselator;
  v3d: Array[0..3] of TAffineDblVector;
  vaff: Array[0..3] of TAffineVector;
  v: integer;
begin
  poly := gluNewTess;
  gluTessCallback(poly, GLU_TESS_BEGIN, @glBegin);
  gluTessCallback(poly, GLU_TESS_VERTEX, @glVertex3dv);
  gluTessCallback(poly, GLU_TESS_END, @glEnd);

  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glEnable(GL_CULL_FACE);
  glutessBeginPolygon(poly, nil);
    gluTessBeginContour(poly);
      with Solid do begin
        glColor3f(Color.X, Color.Y, Color.Z); // color

        // The gluTessVertex function need an array of TVector3D
        SetVector(v3d[0], Primary);
        SetVector(v3d[1], Second);
        SetVector(v3d[2], Third);
        SetVector(v3d[3], Fourth);

        // The PolygonSignedArea function need an array of TAffineVector
        vaff[0] := Primary;
        vaff[1] := Second;
        vaff[2] := Third;
        vaff[3] := Fourth;
        // OutputDebugString(PAnsiChar(Format('PolygonSignedArea = %f', [PolygonSignedArea(@vaff, 4)])));

        // sign of the polygon area
        if PolygonSignedArea(@vaff, 4) < 0 then begin
          // polygon is drawn counter-clockwise
          for v:=3 downto 0 do
            gluTessVertex(poly, v3d[v], @v3d[v]);
        end else begin
          // polygon is drawn clockwise
          for v:=0 to 3 do
            gluTessVertex(poly, v3d[v], @v3d[v]);
        end;

        {
        OutputDebugString(PAnsiChar(Format('Solid (%f;%f;%f) (%f;%f;%f) (%f;%f;%f) (%f;%f;%f)',
          [Primary[0], Primary[1], Primary[2], Second[0], Second[1], Second[2],
          Third[0], Third[1], Third[2], Fourth[0], Fourth[1], Fourth[2]])));
        }
        
        {
        glColor3f(0, 255, 0); // color
        // draw points
        DrawGLArc(0, 360, 0.01, Primary);
        DrawGLArc(0, 360, 0.01, Second);
        DrawGLArc(0, 360, 0.01, Third);
        DrawGLArc(0, 360, 0.01, Fourth);
        // draw lines between points
        glBegin(GL_LINE_LOOP);
          glVertex3f(Primary[0], Primary[1], Primary[2]);
          glVertex3f(Second[0], Second[1], Second[2]);
          glVertex3f(Fourth[0], Fourth[1], Fourth[2]);
          glVertex3f(Third[0], Third[1], Third[2]);
        glEnd;
        }
      end;
    gluTessEndContour(poly);
  glutessEndPolygon(poly);
  gluDeleteTess(poly);
end;

end.


// GLParametricSurfaces
{: Parametric surface implementation (like Bezier and BSpline surfaces)
   for GLScene.<p>
   
   Notes: 
   The MOParametricSurface is a TMeshObject descendant that can be used
   to render parametric surfaces. The Renderer property defines if the
   surface should be rendered using OpenGL mesh evaluators (through GLU 
   Nurbs for BSplines) or through GLScene using the CurvesAndSurfaces.pas 
   routines to generate the mesh vertices and then rendered through the 
   standard TMeshObject render routine. Please note that BSplines aren't 
   correctly handled yet in the CurvesAndSurfaces unit so the output mesh 
   in GLScene rendering mode is wrong. I'll have it fixed when I know 
   what's going wrong. The GLU Nurbs and glMeshEval Beziers work well 
   though.<p>
   
   <b>History : </b><font size=-1><ul>
      <li>20/08/03 - SG - Weighted control points.
      <li>18/07/03 - SG - Creation.
   </ul></font>
}
unit GLParametricSurfaces;

interface

uses
  GLVectorFileObjects, CurvesAndSurfaces, VectorGeometry, VectorLists,
  PersistentClasses, GLTexture, OpenGL1x;

type

  {: psrGLScene tells the surface to render using GLScene code to build
     the mesh, whereas, psrOpenGL uses glEvalMesh2 or gluNurbsRenderer
     calls to render the surface. }
  TParametricSurfaceRenderer = (psrGLScene, psrOpenGL);

  {: psbBezier indicates building the surface with Bernstein basis
     functions, no knot or order properties are used.
     psbBSpline indicates building the surface using BSpline basis
     functions, these require orders and knot vectors to define the
     control point influences on the surface. }
  TParametricSurfaceBasis = (psbBezier, psbBSpline);

  TMOParametricSurface = class (TMeshObject)
    private
      FControlPoints,
      FWeightedControlPoints : TAffineVectorList;
      FKnotsU,
      FKnotsV,
      FWeights : TSingleList;
      FOrderU,
      FOrderV,
      FCountU,
      FCountV,
      FResolution : Integer;
      FAutoKnots : Boolean;
      FContinuity : TBSplineContinuity;
      FRenderer : TParametricSurfaceRenderer;
      FBasis : TParametricSurfaceBasis;

      procedure SetControlPoints(Value : TAffineVectorList);
      procedure SetKnotsU(Value : TSingleList);
      procedure SetKnotsV(Value : TSingleList);
      procedure SetWeights(Value : TSingleList);
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure BuildList(var mrci : TRenderContextInfo); override;
      procedure Prepare; override;
      procedure Clear; override;
      {: Generates a mesh approximation of the surface defined by the 
         properties below. This is used to construct the mesh when using
         Renderer = psrGLScene. If you want to render using OpenGL calls
         but would like to obtain the mesh data also use this call to
         generate the mesh data. Fills in Vertices, Normals, etc. }
      procedure GenerateMesh;

      //: Control points define the parametric surface.
      property ControlPoints : TAffineVectorList read FControlPoints write SetControlPoints;
      {: KnotsU and KnotsV are the knot vectors in the U and V direction. Knots
         define the continuity of curves and how control points influence the 
         parametric values to build the surface. }
      property KnotsU : TSingleList read FKnotsU write SetKnotsU;
      property KnotsV : TSingleList read FKnotsV write SetKnotsV;
      {: Weights define how much a control point effects the surface. }
      property Weights : TSingleList read FWeights write SetWeights;
      //: OrderU and OrderV defines the curve order in the U and V direction
      property OrderU : Integer read FOrderU write FOrderU;
      property OrderV : Integer read FOrderV write FOrderV;
      {: CountU and CountV describe the number of control points in the 
         U and V direciton. Basically a control point width and height 
         in (u,v) space. }
      property CountU : Integer read FCountU write FCountU;
      property CountV : Integer read FCountV write FCountV;
      {: Defines how fine the resultant mesh will be. Higher values create
         finer meshes. Resolution = 50 would produce a 50x50 mesh. 
         The GLU Nurbs rendering uses resolution as the U_STEP and V_STEP
         using the sampling method GLU_DOMAIN_DISTANCE, so the resolution
         works a little differently there. }
      property Resolution : Integer read FResolution write FResolution;
      {: Automatically generate the knot vectors based on the Continuity.
         Only applies to BSpline surfaces. }
      property AutoKnots : Boolean read FAutoKnots write FAutoKnots;
      property Continuity : TBSplineContinuity read FContinuity write FContinuity;
      {: Determines whether to use OpenGL calls (psrOpenGL) or the GLScene 
         mesh objects (psrGLScene) to render the surface. }
      property Renderer : TParametricSurfaceRenderer read FRenderer write FRenderer;
      //: Basis determines the style of curve, psbBezier or psbBSpline
      property Basis : TParametricSurfaceBasis read FBasis write FBasis;
  end;

implementation

// Create
//
constructor TMOParametricSurface.Create;
begin
  inherited;

  FControlPoints:=TAffineVectorList.Create;
  FWeightedControlPoints:=TAffineVectorList.Create;
  FKnotsU:=TSingleList.Create;
  FKnotsV:=TSingleList.Create;
  FWeights:=TSingleList.Create;
  
  Resolution:=20;
end;

// Destroy
//
destructor TMOParametricSurface.Destroy;
begin
  FControlPoints.Free;
  FWeightedControlPoints.Free;
  FKnotsU.Free;
  FKnotsV.Free;
  FWeights.Free;
end;

// WriteToFiler
//
procedure TMOParametricSurface.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version
    FControlPoints.WriteToFiler(writer);
    FKnotsU.WriteToFiler(writer);
    FKnotsV.WriteToFiler(writer);
    FWeights.WriteToFiler(writer);
    WriteInteger(FOrderU);
    WriteInteger(FOrderV);
    WriteInteger(FCountU);
    WriteInteger(FCountV);
    WriteInteger(FResolution);
    WriteBoolean(FAutoKnots);
    WriteInteger(Integer(FContinuity));
    WriteInteger(Integer(FRenderer));
    WriteInteger(Integer(FBasis));
  end;
end;

// ReadFromFiler
//
procedure TMOParametricSurface.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion = 0 then with reader do begin
    FControlPoints.ReadFromFiler(reader);
    FKnotsU.ReadFromFiler(reader);
    FKnotsV.ReadFromFiler(reader);
    FWeights.ReadFromFiler(reader);
    FOrderU:=ReadInteger;
    FOrderV:=ReadInteger;
    FCountU:=ReadInteger;
    FCountV:=ReadInteger;
    FResolution:=ReadInteger;
    FAutoKnots:=ReadBoolean;
    FContinuity:=TBSplineContinuity(ReadInteger);
    FRenderer:=TParametricSurfaceRenderer(ReadInteger);
    FBasis:=TParametricSurfaceBasis(ReadInteger);
  end else RaiseFilerException(archiveVersion);
end;

// BuildList
//
procedure TMOParametricSurface.BuildList(var mrci : TRenderContextInfo);
var
  NurbsRenderer : PGLUNurbs;
begin
  case FRenderer of
    psrGLScene : inherited;
    psrOpenGL : begin
      glPushAttrib(GL_ALL_ATTRIB_BITS);
      //glEnable(GL_MAP2_TEXTURE_COORD_3);
      glEnable(GL_MAP2_VERTEX_3);
      glEnable(GL_AUTO_NORMAL);
      glEnable(GL_NORMALIZE);

      case FBasis of
        psbBezier : begin
          glMapGrid2f(FResolution,0,1,FResolution,0,1);
          //glMap2f(GL_MAP2_TEXTURE_COORD_3,0,1,3,2,0,1,6,2,@TexCoords.List[0]);
          glMap2f(GL_MAP2_VERTEX_3,0,1,3,FCountU,0,1,3*FCountU,FCountV,@FWeightedControlPoints.List[0]);
          glEvalMesh2(GL_FILL,0,FResolution,0,FResolution);
        end;

        psbBSpline : begin
          NurbsRenderer:=gluNewNurbsRenderer;
          gluNurbsProperty(NurbsRenderer, GLU_DISPLAY_MODE, GLU_FILL);

          gluNurbsProperty(NurbsRenderer, GLU_SAMPLING_METHOD, GLU_DOMAIN_DISTANCE);
          gluNurbsProperty(NurbsRenderer, GLU_U_STEP, FResolution);
          gluNurbsProperty(NurbsRenderer, GLU_V_STEP, FResolution);

          gluBeginSurface(NurbsRenderer);
            {gluNurbsSurface(NurbsRenderer,
                            FKnotsU.Count, @FKnotsU.List[0],
                            FKnotsV.Count, @FKnotsV.List[0],
                            3, FCountU*3,
                            @FWeightedControlPoints.List[0],
                            FOrderU, FOrderV,
                            GL_MAP2_TEXTURE_COORD_3);//}
            gluNurbsSurface(NurbsRenderer,
                            FKnotsU.Count, @FKnotsU.List[0],
                            FKnotsV.Count, @FKnotsV.List[0],
                            3, FCountU*3,
                            @FWeightedControlPoints.List[0],
                            FOrderU, FOrderV,
                            GL_MAP2_VERTEX_3);
          gluEndSurface(NurbsRenderer);
          gluDeleteNurbsRenderer(NurbsRenderer);
        end;

      end;
      glPopAttrib;
    end;
  end;
end;

// Prepare
//
procedure TMOParametricSurface.Prepare;
var
  i : integer;
begin
  // We want to clear everything but the parametric surface
  // data (control points and knot vectors).
  inherited Clear;
  
  // Apply weights to control points
  FWeightedControlPoints.Assign(FControlPoints);
  if FWeights.Count=FControlPoints.Count then
    for i:=0 to FWeightedControlPoints.Count-1 do
      FWeightedControlPoints[i]:=VectorScale(FWeightedControlPoints[i],FWeights[i]);

  case FRenderer of
    psrGLScene : begin
      GenerateMesh;
    end;
    psrOpenGL : begin
      if (FAutoKnots) and (FBasis=psbBSpline) then begin
        GenerateKnotVector(FKnotsU,FCountU,FOrderU,FContinuity);
        GenerateKnotVector(FKnotsV,FCountV,FOrderV,FContinuity);
      end;
    end;
  end;
end;

// Clear
//
procedure TMOParametricSurface.Clear;
begin
  inherited;
  FControlPoints.Clear;
  FKnotsU.Clear;
  FKnotsV.Clear;
  FWeights.Clear;
end;

// GenerateMesh
//
procedure TMOParametricSurface.GenerateMesh;
var
  i,j : Integer;
  fg : TFGVertexIndexList;
begin
  case FBasis of
    psbBezier  : begin
      if FAutoKnots then begin
        FKnotsU.Clear;
        FKnotsV.Clear;
      end;
      GenerateBezierSurface(FResolution,FCountU,FCountV,FControlPoints,Vertices);
    end;
    psbBSpline : begin
      if FAutoKnots then begin
        GenerateKnotVector(FKnotsU,FCountU,FOrderU,FContinuity);
        GenerateKnotVector(FKnotsV,FCountV,FOrderV,FContinuity);
      end;
      GenerateBSplineSurface(FResolution,FOrderU,FOrderV,FCountU,FCountV,FKnotsU,FKnotsV,FControlPoints,Vertices);
    end;
  end;

  Mode:=momFaceGroups;
  fg:=TFGVertexIndexList.CreateOwned(FaceGroups);
  fg.Mode:=fgmmTriangles;
  for j:=0 to FResolution-1 do with fg do
    for i:=0 to FResolution-1 do begin
      VertexIndices.Add(i+(FResolution+1)*j);
      VertexIndices.Add((i+1)+(FResolution+1)*j);
      VertexIndices.Add(i+(FResolution+1)*(j+1));

      VertexIndices.Add(i+(FResolution+1)*(j+1));
      VertexIndices.Add((i+1)+(FResolution+1)*j);
      VertexIndices.Add((i+1)+(FResolution+1)*(j+1));
    end;
  BuildNormals(fg.VertexIndices,momTriangles);

end;

// SetControlPoints
//
procedure TMOParametricSurface.SetControlPoints(Value : TAffineVectorList);
begin
  FControlPoints.Assign(Value);
end;

// SetKnotsU
//
procedure TMOParametricSurface.SetKnotsU(Value : TSingleList);
begin
  FKnotsU.Assign(Value);
end;

// SetKnotsV
//
procedure TMOParametricSurface.SetKnotsV(Value : TSingleList);
begin
  FKnotsV.Assign(Value);
end;

procedure TMOParametricSurface.SetWeights(Value : TSingleList);
begin
  FWeights.Assign(Value);
end;

end.

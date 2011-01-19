//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xAtmosphere <p>

   This unit contains classes that imitate an atmosphere around a planet.<p>

   <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>27/02/10 - Yar - Adapted GLAtmosphere to OpenGL3x
   </ul></font><p>
}

unit GL3xAtmosphere;

interface

{$I GLScene.inc}

uses
  SysUtils,
  Classes,
  // GLScene
  GLScene,
  GLCadencer,
  VectorGeometry,
  GLState,
  GLContext,
  GLStrings,
  GLColor,
  GLSLShader,
  GL3xObjects,
  GLShaderManager,
  GLRenderContextInfo
{$IFDEF GLS_DELPHI}
  , VectorTypes
{$ENDIF};

type
  EGLBAtmosphereException = class(Exception);

  {: With aabmOneMinusSrcAlpha atmosphere is transparent to other objects,
     but has problems, which are best seen when the Atmosphere radius is big.
     With bmOneMinusDstColor atmosphere doesn't have these problems, but offers
     limited transparency (when you look closely on the side). }

  TGL3xAtmosphereBlendingMode = (abmOneMinusDstColor, abmOneMinusSrcAlpha);

  {: This class imitates an atmosphere around a planet. }
  TGL3xCustomAtmosphere = class(TGL3xBaseSceneObject)
  private
    // Used in DoRenderl
    cosCache, sinCache: array of Single;
    VertexCash: PAffineVectorArray;

    FSlices: Integer;
    FBlendingMode: TGL3xAtmosphereBlendingMode;
    FPlanetRadius: Single;
    FAtmosphereRadius: Single;
    FOpacity: Single;
    FLowAtmColor: TGLColor;
    FHighAtmColor: TGLColor;
    FSun: TGLBaseSceneObject;
    procedure SetSun(const Value: TGLBaseSceneObject);
    procedure SetAtmosphereRadius(const Value: Single);
    procedure SetPlanetRadius(const Value: Single);
    function StoreAtmosphereRadius: Boolean;
    function StoreOpacity: Boolean;
    function StorePlanetRadius: Boolean;
    procedure SetSlices(const Value: Integer);
    procedure SetLowAtmColor(const AValue: TGLColor);
    procedure SetHighAtmColor(const AValue: TGLColor);
    procedure EnableGLBlendingMode(StatesCash: TGLStateCache);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BuildMesh; override;
  public
    property Sun: TGLBaseSceneObject read FSun write SetSun;

    property Slices: Integer read FSlices write SetSlices default 60;
    property Opacity: Single read FOpacity write FOpacity stored StoreOpacity;

    //: AtmosphereRadius > PlanetRadius!!!
    property AtmosphereRadius: Single read FAtmosphereRadius write
      SetAtmosphereRadius stored StoreAtmosphereRadius;
    property PlanetRadius: Single read FPlanetRadius write SetPlanetRadius stored
      StorePlanetRadius;

    //: Use value slightly lower than actual radius, for antialiasing effect.
    property LowAtmColor: TGLColor read FLowAtmColor write SetLowAtmColor;
    property HighAtmColor: TGLColor read FHighAtmColor write SetHighAtmColor;
    property BlendingMode: TGL3xAtmosphereBlendingMode read FBlendingMode
      write FBlendingMode default abmOneMinusSrcAlpha;

    procedure SetOptimalAtmosphere(const ARadius: Single); //absolute
    procedure SetOptimalAtmosphere2(const ARadius: Single); //relative
    procedure TogleBlendingMode; //changes between 2 blending modes

    //: Standard component stuff.
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    //: Used to determine extents.
    function AxisAlignedDimensionsUnscaled: TVector; override;
  end;

  TGL3xAtmosphere = class(TGL3xCustomAtmosphere)
  published
    property Sun;
    property Slices;
    property Opacity;
    property AtmosphereRadius;
    property PlanetRadius;
    property LowAtmColor;
    property HighAtmColor;
    property BlendingMode;

    property Position;
    property ObjectsSorting;
    property ShowAxes;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

implementation

uses
  BaseClasses, GL3xMesh, GL3xStaticMesh, GLDrawTechnique;

const
  EPS = 0.0001;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shaders'}{$ENDIF}

  Atmosphere_vp150: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'uniform mat4 ModelMatrix;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'uniform vec3 CameraWorldPosition;' + #10#13 +
    'uniform vec4 spherePosition;' + #10#13 +
    'uniform vec3 sunPosition;' + #10#13 +
    'uniform vec3 LowAtmColor;' + #10#13 +
    'uniform vec3 HighAtmColor;' + #10#13 +
    'uniform float invAtmosphereHeight;' + #10#13 +
    'uniform float PlanetRadius;' + #10#13 +
    'uniform float AtmoRadius;' + #10#13 +
    'uniform float Opacity;' + #10#13 +
    'out vec4 color;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +
    'vec3 lightingVector;' + #10#13 +
    'bool RayCastSphereIntersect(vec3 rayVector, float sphereRadius, out vec3 i1, out vec3 i2)' + #10#13
    +
    '{' + #10#13 +
    '	float proj = dot(rayVector, spherePosition.xyz - CameraWorldPosition);' + #10#13 +
    '	vec3 projPoint = rayVector * proj + CameraWorldPosition;' + #10#13 +
    ' vec3 range = spherePosition.xyz - projPoint;' + #10#13 +
    '	float d2 = sphereRadius * sphereRadius - dot(range, range);' + #10#13 +
    '	if (d2>=0.0)' + #10#13 +
    '	{' + #10#13 +
    '   d2 = sqrt(d2);' + #10#13 +
    '		i1 = rayVector * (proj-d2) + CameraWorldPosition;' + #10#13 +
    '		i2 = rayVector * (proj+d2) + CameraWorldPosition;' + #10#13 +
    '		return (true);' + #10#13 +
    '	}' + #10#13 +
    '	return (false);' + #10#13 +
    '}' + #10#13 +
    'vec4 AtmosphereColor(vec3 rayStart, vec3 rayEnd)' + #10#13 +
    '{' + #10#13 +
    '  vec4 result = vec4(0.0, 0.0, 0.0, 0.0);' + #10#13 +
    '  float rayLength = distance(rayStart, rayEnd);' + #10#13 +
    '  int n = int(3.0 * rayLength * invAtmosphereHeight) + 2;' + #10#13 +
    '  if (n > 10) n = 10;' + #10#13 +
    '  float invN = 1.0 / float(n);' + #10#13 +
    '  float contrib = rayLength * Opacity * invN;' + #10#13 +
    '  float decay = 1.0 - contrib * 0.5;' + #10#13 +
    '  contrib *= 1.0/1.1;' + #10#13 +
    '  vec3 normal, atmPoint;' + #10#13 +
    '  for (int I = n-1; I>-1; I--)' + #10#13 +
    '  {' + #10#13 +
    '    atmPoint = mix(rayStart, rayEnd, float(I) * invN) - spherePosition.xyz;' + #10#13
    +
    '    normal = normalize(atmPoint);' + #10#13 +
    '    float intensity = dot(normal, lightingVector) + 0.1;' + #10#13 +
    '    if (intensity > 0.0)' + #10#13 +
    '    {' + #10#13 +
    '      intensity *= contrib;' + #10#13 +
    '      float alt = (length(atmPoint) - PlanetRadius) * invAtmosphereHeight;' + #10#13
    +
    '      vec3 altColor = mix(LowAtmColor, HighAtmColor, alt);' + #10#13 +
    '      result.rgb = result.rgb * decay + altColor * intensity;' + #10#13 +
    '    }' + #10#13 +
    '    else {result.rgb *= decay;}' + #10#13 +
    '  }' + #10#13 +
    '  result.a = float(n) * contrib * Opacity * 0.1;' + #10#13 +
    '  return (result);' + #10#13 +
    '}' + #10#13 +
    'float SphereVisibleRadius(float distance, float radius)' + #10#13 +
    '{' + #10#13 +
    '   float ir = sqrt(distance-radius);' + #10#13 +
    '   float tr = (distance+radius-ir*ir)/(2.0*ir);' + #10#13 +
    '   return sqrt(radius+tr*tr);' + #10#13 +
    '}' + #10#13 +
    'vec4 ComputeColor(inout vec3 rayDest)' + #10#13 +
    '{' + #10#13 +
    ' vec3 ai1, ai2, pi1, pi2;' + #10#13 +
    ' vec3 rayVector = normalize(rayDest - CameraWorldPosition);' + #10#13 +
    ' if (RayCastSphereIntersect(rayVector, AtmoRadius, ai1, ai2))' + #10#13 +
    ' {' + #10#13 +
    '   if (RayCastSphereIntersect(rayVector, PlanetRadius, pi1, pi2))' + #10#13  +
    '   {' + #10#13 +
    '      return AtmosphereColor(ai1, pi1);' + #10#13 +
    '   }' + #10#13 +
    '   else' + #10#13 +
    '   {' + #10#13 +
    '      return AtmosphereColor(ai1, ai2);' + #10#13 +
    '   }' + #10#13 +
    ' }' + #10#13 +
    ' return vec4(0.0);' + #10#13 +
    '}' + #10#13 +
    'void main()' + #10#13 +
    '{' + #10#13 +
    ' vec4 WorldPosition = ModelMatrix * vec4(0.0, Position.x*Position.yz, 1.0);' + #10#13 +
    ' vec3 CameraVector = WorldPosition.xyz - CameraWorldPosition;' + #10#13 +
    ' float newRadius = SphereVisibleRadius(spherePosition.w, Position.x*Position.x);' + #10#13 +
    ' WorldPosition = ModelMatrix * vec4(0.0, newRadius*Position.yz, 1.0);' + #10#13 +
    ' lightingVector = normalize(sunPosition - WorldPosition.xyz);' + #10#13 +
    '	color = ComputeColor(WorldPosition.xyz);' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * WorldPosition;' + #10#13 +
    '}';

  Atmosphere_fp150: AnsiString =
    '#version 150' + #10#13 +
    'precision highp float;' + #10#13 +
    'in vec4 color;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main()' + #10#13 +
    '{' + #10#13 +
    '	FragColor = color;' + #10#13 +
    '}';
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

var
  AtmosphereProgram: IGLName;
  AtmosphereVertexObject: IGLName;
  AtmosphereFragmentObject: IGLName;

    uniformSpherePosition,
    uniformSunPosition,
    uniformInvAtmosphereHeight,
    uniformOpacity,
    uniformLowAtmColor,
    uniformHighAtmColor,
    uniformPlanetRadius,
    uniformAtmoRadius: TGLSLUniform;

procedure InitAtmosphereShader;
begin
  if AtmosphereProgram = nil then
  begin
    uniformSpherePosition := TGLSLUniform.RegisterUniform('spherePosition');
    uniformSunPosition := TGLSLUniform.RegisterUniform('sunPosition');
    uniformInvAtmosphereHeight := TGLSLUniform.RegisterUniform('invAtmosphereHeight');
    uniformOpacity := TGLSLUniform.RegisterUniform('Opacity');
    uniformLowAtmColor := TGLSLUniform.RegisterUniform('LowAtmColor');
    uniformHighAtmColor := TGLSLUniform.RegisterUniform('HighAtmColor');
    uniformPlanetRadius := TGLSLUniform.RegisterUniform('PlanetRadius');
    uniformAtmoRadius := TGLSLUniform.RegisterUniform('AtmoRadius');

    with ShaderManager do
    try
      BeginWork;
      DefineShaderProgram(AtmosphereProgram,
        [ptVertex, ptFragment], 'AtmosphereProgram');
      DefineShaderObject(AtmosphereVertexObject, Atmosphere_vp150,
        [ptVertex], 'AtmosphereVertexObject');
      DefineShaderObject(AtmosphereFragmentObject, Atmosphere_fp150,
        [ptFragment], 'AtmosphereFragmentObject');
      AttachShaderObjectToProgram(AtmosphereVertexObject, AtmosphereProgram);
      AttachShaderObjectToProgram(AtmosphereFragmentObject, AtmosphereProgram);
      LinkShaderProgram(AtmosphereProgram);
    finally
      EndWork;
    end;
  end;
end;

constructor TGL3xCustomAtmosphere.Create(AOwner: TComponent);
begin
  inherited;

  with MeshManager do
    try
      BeginWork;
      FMesh := CreateMesh(Self.ClassName, TGL3xStaticMesh, '', '');
    finally
      EndWork;
    end;
  StructureChanged;

  FLowAtmColor := TGLColor.Create(Self);
  FHighAtmColor := TGLColor.Create(Self);

  FOpacity := 2.1;
  SetSlices(60);
  FAtmosphereRadius := 3.55;
  FPlanetRadius := 3.395;
  FLowAtmColor.Color := VectorMake(1, 1, 1, 1);
  FHighAtmColor.Color := VectorMake(0, 0, 1, 1);

  FBlendingMode := abmOneMinusSrcAlpha;
end;

destructor TGL3xCustomAtmosphere.Destroy;
begin
  FLowAtmColor.Free;
  FHighAtmColor.Free;
  FreeMem(VertexCash);
  inherited;
end;

procedure TGL3xCustomAtmosphere.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  M, Basis: TMatrix;
  sunPos: TVector;
  XAxis, ZAxis, YAxis: TVector;
begin
  if GL.VERSION_2_1 then
  begin
    // Render self
    if ARenderSelf and not ARci.ignoreMaterials then
    begin
      if AtmosphereProgram = nil then
        InitAtmosphereShader;

      if ocStructure in Changes then
        BuildMesh;

      if ShaderManager.IsProgramLinked(AtmosphereProgram) then
      begin
        XAxis := VectorSubtract(AbsolutePosition, ARci.CameraPosition);
        NormalizeVector(XAxis);
        ZAxis := VectorCrossProduct(ARci.CameraUp, XAxis);
        NormalizeVector(ZAxis);
        YAxis := VectorCrossProduct(XAxis, ZAxis);
        Basis[0] := XAxis;
        Basis[1] := YAxis;
        Basis[2] := ZAxis;
        Basis[3] := NullHmgPoint;
        M := MatrixMultiply(Basis, Self.AbsoluteMatrix);
        if Assigned(FSun) then
          sunPos := FSun.AbsolutePosition
        else
          sunPos := ARci.CameraPosition;

        with ShaderManager do
        begin
          UseProgram(AtmosphereProgram);
          // All calculations are doing in world space
          UniformMat4f(uniformModelMatrix, M);
          UniformMat4f(uniformViewProjectionMatrix, ARci.PipelineTransformation.ViewProjectionMatrix);

          Uniform3f(uniformCameraWorldPosition, AffineVectorMake(ARci.PipelineTransformation.CameraPosition));

          Uniform3f(uniformSunPosition, AffineVectorMake(sunPos));
          sunPos := AbsolutePosition;
          sunPos[3] := VectorDistance2(sunPos, ARci.CameraPosition);
          Uniform4f(uniformSpherePosition, sunPos);
          Uniform1f(uniformInvAtmosphereHeight,
            1 / (FAtmosphereRadius - FPlanetRadius));
          Uniform1f(uniformOpacity, FOpacity);
          Uniform3f(uniformLowAtmColor, AffineVectorMake(FLowAtmColor.Color));
          Uniform3f(uniformHighAtmColor, AffineVectorMake(FHighAtmColor.Color));
          Uniform1f(uniformPlanetRadius, FPlanetRadius);
          Uniform1f(uniformAtmoRadius, FAtmosphereRadius);

          with ARci.GLStates do
          begin
            EnableDepthTest := False;
            DepthWriteMask := False;
            Enable(stBlend);
            Enable(stCullFace);
          end;
          EnableGLBlendingMode(ARci.GLStates);

          ARci.ignoreMaterials := True;
          try
            DrawManager.Draw(ARci, FMesh);
          finally
            ARci.ignoreMaterials := False;
          end;
          if not ARci.GLStates.ForwardContext then
            UseFixedFunctionPipeline;
        end;
      end;
    end;
  end;
  // Render children
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TGL3xCustomAtmosphere.BuildMesh;
var
  radius: Single;
  I, J, k0, k1, kt: Integer;
  Builder: TGL3xStaticMeshBuilder;
begin
  with MeshManager do
    try
      BeginWork;
      Builder := TGL3xStaticMeshBuilder(GetMeshBuilder(FMesh));
      with Builder do
      begin
        BeginMeshAssembly;
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        for I := 0 to 13 do
        begin
          if I < 5 then
            radius := FPlanetRadius * Sqrt(I * (1 / 5))
          else
            radius := FPlanetRadius + (I - 5.1) * (FAtmosphereRadius - FPlanetRadius) * (1 / 6.9);
          k0 := (I and 1) * (FSlices + 1);
          k1 := (FSlices + 1) - k0;
          for J := 0 to FSlices do
          begin
            VertexCash[k0 + J] :=
              AffineVectorMake(radius, sinCache[J], cosCache[J]);
            if I = 0 then
              Break;
          end;

          if I > 1 then
          begin
            if I = 13 then
            begin
              for J := FSlices downto 0 do
              begin
                kt := k1 + J;
                Attribute3f(attrPosition, VertexCash[kt]);
                EmitVertex;
                kt := k0 + J;
                Attribute3f(attrPosition, VertexCash[kt]);
                EmitVertex;
              end;
              RestartStrip;
            end
            else
            begin
              for J := FSlices downto 0 do
              begin
                kt := k1 + J;
                Attribute3f(attrPosition, VertexCash[kt]);
                EmitVertex;
                kt := k0 + J;
                Attribute3f(attrPosition, VertexCash[kt]);
                EmitVertex;
              end;
              RestartStrip;
            end;
          end
          else if I = 1 then
          begin
            BeginBatch(mpTRIANGLE_FAN);
            Attribute3f(attrPosition, VertexCash[k1]);
            EmitVertex;
            for J := k0 + FSlices downto k0 do
            begin
              Attribute3f(attrPosition, VertexCash[J]);
              EmitVertex;
            end;
            EndBatch;
            BeginBatch(mpTRIANGLE_STRIP);
          end;
        end;
        EndBatch;
        WeldVertices;
        EndMeshAssembly;
      end;
    finally
      EndWork;
    end;
  ClearStructureChanged;
end;

procedure TGL3xCustomAtmosphere.TogleBlendingMode;
begin
  if FBlendingMode = abmOneMinusSrcAlpha then
    FBlendingMode := abmOneMinusDstColor
  else
    FBlendingMode := abmOneMinusSrcAlpha;
  NotifyChange(Self);
end;

procedure TGL3xCustomAtmosphere.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGL3xCustomAtmosphere then
  begin
    SetSlices(TGL3xCustomAtmosphere(Source).FSlices);
    FOpacity := TGL3xCustomAtmosphere(Source).FOpacity;
    FAtmosphereRadius := TGL3xCustomAtmosphere(Source).FAtmosphereRadius;
    FPlanetRadius := TGL3xCustomAtmosphere(Source).FPlanetRadius;
    FLowAtmColor.Color := TGL3xCustomAtmosphere(Source).FLowAtmColor.Color;
    FHighAtmColor.Color := TGL3xCustomAtmosphere(Source).FHighAtmColor.Color;
    FBlendingMode := TGL3xCustomAtmosphere(Source).FBlendingMode;
    SetSun(TGL3xCustomAtmosphere(Source).FSun);
  end;
end;

procedure TGL3xCustomAtmosphere.SetSun(const Value: TGLBaseSceneObject);
begin
  if FSun <> nil then
    FSun.RemoveFreeNotification(Self);
  FSun := Value;
  if FSun <> nil then
    FSun.FreeNotification(Self);
end;

function TGL3xCustomAtmosphere.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := FAtmosphereRadius;
  Result[1] := Result[0];
  Result[2] := Result[0];
  Result[3] := 0;
end;

procedure TGL3xCustomAtmosphere.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSun) then
    FSun := nil;
end;

procedure TGL3xCustomAtmosphere.SetAtmosphereRadius(
  const Value: Single);
begin
  FAtmosphereRadius := Value;
  if Value <= FPlanetRadius then
    FPlanetRadius := FAtmosphereRadius / 1.01;
  StructureChanged;
end;

procedure TGL3xCustomAtmosphere.SetPlanetRadius(const Value: Single);
begin
  FPlanetRadius := Value;
  if Value >= FAtmosphereRadius then
    FAtmosphereRadius := FPlanetRadius * 1.01;
  StructureChanged;
end;

procedure TGL3xCustomAtmosphere.EnableGLBlendingMode(StatesCash: TGLStateCache);
begin
  with StatesCash do
    case FBlendingMode of
      abmOneMinusDstColor: SetBlendFunc(bfDstAlpha,
          bfOneMinusDstColor);
      abmOneMinusSrcAlpha: SetBlendFunc(bfDstAlpha,
          bfOneMinusSrcAlpha);
    else
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  NotifyChange(Self);
end;

function TGL3xCustomAtmosphere.StoreAtmosphereRadius: Boolean;
begin
  Result := Abs(FAtmosphereRadius - 3.55) > EPS;
end;

function TGL3xCustomAtmosphere.StoreOpacity: Boolean;
begin
  Result := Abs(FOpacity - 2.1) > EPS;
end;

function TGL3xCustomAtmosphere.StorePlanetRadius: Boolean;
begin
  Result := Abs(FPlanetRadius - 3.395) > EPS;
end;

procedure TGL3xCustomAtmosphere.SetSlices(const Value: Integer);
begin
  if Value > 0 then
  begin
    FSlices := Value;
    SetLength(cosCache, FSlices + 1);
    SetLength(sinCache, FSlices + 1);
    PrepareSinCosCache(sinCache, cosCache, 0, 360);

    GetMem(VertexCash, 2 * (FSlices + 1) * SizeOf(TVector));
  end
  else
    raise EGLBAtmosphereException.Create('Slices must be more than zero!');

  StructureChanged;
end;

procedure TGL3xCustomAtmosphere.SetLowAtmColor(const AValue: TGLColor);
begin
  FLowAtmColor.Assign(AValue);
end;

procedure TGL3xCustomAtmosphere.SetHighAtmColor(const AValue: TGLColor);
begin
  FHighAtmColor.Assign(AValue);
end;

procedure TGL3xCustomAtmosphere.SetOptimalAtmosphere(const ARadius: Single);
begin
  FAtmosphereRadius := ARadius + 0.25;
  FPlanetRadius := ARadius - 0.07;
  StructureChanged;
end;

procedure TGL3xCustomAtmosphere.SetOptimalAtmosphere2(const ARadius: Single);
begin
  FAtmosphereRadius := ARadius + ARadius / 15;
  FPlanetRadius := ARadius - ARadius / 50;
  StructureChanged;
end;

initialization
  RegisterClasses([TGL3xCustomAtmosphere, TGL3xAtmosphere]);

finalization
  AtmosphereProgram := nil;
  AtmosphereVertexObject := nil;
  AtmosphereFragmentObject := nil;

end.


//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xAtmosphere <p>

   This unit contains classes that imitate an atmosphere around a planet.<p>

   <b>History : </b><font size=-1><ul>
      <li>27/02/10 - Yar - Adapted GLAtmosphere to OpenGL3x
   </ul></font><p>
}

unit GL3xAtmosphere;

interface

{$I GLScene.inc}

uses
  // VCL
  SysUtils, Classes,

  // GLScene
  GLScene, GLCadencer, OpenGL1x, VectorGeometry, GLState,
  GLContext, GLStrings, GLColor, GLSLShader,
  GL3xObjects, GLVBOManagers, GLRenderContextInfo;

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
    VertexCash: PVectorArray;

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
    procedure EnableGLBlendingMode(var rci: TRenderContextInfo);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
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
    property LowAtmColor: TGLColor read FLowAtmColor;
    property HighAtmColor: TGLColor read FHighAtmColor;
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
    procedure BuildList(var rci: TRenderContextInfo); override;
    //: Used to determine extents.
    function AxisAlignedDimensionsUnscaled: TVector; override;
  end;

  TGL3xAtmosphere = class(TGL3xCustomAtmosphere)
  published
    property BuiltProperties;
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

const
  EPS = 0.0001;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shaders'}{$ENDIF}
  Atmosphere_vp120: AnsiString =
    '#version 120' + #10#13 +
    'invariant attribute vec3 Position;' + #10#13 +
    'uniform mat4 ModelMatrix;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'uniform vec3 eyePosition;' + #10#13 +
    'uniform vec3 spherePosition;' + #10#13 +
    'uniform vec3 sunPosition;' + #10#13 +
    'uniform vec3 LowAtmColor;' + #10#13 +
    'uniform vec3 HighAtmColor;' + #10#13 +
    'uniform float invAtmosphereHeight;' + #10#13 +
    'uniform float PlanetRadius;' + #10#13 +
    'uniform float AtmosphereRadius;' + #10#13 +
    'uniform float Opacity;' + #10#13 +
    'varying vec4 color;' + #10#13 +
    'varying vec2 texcoord;' + #10#13 +
    'vec3 lightingVector;' + #10#13 +
    'bool RayCastSphereIntersect(vec3 rayVector, float sphereRadius, out vec3 i1, out vec3 i2)' + #10#13
      +
    '{' + #10#13 +
    '	float proj = dot(rayVector, spherePosition - eyePosition);' + #10#13 +
    '	vec3 projPoint = rayVector * proj + eyePosition;' + #10#13 +
    ' vec3 range = spherePosition - projPoint;' + #10#13 +
    '	float d2 = sphereRadius * sphereRadius - dot(range, range);' + #10#13 +
    '	if (d2>=0.0)' + #10#13 +
    '	{' + #10#13 +
    '   d2 = sqrt(d2);' + #10#13 +
    '		i1 = rayVector * (proj-d2) + eyePosition;' + #10#13 +
    '		i2 = rayVector * (proj+d2) + eyePosition;' + #10#13 +
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
    '  float contrib = rayLength * Opacity / float(n);' + #10#13 +
    '  float decay = 1.0 - contrib * 0.5;' + #10#13 +
    '  contrib *= 1.0/1.1;' + #10#13 +
    '  vec3 normal, atmPoint;' + #10#13 +
    '  for (int I = n-1; I>=0; I--)' + #10#13 +
    '  {' + #10#13 +
    '    atmPoint = mix(rayStart, rayEnd, float(I) / float(n)) - spherePosition;' + #10#13
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
    '  result.a = n * contrib * Opacity * 0.1;' + #10#13 +
    '  return (result);' + #10#13 +
    '}' + #10#13 +
    'vec4 ComputeColor(inout vec3 rayDest)' + #10#13 +
    '{' + #10#13 +
    ' vec3 ai1, ai2, pi1, pi2;' + #10#13 +
    ' vec3 rayVector = normalize(rayDest - eyePosition);' + #10#13 +
    ' if (RayCastSphereIntersect(rayVector, AtmosphereRadius, ai1, ai2))' + #10#13
      +
    ' {' + #10#13 +
    '   rayDest = ai1;' + #10#13 +
    '   if (RayCastSphereIntersect(rayVector, PlanetRadius, pi1, pi2))' + #10#13
      +
    '   {' + #10#13 +
    '      return AtmosphereColor(ai1, pi1);' + #10#13 +
    '   }' + #10#13 +
    '   else' + #10#13 +
    '   {' + #10#13 +
    '      return AtmosphereColor(ai1, ai2);' + #10#13 +
    '   }' + #10#13 +
    ' }' + #10#13 +
    ' return vec4(0.0, 0.0, 0.0, 0.0);' + #10#13 +
    '}' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec4 pos = ModelMatrix * vec4(Position, 1.0);' + #10#13 +
    ' lightingVector = normalize(sunPosition - pos.xyz);' + #10#13 +
    '	color = ComputeColor(pos.xyz);' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * pos;' + #10#13 +
    '}';

  Atmosphere_fp120: AnsiString =
    '#version 120' + #10#13 +
    'varying vec4 color;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	gl_FragColor = color;' + #10#13 +
    '}';

  Atmosphere_vp150: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'uniform mat4 ModelMatrix;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'uniform vec3 eyePosition;' + #10#13 +
    'uniform vec3 spherePosition;' + #10#13 +
    'uniform vec3 sunPosition;' + #10#13 +
    'uniform vec3 LowAtmColor;' + #10#13 +
    'uniform vec3 HighAtmColor;' + #10#13 +
    'uniform float invAtmosphereHeight;' + #10#13 +
    'uniform float PlanetRadius;' + #10#13 +
    'uniform float AtmosphereRadius;' + #10#13 +
    'uniform float Opacity;' + #10#13 +
    'out vec4 color;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +
    'vec3 lightingVector;' + #10#13 +
    'bool RayCastSphereIntersect(vec3 rayVector, float sphereRadius, out vec3 i1, out vec3 i2)' + #10#13
      +
    '{' + #10#13 +
    '	float proj = dot(rayVector, spherePosition - eyePosition);' + #10#13 +
    '	vec3 projPoint = rayVector * proj + eyePosition;' + #10#13 +
    ' vec3 range = spherePosition - projPoint;' + #10#13 +
    '	float d2 = sphereRadius * sphereRadius - dot(range, range);' + #10#13 +
    '	if (d2>=0.0)' + #10#13 +
    '	{' + #10#13 +
    '   d2 = sqrt(d2);' + #10#13 +
    '		i1 = rayVector * (proj-d2) + eyePosition;' + #10#13 +
    '		i2 = rayVector * (proj+d2) + eyePosition;' + #10#13 +
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
    '  float contrib = rayLength * Opacity / float(n);' + #10#13 +
    '  float decay = 1.0 - contrib * 0.5;' + #10#13 +
    '  contrib *= 1.0/1.1;' + #10#13 +
    '  vec3 normal, atmPoint;' + #10#13 +
    '  for (int I = n-1; I>=0; I--)' + #10#13 +
    '  {' + #10#13 +
    '    atmPoint = mix(rayStart, rayEnd, float(I) / float(n)) - spherePosition;' + #10#13
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
    '  result.a = n * contrib * Opacity * 0.1;' + #10#13 +
    '  return (result);' + #10#13 +
    '}' + #10#13 +
    'vec4 ComputeColor(inout vec3 rayDest)' + #10#13 +
    '{' + #10#13 +
    ' vec3 ai1, ai2, pi1, pi2;' + #10#13 +
    ' vec3 rayVector = normalize(rayDest - eyePosition);' + #10#13 +
    ' if (RayCastSphereIntersect(rayVector, AtmosphereRadius, ai1, ai2))' + #10#13
      +
    ' {' + #10#13 +
    '   rayDest = ai1;' + #10#13 +
    '   if (RayCastSphereIntersect(rayVector, PlanetRadius, pi1, pi2))' + #10#13
      +
    '   {' + #10#13 +
    '      return AtmosphereColor(ai1, pi1);' + #10#13 +
    '   }' + #10#13 +
    '   else' + #10#13 +
    '   {' + #10#13 +
    '      return AtmosphereColor(ai1, ai2);' + #10#13 +
    '   }' + #10#13 +
    ' }' + #10#13 +
    ' return vec4(0.0, 0.0, 0.0, 0.0);' + #10#13 +
    '}' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec4 pos = ModelMatrix * vec4(Position, 1.0);' + #10#13 +
    ' lightingVector = normalize(sunPosition - pos.xyz);' + #10#13 +
    '	color = ComputeColor(pos.xyz);' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * pos;' + #10#13 +
    '}';

  Atmosphere_fp150: AnsiString =
    '#version 150' + #10#13 +
    'precision highp float;'+#10#13+
    'in vec4 color;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	FragColor = color;' + #10#13 +
    '}';
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

var
  AtmosphereShader: TGLProgramHandle = nil;
  ProgramWorks: Boolean = true;

constructor TGL3xCustomAtmosphere.Create(AOwner: TComponent);
begin
  inherited;
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
  if GL_VERSION_2_1 then
  begin
    // Render self
    if ARenderSelf then
    begin
      if AtmosphereShader.Handle = 0 then
      begin
        AtmosphereShader.AllocateHandle;
        with AtmosphereShader do
        begin
          if GL_VERSION_3_2 then
          begin
            AddShader(TGLVertexShaderHandle, string(Atmosphere_vp150), true);
            AddShader(TGLFragmentShaderHandle, string(Atmosphere_fp150), true);
          end
          else begin
            AddShader(TGLVertexShaderHandle, string(Atmosphere_vp120), true);
            AddShader(TGLFragmentShaderHandle, string(Atmosphere_fp120), true);
          end;
          if not LinkProgram then
            ProgramWorks := false;
          if not ValidateProgram then
            ProgramWorks := false;
        end;
      end;

      if ProgramWorks then
      begin
        XAxis := VectorSubtract(AbsolutePosition, ARci.CameraPosition);
        NormalizeVector(XAxis);
        ZAxis := VectorCrossProduct(ARci.CameraUp, XAxis);
        NormalizeVector(ZAxis);
        YAxis := VectorCrossProduct(XAxis, ZAxis);
        NormalizeVector(YAxis);
        Basis[0] := XAxis;
        Basis[1] := YAxis;
        Basis[2] := ZAxis;
        Basis[3] := NullHmgPoint;
        M := MatrixMultiply(Basis, Self.AbsoluteMatrix);
        if Assigned(FSun) then
          sunPos := FSun.AbsolutePosition
        else
          sunPos := ARci.CameraPosition;

        with AtmosphereShader do
        begin
          UseProgramObject;
          // All calculations are doing in absolute space
          UniformMatrix4fv['ModelMatrix'] := M;
          UniformMatrix4fv['ViewProjectionMatrix'] := MatrixMultiply(
            TGLSceneBuffer(ARci.buffer).ViewMatrix,
            TGLSceneBuffer(ARci.buffer).ProjectionMatrix);

          Uniform3f['eyePosition'] := AffineVectorMake(ARci.CameraPosition);
          Uniform3f['spherePosition'] := AffineVectorMake(AbsolutePosition);
          Uniform3f['sunPosition'] := AffineVectorMake(sunPos);
          Uniform1f['invAtmosphereHeight'] :=
            1 / (FAtmosphereRadius - FPlanetRadius);
          Uniform1f['Opacity'] := FOpacity;
          Uniform3f['LowAtmColor'] := AffineVectorMake(FLowAtmColor.Color);
          Uniform3f['HighAtmColor'] := AffineVectorMake(FHighAtmColor.Color);
          Uniform1f['PlanetRadius'] := FPlanetRadius;
          Uniform1f['AtmosphereRadius'] := FAtmosphereRadius;

          FBuiltProperties.Usage := FBuiltProperties.Usage;

          if not FBuiltProperties.Manager.IsBuilded then
          begin
            try
              Self.BuildList(ARci);
            except
              FBuiltProperties.Manager.Discard;
              Self.Visible := false;
            end;
            ObjectStyle := ObjectStyle - [osBuiltStage];
          end
          else begin
            ARci.GLStates.EnableDepthTest := False;
            ARci.GLStates.Enable(stBlend);
            EnableGLBlendingMode(ARci);
            FBuiltProperties.Manager.RenderClient(FBuiltProperties, ARci);
          end;

          EndUseProgramObject;
        end;
      end;
    end;
  end;
  // Render children
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TGL3xCustomAtmosphere.BuildList(var rci: TRenderContextInfo);
var
  radius: Single;
  I, J, k0, k1, kt: Integer;
begin
  if FSun <> nil then
  begin
    Assert(FAtmosphereRadius > FPlanetRadius);

    with BuiltProperties.Manager do
    begin
      BeginObject(BuiltProperties);
      for I := 0 to 13 do
      begin
        if I < 5 then
          radius := FPlanetRadius * Sqrt(I * (1 / 5))
        else
          radius := FPlanetRadius + (I - 5.1) * (FAtmosphereRadius -
            FPlanetRadius)
            * (1 / 6.9);
        k0 := (I and 1) * (FSlices + 1);
        k1 := (FSlices + 1) - k0;
        for J := 0 to FSlices do
        begin
          VectorCombine(ZHmgVector, YHmgVector,
            cosCache[J] * radius, sinCache[J] * radius, VertexCash[k0 + J]);
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
              Vertex(VertexCash[kt][0], VertexCash[kt][1], VertexCash[kt][2]);
              kt := k0 + J;
              Vertex(VertexCash[kt][0], VertexCash[kt][1], VertexCash[kt][2]);
            end;
            RestartStrip;
          end
          else
          begin
            for J := FSlices downto 0 do
            begin
              kt := k1 + J;
              Vertex(VertexCash[kt][0], VertexCash[kt][1], VertexCash[kt][2]);
              kt := k0 + J;
              Vertex(VertexCash[kt][0], VertexCash[kt][1], VertexCash[kt][2]);
            end;
            RestartStrip;
          end;
        end
        else if I = 1 then
        begin
          BeginPrimitives(GLVBOM_TRIANGLE_FAN);
          Vertex(VertexCash[k1][0], VertexCash[k1][1], VertexCash[k1][2]);
          for J := k0 + FSlices downto k0 do
            Vertex(VertexCash[J][0], VertexCash[J][1], VertexCash[J][2]);
          EndPrimitives;
          BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
        end;
      end;
      EndPrimitives;
      EndObject;
    end;
  end;
  inherited;
end;

procedure TGL3xCustomAtmosphere.TogleBlendingMode;
begin
  if FBlendingMode = abmOneMinusSrcAlpha then
    FBlendingMode := abmOneMinusDstColor
  else
    FBlendingMode := abmOneMinusSrcAlpha;
end;

procedure TGL3xCustomAtmosphere.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGL3xCustomAtmosphere then
  begin
    Assert(not FBuiltProperties.Manager.IsBuilded, glsCanNotRebuild);
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
  Assert(not FBuiltProperties.Manager.IsBuilded, glsCanNotRebuild);
  FAtmosphereRadius := Value;
  if Value <= FPlanetRadius then
    FPlanetRadius := FAtmosphereRadius / 1.01;
end;

procedure TGL3xCustomAtmosphere.SetPlanetRadius(const Value: Single);
begin
  Assert(not FBuiltProperties.Manager.IsBuilded, glsCanNotRebuild);
  FPlanetRadius := Value;
  if Value >= FAtmosphereRadius then
    FAtmosphereRadius := FPlanetRadius * 1.01;
end;

procedure TGL3xCustomAtmosphere.EnableGLBlendingMode(
  var rci: TRenderContextInfo);
begin
  with rci.GLStates do
    case FBlendingMode of
      abmOneMinusDstColor: SetBlendFunc(bfDstAlpha,
        bfOneMinusDstColor);
      abmOneMinusSrcAlpha: SetBlendFunc(bfDstAlpha,
        bfOneMinusSrcAlpha);
    else
      Assert(False, glsErrorEx + glsUnknownType);
    end;
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
  Assert(not FBuiltProperties.Manager.IsBuilded, glsCanNotRebuild);
  if Value > 0 then
  begin
    FSlices := Value;
    SetLength(cosCache, FSlices + 1);
    SetLength(sinCache, FSlices + 1);
    PrepareSinCosCache(sinCache, cosCache, 0, 360);

    GetMem(VertexCash, 2 * (FSlices + 1) * SizeOf(TVector));
  end
  else
    raise EGLBAtmosphereException.Create('Slices must be more than0!');
end;

procedure TGL3xCustomAtmosphere.SetOptimalAtmosphere(const ARadius: Single);
begin
  Assert(not FBuiltProperties.Manager.IsBuilded, glsCanNotRebuild);
  FAtmosphereRadius := ARadius + 0.25;
  FPlanetRadius := ARadius - 0.07;
end;

procedure TGL3xCustomAtmosphere.SetOptimalAtmosphere2(const ARadius: Single);
begin
  Assert(not FBuiltProperties.Manager.IsBuilded, glsCanNotRebuild);
  FAtmosphereRadius := ARadius + ARadius / 15;
  FPlanetRadius := ARadius - ARadius / 50;
end;

initialization
  RegisterClasses([TGL3xCustomAtmosphere, TGL3xAtmosphere]);
  AtmosphereShader := TGLProgramHandle.Create;

finalization

  AtmosphereShader.Destroy;
  AtmosphereShader := nil;

end.


//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   This unit contains classes that imitate an atmosphere around a planet.<p>
    Comments:
      1) Eats a lot of CPU (reduces FPS from 1240 to 520 on my PC with cSlices=100)
      2) Alpha in LowAtmColor, HighAtmColor is ignored.
 
}

unit GLS.Atmosphere;

interface

{$I GLScene.inc}

uses
  System.SysUtils, System.Classes,
  //GLS
  GLS.Scene, GLS.Objects, GLS.Cadencer, GLS.OpenGLTokens, GLS.VectorGeometry,
  GLS.Context, GLS.Strings, GLS.Color, GLS.RenderContextInfo, GLS.State,
  GLS.CrossPlatform, GLS.VectorTypes;

type
   EGLAtmosphereException = class(Exception);

   {
   With aabmOneMinusSrcAlpha atmosphere is transparent to other objects,
   but has problems, which are best seen when the Atmosphere radius is big.

   With bmOneMinusDstColor atmosphere doesn't have these problems, but offers
   limited transparency (when you look closely on the side).
  }
  TVKAtmosphereBlendingMode = (abmOneMinusDstColor, abmOneMinusSrcAlpha);

  { This class imitates an atmosphere around a planet. }
  TVKCustomAtmosphere = class(TVKBaseSceneObject)
  private
    // Used in DoRenderl
    cosCache, sinCache: array of Single;
    pVertex, pColor: PVectorArray;

    FSlices: Integer;
    FBlendingMode: TVKAtmosphereBlendingMode;
    FPlanetRadius: Single;
    FAtmosphereRadius: Single;
    FOpacity: Single;
    FLowAtmColor: TVKColor;
    FHighAtmColor: TVKColor;
    FSun: TVKBaseSceneObject;
    procedure SetSun(const Value: TVKBaseSceneObject);
    procedure SetAtmosphereRadius(const Value: Single);
    procedure SetPlanetRadius(const Value: Single);
    procedure EnableGLBlendingMode(StateCache: TVKStateCache);
    function StoreAtmosphereRadius: Boolean;
    function StoreOpacity: Boolean;
    function StorePlanetRadius: Boolean;
    procedure SetSlices(const Value: Integer);
    procedure SetLowAtmColor(const AValue: TVKColor);
    procedure SetHighAtmColor(const AValue: TVKColor);
    function StoreLowAtmColor: Boolean;
    function StoreHighAtmColor: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Sun: TVKBaseSceneObject read FSun write SetSun;

    property Slices: Integer read FSlices write SetSlices default 60;
    property Opacity: Single read FOpacity write FOpacity stored StoreOpacity;

    // AtmosphereRadius > PlanetRadius!!!
    property AtmosphereRadius: Single read FAtmosphereRadius write SetAtmosphereRadius stored StoreAtmosphereRadius;
    property PlanetRadius: Single read FPlanetRadius write SetPlanetRadius stored StorePlanetRadius;

    // Use value slightly lower than actual radius, for antialiasing effect.
    property LowAtmColor: TVKColor read FLowAtmColor write SetLowAtmColor stored StoreLowAtmColor;
    property HighAtmColor: TVKColor read FHighAtmColor write SetHighAtmColor stored StoreHighAtmColor;
    property BlendingMode: TVKAtmosphereBlendingMode read FBlendingMode
                               write FBlendingMode default abmOneMinusSrcAlpha;

    procedure SetOptimalAtmosphere(const ARadius: Single);  //absolute
    procedure SetOptimalAtmosphere2(const ARadius: Single); //relative
    procedure TogleBlendingMode; //changes between 2 blending modes

    // Standard component stuff.
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Main rendering procedure.
    procedure DoRender(var rci: TRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    // Used to determine extents.
    function AxisAlignedDimensionsUnscaled : TVector; override;
  end;

  TVKAtmosphere = class(TVKCustomAtmosphere)
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

const
  EPS = 0.0001;
  cIntDivTable: array [2..20] of Single =
    (1 / 2, 1 / 3, 1 / 4, 1 / 5, 1 / 6, 1 / 7, 1 / 8, 1 / 9, 1 / 10,
    1 / 11, 1 / 12, 1 / 13, 1 / 14, 1 / 15, 1 / 16, 1 / 17, 1 / 18, 1 / 19, 1 / 20);

procedure TVKCustomAtmosphere.SetOptimalAtmosphere(const ARadius: Single);
begin
  FAtmosphereRadius := ARadius + 0.25;
  FPlanetRadius := ARadius - 0.07;
end;


procedure TVKCustomAtmosphere.SetOptimalAtmosphere2(const ARadius: Single);
begin
  FAtmosphereRadius := ARadius + ARadius / 15;
  FPlanetRadius := ARadius - ARadius / 50;
end;

constructor TVKCustomAtmosphere.Create(AOwner: TComponent);
begin
  inherited;
  FLowAtmColor := TVKColor.Create(Self);
  FHighAtmColor := TVKColor.Create(Self);

  FOpacity := 2.1;
  SetSlices(60);
  FAtmosphereRadius := 3.55;
  FPlanetRadius := 3.395;
  FLowAtmColor.Color := VectorMake(1, 1, 1, 1);
  FHighAtmColor.Color := VectorMake(0, 0, 1, 1);

  FBlendingMode := abmOneMinusSrcAlpha;
end;


destructor TVKCustomAtmosphere.Destroy;
begin
  FLowAtmColor.Free;
  FHighAtmColor.Free;
  FreeMem(pVertex);
  FreeMem(pColor);
  inherited;
end;


procedure TVKCustomAtmosphere.DoRender(var rci: TRenderContextInfo; renderSelf, renderChildren: Boolean);
var
  radius, invAtmosphereHeight:    Single;
  sunPos, eyePos, lightingVector: TVector;
  diskNormal, diskRight, diskUp:  TVector;


  function AtmosphereColor(const rayStart, rayEnd: TVector): TColorVector;
  var
    I, n:     Integer;
    atmPoint, normal: TVector;
    altColor: TColorVector;
    alt, rayLength, contrib, decay, intensity, invN: Single;
  begin
    Result := clrTransparent;
    rayLength := VectorDistance(rayStart, rayEnd);
    n := Round(3 * rayLength * invAtmosphereHeight) + 2;
    if n > 10 then
      n := 10;
    invN := cIntDivTable[n];//1/n;
    contrib := rayLength * invN * Opacity;
    decay := 1 - contrib * 0.5;
    contrib := contrib * (1 / 1.1);
    for I := n - 1 downto 0 do
    begin
      VectorLerp(rayStart, rayEnd, I * invN, atmPoint);
      // diffuse lighting normal
      normal := VectorNormalize(atmPoint);
      // diffuse lighting intensity
      intensity := VectorDotProduct(normal, lightingVector) + 0.1;
      if PInteger(@intensity)^ > 0 then
      begin
        // sample on the lit side
        intensity := intensity * contrib;
        alt := (VectorLength(atmPoint) - FPlanetRadius) * invAtmosphereHeight;
        VectorLerp(LowAtmColor.Color, HighAtmColor.Color, alt, altColor);
        Result.V[0] := Result.V[0] * decay + altColor.V[0] * intensity;
        Result.V[1] := Result.V[1] * decay + altColor.V[1] * intensity;
        Result.V[2] := Result.V[2] * decay + altColor.V[2] * intensity;
      end
      else
      begin
        // sample on the dark sid
        Result.V[0] := Result.V[0] * decay;
        Result.V[1] := Result.V[1] * decay;
        Result.V[2] := Result.V[2] * decay;
      end;
    end;
    Result.V[3] := n * contrib * Opacity * 0.1;
  end;


  function ComputeColor(var rayDest: TVector; mayHitGround: Boolean): TColorVector;
  var
    ai1, ai2, pi1, pi2: TVector;
    rayVector: TVector;
  begin
    rayVector := VectorNormalize(VectorSubtract(rayDest, eyePos));
    if RayCastSphereIntersect(eyePos, rayVector, NullHmgPoint,
      FAtmosphereRadius, ai1, ai2) > 1 then
    begin
      // atmosphere hit
      if mayHitGround and (RayCastSphereIntersect(eyePos, rayVector,
        NullHmgPoint, FPlanetRadius, pi1, pi2) > 0) then
      begin
        // hit ground
        Result := AtmosphereColor(ai1, pi1);
      end
      else
      begin
        // through atmosphere only
        Result := AtmosphereColor(ai1, ai2);
      end;
      rayDest := ai1;
    end
    else
      Result := clrTransparent;
  end;

var
  I, J, k0, k1:    Integer;
begin
  if FSun <> nil then
  begin
    Assert(FAtmosphereRadius > FPlanetRadius);

    sunPos := VectorSubtract(FSun.AbsolutePosition, AbsolutePosition);
    eyepos := VectorSubtract(rci.CameraPosition, AbsolutePosition);

    diskNormal := VectorNegate(eyePos);
    NormalizeVector(diskNormal);
    diskRight := VectorCrossProduct(rci.CameraUp, diskNormal);
    NormalizeVector(diskRight);
    diskUp := VectorCrossProduct(diskNormal, diskRight);
    NormalizeVector(diskUp);

    invAtmosphereHeight := 1 / (FAtmosphereRadius - FPlanetRadius);
    lightingVector := VectorNormalize(sunPos); // sun at infinity

    rci.GLStates.DepthWriteMask := False;
    rci.GLStates.Disable(stLighting);
    rci.GLStates.Enable(stBlend);
    EnableGLBlendingMode(rci.GLStates);
    for I := 0 to 13 do
    begin
      if I < 5 then
        radius := FPlanetRadius * Sqrt(I * (1 / 5))
      else
        radius := FPlanetRadius + (I - 5.1) * (FAtmosphereRadius - FPlanetRadius) * (1 / 6.9);
      radius := SphereVisibleRadius(VectorLength(eyePos), radius);
      k0 := (I and 1) * (FSlices + 1);
      k1 := (FSlices + 1) - k0;
      for J := 0 to FSlices do
      begin
        VectorCombine(diskRight, diskUp,
          cosCache[J] * radius, sinCache[J] * radius,
          pVertex[k0 + J]);
        if I < 13 then
          pColor[k0 + J] := ComputeColor(pVertex[k0 + J], I <= 7);
        if I = 0 then
          Break;
      end;

      if I > 1 then
      begin
        if I = 13 then
        begin
          // GL.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          GL.Begin_(GL_QUAD_STRIP);
          for J := FSlices downto 0 do
          begin
            GL.Color4fv(@pColor[k1 + J]);
            GL.Vertex3fv(@pVertex[k1 + J]);
            GL.Color4fv(@clrTransparent);
            GL.Vertex3fv(@pVertex[k0 + J]);
          end;
          GL.End_;
        end
        else
        begin
          // GL.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_DST_COLOR);
          GL.Begin_(GL_QUAD_STRIP);
          for J := FSlices downto 0 do
          begin
            GL.Color4fv(@pColor[k1 + J]);
            GL.Vertex3fv(@pVertex[k1 + J]);
            GL.Color4fv(@pColor[k0 + J]);
            GL.Vertex3fv(@pVertex[k0 + J]);
          end;
          GL.End_;
        end;
      end
      else if I = 1 then
      begin
        //GL.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        GL.Begin_(GL_TRIANGLE_FAN);
        GL.Color4fv(@pColor[k1]);
        GL.Vertex3fv(@pVertex[k1]);
        for J := k0 + FSlices downto k0 do
        begin
          GL.Color4fv(@pColor[J]);
          GL.Vertex3fv(@pVertex[J]);
        end;
        GL.End_;
      end;
    end;
  end;
  inherited;
end;

procedure TVKCustomAtmosphere.TogleBlendingMode;
begin
  if FBlendingMode = abmOneMinusSrcAlpha then
    FBlendingMode := abmOneMinusDstColor
  else
    FBlendingMode := abmOneMinusSrcAlpha;
end;

procedure TVKCustomAtmosphere.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVKCustomAtmosphere then
  begin
    SetSlices(TVKCustomAtmosphere(Source).FSlices);
    FOpacity := TVKCustomAtmosphere(Source).FOpacity;
    FAtmosphereRadius := TVKCustomAtmosphere(Source).FAtmosphereRadius;
    FPlanetRadius := TVKCustomAtmosphere(Source).FPlanetRadius;
    FLowAtmColor.Color := TVKCustomAtmosphere(Source).FLowAtmColor.Color;
    FHighAtmColor.Color := TVKCustomAtmosphere(Source).FHighAtmColor.Color;
    FBlendingMode := TVKCustomAtmosphere(Source).FBlendingMode;
    SetSun(TVKCustomAtmosphere(Source).FSun);
  end;
end;

procedure TVKCustomAtmosphere.SetSun(const Value: TVKBaseSceneObject);
begin
  if FSun <> nil then FSun.RemoveFreeNotification(Self);
  FSun := Value;
  if FSun <> nil then FSun.FreeNotification(Self);
end;

function TVKCustomAtmosphere.AxisAlignedDimensionsUnscaled : TVector;
begin
  Result.V[0] := FAtmosphereRadius;
  Result.V[1] := Result.V[0];
  Result.V[2] := Result.V[0];
  Result.V[3] := 0;
end;

procedure TVKCustomAtmosphere.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSun) then
    FSun := nil;
end;

procedure TVKCustomAtmosphere.SetAtmosphereRadius(
  const Value: Single);
begin
  FAtmosphereRadius := Value;
  if Value <= FPlanetRadius then
    FPlanetRadius := FAtmosphereRadius / 1.01;
end;

procedure TVKCustomAtmosphere.SetPlanetRadius(const Value: Single);
begin
  FPlanetRadius := Value;
  if Value >= FAtmosphereRadius then
    FAtmosphereRadius := FPlanetRadius * 1.01;
end;

procedure TVKCustomAtmosphere.EnableGLBlendingMode(StateCache: TVKStateCache);
begin
  case FBlendingMode of
    abmOneMinusDstColor:
      StateCache.SetBlendFunc(bfDstAlpha, bfOneMinusDstColor);
    abmOneMinusSrcAlpha:
      StateCache.SetBlendFunc(bfDstAlpha, bfOneMinusSrcAlpha);
  else
    Assert(False, vksErrorEx + vksUnknownType);
  end;
  StateCache.Enable(stAlphaTest);
end;

function TVKCustomAtmosphere.StoreAtmosphereRadius: Boolean;
begin
  Result := Abs(FAtmosphereRadius - 3.55) > EPS;
end;

function TVKCustomAtmosphere.StoreOpacity: Boolean;
begin
  Result := Abs(FOpacity - 2.1) > EPS;
end;

function TVKCustomAtmosphere.StorePlanetRadius: Boolean;
begin
  Result := Abs(FPlanetRadius - 3.395) > EPS;
end;

procedure TVKCustomAtmosphere.SetSlices(const Value: Integer);
begin
  if Value > 0 then
  begin
    FSlices := Value;
    SetLength(cosCache, FSlices + 1);
    SetLength(sinCache, FSlices + 1);
    PrepareSinCosCache(sinCache, cosCache, 0, 360);

    GetMem(pVertex, 2 * (FSlices + 1) * SizeOf(TVector));
    GetMem(pColor, 2 * (FSlices + 1) * SizeOf(TVector));
  end
  else
    raise EGLAtmosphereException.Create('Slices must be more than0!');
end;

procedure TVKCustomAtmosphere.SetHighAtmColor(const AValue: TVKColor);
begin
  FHighAtmColor.Assign(AValue);
end;

procedure TVKCustomAtmosphere.SetLowAtmColor(const AValue: TVKColor);
begin
  FLowAtmColor.Assign(AValue);
end;

function TVKCustomAtmosphere.StoreHighAtmColor: Boolean;
begin
  Result := not VectorEquals(FHighAtmColor.Color, VectorMake(0, 0, 1, 1));
end;

function TVKCustomAtmosphere.StoreLowAtmColor: Boolean;
begin
  Result := not VectorEquals(FLowAtmColor.Color, VectorMake(1, 1, 1, 1));
end;

initialization
  RegisterClasses([TVKCustomAtmosphere, TVKAtmosphere]);

end.


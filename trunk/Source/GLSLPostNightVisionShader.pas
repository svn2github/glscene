//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLPostThermalVisionShader <p>

   A shader that simulate a Night Vision of the scene throw a mask if enabled,
   or of the entire scene.<p>

   <b>History : </b><font size=-1><ul>
      <li>01/11/15 - J.Delauney - Initial version (contributed to GLScene)

}
unit GLSLPostNightVisionShader;

interface

{$I GLScene.inc}

uses
  System.Classes,
  // GLS
  GLTexture, GLScene, GLVectorGeometry, GLContext,
  GLSLShader, GLCustomShader, GLRenderContextInfo, GLTextureFormat,  GLMaterial,
  GLVectorTypes;


type
  TGLCustomGLSLPostNightVisionShader = class(TGLCustomGLSLShader, IGLPostShader)
  private

    FMaterialLibrary: TGLAbstractMaterialLibrary;

    FLuminanceThreshold: Single;
    FColorAmplification:Single;
    FElapsedTime : Single;
    FUseMask : Integer;
    FNoiseTex : TGLTexture;
    FMaskTex : TGLTexture;
    FNoiseTexName  : TGLLibMaterialName;
    FMaskTexName        : TGLLibMaterialName;

    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;

    function StoreLuminanceThreshold: Boolean;
    function StoreColorAmplification: Boolean;

    procedure SetMaskTexTexture(const Value: TGLTexture);
    procedure SetNoiseTexTexture(const Value: TGLTexture);

    function GetNoiseTexName: TGLLibMaterialName;
    procedure SetNoiseTexName(const Value: TGLLibMaterialName);
    function GetMaskTexName: TGLLibMaterialName;
    procedure SetMaskTexName(const Value: TGLLibMaterialName);

    function GetMaterialLibrary: TGLAbstractMaterialLibrary;

  protected
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;

    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;

    property LuminanceThreshold: Single read FLuminanceThreshold write FLuminanceThreshold stored StoreLuminanceThreshold;
    property ColorAmplification: Single read FColorAmplification write FColorAmplification stored StoreColorAmplification;
    property ElapsedTime: Single read FElapsedTime write FElapsedTime stored false;

    property MaterialLibrary: TGLAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property NoiseTex: TGLTexture read FNoiseTex write SetNoiseTexTexture;
    property NoiseTexName: TGLLibMaterialName read GetNoiseTexName write SetNoiseTexName;
    property MaskTex: TGLTexture read FMaskTex write SetMaskTexTexture;
    property MaskTexName: TGLLibMaterialName read GetMaskTexName write SetMaskTexName;
    property UseMask : Integer read FUseMask write FUseMask;
  end;

  TGLSLPostNightVisionShader = class(TGLCustomGLSLPostNightVisionShader)
  published
    property LuminanceThreshold;
    property ColorAmplification;
    property ElapsedTime;
    property MaterialLibrary;
    property NoiseTexName;
    property MaskTexName;
    property UseMask;

  end;

//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
implementation
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------

{ TGLCustomGLSLPostThermalVisionShader }

constructor TGLCustomGLSLPostNightVisionShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float luminanceThreshold; ');
   Add('uniform float colorAmplification; ');
   Add('uniform float elapsedTime; ');
   Add('uniform int useMask;');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2D noiseTex; ');
   Add('uniform sampler2D maskTex; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('void main () ');
   Add('{ ');
   Add('  vec2 uv = vTexCoord * ScreenExtents; ');
   Add('  vec4 finalColor; ');
   Add('  vec2 uvv; ');
   Add('  uvv.x = 0.4*sin(elapsedTime*50.0); ');
   Add('  uvv.y = 0.4*cos(elapsedTime*50.0); ');
   Add('  float m = 1; ');
   Add('  if (useMask==1) { m = texture2D(maskTex, uv.st).r; } ');  // Problem Here I don't know how to solve ????
   Add('  vec3 n = texture2D(noiseTex,(uv.st*3.5) + uvv).rgb; ');
   Add('  vec3 c = texture2DRect(ScreenTex, uv.st+(n.xy*0.005)).rgb; ');
   Add('    float lum = dot(vec3(0.30, 0.59, 0.11), c); ');
   Add('    if (lum < luminanceThreshold) ');
   Add('        c *= colorAmplification; ');
   Add('  vec3 visionColor = vec3(0.1, 0.95, 0.2); ');
   Add('  finalColor.rgb = (c + (n*0.2)) * visionColor * m; ');
   Add(' ');
   Add('  gl_FragColor.rgb = finalColor.rgb; ');
   Add('  gl_FragColor.a = 1.0; ');
   Add('} ');

  end;

  FLuminanceThreshold := 0.2;
  FColorAmplification := 4.0;
  FElapsedTime:=0.1;
  FUseMask:=0; // Shader not working if we want to use mask

end;

procedure TGLCustomGLSLPostNightVisionShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['luminanceThreshold'] := FLuminanceThreshold;
  GetGLSLProg.Uniform1f['colorAmplification'] := FColorAmplification;
  GetGLSLProg.Uniform1f['elapsedTime'] := FElapsedTime;
  GetGLSLProg.Uniform1i['useMask'] := FUseMask;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);
  GetGLSLProg.UniformTextureHandle['noiseTex',0,ttTexture2D]:= FNoiseTex.Handle;
  GetGLSLProg.UniformTextureHandle['maskTex',1,ttTexture2D]:= FMaskTex.Handle;

end;

function TGLCustomGLSLPostNightVisionShader.DoUnApply(
  var rci: TRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostNightVisionShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[7, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostNightVisionShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;


function TGLCustomGLSLPostNightVisionShader.StoreLuminanceThreshold: Boolean;
begin
  Result := Abs(FLuminanceThreshold - 0.1) > 0.00001;
end;

function TGLCustomGLSLPostNightVisionShader.StoreColorAmplification: Boolean;
begin
  Result := Abs(FColorAmplification - 0.1) > 0.00001;
end;


procedure TGLCustomGLSLPostNightVisionShader.SetMaskTexTexture(const Value: TGLTexture);
begin
  if FMaskTex = Value then Exit;
  MaskTex := Value;
  NotifyChange(Self)
end;

procedure TGLCustomGLSLPostNightVisionShader.SetNoiseTexTexture(const Value: TGLTexture);
begin
  if FNoiseTex = Value then Exit;
  NoiseTex := Value;
  NotifyChange(Self);
end;

function TGLCustomGLSLPostNightVisionShader.GetNoiseTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TGLCustomGLSLPostNightVisionShader.SetNoiseTexName(const Value: TGLLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TGLCustomGLSLPostNightVisionShader.GetMaskTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMaskTex);
  if Result = '' then Result := FMaskTexName;
end;

procedure TGLCustomGLSLPostNightVisionShader.SetMaskTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMaskTexName = Value then Exit;
  FMaskTexName  := Value;

  FMaskTex := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FMaskTexName);
  NotifyChange(Self);
end;

function TGLCustomGLSLPostNightVisionShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLCustomGLSLPostNightVisionShader.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLCustomGLSLPostNightVisionShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        // Need to nil the textures that were owned by it
        if FNoiseTex <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FNoiseTex);
          if Index <> -1 then
            SetNoiseTexTexture(nil);
        end;

        if FMaskTex <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMaskTex);
          if Index <> -1 then
            SetMaskTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

end.


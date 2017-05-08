//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    This is a collection of GLSL Diffuse Specular shaders, comes in these variaties
              (to know what these suffixes and prefixes mean see VXS.CustomShader.pas):
      - TVXSLDiffuseSpecularShader
      - TVXSLDiffuseSpecularShaderMT

      - TVXSLMLDiffuseSpecularShader
      - TVXSLMLDiffuseSpecularShaderMT

    Notes:
     1) TVXSLDiffuseSpecularShader takes all Material parameters directly
      from OpenGL (that includes TVXMaterial's)
     2) TVXSLDiffuseSpecularShader takes all Light parameters directly
      from OpenGL (that includes TVXLightSource's)
}

unit VXS.GLSLDiffuseSpecularShader;

interface

{$I VXScene.inc}

uses
  System.Classes, System.SysUtils,
  
  VXS.Texture, VXS.Scene, VXS.VectorGeometry, Winapi.OpenGL, Winapi.OpenGLext,  VXS.Strings,
  VXS.CustomShader, VXS.GLSLShader, VXS.Color, VXS.RenderContextInfo, VXS.Material;

type
  EGLSLDiffuseSpecularShaderException = class(EGLSLShaderException);

  // Abstract class.
  TVXBaseCustomGLSLDiffuseSpecular = class(TVXCustomGLSLShader)
  private
    FLightPower: Single;
    FRealisticSpecular: Boolean;
    FFogSupport: TVXShaderFogSupport;
    procedure SetRealisticSpecular(const Value: Boolean);
    procedure SetFogSupport(const Value: TVXShaderFogSupport);
  protected
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    property LightPower: Single read FLightPower write FLightPower;
    property RealisticSpecular: Boolean read FRealisticSpecular write SetRealisticSpecular;

    // User can disable fog support and save some FPS if he doesn't need it.
    property FogSupport: TVXShaderFogSupport read FFogSupport write SetFogSupport default sfsAuto;
  end;

  // Abstract class.
  TVXBaseGLSLDiffuseSpecularShaderMT = class(TVXBaseCustomGLSLDiffuseSpecular, IGLMaterialLibrarySupported)
  private
    FMaterialLibrary: TVXMaterialLibrary;
    FMainTexture: TVXTexture;
    FMainTextureName: TVXLibMaterialName;
    function GetMainTextureName: TVXLibMaterialName;
    procedure SetMainTextureName(const Value: TVXLibMaterialName);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
  protected
    procedure SetMaterialLibrary(const Value: TVXMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
  public
    property MainTexture: TVXTexture read FMainTexture write FMainTexture;
    property MainTextureName: TVXLibMaterialName read GetMainTextureName write SetMainTextureName;
    property MaterialLibrary: TVXMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
  end;

                     {********  Single Light  ************}

  TVXCustomGLSLDiffuseSpecularShader = class(TVXBaseCustomGLSLDiffuseSpecular)
  protected
    procedure DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
  end;


  TVXCustomGLSLDiffuseSpecularShaderMT = class(TVXBaseGLSLDiffuseSpecularShaderMT)
  protected
    procedure DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject); override;
  end;

                     {********  Multi Light  ************}

  { Note: probably LightCount should be replaced by LightSources, like in
     GLSLBumpShader.pas }

  TLightRecord = record
    Enabled: Boolean;
    Style: TLightStyle;
  end;

  TVXCustomGLSLMLDiffuseSpecularShader = class(TVXBaseCustomGLSLDiffuseSpecular)
  private
    FLightTrace: array[0..7] of TLightRecord;
  protected
    procedure DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TVXRenderContextInfo; Sender: TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
  end;

  TVXCustomGLSLMLDiffuseSpecularShaderMT = class(TVXBaseGLSLDiffuseSpecularShaderMT)
  private
    FLightTrace: array[0..7] of TLightRecord;
  protected
    procedure DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TVXRenderContextInfo; Sender: TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
  end;


                     {********  Published Stuff  ************}

  TVXSLDiffuseSpecularShaderMT = class(TVXCustomGLSLDiffuseSpecularShaderMT)
  published
    property MainTextureName;

    property LightPower;
    property FogSupport;
  end;

  TVXSLDiffuseSpecularShader = class(TVXCustomGLSLDiffuseSpecularShader)
  published
    property LightPower;
    property FogSupport;
  end;


  TVXSLMLDiffuseSpecularShaderMT = class(TVXCustomGLSLMLDiffuseSpecularShaderMT)
  published
    property MainTextureName;

    property LightPower;
    property FogSupport;
  end;

  TVXSLMLDiffuseSpecularShader = class(TVXCustomGLSLMLDiffuseSpecularShader)
  published
    property LightPower;
    property FogSupport;
  end;
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

procedure GetVertexProgramCode(const Code: TStrings;
  AFogSupport: Boolean; var rci: TVXRenderContextInfo);
begin
  with Code do
  begin
    Clear;
    Add('varying vec3 Normal; ');
    Add('varying vec4 Position; ');
    if AFogSupport then
    begin
      Add('varying float fogFactor; ');
    end;
    Add(' ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  gl_Position = ftransform(); ');
    Add('  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0; ');
    Add('  Normal = normalize(gl_NormalMatrix * gl_Normal); ');
    Add('  Position = gl_ModelViewMatrix * gl_Vertex; ');

    if AFogSupport then
    begin
    Add('  const float LOG2 = 1.442695; ');
    Add('  gl_FogFragCoord = length(Position.xyz); ');

    case TVXSceneBuffer(rci.buffer).FogEnvironment.FogMode of
      fmLinear:
        Add('  fogFactor = (gl_Fog.end - gl_FogFragCoord) * gl_Fog.scale; ');
      fmExp, // Yep, I don't know about this type, so I use fmExp2.
      fmExp2:
      begin
        Add('  fogFactor = exp2( -gl_Fog.density * ');
        Add('  				   gl_Fog.density * ');
        Add('  				   gl_FogFragCoord * ');
        Add('  				   gl_FogFragCoord * ');
        Add('  				   LOG2 ); ');
      end;
    else
      Assert(False, strUnknownType);
    end;

      Add('  fogFactor = clamp(fogFactor, 0.0, 1.0); ');
    end;

    Add('} ');
  end;
end;

procedure AddLightSub(const Code: TStrings);
begin
  with Code do
  begin
    Add('void pointLight(in int i, in vec3 normal, in vec3 eye, in vec3 ecPosition3)');
    Add('{');
    Add('   float nDotVP;       // normal . light direction');
    Add('   float nDotHV;       // normal . light half vector');
    Add('   float pf;           // power factor');
    Add('   float attenuation;  // computed attenuation factor');
    Add('   float d;            // distance from surface to light source');
    Add('   vec3  VP;           // direction from surface to light position');
    Add('   vec3  halfVector;   // direction of maximum highlights');
    Add(' ');
    Add('   // Compute vector from surface to light position');
    Add('   VP = vec3 (gl_LightSource[i].position) - ecPosition3;');
    Add(' ');
    Add('   // Compute distance between surface and light position');
    Add('   d = length(VP);');
    Add(' ');
    Add('   // Normalize the vector from surface to light position');
    Add('   VP = normalize(VP);');
    Add(' ');
    Add('   // Compute attenuation');
    Add('   attenuation = 1.0 / (gl_LightSource[i].constantAttenuation +');
    Add('       gl_LightSource[i].linearAttenuation * d +');
    Add('       gl_LightSource[i].quadraticAttenuation * d * d);');
    Add(' ');
    Add('   halfVector = normalize(VP + eye);');
    Add(' ');
    Add('   nDotVP = max(0.0, dot(normal, VP));');
    Add('   nDotHV = max(0.0, dot(normal, halfVector));');
    Add(' ');
    Add('   if (nDotVP == 0.0)');
    Add('   {');
    Add('       pf = 0.0;');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       pf = pow(nDotHV, gl_FrontMaterial.shininess);');
    Add(' ');
    Add('   }');
    Add('   Ambient  += gl_LightSource[i].ambient * attenuation;');
    Add('   Diffuse  += gl_LightSource[i].diffuse * nDotVP * attenuation;');
    Add('   Specular += gl_LightSource[i].specular * pf * attenuation;');
    Add('}');
    Add(' ');
    Add('void directionalLight(in int i, in vec3 normal)');
    Add('{');
    Add('   float nDotVP;         // normal . light direction');
    Add('   float nDotHV;         // normal . light half vector');
    Add('   float pf;             // power factor');
    Add(' ');
    Add('   nDotVP = max(0.0, dot(normal, normalize(vec3 (gl_LightSource[i].position))));');
    Add('   nDotHV = max(0.0, dot(normal, vec3 (gl_LightSource[i].halfVector)));');
    Add(' ');
    Add('   if (nDotVP == 0.0)');
    Add('   {');
    Add('       pf = 0.0;');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       pf = pow(nDotHV, gl_FrontMaterial.shininess);');
    Add(' ');
    Add('   }');
    Add('   Ambient  += gl_LightSource[i].ambient;');
    Add('   Diffuse  += gl_LightSource[i].diffuse * nDotVP;');
    Add('   Specular += gl_LightSource[i].specular * pf;');
    Add('}');
    Add('void spotLight(in int i, in vec3 normal, in vec3 eye, in vec3 ecPosition3)');
    Add('{');
    Add('   float nDotVP;            // normal . light direction');
    Add('   float nDotHV;            // normal . light half vector');
    Add('   float pf;                // power factor');
    Add('   float spotDot;           // cosine of angle between spotlight');
    Add('   float spotAttenuation;   // spotlight attenuation factor');
    Add('   float attenuation;       // computed attenuation factor');
    Add('   float d;                 // distance from surface to light source');
    Add('   vec3  VP;                // direction from surface to light position');
    Add('   vec3  halfVector;        // direction of maximum highlights');
    Add(' ');
    Add('   // Compute vector from surface to light position');
    Add('   VP = vec3 (gl_LightSource[i].position) - ecPosition3;');
    Add(' ');
    Add('   // Compute distance between surface and light position');
    Add('   d = length(VP);');
    Add(' ');
    Add('   // Normalize the vector from surface to light position');
    Add('   VP = normalize(VP);');
    Add(' ');
    Add('   // Compute attenuation');
    Add('   attenuation = 1.0 / (gl_LightSource[i].constantAttenuation +');
    Add('       gl_LightSource[i].linearAttenuation * d +');
    Add('       gl_LightSource[i].quadraticAttenuation * d * d);');
    Add(' ');
    Add('   // See if point on surface is inside cone of illumination');
    Add('   spotDot = dot(-VP, normalize(gl_LightSource[i].spotDirection));');
    Add(' ');
    Add('   if (spotDot < gl_LightSource[i].spotCosCutoff)');
    Add('   {');
    Add('       spotAttenuation = 0.0; // light adds no contribution');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       spotAttenuation = pow(spotDot, gl_LightSource[i].spotExponent);');
    Add(' ');
    Add('   }');
    Add('   // Combine the spotlight and distance attenuation.');
    Add('   attenuation *= spotAttenuation;');
    Add(' ');
    Add('   halfVector = normalize(VP + eye);');
    Add(' ');
    Add('   nDotVP = max(0.0, dot(normal, VP));');
    Add('   nDotHV = max(0.0, dot(normal, halfVector));');
    Add(' ');
    Add('   if (nDotVP == 0.0)');
    Add('   {');
    Add('       pf = 0.0;');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       pf = pow(nDotHV, gl_FrontMaterial.shininess);');
    Add(' ');
    Add('   }');
    Add('   Ambient  += gl_LightSource[i].ambient * attenuation;');
    Add('   Diffuse  += gl_LightSource[i].diffuse * nDotVP * attenuation;');
    Add('   Specular += gl_LightSource[i].specular * pf * attenuation;');
    Add(' ');
    Add('}');
    Add('void infiniteSpotLight(in int i, in vec3 normal)');
    Add('{');
    Add('   float nDotVP;         // normal . light direction');
    Add('   float nDotHV;         // normal . light half vector');
    Add('   float pf;             // power factor');
    Add('   float spotAttenuation;');
    Add('   vec3  Ppli;');
    Add('   vec3  Sdli;');
    Add(' ');
    Add('   nDotVP = max(0.0, dot(normal, normalize(vec3 (gl_LightSource[i].position))));');
    Add('   nDotHV = max(0.0, dot(normal, vec3 (gl_LightSource[i].halfVector)));');
    Add(' ');
    Add('   Ppli = -normalize(vec3(gl_LightSource[i].position));');
    Add('   Sdli = normalize(vec3(gl_LightSource[i].spotDirection));');
    Add(' ');
    Add('   spotAttenuation = pow(dot(Ppli, Sdli), gl_LightSource[i].spotExponent);');
    Add('   if (nDotVP == 0.0)');
    Add('   {');
    Add('       pf = 0.0;');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       pf = pow(nDotHV, gl_FrontMaterial.shininess);');
    Add(' ');
    Add('   }');
    Add('   Ambient  += gl_LightSource[i].ambient * spotAttenuation;');
    Add('   Diffuse  += gl_LightSource[i].diffuse * nDotVP * spotAttenuation;');
    Add('   Specular += gl_LightSource[i].specular * pf * spotAttenuation;');
    Add('}');
  end;
end;

procedure GetMLFragmentProgramCodeMid(const Code: TStrings;
  const CurrentLight: Integer; AStyle: TLightStyle);
begin
  with Code do
  begin
    case AStyle of
      lsOmni: Add(Format('  pointLight(%d, N, eye, Pos); ', [CurrentLight]));
      lsSpot: Add(Format('  spotLight(%d, N, eye, Pos); ', [CurrentLight]));
      lsParallel: Add(Format('  directionalLight(%d, N); ', [CurrentLight]));
      lsParallelSpot: Add(Format('  infiniteSpotLight(%d, N); ', [CurrentLight]));
    end;
  end;
end;

procedure GetFragmentProgramCode(const Code: TStrings;
  const ARealisticSpecular: Boolean; const AFogSupport: Boolean;
  aRci: TVXRenderContextInfo);
var
  scene: TVXScene;
begin
  with Code do
  begin
    Clear;
    Add('uniform float LightIntensity; ');
    Add('uniform sampler2D MainTexture; ');
    Add(' ');
    Add('varying vec3 Normal; ');
    Add('varying vec4 Position; ');

    if AFogSupport then
    begin
      Add('varying float fogFactor; ');
    end;

    Add('vec4 Ambient;');
    Add('vec4 Diffuse;');
    Add('vec4 Specular;');

    AddLightSub(Code);

    Add('void main(void) ');
    Add('{ ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, gl_TexCoord[0].xy); ');
    Add('  vec3 eye = vec3(0.0, 0.0, 1.0); ');
    Add('  Diffuse = vec4(0.0); ');
    Add('  Specular = vec4(0.0); ');
    Add('  Ambient = vec4(0.0); ');
    Add('  vec3 Pos = Position.xyz; ');
    Add('  vec3 N = normalize(Normal); ');
    scene := TVXScene(ARci.scene);
    if (scene.Lights.Count > 0) then
      GetMLFragmentProgramCodeMid(Code, 0,
      TVXLightSource(scene.Lights[0]).LightStyle);

    if ARealisticSpecular then
      Add('  gl_FragColor = LightIntensity * (TextureContrib * (gl_FrontLightModelProduct.sceneColor + Ambient * gl_FrontMaterial.ambient + Diffuse * gl_FrontMaterial.diffuse) + Specular * gl_FrontMaterial.specular); ')
    else
      Add('  gl_FragColor = LightIntensity * TextureContrib * (gl_FrontLightModelProduct.sceneColor + Ambient * gl_FrontMaterial.ambient + Diffuse * gl_FrontMaterial.diffuse + Specular * gl_FrontMaterial.specular); ');

    if AFogSupport then
      Add('  gl_FragColor = mix(gl_Fog.color, gl_FragColor, fogFactor);');

    Add('  gl_FragColor.a = TextureContrib.a; ');
    Add('} ');
  end;
end;

procedure GetMLFragmentProgramCodeBeg(const Code: TStrings;
  const AFogSupport: Boolean);
begin
  with Code do
  begin
    Clear;
    Add('uniform sampler2D MainTexture;');
    Add('uniform float LightIntensity; ');
    Add('uniform float SpecPower; ');
    Add('varying vec3 Normal;');
    Add('varying vec4 Position;');
    if AFogSupport then
    begin
      Add('varying float fogFactor;');
    end;
    Add('vec4 Ambient;');
    Add('vec4 Diffuse;');
    Add('vec4 Specular;');
    AddLightSub(Code);

    Add('void main(void) ');
    Add('{ ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, gl_TexCoord[0].st); ');
    Add('  vec3 eye = vec3(0.0, 0.0, 1.0); ');
    Add('  Diffuse = vec4(0.0); ');
    Add('  Specular = vec4(0.0); ');
    Add('  Ambient = vec4(0.0); ');
    Add('  vec3 Pos = Position.xyz; ');
    Add('  vec3 N = normalize(Normal); ');
  end;
end;

procedure GetMLFragmentProgramCodeEnd(const Code: TStrings;
  const ARealisticSpecular: Boolean;
  AFogSupport: Boolean);
begin
  with Code do
  begin
    if ARealisticSpecular then
      Add('  gl_FragColor = LightIntensity * (TextureContrib * (gl_FrontLightModelProduct.sceneColor + Ambient * gl_FrontMaterial.ambient + Diffuse * gl_FrontMaterial.diffuse) + Specular * gl_FrontMaterial.specular); ')
    else
      Add('  gl_FragColor = LightIntensity * TextureContrib * (gl_FrontLightModelProduct.sceneColor + Ambient * gl_FrontMaterial.ambient + Diffuse * gl_FrontMaterial.diffuse + Specular * gl_FrontMaterial.specular); ');

    if AFogSupport then
      Add('  gl_FragColor = mix(gl_Fog.color, gl_FragColor, fogFactor);');

    Add('  gl_FragColor.a = TextureContrib.a; ');
    Add('} ');
  end;
end;


{ TVXBaseCustomGLSLDiffuseSpecular }

constructor TVXBaseCustomGLSLDiffuseSpecular.Create(
  AOwner: TComponent);
begin
  inherited;

  FLightPower     := 1;
  FFogSupport := sfsAuto;
  TStringList(VertexProgram.Code).OnChange := nil;
  TStringList(FragmentProgram.Code).OnChange := nil;
  VertexProgram.Enabled := true;
  FragmentProgram.Enabled := true;
end;

procedure TVXBaseCustomGLSLDiffuseSpecular.DoApply(
  var rci: TVXRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  Param['LightIntensity'].AsVector1f := FLightPower;
end;

function TVXBaseCustomGLSLDiffuseSpecular.DoUnApply(
  var rci: TVXRenderContextInfo): Boolean;
begin
  Result := False;
  GetGLSLProg.EndUseProgramObject;
end;

procedure TVXBaseCustomGLSLDiffuseSpecular.SetFogSupport(
  const Value: TVXShaderFogSupport);
begin
  if FFogSupport <> Value then
  begin
    FFogSupport := Value;
    Self.FinalizeShader;
  end;
end;

procedure TVXBaseCustomGLSLDiffuseSpecular.SetRealisticSpecular(
  const Value: Boolean);
begin
  if FRealisticSpecular <> Value then
  begin
    FRealisticSpecular := Value;
    Self.FinalizeShader;
  end;
end;


{ TVXBaseGLSLDiffuseSpecularShaderMT }

procedure TVXBaseGLSLDiffuseSpecularShaderMT.DoApply(
  var rci: TVXRenderContextInfo; Sender: TObject);
begin
  inherited;
  Param['MainTexture'].AsTexture2D[0] := FMainTexture;
end;

function TVXBaseGLSLDiffuseSpecularShaderMT.GetMainTextureName: TVXLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfTexture(FMainTexture);
end;

function TVXBaseGLSLDiffuseSpecularShaderMT.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVXBaseGLSLDiffuseSpecularShaderMT.Notification(
  AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        //need to nil the textures that were ownned by it
        if FMainTexture <> nil then
        begin
          Index := FMaterialLibrary.Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            FMainTexture := nil;
        end;
        FMaterialLibrary := nil;
      end;
end;

procedure TVXBaseGLSLDiffuseSpecularShaderMT.SetMainTextureName(
  const Value: TVXLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FMainTextureName := Value;
    if not (csLoading in ComponentState) then
      raise EGLSLDiffuseSpecularShaderException.Create(strErrorEx + strMatLibNotDefined);
  end
  else
  begin
    FMainTexture := FMaterialLibrary.TextureByName(Value);
    FMainTextureName := '';
  end;
end;

procedure TVXBaseGLSLDiffuseSpecularShaderMT.SetMaterialLibrary(
  const Value: TVXMaterialLibrary);
begin
  if FMaterialLibrary <> nil then
    FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;

  if FMaterialLibrary <> nil then
  begin
    FMaterialLibrary.FreeNotification(Self);

    if FMainTextureName <> '' then
      SetMainTextureName(FMainTextureName);
  end
  else
    FMainTextureName := '';
end;

{ TVXCustomGLSLDiffuseSpecularShaderMT }

procedure TVXCustomGLSLDiffuseSpecularShaderMT.DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject);
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  GetFragmentProgramCode(FragmentProgram.Code, FRealisticSpecular, IsFogEnabled(FFogSupport, rci), rci);
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

{ TVXCustomGLSLDiffuseSpecularShader }

procedure TVXCustomGLSLDiffuseSpecularShader.DoApply(
  var rci: TVXRenderContextInfo; Sender: TObject);
begin
  inherited;
  Param['MainTexture'].AsVector1i := 0;  // Use the current texture.
end;

procedure TVXCustomGLSLDiffuseSpecularShader.DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject);
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  GetFragmentProgramCode(FragmentProgram.Code, FRealisticSpecular, IsFogEnabled(FFogSupport, rci), rci);
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

{ TVXCustomGLSLMLDiffuseSpecularShader }

constructor TVXCustomGLSLMLDiffuseSpecularShader.Create(
  AOwner: TComponent);
begin
  inherited;
end;

procedure TVXCustomGLSLMLDiffuseSpecularShader.DoApply(var rci: TVXRenderContextInfo; Sender: TObject);
var
  I: Integer;
  scene: TVXScene;
  needRecompile: Boolean;
begin
  scene := TVXScene(rci.scene);
  needRecompile := False;
  for I := 0 to scene.Lights.Count - 1 do
  begin
    if Assigned(scene.Lights[I]) then
    begin
      if FLightTrace[I].Enabled <> TVXLightSource(scene.Lights[I]).Shining then
      begin
        needRecompile := True;
        break;
      end;
      if FLightTrace[I].Style <> TVXLightSource(scene.Lights[I]).LightStyle then
      begin
        needRecompile := True;
        break;
      end;
    end
    else
      if FLightTrace[I].Enabled then
      begin
        needRecompile := True;
        break;
      end;
  end;
  if needRecompile then
  begin
    FinalizeShader;
    InitializeShader(rci, Sender);
  end;

  inherited;
  Param['MainTexture'].AsVector1i := 0;  // Use the current texture.
end;

procedure TVXCustomGLSLMLDiffuseSpecularShader.DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject);
var
  I: Integer;
  scene: TVXScene;
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  with FragmentProgram.Code do
  begin
    GetMLFragmentProgramCodeBeg(FragmentProgram.Code, IsFogEnabled(FFogSupport, rci));

    // Repeat for all lights.
    scene := TVXScene(rci.scene);
    for I := 0 to scene.Lights.Count - 1 do
    begin
      if Assigned(scene.Lights[I]) then
      begin
        FLightTrace[I].Enabled := TVXLightSource(scene.Lights[I]).Shining;
        FLightTrace[I].Style := TVXLightSource(scene.Lights[I]).LightStyle;
        if FLightTrace[I].Enabled then
          GetMLFragmentProgramCodeMid(FragmentProgram.Code, I, FLightTrace[I].Style);
      end
      else
        FLightTrace[I].Enabled := False;
    end;

    GetMLFragmentProgramCodeEnd(FragmentProgram.Code,
      FRealisticSpecular, IsFogEnabled(FFogSupport, rci));
  end;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited DoInitialize(rci, Sender);
end;

{ TVXCustomGLSLMLDiffuseSpecularShaderMT }

constructor TVXCustomGLSLMLDiffuseSpecularShaderMT.Create(
  AOwner: TComponent);
begin
  inherited;
end;

procedure TVXCustomGLSLMLDiffuseSpecularShaderMT.DoApply(
  var rci: TVXRenderContextInfo; Sender: TObject);
var
  I: Integer;
  scene: TVXScene;
  needRecompile: Boolean;
begin
  scene := TVXScene(rci.scene);
  needRecompile := False;
  for I := 0 to scene.Lights.Count - 1 do
  begin
    if Assigned(scene.Lights[I]) then
    begin
      if FLightTrace[I].Enabled <> TVXLightSource(scene.Lights[I]).Shining then
      begin
        needRecompile := True;
        break;
      end;
      if FLightTrace[I].Style <> TVXLightSource(scene.Lights[I]).LightStyle then
      begin
        needRecompile := True;
        break;
      end;
    end
    else
      if FLightTrace[I].Enabled then
      begin
        needRecompile := True;
        break;
      end;
  end;
  if needRecompile then
  begin
    FinalizeShader;
    InitializeShader(rci, Sender);
  end;

  inherited;
end;

procedure TVXCustomGLSLMLDiffuseSpecularShaderMT.DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject);
var
  I: Integer;
  scene: TVXScene;
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  with FragmentProgram.Code do
  begin
    GetMLFragmentProgramCodeBeg(FragmentProgram.Code, IsFogEnabled(FFogSupport, rci));

    // Repeat for all lights.
    scene := TVXScene(rci.scene);
    for I := 0 to scene.Lights.Count - 1 do
    begin
      if Assigned(scene.Lights[I]) then
      begin
        FLightTrace[I].Enabled := TVXLightSource(scene.Lights[I]).Shining;
        FLightTrace[I].Style := TVXLightSource(scene.Lights[I]).LightStyle;
        if FLightTrace[I].Enabled then
          GetMLFragmentProgramCodeMid(FragmentProgram.Code, I, FLightTrace[I].Style);
      end
      else
        FLightTrace[I].Enabled := False;
    end;

    GetMLFragmentProgramCodeEnd(FragmentProgram.Code,
      FRealisticSpecular, IsFogEnabled(FFogSupport, rci));
  end;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

initialization
  RegisterClasses([
                  TVXCustomGLSLDiffuseSpecularShader,
                  TVXCustomGLSLDiffuseSpecularShaderMT,
                  TVXCustomGLSLMLDiffuseSpecularShader,
                  TVXCustomGLSLMLDiffuseSpecularShaderMT,

                  TVXSLDiffuseSpecularShader,
                  TVXSLDiffuseSpecularShaderMT,
                  TVXSLMLDiffuseSpecularShader,
                  TVXSLMLDiffuseSpecularShaderMT
                  ]);

end.


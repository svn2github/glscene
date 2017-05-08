//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Fur shader that simulate Fur / Hair / Grass. 
   At this time only one light source is supported
}
unit VXS.GLSLFurShader;

interface

{$I VXScene.inc}

uses
  System.Classes,
  
  VXS.Scene, VXS.CrossPlatform, VXS.BaseClasses, VXS.State, Winapi.OpenGL, Winapi.OpenGLext,  VXS.OpenGL1x, 
  VXS.Context, VXS.RenderContextInfo, VXS.Coordinates, VXS.VectorGeometry, VXS.VectorTypes,
  VXS.TextureFormat, VXS.Color, VXS.Texture, VXS.Material, GLSLS.Shader, VXS.CustomShader;

type
  TVXCustomGLSLFurShader = class(TVXCustomGLSLShader)
  private

    FMaterialLibrary: TVXAbstractMaterialLibrary;

    FCurrentPass: Integer;

    FPassCount: Single;
    FFurLength: Single;
    FMaxFurLength: Single;
    FFurScale: Single;
    FRandomFurLength : Boolean;

    FColorScale: TVXColor;
    FAmbient: TVXColor;

    FGravity : TVXCoordinates;
    FLightIntensity : Single;

    FMainTex  : TVXTexture;
    FNoiseTex : TVXTexture;
    FNoiseTexName  : TVXLibMaterialName;
    FMainTexName   : TVXLibMaterialName;

    FBlendSrc : TBlendFunction;
    FBlendDst : TBlendFunction;
   // FBlendEquation : TBlendEquation;


    function GetMaterialLibrary: TVXAbstractMaterialLibrary;

    procedure SetMainTexTexture(const Value: TVXTexture);
    procedure SetNoiseTexTexture(const Value: TVXTexture);

    function GetNoiseTexName: TVXLibMaterialName;
    procedure SetNoiseTexName(const Value: TVXLibMaterialName);
    function GetMainTexName: TVXLibMaterialName;
    procedure SetMainTexName(const Value: TVXLibMaterialName);

    procedure SetGravity(APosition:TVXCoordinates);
    procedure SetAmbient(AValue: TVXColor);
    procedure SetColorScale(AValue: TVXColor);


  protected
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;

    procedure SetMaterialLibrary(const Value: TVXAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    //Common stuff
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;


    property PassCount: Single read FPassCount write FPassCount;
    property FurLength: Single read FFurLength write FFurLength;
    property MaxFurLength: Single read FMaxFurLength write FMaxFurLength;
    property FurDensity: Single read FFurScale write FFurScale;
    property RandomFurLength : Boolean read FRandomFurLength Write FRandomFurLength;

    property ColorScale: TVXColor read FColorScale Write setColorScale;
    property Ambient: TVXColor read FAmbient write setAmbient;

    property MaterialLibrary: TVXAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TVXTexture read FMainTex write SetMainTexTexture;
    property MainTextureName: TVXLibMaterialName read GetMainTexName write SetMainTexName;
    property NoiseTexture: TVXTexture read FNoiseTex write SetNoiseTexTexture;
    property NoiseTextureName: TVXLibMaterialName read GetNoiseTexName write SetNoiseTexName;

    //property BlendEquation : TBlendEquation read FBlendEquation write FBlendEquation default beMin;
    property BlendSrc  : TBlendFunction read FBlendSrc write FBlendSrc default bfSrcColor;
    property BlendDst  : TBlendFunction read FBlendDst write FBlendDst default bfOneMinusDstColor;
    property Gravity : TVXCoordinates Read FGravity write setGravity;
    property LightIntensity : Single read FLightIntensity Write FLightIntensity;


  end;

  TVXSLFurShader = class(TVXCustomGLSLFurShader)
  published
    property PassCount;
    property FurLength;
    property MaxFurLength;
    property FurDensity;
    property RandomFurLength;
    property ColorScale;

    property Ambient;
    property LightIntensity;

    property Gravity;

    property BlendSrc;
    property BlendDst;

    property MainTexture;
    property MainTextureName;
    property NoiseTexture;
    property NoiseTextureName;


  end;

implementation

{ TVXCustomGLSLFurShader }

constructor TVXCustomGLSLFurShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('uniform float fFurLength; ');
    Add('uniform float fFurMaxLength; ');
    Add('uniform float pass_index; ');
    Add('uniform int UseRandomLength; ');
    Add('uniform float fLayer; // 0 to 1 for the level ');
    Add('uniform vec3 vGravity; ');

    Add('varying vec3 normal; ');
    Add('varying vec2  vTexCoord; ');
    Add('varying vec3 lightVec; ');
   // Add('varying vec3 viewVec; ');

   Add('float rand(vec2 co){ ');
   Add(' return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453); ');
   Add('} ');

    Add('void main() ');
    Add('{ ');
    Add('  mat4 mWorld = gl_ModelViewMatrix; ');
    Add('  vec3 Normal = gl_Normal; ');
    Add('  vec4 Position = gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  normal = gl_NormalMatrix * gl_Normal; ');
    // Additional Gravit/Force Code
    Add('  vec3 vGravity2 = vGravity *mat3(mWorld ); ');
    // We use the pow function, so that only the tips of the hairs bend
    Add('  float k = pow(fLayer, 3.0); ');

    // Random the Hair length  perhaps will can use a texture map for controling.
    Add(' vec3 vNormal = normalize( Normal * mat3(mWorld )); ');
    Add(' float RandomFurLength; ');
    Add('  if (UseRandomLength == 1) { RandomFurLength = fFurLength+fFurLength*rand(vNormal.xy); } ');
    Add('  else { RandomFurLength = fFurLength ; } ');

    Add('  RandomFurLength = pass_index*(RandomFurLength * vNormal); ');
    Add('  if (RandomFurLength > fFurMaxLength ) { RandomFurLength = fFurMaxLength; } ');

    Add('  Position.xyz += RandomFurLength +(vGravity2 * k);  ');

    Add('  Position.xyz += pass_index*(fFurLength * Normal)+(vGravity2 * k);  ');
    Add('  vTexCoord = gl_MultiTexCoord0; ');
    Add('   ');
    Add('   gl_Position =  gl_ModelViewProjectionMatrix * Position; ');
    Add('  lightVec = vec3(lightPos - vert); ');

    //  Add('  viewVec = -vec3(vert); ');
    Add('normal = vNormal; ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    clear;
    Add('uniform vec4 fcolorScale; ');
    Add('uniform float pass_index; ');
    Add('uniform float fFurScale; ');
    Add('uniform vec4 vAmbient; ');
    Add('uniform float fLayer; // 0 to 1 for the level ');
    Add('uniform float vLightIntensity; ');

    Add('uniform sampler2D FurTexture; ');
    Add('uniform sampler2D ColourTexture; ');

    //textures
    Add('varying vec2 vTexCoord; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');
//    Add('varying vec3 viewVec; ');

    Add('void main() ');
    Add('{ ');
    // A Faking shadow
    Add('  vec4 fAlpha = texture2D( FurTexture, vTexCoord*fFurScale );     ');
    Add('  float fakeShadow =  mix(0.3, 1.0, fAlpha.a-fLayer); ');
    Add('     ');

    Add('  vec4 FinalColour = vec4(0.0,0.0,0.0,1.0); ');

    Add('FinalColour = (fcolorScale*texture2D( ColourTexture, vTexCoord))*fakeShadow; ');

    // This comment part it's for controling if we must draw the hair according the red channel and the alpha in NoiseMap
    // Don' t work well a this time the NoiseMap must be perfect
//    Add('float visibility = 0.0; ');
//    Add('if (pass_index == 1.0) ');
//    Add('{ ');
//    Add('   visibility = 1.0;  ');
//    Add('} ');
//    Add('else ');
//    Add('{ ');
//    Add('  if (fAlpha.a<fAlpha.r) { visibility = 0.0; } ');
//    Add('  else { visibility =mix(0.1,1.0,(1.02-fLayer)); } //-1.0; ');
//    Add('} ');

    Add('float visibility =mix(0.1,1.0,(1.02-fLayer)); ');   // The Last past must be transparent

    // Simply Lighting - For this time only ONE light source is supported
    Add('vec4 ambient = vAmbient*FinalColour;  ');
    Add('vec4 diffuse = FinalColour; ');
    Add('vec3 L = normalize(lightVec); ');
    Add('float NdotL = dot(L, normal); ');
    Add('// "Half-Lambert" technique for more pleasing diffuse term ');
    Add('diffuse = diffuse*(0.5*NdotL+0.5); ');
    Add('FinalColour = vLightIntensity*(ambient+ diffuse); // + no specular; ');
    Add('FinalColour.a = visibility ; ');
    Add('    // Return the calculated color ');
    Add('    gl_FragColor= FinalColour; ');
    Add('} ');
  end;

  //Fur stuff
  FPassCount := 16; // More is greater more the fur is dense
  FFurLength := 0.3000;  // The minimal Hair length
  FMaxFurLength := 3.0;
  FRandomFurLength := false;
  FFurScale:=1.0;

  FColorScale := TVXColor.Create(Self);
  FColorScale.SetColor(0.2196,0.2201,0.2201,1.0);

  FAmbient := TVXColor.Create(Self);
  FAmbient.SetColor(1.0,1.0,1.0,1.0);

  // The Blend Funcs are very important for realistic fur rendering it can vary follow your textures
  FBlendSrc := bfOneMinusSrcColor;
  FBlendDst := bfOneMinusSrcAlpha;
  FGravity := TVXCoordinates.Create(self);
  FGravity.AsAffineVector := AffinevectorMake(0.0,0.0,0.0);
  FLightIntensity := 2.5;
end;

destructor TVXCustomGLSLFurShader.Destroy;
begin
  Enabled:=false;
  FGravity.Free;
  FColorScale.Destroy;
  FAmbient.Destroy;
  inherited;
end;

procedure TVXCustomGLSLFurShader.DoApply(var rci: TVXRenderContextInfo;Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  //Fur stuff
  FCurrentPass := 1;

  param['pass_index'].AsVector1f := 1.0;
  param['fFurLength'].AsVector1f := FFurLength;

  param['fFurMaxLength'].AsVector1f := FMaxFurLength;
  param['fFurScale'].AsVector1f := FFurScale;
  if FRandomFurLength then param['UseRandomLength'].AsVector1i := 1
  else param['UseRandomLength'].AsVector1i := 0;

  param['fcolorScale'].AsVector4f := FColorScale.Color;
  param['FurTexture'].AsTexture2D[0] := FNoiseTex;
  param['ColourTexture'].AsTexture2D[1] := FMainTex;
  param['vGravity'].AsVector3f := FGravity.AsAffineVector;

  param['vAmbient'].AsVector4f := FAmbient.Color; //vectorMake(0.5,0.5,0.5,1.0);
  param['fLayer'].AsVector1f := 1/PassCount;
  param['vLightIntensity'].AsVector1f := FLightIntensity;


  glPushAttrib(GL_COLOR_BUFFER_BIT);
  glEnable(GL_BLEND);
  gl.BlendFunc(cGLBlendFunctionToGLEnum[FBlendSrc],cGLBlendFunctionToGLEnum[FBlendDst]);

 // GL.BlendFunc(GL_SRC_ALPHA, cGLBlendFunctionToGLEnum[FBlendSrc]);
 //GL.BlendFunc(GL_DST_ALPHA,cGLBlendFunctionToGLEnum[FBlendDst]);
 // gl.BlendEquation(cGLBlendEquationToGLEnum[BlendEquation]);

end;

function TVXCustomGLSLFurShader.DoUnApply(var rci: TVXRenderContextInfo): Boolean;
begin
  if FCurrentPass < PassCount then
  begin
    Inc(FCurrentPass);
    //GetGLSLProg.Uniform1f['pass_index'] := FCurrentPass;
    param['pass_index'].AsVector1f  := FCurrentPass;
    param['fLayer'].AsVector1f := FCurrentPass/PassCount;
    Result := True;
  end
  else
  begin
   // glActiveTextureARB(GL_TEXTURE0_ARB);
    gl.ActiveTexture(GL_TEXTURE0_ARB);
    GetGLSLProg.EndUseProgramObject;
    glPopAttrib;
    Result := False;
  end;
end;


function TVXCustomGLSLFurShader.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVXCustomGLSLFurShader.SetMaterialLibrary(const Value: TVXAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TVXAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TVXCustomGLSLFurShader.SetMainTexTexture(const Value: TVXTexture);
begin
  if FMainTex = Value then Exit;
  FMainTex := Value;
  NotifyChange(Self)
end;

procedure TVXCustomGLSLFurShader.SetNoiseTexTexture(const Value: TVXTexture);
begin
  if FNoiseTex = Value then Exit;
  FNoiseTex := Value;
  NotifyChange(Self);
end;

function TVXCustomGLSLFurShader.GetNoiseTexName: TVXLibMaterialName;
begin
  Result := TVXMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TVXCustomGLSLFurShader.SetNoiseTexName(const Value: TVXLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TVXMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TVXCustomGLSLFurShader.GetMainTexName: TVXLibMaterialName;
begin
  Result := TVXMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTex);
  if Result = '' then Result := FMainTexName;
end;

procedure TVXCustomGLSLFurShader.SetMainTexName(const Value: TVXLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTex := TVXMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


procedure TVXCustomGLSLFurShader.Notification(AComponent: TComponent; Operation: TOperation);
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
          Index := TVXMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FNoiseTex);
          if Index <> -1 then
            SetNoiseTexTexture(nil);
        end;

        if FMainTex <> nil then
        begin
          Index := TVXMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTex);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

procedure TVXCustomGLSLFurShader.SetGravity(APosition: TVXCoordinates);
begin
  FGravity.SetPoint(APosition.DirectX, APosition.DirectY, APosition.DirectZ);
end;

procedure TVXCustomGLSLFurShader.SetAmbient(AValue: TVXColor);
begin
  FAmbient.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLFurShader.SetColorScale(AValue: TVXColor);
begin
  FColorScale.DirectColor := AValue.Color;
end;

end.


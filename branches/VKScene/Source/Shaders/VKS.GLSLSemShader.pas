//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   SEM shader : Spherical Environment Mapping
   The main idea of SEM is to get the UV coordinates (which are used to lookup the matCap texture)
   from the normal vector on the fragment instead of the original texture coordinates from the object. 
    
   A material using SEM is very useful to highlight variations in the mesh: creases, bumps, even slow ondulations.
   It doesn't work that well on a cube, for instance. And does absolutely nothing on a sphere:
   SEM on a sphere is exactly the same as a planar projection of the matCap texture. 

   At this time only one light source is supported
               
}


unit VKS.GLSLSemShader;

interface

{$I VKScene.inc}

uses
  System.Classes,
  
  VKS.Scene, VKS.CrossPlatform, VKS.BaseClasses, VKS.State, Winapi.OpenGL, Winapi.OpenGLext,  VKS.OpenGL1x, 
  VKS.Context, VKS.RenderContextInfo, VKS.VectorGeometry, VKS.Coordinates, VKS.TextureFormat, 
  VKS.Color, VKS.Texture, VKS.Material, GLSL.Shader, VKS.CustomShader;

//TVKCustomGLSLSimpleSemShader
//
{ Custom class for GLSLSEMShader. 
 SEM Shader : Spherical Environment Mapping }
Type
TVKCustomGLSLSemShader = class(TVKCustomGLSLShader)
  private
    FAmbientColor: TVKColor;
//    FDiffuseColor: TVKColor;
    FSpecularColor: TVKColor;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;

    FMaterialLibrary: TVKAbstractMaterialLibrary;
    FMainTexture: TVKTexture;
    FMainTexName   : TVKLibMaterialName;

//    FSpecularPower: Single;
//    FLightPower: Single;

    function GetMaterialLibrary: TVKAbstractMaterialLibrary;

    procedure SetMainTexTexture(const Value: TVKTexture);
    function GetMainTexName: TVKLibMaterialName;
    procedure SetMainTexName(const Value: TVKLibMaterialName);

    //procedure SetDiffuseColor(AValue: TVKColor);
    procedure SetAmbientColor(AValue: TVKColor);
    procedure SetSpecularColor(AValue: TVKColor);

  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVKRenderContextInfo): Boolean; override;

    procedure SetMaterialLibrary(const Value: TVKAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

//    property DiffuseColor : TVKColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TVKColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TVKColor Read FAmbientColor Write setAmbientColor;

    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;

    property MaterialLibrary: TVKAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TVKTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TVKLibMaterialName read GetMainTexName write SetMainTexName;

//    property SpecularPower: Single read FSpecularPower write FSpecularPower;
//    property LightPower: Single read FLightPower write FLightPower;

  end;

  TVKSLSemShader = class(TVKCustomGLSLSemShader)
  published

    property AmbientColor;
//    property DiffuseColor;
    property SpecularColor;

    property AmbientFactor;
    property DiffuseFactor;
    property SpecularFactor;

    property MaterialLibrary;
    property MainTexture;
    property MainTextureName;


  end;
implementation

constructor TVKCustomGLSLSemShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');

    Add('void main() { ');
    Add('  vec4 p = gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  lightVec = vec3(lightPos - p); ');
    Add('  viewVec = -vec3(p); ');
    Add('  normal = normalize(gl_NormalMatrix * gl_Normal ); ');

    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    clear;
    Add('uniform vec4 AmbientColor; ');
    Add('uniform vec4 SpecularColor; ');
    Add('uniform float DiffuseIntensity; ');
    Add('uniform float AmbientIntensity; ');
    Add('uniform float SpecularIntensity; ');

    Add('uniform sampler2D MainTexture; ');

    Add('varying vec3 viewVec; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');

    Add('void main() { ');
    Add('  vec3 V = normalize(viewVec); ');
    Add('  vec3 r = reflect( V, normal ); ');
    Add('  float m = 2.0 * sqrt( pow( r.x, 2.0 ) + pow( r.y, 2.0 ) + pow( r.z + 1.0, 2.0 ) ); ');
    Add('  vec2 vN = r.xy / m + 0.5; ');
    Add('  vec4 DiffuseColor;    ');
    Add('  DiffuseColor = texture2D( MainTexture, vN ); //.rgb; ');

    // Simple Lighting
    Add('  vec3 L = normalize(lightVec); ');

    Add('  vec3 halfAngle = normalize(L + V); ');
    Add('  float NdotL = dot(L, normal); ');
    Add('  float NdotH = clamp(dot(halfAngle, normal), 0.0, 1.0); ');
    Add('  // "Half-Lambert" technique for more pleasing diffuse term ');
    Add('  float diffuse = DiffuseColor*(0.5 * NdotL + 0.5); ');
    Add('  float specular = pow(NdotH, 64.0); ');

    Add('  vec4 FinalColour = AmbientColor*AmbientIntensity + ');
    Add('                     DiffuseColor*diffuse*DiffuseIntensity + ');
    Add('                     SpecularColor*specular*SpecularIntensity; ');

    Add('  gl_FragColor = FinalColour; //vec4( FinalColour, 1.0 ); ');
    Add('} ');
  end;

  FAmbientColor := TVKColor.Create(Self);
  //FDiffuseColor := TVKColor.Create(Self);
  FSpecularColor := TVKColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1.0);
  //FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1.0, 1.0, 1.0, 1.0);
  FAmbientFactor  := 0.8;
  FDiffuseFactor  :=0.9;
  FSpecularFactor :=0.8;


end;

destructor TVKCustomGLSLSemShader.Destroy;
begin
  FAmbientColor.Destroy;
 // FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TVKCustomGLSLSemShader.DoApply(var rci: TVKRenderContextInfo; Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
  //Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  param['SpecularColor'].AsVector4f := FSpecularColor.Color;
  param['AmbientIntensity'].AsVector1f := FAmbientFactor;
  param['DiffuseIntensity'].AsVector1f := FDiffuseFactor;
  param['SpecularIntensity'].AsVector1f := FSpecularFactor;

//  Param['SpecPower'].AsVector1f := FSpecularPower;
//  Param['LightIntensity'].AsVector1f := FLightPower;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

function TVKCustomGLSLSemShader.DoUnApply(var rci: TVKRenderContextInfo): Boolean;
begin
  gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;


function TVKCustomGLSLSemShader.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVKCustomGLSLSemShader.SetMaterialLibrary(const Value: TVKAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TVKAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TVKCustomGLSLSemShader.SetMainTexTexture(const Value: TVKTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TVKCustomGLSLSemShader.GetMainTexName: TVKLibMaterialName;
begin
  Result := TVKMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TVKCustomGLSLSemShader.SetMainTexName(const Value: TVKLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TVKMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


//procedure TVKCustomGLSLSemShader.SetDiffuseColor(AValue: TVKColor);
//begin
//  FDiffuseColor.DirectColor := AValue.Color;
//end;

procedure TVKCustomGLSLSemShader.SetAmbientColor(AValue: TVKColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLSemShader.SetSpecularColor(AValue: TVKColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLSemShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin

        if FMainTexture <> nil then
        begin
          Index := TVKMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;


end.

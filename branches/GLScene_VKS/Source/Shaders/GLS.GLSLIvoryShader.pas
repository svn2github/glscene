//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   Ivory shader simulate Ivory material. 
   At this time only one light source is supported
}

unit GLS.GLSLIvoryShader;

interface

{$I GLScene.inc}

uses
  System.Classes,
  //GLS
  GLS.Scene, GLS.CrossPlatform, GLS.BaseClasses, GLS.State, GLS.OpenGLTokens, GLS.OpenGL1x, 
  GLS.Context, GLS.RenderContextInfo, GLS.VectorGeometry, GLS.Coordinates,
  GLS.TextureFormat, GLS.Color, GLS.Texture, GLS.Material,
  GLSL.Shader, GLS.CustomShader;

//TVKCustomGLSLIvoryShader
//
{ Custom class for GLSLIvoryShader. 
 A shader that simulate Ivory Material }
type
  TVKCustomGLSLIvoryShader = class(TVKCustomGLSLShader)
  private

  protected
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

  end;

type
  TVKSLIvoryShader = class(TVKCustomGLSLIvoryShader)

  end;

implementation


{ TVKCustomGLSLIvoryShader }

constructor TVKCustomGLSLIvoryShader.Create(AOwner: TComponent);
begin
  inherited;

  with VertexProgram.Code do
  begin
    Clear;
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  normal = gl_NormalMatrix * gl_Normal; ');
    Add('  lightVec = vec3(lightPos - vert); ');
    Add('  viewVec = -vec3(vert); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Clear;
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('vec3 norm = normalize(normal); ');
    Add('vec3 L = normalize(lightVec); ');
    Add('vec3 V = normalize(viewVec); ');
    Add('vec3 halfAngle = normalize(L + V); ');
    Add('float NdotL = dot(L, norm); ');
    Add('float NdotH = clamp(dot(halfAngle, norm), 0.0, 1.0); ');
    Add('// "Half-Lambert" technique for more pleasing diffuse term ');
    Add('float diffuse = 0.5 * NdotL + 0.5; ');
    Add('float specular = pow(NdotH, 64.0); ');
    Add('float result = diffuse + specular; ');
    Add('gl_FragColor = vec4(result); ');
    Add('} ');
  end;


  // Initial stuff.

end;

destructor TVKCustomGLSLIvoryShader.Destroy;
begin

  inherited;
end;

procedure TVKCustomGLSLIvoryShader.DoApply(var rci: TRenderContextInfo;
  Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;

end;

function TVKCustomGLSLIvoryShader.DoUnApply(
  var rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  GetGLSLProg.EndUseProgramObject;
end;



end.

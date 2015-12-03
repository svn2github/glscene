//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLPostThermalVisionShader <p>

   A shader that Simulate a Grayscal Thresold Vision of the entire scene.<p>

   <b>History : </b><font size=-1><ul>
      <li>01/11/15 - J.Delauney - Initial version (contributed to GLScene)

}
unit GLSLPostDreamVisionShader;

interface

{$I GLScene.inc}

uses
  System.Classes,
  // GLS
  GLTexture, GLScene, GLVectorGeometry, GLContext,
  GLSLShader, GLCustomShader, GLRenderContextInfo, GLTextureFormat,
  GLVectorTypes;


type
  TGLCustomGLSLPostDreamVisionShader = class(TGLCustomGLSLShader, IGLPostShader)
  private

    FThreshold : Single; // In percent 0..100;

    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;

   function StoreThreshold: Boolean;

  protected
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

     property Threshold: Single read FThreshold write FThreshold stored StoreThreshold;
  end;

  TGLSLPostDreamVisionShader = class(TGLCustomGLSLPostDreamVisionShader)
  published
    property Threshold;

  end;

//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
implementation
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------

{ TGLCustomGLSLPostThermalVisionShader }

constructor TGLCustomGLSLPostDreamVisionShader.Create(
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
   Add('uniform float Threshold; ');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('void main() ');
   Add('{ ');
   Add('   vec2 uv = vTexCoord * ScreenExtents; ');
   Add('   vec3 c = texture2DRect(ScreenTex, uv).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.001).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.003).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.005).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.007).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.009).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.011).rgb; ');
   Add(' ');
   Add('   c += texture2DRect(ScreenTex, uv-0.001).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.003).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.005).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.007).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.009).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.011).rgb; ');
   Add(' ');
   Add('   c.rgb = vec3((c.r+c.g+c.b)/3.0); ');
   Add('   c = c / Threshold; ');
   Add('   gl_FragColor = vec4(c,1.0); ');
   Add('} ');

  end;

  FThreshold  := 5;
end;

procedure TGLCustomGLSLPostDreamVisionShader.DoApply(
  var rci: TRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['Threshold'] := (FThreshold*255)/100;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TGLCustomGLSLPostDreamVisionShader.DoUnApply(
  var rci: TRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostDreamVisionShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[2, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostDreamVisionShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TGLCustomGLSLPostDreamVisionShader.StoreThreshold: Boolean;
begin
  Result := (Abs(FThreshold) > 0) and (Abs(FThreshold) <= 100);
end;


end.


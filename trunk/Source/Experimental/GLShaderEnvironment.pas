//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLShaderEnvironment<p>

  <b>History : </b><font size=-1><ul>
  <li>21/01/11 - Yar - Classes  of shader environment becomes static
  <li>08/11/10 - Yar - Creation
  </ul></font>
}

unit GLShaderEnvironment;

interface

{$I GLScene.inc}

uses
{$IFDEF FPC}
  LCLVersion,
{$ENDIF}
  Classes,
  SysUtils,
  BaseClasses,
  GLContext,
  GLState,
  GL3xMaterialTokens,
  GL3xMaterial,
  GLRenderContextInfo,
  GLCrossPlatform,
  GLShaderManager,
  VectorGeometry
{$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

const
  HackHackHack = 0;

{$IFDEF FPC}
  {$IF (LCL_RELEASE < 31)}
    {$DEFINE GLS_GENERIC_PREFIX}
  {$IFEND}
{$ENDIF}

type

  TShaderEnvTime = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

  TShaderEnvModelMatrix = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

  TShaderEnvNormalMatrix = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

  TShaderEnvInvModelMatrix = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

  TShaderEnvViewProjectionMatrix = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

  TShaderEnvCameraWorldPosition = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

  TShaderEnvLights = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

  TShaderEnvLightNumber = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

  TShaderEnvGUIAlpha = class(TBaseShaderEnvironment)
  public
    class procedure Apply(var ARci: TRenderContextInfo); override;
  end;

var
  vGUIAlpha: Single = 1.0;

implementation

uses
  OpenGLTokens, GLSRedBlackTree, GLSLog;

type
  TLightsBufferTree = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GRedBlackTree < TGLStateCache, TGLUniformBufferHandle > ;

var
  LightsBufferPerContext: TLightsBufferTree;
  ubLightsBlock: TGLSLUniformBlock;
  uLightIndices: TGLSLUniform;
  uLightNumber: TGLSLUniform;

  uCameraWorldPosition: TGLSLUniform;
  uModelMatrix: TGLSLUniform;
  uNormalMatrix: TGLSLUniform;
  uViewProjectionMatrix: TGLSLUniform;
  uInvModelMatrix: TGLSLUniform;

  uWorldPosition: TGLSLUniform;
  uAmbient: TGLSLUniform;
  uDiffuse: TGLSLUniform;
  uSpecular: TGLSLUniform;
  uSpotDirection: TGLSLUniform;
  uSpotCosCutoffExponent: TGLSLUniform;
  uAttenuation: TGLSLUniform;
  uTime: TGLSLUniform;
  uGUIAlphaMult: TGLSLUniform;

  bLightBufferSizeFlag: Boolean = False;

function CompareGLState(const Item1, Item2: TGLStateCache): Integer;
begin
  if PtrUint(Item1) < PtrUint(Item2) then
    exit(-1)
  else if PtrUint(Item1) = PtrUint(Item2) then
    exit(0)
  else
    exit(1);
end;

procedure LightsBufferDestroyer(AKey: TGLStateCache; AValue: TGLUniformBufferHandle; out AContinue: Boolean);
begin
  AValue.Destroy;
  AContinue := True;
end;

procedure LightsChanged(Sender: TObject);
var
  LightsBuffer: TGLUniformBufferHandle;
begin
  if (Sender is TGLStateCache)
    and LightsBufferPerContext.Find(TGLStateCache(Sender), LightsBuffer) then
    LightsBuffer.NotifyChangesOfData;
end;

class procedure TShaderEnvTime.Apply(var ARci: TRenderContextInfo);
begin
  ShaderManager.Uniform1f(uTime, 0.001*GLSTime);
end;

class procedure TShaderEnvGUIAlpha.Apply(var ARci: TRenderContextInfo);
begin
  ShaderManager.Uniform1f(uGUIAlphaMult, vGUIAlpha);
end;

class procedure TShaderEnvModelMatrix.Apply(var ARci: TRenderContextInfo);
begin
  ShaderManager.UniformMat4f(uModelMatrix, CurrentGLContext.PipelineTransformation.ModelMatrix);
end;

class procedure TShaderEnvNormalMatrix.Apply(var ARci: TRenderContextInfo);
begin
  ShaderManager.UniformMat3f(uNormalMatrix, CurrentGLContext.PipelineTransformation.NormalModelMatrix);
end;

class procedure TShaderEnvInvModelMatrix.Apply(var ARci: TRenderContextInfo);
begin
  ShaderManager.UniformMat4f(uInvModelMatrix, CurrentGLContext.PipelineTransformation.InvModelMatrix);
end;

class procedure TShaderEnvViewProjectionMatrix.Apply(var ARci: TRenderContextInfo);
begin
  ShaderManager.UniformMat4f(uViewProjectionMatrix, CurrentGLContext.PipelineTransformation.ViewProjectionMatrix);
end;

class procedure TShaderEnvCameraWorldPosition.Apply(var ARci: TRenderContextInfo);
begin
  ShaderManager.Uniform4f(uCameraWorldPosition, CurrentGLContext.PipelineTransformation.CameraPosition);
end;

class procedure TShaderEnvLights.Apply(var ARci: TRenderContextInfo);
var
  RC: TGLContext;
  LightsBuffer: TGLUniformBufferHandle;
  ptr: PSingle;
begin
  RC := SafeCurrentGLContext;
  if TGLUniformBufferHandle.IsSupported then
  begin
    if not LightsBufferPerContext.Find(RC.GLStates, LightsBuffer) then
    begin
      LightsBuffer := TGLUniformBufferHandle.Create;
      LightsBufferPerContext.Add(RC.GLStates, LightsBuffer);
      RC.GLStates.OnLightsChanged := LightsChanged;
    end;

    LightsBuffer.AllocateHandle;
    if LightsBuffer.IsDataNeedUpdate then
    begin
      LightsBuffer.BindBufferData(RC.GLStates.GetLightStateAsAddress, ubLightsBlock.DataSize, GL_STATIC_DRAW);
      LightsBuffer.NotifyDataUpdated;
      if not bLightBufferSizeFlag then
      begin
        GLSLogger.LogDebugFmt('LightsBuffer size: host %d, device %d', [SizeOf(TShaderLightSourceState), ubLightsBlock.DataSize]);
        bLightBufferSizeFlag := True;
      end;
    end;
    LightsBuffer.BindBase(0);
    ubLightsBlock.BindingIndex := 0;
  end
  else
    with ShaderManager do
    begin
      ptr := RC.GLStates.GetLightStateAsAddress;
      Uniform4f(uWorldPosition, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(uAmbient, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(uDiffuse, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(uSpecular, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(uSpotDirection, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(uSpotCosCutoffExponent, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(uAttenuation, ptr, 8);
    end;
  ShaderManager.Uniform1I(uLightIndices, RC.GLStates.GetLightIndicesAsAddress, RC.GLStates.LightNumber)
end;

class procedure TShaderEnvLightNumber.Apply(var ARci: TRenderContextInfo);
begin
  ShaderManager.Uniform1I(uLightNumber, CurrentGLContext.GLStates.LightNumber);
end;

initialization

RegisterClasses([
  TShaderEnvTime,
  TShaderEnvGUIAlpha,
  TShaderEnvModelMatrix,
  TShaderEnvInvModelMatrix,
  TShaderEnvNormalMatrix,
  TShaderEnvViewProjectionMatrix,
  TShaderEnvCameraWorldPosition,
  TShaderEnvLights,
  TShaderEnvLightNumber]);

  LightsBufferPerContext := TLightsBufferTree.Create(CompareGLState, nil);
  ubLightsBlock := TGLSLUniformBlock.RegisterUniformBlock('LightsBlock');
  uLightIndices := TGLSLUniform.RegisterUniform('LightIndices');
  uLightNumber := TGLSLUniform.RegisterUniform('LightNumber');

  uCameraWorldPosition := TGLSLUniform.RegisterUniform('CameraWorldPosition');
  uModelMatrix := TGLSLUniform.RegisterUniform('ModelMatrix');
  uNormalMatrix := TGLSLUniform.RegisterUniform('NormalMatrix');
  uViewProjectionMatrix := TGLSLUniform.RegisterUniform('ViewProjectionMatrix');
  uInvModelMatrix := TGLSLUniform.RegisterUniform('InvModelMatrix');

  uWorldPosition := TGLSLUniform.RegisterUniform('WorldPosition');
  uAmbient := TGLSLUniform.RegisterUniform('Ambient');
  uDiffuse := TGLSLUniform.RegisterUniform('Diffuse');
  uSpecular := TGLSLUniform.RegisterUniform('Specular');
  uSpotDirection := TGLSLUniform.RegisterUniform('SpotDirection');
  uSpotCosCutoffExponent := TGLSLUniform.RegisterUniform('SpotCosCutoffExponent');
  uAttenuation := TGLSLUniform.RegisterUniform('Attenuation');

  uTime := TGLSLUniform.RegisterUniform('Time');
  uGUIAlphaMult := TGLSLUniform.RegisterUniform('GUIAlphaMult');


finalization

  LightsBufferPerContext.ForEach(LightsBufferDestroyer);
  LightsBufferPerContext.Destroy;
  LightsBufferPerContext := nil;

end.

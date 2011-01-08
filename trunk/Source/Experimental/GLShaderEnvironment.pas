//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLShaderEnvironment<p>

  <b>History : </b><font size=-1><ul>
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
  GLCrossPlatform,
  GLShaderManager,
  VectorGeometry
{$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

{$IFDEF FPC}
  {$IF (FPC_VERSION = 2) and (FPC_RELEASE < 5)}
    {$DEFINE GLS_GENERIC_PREFIX}
  {$IFEND}
{$ENDIF}

type

  TShaderEnvSamplers = class(TBaseShaderEnvironment)
  private
    FTextureSamplerArray: TTextureSamplerArray;
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

  TShaderEnvTime = class(TBaseShaderEnvironment)
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

  TShaderEnvModelMatrix = class(TBaseShaderEnvironment)
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

  TShaderEnvNormalMatrix = class(TBaseShaderEnvironment)
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

  TShaderEnvInvModelMatrix = class(TBaseShaderEnvironment)
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

  TShaderEnvViewProjectionMatrix = class(TBaseShaderEnvironment)
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

  TShaderEnvCameraWorldPosition = class(TBaseShaderEnvironment)
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

  TShaderEnvLights = class(TBaseShaderEnvironment)
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

  TShaderEnvLightNumber = class(TBaseShaderEnvironment)
  public
    constructor Create(const AArray: TTextureSamplerArray); override;
    procedure Apply; override;
  end;

implementation

uses
  OpenGLTokens, GLSRedBlackTree, GLSLog;

type
  TLightsBufferTree = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GRedBlackTree < TGLStateCache, TGLUniformBufferHandle > ;

var
  LightsBufferPerContext: TLightsBufferTree;
  LightsBlock: TGLSLUniformBlock;
  LightIndices: TGLSLUniform;

  WorldPosition: TGLSLUniform;
  Ambient: TGLSLUniform;
  Diffuse: TGLSLUniform;
  Specular: TGLSLUniform;
  SpotDirection: TGLSLUniform;
  SpotCosCutoffExponent: TGLSLUniform;
  Attenuation: TGLSLUniform;

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

constructor TShaderEnvSamplers.Create(const AArray: TTextureSamplerArray);
begin
  FTextureSamplerArray := AArray;
end;

procedure TShaderEnvSamplers.Apply;
begin

  if High(FTextureSamplerArray) < 0 then
    exit;

  MaterialManager.ApplyTextureSampler(FTextureSamplerArray[0].TextureName, FTextureSamplerArray[0].SamplerName, uniformTexUnit0);

  if High(FTextureSamplerArray) < 1 then
    exit;

  MaterialManager.ApplyTextureSampler(FTextureSamplerArray[1].TextureName, FTextureSamplerArray[1].SamplerName, uniformTexUnit1);

  if High(FTextureSamplerArray) < 2 then
    exit;

  MaterialManager.ApplyTextureSampler(FTextureSamplerArray[2].TextureName, FTextureSamplerArray[2].SamplerName, uniformTexUnit2);

  if High(FTextureSamplerArray) < 3 then
    exit;

  MaterialManager.ApplyTextureSampler(FTextureSamplerArray[3].TextureName, FTextureSamplerArray[3].SamplerName, uniformTexUnit3);

  if High(FTextureSamplerArray) < 4 then
    exit;

  MaterialManager.ApplyTextureSampler(FTextureSamplerArray[4].TextureName, FTextureSamplerArray[4].SamplerName, uniformTexUnit4);

  if High(FTextureSamplerArray) < 5 then
    exit;

  MaterialManager.ApplyTextureSampler(FTextureSamplerArray[5].TextureName, FTextureSamplerArray[5].SamplerName, uniformTexUnit5);

  if High(FTextureSamplerArray) < 6 then
    exit;

  MaterialManager.ApplyTextureSampler(FTextureSamplerArray[6].TextureName, FTextureSamplerArray[6].SamplerName, uniformTexUnit6);

  if High(FTextureSamplerArray) < 7 then
    exit;

  MaterialManager.ApplyTextureSampler(FTextureSamplerArray[7].TextureName, FTextureSamplerArray[7].SamplerName, uniformTexUnit7);
end;

constructor TShaderEnvTime.Create(const AArray: TTextureSamplerArray);
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('Time');
end;

procedure TShaderEnvTime.Apply;
begin
  ShaderManager.Uniform1f(FGLSLUniform, 0.001*GLSTime);
end;

constructor TShaderEnvModelMatrix.Create(const AArray: TTextureSamplerArray);
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('ModelMatrix');
end;

procedure TShaderEnvModelMatrix.Apply;
begin
  ShaderManager.UniformMat4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.ModelMatrix);
end;

constructor TShaderEnvNormalMatrix.Create(const AArray: TTextureSamplerArray);
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('NormalMatrix');
end;

procedure TShaderEnvNormalMatrix.Apply;
begin
  ShaderManager.UniformMat3f(FGLSLUniform, CurrentGLContext.PipelineTransformation.NormalModelMatrix);
end;

constructor TShaderEnvInvModelMatrix.Create(const AArray: TTextureSamplerArray);
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('InvModelMatrix');
end;

procedure TShaderEnvInvModelMatrix.Apply;
begin
  ShaderManager.UniformMat4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.InvModelMatrix);
end;

constructor TShaderEnvViewProjectionMatrix.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('ViewProjectionMatrix');
end;

procedure TShaderEnvViewProjectionMatrix.Apply;
begin
  ShaderManager.UniformMat4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.ViewProjectionMatrix);
end;

constructor TShaderEnvCameraWorldPosition.Create(const AArray: TTextureSamplerArray);
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('CameraWorldPosition');
end;

procedure TShaderEnvCameraWorldPosition.Apply;
begin
  ShaderManager.Uniform4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.CameraPosition);
end;

constructor TShaderEnvLights.Create;
begin
//  FGLSLUniform := TGLSLUniform.RegisterUniform('LightIndex[0]');
end;

procedure TShaderEnvLights.Apply;
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
      LightsBuffer.BindBufferData(RC.GLStates.GetLightStateAsAddress, LightsBlock.DataSize, GL_STATIC_DRAW);
      LightsBuffer.NotifyDataUpdated;
      if not bLightBufferSizeFlag then
      begin
        GLSLogger.LogDebug(Format('LightsBuffer size: host %d, device %d', [SizeOf(TShaderLightSourceState), LightsBlock.DataSize]));
        bLightBufferSizeFlag := True;
      end;
    end;
    LightsBuffer.BindBase(0);
    LightsBlock.BindingIndex := 0;
  end
  else
    with ShaderManager do
    begin
      ptr := RC.GLStates.GetLightStateAsAddress;
      Uniform4f(WorldPosition, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(Ambient, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(Diffuse, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(Specular, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(SpotDirection, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(SpotCosCutoffExponent, ptr, 8);
      Inc(ptr, 4 * 8);
      Uniform4f(Attenuation, ptr, 8);
    end;
  ShaderManager.Uniform1I(LightIndices, RC.GLStates.GetLightIndicesAsAddress, RC.GLStates.LightNumber)
end;

constructor TShaderEnvLightNumber.Create(const AArray: TTextureSamplerArray);
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('LightNumber');
end;

procedure TShaderEnvLightNumber.Apply;
begin
  ShaderManager.Uniform1I(FGLSLUniform, CurrentGLContext.GLStates.LightNumber);
end;

initialization

RegisterClasses([
  TShaderEnvSamplers,
  TShaderEnvTime,
  TShaderEnvModelMatrix,
  TShaderEnvInvModelMatrix,
  TShaderEnvNormalMatrix,
  TShaderEnvViewProjectionMatrix,
  TShaderEnvCameraWorldPosition,
  TShaderEnvLights,
  TShaderEnvLightNumber]);

  LightsBufferPerContext := TLightsBufferTree.Create(CompareGLState, nil);
  LightsBlock := TGLSLUniformBlock.RegisterUniformBlock('LightsBlock');
  LightIndices := TGLSLUniform.RegisterUniform('LightIndices');

  WorldPosition := TGLSLUniform.RegisterUniform('WorldPosition');
  Ambient := TGLSLUniform.RegisterUniform('Ambient');
  Diffuse := TGLSLUniform.RegisterUniform('Diffuse');
  Specular := TGLSLUniform.RegisterUniform('Specular');
  SpotDirection := TGLSLUniform.RegisterUniform('SpotDirection');
  SpotCosCutoffExponent := TGLSLUniform.RegisterUniform('SpotCosCutoffExponent');
  Attenuation := TGLSLUniform.RegisterUniform('Attenuation');

finalization

  LightsBufferPerContext.ForEach(LightsBufferDestroyer);
  LightsBufferPerContext.Destroy;
  LightsBufferPerContext := nil;

end.

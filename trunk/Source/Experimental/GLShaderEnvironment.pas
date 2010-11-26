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
  Classes,
  BaseClasses,
  GLCrossPlatform,
  GLShaderManager,
  GL3xMaterialTokens,
  VectorGeometry
{$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

type

  TBaseShaderEnvironment = class(TPersistent)
  protected
    FGLSLUniform: TGLSLUniform;
    FGLSLUniformBlock: TGLSLUniformBlock;
  public
    constructor Create; virtual; abstract;
    procedure Apply; virtual; abstract;
  end;

  TShaderEnvSamplers = class(TBaseShaderEnvironment)
  private
    FTextureSamplerArray: TTextureSamplerArray;
  public
    constructor Create; override;
    procedure Apply; override;
    procedure SetTextureSampler(const AArray: TTextureSamplerArray);
  end;

  TShaderEnvTime = class(TBaseShaderEnvironment)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TShaderEnvModelMatrix = class(TBaseShaderEnvironment)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TShaderEnvNormalMatrix = class(TBaseShaderEnvironment)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TShaderEnvInvModelMatrix = class(TBaseShaderEnvironment)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TShaderEnvViewProjectionMatrix = class(TBaseShaderEnvironment)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TShaderEnvCameraWorldPosition = class(TBaseShaderEnvironment)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TShaderEnvLights = class(TBaseShaderEnvironment)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TShaderEnvLightNumber = class(TBaseShaderEnvironment)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TBaseShaderEnvironmentClass = class of TBaseShaderEnvironment;



implementation

uses
  SysUtils,
  GLVBOManager,
  GLContext,
  GL3xMaterial;

constructor TShaderEnvSamplers.Create;
begin
end;

procedure TShaderEnvSamplers.SetTextureSampler(const AArray: TTextureSamplerArray);
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

constructor TShaderEnvTime.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('Time');
end;

procedure TShaderEnvTime.Apply;
begin
  ShaderManager.Uniform1f(FGLSLUniform, 0.001*GLSTime);
end;

constructor TShaderEnvModelMatrix.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('ModelMatrix');
end;

procedure TShaderEnvModelMatrix.Apply;
begin
  ShaderManager.UniformMat4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.ModelMatrix);
end;

constructor TShaderEnvNormalMatrix.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('NormalMatrix');
end;

procedure TShaderEnvNormalMatrix.Apply;
begin
  ShaderManager.UniformMat3f(FGLSLUniform, CurrentGLContext.PipelineTransformation.NormalModelMatrix);
end;

constructor TShaderEnvInvModelMatrix.Create;
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

constructor TShaderEnvCameraWorldPosition.Create;
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
begin
  MaterialManager.BindLightsBlock;
end;

constructor TShaderEnvLightNumber.Create;
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

end.
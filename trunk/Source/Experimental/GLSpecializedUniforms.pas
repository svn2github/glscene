unit GLSpecializedUniforms;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,
  OpenGLTokens,
  GLShaderManager,
  VectorGeometry,
  GL3xTexture
  {$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

type

  TBaseSpecialUniform = class(TPersistent)
  protected
    FGLSLUniform: TGLSLUniform;
    FGLSLUniformBlock: TGLSLUniformBlock;
  public
    constructor Create; virtual; abstract;
    procedure Apply; virtual; abstract;
  end;

  TUniformSamplers = class(TBaseSpecialUniform)
  private
    FTextures: array[0..47] of TGL3xTextureName;
  public
    constructor Create; override;
    procedure Apply; override;
    procedure AddTexture(const AName: TGL3xTextureName);
  end;

  TUniformTime = class(TBaseSpecialUniform)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TUniformModelMatrix = class(TBaseSpecialUniform)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TUniformInvModelMatrix = class(TBaseSpecialUniform)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TUniformViewProjectionMatrix = class(TBaseSpecialUniform)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TUniformCameraWorldPosition = class(TBaseSpecialUniform)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TUniformLights = class(TBaseSpecialUniform)
  public
    constructor Create; override;
    procedure Apply; override;
  end;

  TSpecialUniformClass = class of TBaseSpecialUniform;


implementation

uses
  GLVBOManager,
  GLContext,
  GLState,
  GL3xMaterial;

constructor TUniformSamplers.Create;
begin
end;

procedure TUniformSamplers.AddTexture(const AName: TGL3xTextureName);
var
  I: Integer;
begin
  for I := 0 to High(FTextures) do
    if Length(FTextures[I]) = 0 then
    begin
      FTextures[I] := AName;
      exit;
    end;
end;

procedure TUniformSamplers.Apply;
var
  I: Integer;
begin
  for I := 0 to High(FTextures) do
    if Length(FTextures[I]) > 0 then
    begin
      MaterialManager.ApplyTexture(FTextures[I], TGLuint(I));
    end;
end;

constructor TUniformTime.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('Time');
end;

procedure TUniformTime.Apply;
begin
  ShaderManager.Uniform1f(FGLSLUniform, Now);
end;

constructor TUniformModelMatrix.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('ModelMatrix');
end;

procedure TUniformModelMatrix.Apply;
begin
  ShaderManager.UniformMat4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.ModelMatrix);
end;

constructor TUniformInvModelMatrix.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('InvModelMatrix');
end;

procedure TUniformInvModelMatrix.Apply;
begin
  ShaderManager.UniformMat4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.InvModelMatrix);
end;

constructor TUniformViewProjectionMatrix.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('ViewProjectionMatrix');
end;

procedure TUniformViewProjectionMatrix.Apply;
begin
  ShaderManager.UniformMat4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.ViewProjectionMatrix);
end;

constructor TUniformCameraWorldPosition.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('CameraWorldPosition');
end;

procedure TUniformCameraWorldPosition.Apply;
begin
  ShaderManager.Uniform4f(FGLSLUniform, CurrentGLContext.PipelineTransformation.CameraPosition);
end;

constructor TUniformLights.Create;
begin
  FGLSLUniform := TGLSLUniform.RegisterUniform('LightIndex[0]');
end;

procedure TUniformLights.Apply;
begin
  MaterialManager.BindLightsBlock;
  ShaderManager.Uniform1I(FGLSLUniform, CurrentGLContext.GLStates.GetLightIndicesAsAddress, MAX_HARDWARE_LIGHT);
end;

initialization

  RegisterClasses([
    TUniformSamplers,
      TUniformTime,
      TUniformModelMatrix,
      TUniformInvModelMatrix,
      TUniformViewProjectionMatrix,
      TUniformCameraWorldPosition,
      TUniformLights
      ]);

end.


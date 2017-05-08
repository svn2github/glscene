//
// GLXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    TVXAsmShader is a wrapper for all ARB shaders 
    This component is only a template and has to be replaced with a
    proper version by someone who uses ARB shaders more then me.
    The history is logged in a former GLS version of the unit.
}
 
unit VXS.AsmShader;

interface

{$I VXScene.inc}

uses
  System.Classes, 
  System.SysUtils,
  
  Winapi.OpenGL, 
  Winapi.OpenGLext,   
  VXS.Context,
  VXS.VectorGeometry, 
  VXS.VectorTypes, 
  VXS.Texture,
  VXS.CustomShader, 
  VXS.RenderContextInfo;

type
  TVXCustomAsmShader = class;
  TVXAsmShaderEvent = procedure(Shader: TVXCustomAsmShader) of object;
  TVXAsmShaderUnUplyEvent = procedure(Shader: TVXCustomAsmShader; var ThereAreMorePasses: Boolean) of object;

  TVXAsmShaderParameter = class(TVXCustomShaderParameter)
  private
    
  protected
    
{
    function GetAsVector1f: Single; override;
    function GetAsVector1i: Integer; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector2i: TVector2i; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector3i: TVector3i; override;
    function GetAsVector4f: TVector; override;
    function GetAsVector4i: TVector4i; override;

    procedure SetAsVector1f(const Value: Single); override;
    procedure SetAsVector1i(const Value: Integer); override;
    procedure SetAsVector2i(const Value: TVector2i); override;
    procedure SetAsVector3i(const Value: TVector3i); override;
    procedure SetAsVector4i(const Value: TVector4i); override;
    procedure SetAsVector2f(const Value: TVector2f); override;
    procedure SetAsVector3f(const Value: TVector3f); override;
    procedure SetAsVector4f(const Value: TVector4f); override;

    function GetAsMatrix2f: TMatrix2f; override;
    function GetAsMatrix3f: TMatrix3f; override;
    function GetAsMatrix4f: TMatrix4f; override;
    procedure SetAsMatrix2f(const Value: TMatrix2f); override;
    procedure SetAsMatrix3f(const Value: TMatrix3f); override;
    procedure SetAsMatrix4f(const Value: TMatrix4f); override;

    procedure SetAsTexture1D(const TextureIndex: Integer;
      const Value: TVXTexture);
    procedure SetAsTexture2D(const TextureIndex: Integer;
      const Value: TVXTexture);
    procedure SetAsTexture3D(const TextureIndex: Integer;
      const Value: TVXTexture);
    procedure SetAsTextureCube(const TextureIndex: Integer;
      const Value: TVXTexture);
    procedure SetAsTextureRect(const TextureIndex: Integer;
      const Value: TVXTexture);

    function GetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word; const Value: Cardinal); override;
}
  end;

  TVXCustomAsmShader = class(TVXCustomShader)
  private
    
    FVPHandle: TVXVertexProgramHandle;
    FFPHandle: TVXFragmentProgramHandle;
    FGPHandle: TVXGeometryProgramHandle;

    FOnInitialize: TVXAsmShaderEvent;
    FOnApply: TVXAsmShaderEvent;
    FOnUnApply: TVXAsmShaderUnUplyEvent;
  protected
    
    procedure ApplyShaderPrograms;
    procedure UnApplyShaderPrograms;
    procedure DestroyARBPrograms; virtual;

    property OnApply: TVXAsmShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TVXAsmShaderUnUplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TVXAsmShaderEvent read FOnInitialize write FOnInitialize;

    procedure DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TVXRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;
  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function ShaderSupported: Boolean; override;
  end;

  TVXAsmShader = class(TVXCustomAsmShader)
  published
    property FragmentProgram;
    property VertexProgram;
    property GeometryProgram;

    property OnApply;
    property OnUnApply;
    property OnInitialize;

    property ShaderStyle;
    property FailedInitAction;
  end;

implementation

{ TVXCustomAsmShader }

procedure TVXCustomAsmShader.DoFinalize;
begin
  inherited;
  DestroyARBPrograms;
end;

procedure TVXCustomAsmShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TVXCustomAsmShader then
  begin
    // Nothing here ...yet
  end;
end;

constructor TVXCustomAsmShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TVXCustomAsmShader.Destroy;
begin
  DestroyARBPrograms;

  inherited Destroy;
end;

procedure TVXCustomAsmShader.DestroyARBPrograms;
begin
  FVPHandle.Free;
  FVPHandle := nil;
  FFPHandle.Free;
  FFPHandle := nil;
  FGPHandle.Free;
  FGPHandle := nil;
end;

procedure TVXCustomAsmShader.DoApply(var rci: TVXRenderContextInfo; Sender: TObject);
begin
  ApplyShaderPrograms();

  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TVXCustomAsmShader.DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject);
begin
  if not ShaderSupported then
  begin
    Enabled := False;
    HandleFailedInitialization;
  end
  else
  begin
    if VertexProgram.Enabled then
    begin
      if not Assigned(FVPHandle) then
        FVPHandle := TVXVertexProgramHandle.CreateAndAllocate;
      FVPHandle.LoadARBProgram(VertexProgram.Code.Text);
      VertexProgram.Enabled := FVPHandle.Ready;
    end;

    if FragmentProgram.Enabled then
    begin
      if not Assigned(FFPHandle) then
        FFPHandle := TVXFragmentProgramHandle.CreateAndAllocate;
      FFPHandle.LoadARBProgram(FragmentProgram.Code.Text);
      FragmentProgram.Enabled := FFPHandle.Ready;
    end;

    if GeometryProgram.Enabled then
    begin
      if not Assigned(FGPHandle) then
        FGPHandle := TVXGeometryProgramHandle.CreateAndAllocate;
      FGPHandle.LoadARBProgram(GeometryProgram.Code.Text);
      GeometryProgram.Enabled := FGPHandle.Ready;
    end;

    Enabled := (FragmentProgram.Enabled or VertexProgram.Enabled or GeometryProgram.Enabled);

    if Enabled then
    begin
      if Assigned(FOnInitialize) then
        FOnInitialize(Self)
    end;
  end;
end;

function TVXCustomAsmShader.DoUnApply(var rci: TVXRenderContextInfo): Boolean;
begin
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result)
  else
    Result := False;

  UnApplyShaderPrograms();
end;

function TVXCustomAsmShader.ShaderSupported: Boolean;
begin
  Result :=
    (VertexProgram.Enabled) or
    (FragmentProgram.Enabled) or
    (GeometryProgram.Enabled);
end;

procedure TVXCustomAsmShader.ApplyShaderPrograms;
begin
  if VertexProgram.Enabled then
  begin
    FVPHandle.Enable;
    FVPHandle.Bind;
  end;
  if FragmentProgram.Enabled then
  begin
    FFPHandle.Enable;
    FFPHandle.Bind;
  end;
  if GeometryProgram.Enabled then
  begin
    FGPHandle.Enable;
    FGPHandle.Bind;
  end;
end;

procedure TVXCustomAsmShader.UnApplyShaderPrograms;
begin
  if VertexProgram.Enabled then
    FVPHandle.Disable;
  if FragmentProgram.Enabled then
    FFPHandle.Disable;
  if GeometryProgram.Enabled then
    FGPHandle.Disable;
end;

initialization
  RegisterClasses([TVXCustomAsmShader, TVXAsmShader]);

end.


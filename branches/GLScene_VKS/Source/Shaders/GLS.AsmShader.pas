//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
    TVKAsmShader is a wrapper for all ARB shaders 
    This component is only a template and has to be replaced with a
    proper version by someone who uses ARB shaders more then me.
}
 
unit GLS.AsmShader;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,
  //GLS
  GLS.OpenGLTokens,  GLS.Context,
  GLS.VectorGeometry, GLS.VectorTypes, GLS.Texture,
  GLS.CustomShader, GLS.RenderContextInfo;

type
  TVKCustomAsmShader = class;
  TVKAsmShaderEvent = procedure(Shader: TVKCustomAsmShader) of object;
  TVKAsmShaderUnUplyEvent = procedure(Shader: TVKCustomAsmShader; var ThereAreMorePasses: Boolean) of object;

  TVKAsmShaderParameter = class(TVKCustomShaderParameter)
  private
    { Private Declarations }
  protected
    { Protected Declarations }
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
      const Value: TVKTexture);
    procedure SetAsTexture2D(const TextureIndex: Integer;
      const Value: TVKTexture);
    procedure SetAsTexture3D(const TextureIndex: Integer;
      const Value: TVKTexture);
    procedure SetAsTextureCube(const TextureIndex: Integer;
      const Value: TVKTexture);
    procedure SetAsTextureRect(const TextureIndex: Integer;
      const Value: TVKTexture);

    function GetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word; const Value: Cardinal); override;
}
  end;

  TVKCustomAsmShader = class(TVKCustomShader)
  private
    { Private Declarations }
    FVPHandle: TVKARBVertexProgramHandle;
    FFPHandle: TVKARBFragmentProgramHandle;
    FGPHandle: TVKARBGeometryProgramHandle;

    FOnInitialize: TVKAsmShaderEvent;
    FOnApply: TVKAsmShaderEvent;
    FOnUnApply: TVKAsmShaderUnUplyEvent;
  protected
    { Protected Declarations }
    procedure ApplyShaderPrograms;
    procedure UnApplyShaderPrograms;
    procedure DestroyARBPrograms; virtual;

    property OnApply: TVKAsmShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TVKAsmShaderUnUplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TVKAsmShaderEvent read FOnInitialize write FOnInitialize;

    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function ShaderSupported: Boolean; override;
  end;

  TVKAsmShader = class(TVKCustomAsmShader)
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

{ TVKCustomAsmShader }

procedure TVKCustomAsmShader.DoFinalize;
begin
  inherited;
  DestroyARBPrograms;
end;

procedure TVKCustomAsmShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TVKCustomAsmShader then
  begin
    // Nothing here ...yet
  end;
end;

constructor TVKCustomAsmShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TVKCustomAsmShader.Destroy;
begin
  DestroyARBPrograms;

  inherited Destroy;
end;

procedure TVKCustomAsmShader.DestroyARBPrograms;
begin
  FVPHandle.Free;
  FVPHandle := nil;
  FFPHandle.Free;
  FFPHandle := nil;
  FGPHandle.Free;
  FGPHandle := nil;
end;

procedure TVKCustomAsmShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  ApplyShaderPrograms();

  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TVKCustomAsmShader.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
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
        FVPHandle := TVKARBVertexProgramHandle.CreateAndAllocate;
      FVPHandle.LoadARBProgram(VertexProgram.Code.Text);
      VertexProgram.Enabled := FVPHandle.Ready;
    end;

    if FragmentProgram.Enabled then
    begin
      if not Assigned(FFPHandle) then
        FFPHandle := TVKARBFragmentProgramHandle.CreateAndAllocate;
      FFPHandle.LoadARBProgram(FragmentProgram.Code.Text);
      FragmentProgram.Enabled := FFPHandle.Ready;
    end;

    if GeometryProgram.Enabled then
    begin
      if not Assigned(FGPHandle) then
        FGPHandle := TVKARBGeometryProgramHandle.CreateAndAllocate;
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

function TVKCustomAsmShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result)
  else
    Result := False;

  UnApplyShaderPrograms();
end;

function TVKCustomAsmShader.ShaderSupported: Boolean;
begin
  Result :=
    (VertexProgram.Enabled and TVKARBVertexProgramHandle.IsSupported) or
    (FragmentProgram.Enabled and TVKARBFragmentProgramHandle.IsSupported) or
    (GeometryProgram.Enabled and TVKARBGeometryProgramHandle.IsSupported);
end;

procedure TVKCustomAsmShader.ApplyShaderPrograms;
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

procedure TVKCustomAsmShader.UnApplyShaderPrograms;
begin
  if VertexProgram.Enabled then
    FVPHandle.Disable;
  if FragmentProgram.Enabled then
    FFPHandle.Disable;
  if GeometryProgram.Enabled then
    FGPHandle.Disable;
end;

initialization
  RegisterClasses([TVKCustomAsmShader, TVKAsmShader]);

end.


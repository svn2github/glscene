//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPipelineTransformation<p>

   <b>History : </b><font size=-1><ul>
    <li>16/11/10 - Yar - Added NormalModelMatrix
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GLPipelineTransformation;

interface

{$I GLScene.inc}

uses
  VectorGeometry;

const
  MAX_MATRIX_STACK_DEPTH = 128;

type

  TGLPipelineTransformationState =
  (
    trsModelViewChanged,
    trsInvModelViewChanged,
    trsInvModelChanged,
    trsNormalModelChanged,
    trsViewProjChanged,
    trsFrustum
  );

  TGLPipelineTransformationStates = set of TGLPipelineTransformationState;

  TGLPipelineTransformation = class
  private
    FStates: TGLPipelineTransformationStates;
    FStackPos: Integer;
    FModelMatrix: array[0..MAX_MATRIX_STACK_DEPTH-1] of TMatrix;
    FViewMatrix: array[0..MAX_MATRIX_STACK_DEPTH-1] of TMatrix;
    FProjectionMatrix: array[0..MAX_MATRIX_STACK_DEPTH-1] of TMatrix;
    FFFPTransformation: Boolean;

    FInvModelMatrix: TMatrix;
    FNormalModelMatrix: TAffineMatrix;
    FModelViewMatrix: TMatrix;
    FInvModelViewMatrix: TMatrix;
    FViewProjectionMatrix: TMatrix;
    FFrustum: TFrustum;

    FCameraPosition: TVector;

    function GetModelMatrix: TMatrix;
    function GetViewMatrix: TMatrix;
    function GetProjectionMatrix: TMatrix;
    function GetModelViewMatrix: TMatrix;
    function GetInvModelViewMatrix: TMatrix;
    function GetInvModelMatrix: TMatrix;
    function GetNormalModelMatrix: TAffineMatrix;
    function GetViewProjectionMatrix: TMatrix;
    function GetFrustum: TFrustum;

    procedure SetModelMatrix(const AMatrix: TMatrix);
    procedure SetViewMatrix(const AMatrix: TMatrix);
    procedure SetProjectionMatrix(const AMatrix: TMatrix);
  protected
    procedure LoadModelViewMatrix; {$IFDEF GLS_INLINE} inline; {$ENDIF}
    procedure LoadProjectionMatrix; {$IFDEF GLS_INLINE} inline; {$ENDIF}
  public
    constructor Create;

    procedure IdentityAll;
    procedure Push;
    procedure Pop;
    procedure ReplaceFromStack;

    property ModelMatrix: TMatrix read GetModelMatrix write SetModelMatrix;
    property ViewMatrix: TMatrix read GetViewMatrix write SetViewMatrix;
    property ProjectionMatrix: TMatrix read GetProjectionMatrix write SetProjectionMatrix;

    property InvModelMatrix: TMatrix read GetInvModelMatrix;
    property ModelViewMatrix: TMatrix read GetModelViewMatrix;
    property NormalModelMatrix: TAffineMatrix read GetNormalModelMatrix;
    property InvModelViewMatrix: TMatrix read GetInvModelViewMatrix;
    property ViewProjectionMatrix: TMatrix read GetViewProjectionMatrix;
    property Frustum: TFrustum read GetFrustum;

    property CameraPosition: TVector read FCameraPosition write FCameraPosition;
    property FFPTransformation: Boolean read FFFPTransformation write FFFPTransformation;
  end;

implementation

uses
  OpenGLTokens,
  GLContext;

const
  cAllStatesChanged = [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsViewProjChanged, trsNormalModelChanged, trsFrustum];

constructor TGLPipelineTransformation.Create;
begin
  FStackPos := 0;
  IdentityAll;
end;

procedure TGLPipelineTransformation.IdentityAll;
begin
  FModelMatrix[FStackPos] := IdentityHmgMatrix;
  FViewMatrix[FStackPos] := IdentityHmgMatrix;
  FProjectionMatrix[FStackPos] := IdentityHmgMatrix;
  FStates := cAllStatesChanged;
  if FFPTransformation then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TGLPipelineTransformation.Push;
var
  prevPos: Integer;
begin
  Assert(FStackPos <= MAX_MATRIX_STACK_DEPTH);
  prevPos := FStackPos;
  Inc(FStackPos);
  FModelMatrix[FStackPos] := FModelMatrix[prevPos];
  FViewMatrix[FStackPos] := FViewMatrix[prevPos];
  FProjectionMatrix[FStackPos] := FProjectionMatrix[prevPos];
end;

procedure TGLPipelineTransformation.Pop;
begin
  Assert(FStackPos > 0);
  Dec(FStackPos);
  FStates := cAllStatesChanged;
  if FFPTransformation then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TGLPipelineTransformation.ReplaceFromStack;
begin
  Assert(FStackPos > 0);
  FModelMatrix[FStackPos] := FModelMatrix[FStackPos-1];
  FViewMatrix[FStackPos] := FViewMatrix[FStackPos-1];
  FProjectionMatrix[FStackPos] := FProjectionMatrix[FStackPos-1];
  FStates := cAllStatesChanged;
  if FFPTransformation then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TGLPipelineTransformation.LoadModelViewMatrix;
var
  M: TMatrix;
begin
  M := GetModelViewMatrix;
  GL.LoadMatrixf(PGLFloat(@M));
end;

procedure TGLPipelineTransformation.LoadProjectionMatrix;
begin
  GL.MatrixMode(GL_PROJECTION);
  GL.LoadMatrixf(PGLFloat(@FProjectionMatrix[FStackPos]));
  GL.MatrixMode(GL_MODELVIEW);
end;

function TGLPipelineTransformation.GetModelMatrix: TMatrix;
begin
  Result := FModelMatrix[FStackPos];
end;

function TGLPipelineTransformation.GetViewMatrix: TMatrix;
begin
  Result := FViewMatrix[FStackPos];
end;

function TGLPipelineTransformation.GetProjectionMatrix: TMatrix;
begin
  Result := FProjectionMatrix[FStackPos];
end;

procedure TGLPipelineTransformation.SetModelMatrix(const AMatrix: TMatrix);
begin
  FModelMatrix[FStackPos] := AMatrix;
  FStates := FStates + [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged];
  if FFPTransformation then
    LoadModelViewMatrix;
end;

procedure TGLPipelineTransformation.SetViewMatrix(const AMatrix: TMatrix);
begin
  FViewMatrix[FStackPos] := AMatrix;
  FStates := FStates + [trsModelViewChanged, trsInvModelViewChanged, trsViewProjChanged, trsFrustum];
  if FFPTransformation then
    LoadModelViewMatrix;
end;

procedure TGLPipelineTransformation.SetProjectionMatrix(const AMatrix: TMatrix);
begin
  FProjectionMatrix[FStackPos] := AMatrix;
  FStates := FStates + [trsViewProjChanged, trsFrustum];
  if FFPTransformation then
    LoadProjectionMatrix;
end;

function TGLPipelineTransformation.GetModelViewMatrix: TMatrix;
begin
  if trsModelViewChanged in FStates then
  begin
    FModelViewMatrix := MatrixMultiply(FModelMatrix[FStackPos], FViewMatrix[FStackPos]);
    Exclude(FStates, trsModelViewChanged);
  end;
  Result := FModelViewMatrix;
end;

function TGLPipelineTransformation.GetInvModelViewMatrix: TMatrix;
begin
  if trsInvModelViewChanged in FStates then
  begin
    FInvModelViewMatrix := GetModelViewMatrix;
    InvertMatrix(FInvModelViewMatrix);
    Exclude(FStates, trsInvModelViewChanged);
  end;
  Result := FInvModelViewMatrix;
end;

function TGLPipelineTransformation.GetInvModelMatrix: TMatrix;
begin
  if trsInvModelChanged in FStates then
  begin
    FInvModelMatrix := MatrixInvert(FModelMatrix[FStackPos]);
    Exclude(FStates, trsInvModelChanged);
  end;
  Result := FInvModelMatrix;
end;

function TGLPipelineTransformation.GetNormalModelMatrix: TAffineMatrix;
var
  M: TMatrix;
begin
  if trsNormalModelChanged in FStates then
  begin
    M := FModelMatrix[FStackPos];
    NormalizeMatrix(M);
    SetMatrix(FNormalModelMatrix, M);
    Exclude(FStates, trsNormalModelChanged);
  end;
  Result := FNormalModelMatrix;
end;

function TGLPipelineTransformation.GetViewProjectionMatrix: TMatrix;
begin
  if trsViewProjChanged in FStates then
  begin
    FViewProjectionMatrix := MatrixMultiply(FViewMatrix[FStackPos], FProjectionMatrix[FStackPos]);
    Exclude(FStates, trsViewProjChanged);
  end;
  Result := FViewProjectionMatrix;
end;

function TGLPipelineTransformation.GetFrustum: TFrustum;
begin
  if trsFrustum in FStates then
  begin
    FFrustum := ExtractFrustumFromModelViewProjection(GetViewProjectionMatrix);
    Exclude(FStates, trsFrustum);
  end;
  Result := FFrustum;
end;

end.

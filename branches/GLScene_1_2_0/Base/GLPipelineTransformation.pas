//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLPipelineTransformation<p>

  <b>History : </b><font size=-1><ul>
  <li>08/06/11 - Yar - Now matrices becomes as managed reference type
  <li>11/05/11 - Yar - Ranamed TGLPipelineTransformation to TGLTransformation
  <li>16/11/10 - Yar - Added NormalModelMatrix
  <li>23/08/10 - Yar - Creation
  </ul></font>
}

unit GLPipelineTransformation;

interface

{$I GLScene.inc}

uses
  VectorGeometry,
  VectorTypes;

const
  MAX_MATRIX_STACK_DEPTH = 128;

type

  TTransformationChange = (tfcModelViewChanged, tfcInvModelViewChanged,
    tfcInvModelChanged, tfcNormalModelChanged, tfcViewProjChanged, tfcFrustum);

  TTransformationChanges = set of TTransformationChange;

const
  cAllStatesChanged = [tfcModelViewChanged, tfcInvModelViewChanged,
    tfcInvModelChanged, tfcViewProjChanged, tfcNormalModelChanged, tfcFrustum];

type

  TManagedMatrix = array of Single;

  PTransformationRec = ^TTransformationRec;

  TTransformationRec = record
    FStates: TTransformationChanges;
    FModelMatrix: TManagedMatrix;
    FViewMatrix: TManagedMatrix;
    FProjectionMatrix: TManagedMatrix;
    FInvModelMatrix: TManagedMatrix;
    FNormalModelMatrix: TManagedMatrix;
    FModelViewMatrix: TManagedMatrix;
    FInvModelViewMatrix: TManagedMatrix;
    FViewProjectionMatrix: TManagedMatrix;
    FFrustum: TFrustum;
  end;

type

  TOnCustomLoadMatrices = procedure() of object;

  // TGLTransformation
  //
  TGLTransformation = class(TObject)
  private
    FStackPos: Integer;
    FStack: array of TTransformationRec;
    FOnCustomLoadMatrices: TOnCustomLoadMatrices;
    FPickingBox: TVector4i;
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
    procedure SetModelViewMatrix(const AMatrix: TMatrix);
    procedure SetProjectionMatrix(const AMatrix: TMatrix);
    function GetStackTop: TTransformationRec;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
    procedure SetStackTop(const Value: TTransformationRec);
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  protected
    procedure AssignRecord(const ASrc: TTransformationRec;
      var ADst: TTransformationRec);
    property OnCustomLoadMatrices: TOnCustomLoadMatrices
      read FOnCustomLoadMatrices write FOnCustomLoadMatrices;
  public
    constructor Create;

    procedure IdentityAll;
    procedure Push(AValue: PTransformationRec = nil);
    procedure Pop;
    procedure ReplaceFromStack;
    procedure LoadMatrices;

    property StackTop: TTransformationRec read GetStackTop write SetStackTop;
    property ModelMatrix: TMatrix read GetModelMatrix write SetModelMatrix;
    property ViewMatrix: TMatrix read GetViewMatrix write SetViewMatrix;
    property ProjectionMatrix: TMatrix read GetProjectionMatrix
      write SetProjectionMatrix;

    property InvModelMatrix: TMatrix read GetInvModelMatrix;
    property ModelViewMatrix: TMatrix read GetModelViewMatrix write SetModelViewMatrix;
    property NormalModelMatrix: TAffineMatrix read GetNormalModelMatrix;
    property InvModelViewMatrix: TMatrix read GetInvModelViewMatrix;
    property ViewProjectionMatrix: TMatrix read GetViewProjectionMatrix;
    property Frustum: TFrustum read GetFrustum;

    property PickingBox: TVector4i read FPickingBox write FPickingBox;
  end;

var
  vIdentityHmgMatrix: TManagedMatrix;

procedure SetMatrix(var ADest: TManagedMatrix; const ASource: TMatrix);
  overload;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
procedure SetMatrix(var ADest: TMatrix; const ASource: TManagedMatrix);
  overload;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}

implementation

uses
  OpenGLTokens,
  GLContext,
  GLSLog;

procedure SetMatrix(var ADest: TManagedMatrix; const ASource: TMatrix);
begin
  // Break old reference
  ADest := nil;
  // Create new one
  SetLength(ADest, 16);
  // Assign
  ADest[0] := ASource[0][0];
  ADest[1] := ASource[0][1];
  ADest[2] := ASource[0][2];
  ADest[3] := ASource[0][3];

  ADest[4] := ASource[1][0];
  ADest[5] := ASource[1][1];
  ADest[6] := ASource[1][2];
  ADest[7] := ASource[1][3];

  ADest[8] := ASource[2][0];
  ADest[9] := ASource[2][1];
  ADest[10] := ASource[2][2];
  ADest[11] := ASource[2][3];

  ADest[12] := ASource[3][0];
  ADest[13] := ASource[3][1];
  ADest[14] := ASource[3][2];
  ADest[15] := ASource[3][3];
end;

procedure SetMatrix(var ADest: TMatrix; const ASource: TManagedMatrix);
begin
  ADest[0][0] := ASource[0];
  ADest[0][1] := ASource[1];
  ADest[0][2] := ASource[2];
  ADest[0][3] := ASource[3];

  ADest[1][0] := ASource[4];
  ADest[1][1] := ASource[5];
  ADest[1][2] := ASource[6];
  ADest[1][3] := ASource[7];

  ADest[2][0] := ASource[8];
  ADest[2][1] := ASource[9];
  ADest[2][2] := ASource[10];
  ADest[2][3] := ASource[11];

  ADest[3][0] := ASource[12];
  ADest[3][1] := ASource[13];
  ADest[3][2] := ASource[14];
  ADest[3][3] := ASource[15];
end;

procedure TGLTransformation.AssignRecord(const ASrc: TTransformationRec;
  var ADst: TTransformationRec);
begin
  ADst.FModelMatrix := ASrc.FModelMatrix;
  ADst.FViewMatrix := ASrc.FViewMatrix;
  ADst.FProjectionMatrix := ASrc.FProjectionMatrix;
  ADst.FInvModelMatrix := ASrc.FInvModelMatrix;
  ADst.FNormalModelMatrix := ASrc.FNormalModelMatrix;
  ADst.FModelViewMatrix := ASrc.FModelViewMatrix;
  ADst.FInvModelViewMatrix := ASrc.FInvModelViewMatrix;
  ADst.FViewProjectionMatrix := ASrc.FViewProjectionMatrix;
  ADst.FFrustum := ASrc.FFrustum;
  ADst.FStates := ASrc.FStates;
end;

constructor TGLTransformation.Create;
begin
  FStackPos := 0;
  SetLength(FStack, 1);
  IdentityAll;
end;

procedure TGLTransformation.IdentityAll;
begin
  with FStack[FStackPos] do
  begin
//    FModelMatrix := vIdentityHmgMatrix;
//    FViewMatrix := vIdentityHmgMatrix;
//    FProjectionMatrix := vIdentityHmgMatrix;
    SetLength(FModelMatrix, 16);
    SetMatrix(FModelMatrix, IdentityHmgMatrix);
    SetLength(FViewMatrix, 16);
    SetMatrix(FViewMatrix, IdentityHmgMatrix);
    SetLength(FProjectionMatrix, 16);
    SetMatrix(FProjectionMatrix, IdentityHmgMatrix);
    SetLength(FInvModelMatrix, 16);
    SetLength(FNormalModelMatrix, 16);
    SetLength(FModelViewMatrix, 16);
    SetLength(FInvModelViewMatrix, 16);
    SetLength(FViewProjectionMatrix, 16);
    FStates := cAllStatesChanged;
  end;
end;

procedure TGLTransformation.Push(AValue: PTransformationRec);
var
  prevPos: Integer;
begin
  if FStackPos > MAX_MATRIX_STACK_DEPTH then
  begin
    GLSLogger.LogWarningFmt
      ('Transformation stack overflow, more then %d values',
      [MAX_MATRIX_STACK_DEPTH]);
  end;
  prevPos := FStackPos;
  Inc(FStackPos);
  if High(FStack) < FStackPos then
    SetLength(FStack, FStackPos + 1);

  if Assigned(AValue) then
    AssignRecord(AValue^, FStack[FStackPos])
  else
    AssignRecord(FStack[prevPos], FStack[FStackPos]);
end;

procedure TGLTransformation.Pop;
begin
  if FStackPos = 0 then
  begin
    GLSLogger.LogError('Transformation stack underflow');
    exit;
  end;

  Dec(FStackPos);
end;

procedure TGLTransformation.ReplaceFromStack;
begin
  if FStackPos = 0 then
  begin
    GLSLogger.LogError('Transformation stack underflow');
    exit;
  end;
  AssignRecord(FStack[FStackPos - 1], FStack[FStackPos]);
end;

procedure TGLTransformation.SetStackTop(const Value: TTransformationRec);
begin
  AssignRecord(Value, FStack[FStackPos]);
end;

procedure TGLTransformation.LoadMatrices;
var
  M: TMatrix;
begin
  if Assigned(FOnCustomLoadMatrices) then
    FOnCustomLoadMatrices()
  else
    with GL do
    begin
      M := GetModelViewMatrix;
      if EXT_direct_state_access then
      begin
        MatrixLoadf(GL_MODELVIEW, PGLFloat(@M));
        MatrixLoadf(GL_PROJECTION,
          PGLFloat(@FStack[FStackPos].FProjectionMatrix[0]));
      end
      else
      begin
        LoadMatrixf(PGLFloat(@M));
        MatrixMode(GL_PROJECTION);
        LoadMatrixf(PGLFloat(@FStack[FStackPos].FProjectionMatrix[0]));
        MatrixMode(GL_MODELVIEW);
      end;
    end;
end;

function TGLTransformation.GetModelMatrix: TMatrix;
begin
  SetMatrix(Result, FStack[FStackPos].FModelMatrix);
end;

function TGLTransformation.GetViewMatrix: TMatrix;
begin
  SetMatrix(Result, FStack[FStackPos].FViewMatrix);
end;

function TGLTransformation.GetProjectionMatrix: TMatrix;
begin
  SetMatrix(Result, FStack[FStackPos].FProjectionMatrix);
end;

function TGLTransformation.GetStackTop: TTransformationRec;
begin
  AssignRecord(FStack[FStackPos], Result);
end;

procedure TGLTransformation.SetModelMatrix(const AMatrix: TMatrix);
begin
  with FStack[FStackPos] do
  begin
    SetMatrix(FModelMatrix, AMatrix);
    FStates := FStates + [tfcModelViewChanged, tfcInvModelViewChanged,
      tfcInvModelChanged, tfcNormalModelChanged];
  end;
end;

procedure TGLTransformation.SetModelViewMatrix(const AMatrix: TMatrix);
begin
  with FStack[FStackPos] do
  begin
    SetMatrix(FModelMatrix, IdentityHmgMatrix);
    SetMatrix(FViewMatrix, AMatrix);
    FStates := cAllStatesChanged;
  end;
end;

procedure TGLTransformation.SetViewMatrix(const AMatrix: TMatrix);
begin
  with FStack[FStackPos] do
  begin
    SetMatrix(FViewMatrix, AMatrix);
    FStates := FStates + [tfcModelViewChanged, tfcInvModelViewChanged,
      tfcViewProjChanged, tfcFrustum];
  end;
end;

procedure TGLTransformation.SetProjectionMatrix(const AMatrix: TMatrix);
begin
  with FStack[FStackPos] do
  begin
    SetMatrix(FProjectionMatrix, AMatrix);
    FStates := FStates + [tfcViewProjChanged, tfcFrustum];
  end;
end;

function TGLTransformation.GetModelViewMatrix: TMatrix;
begin
  with FStack[FStackPos] do
  begin
    if tfcModelViewChanged in FStates then
    begin
      Result := MatrixMultiply(GetModelMatrix, GetViewMatrix);
      SetMatrix(FModelViewMatrix, Result);
      Exclude(FStates, tfcModelViewChanged);
    end
    else
      SetMatrix(Result, FModelViewMatrix);
  end;
end;

function TGLTransformation.GetInvModelViewMatrix: TMatrix;
begin
  with FStack[FStackPos] do
  begin
    if tfcInvModelViewChanged in FStates then
    begin
      Result := GetModelViewMatrix;
      InvertMatrix(Result);
      SetMatrix(FInvModelViewMatrix, Result);
      Exclude(FStates, tfcInvModelViewChanged);
    end
    else
      SetMatrix(Result, FInvModelViewMatrix);
  end;
end;

function TGLTransformation.GetInvModelMatrix: TMatrix;
begin
  with FStack[FStackPos] do
  begin
    if tfcInvModelChanged in FStates then
    begin
      Result := MatrixInvert(GetModelMatrix);
      SetMatrix(FInvModelMatrix, Result);
      Exclude(FStates, tfcInvModelChanged);
    end
    else
      SetMatrix(Result, FInvModelMatrix);
  end;
end;

function TGLTransformation.GetNormalModelMatrix: TAffineMatrix;
var
  M: TMatrix;
begin
  with FStack[FStackPos] do
  begin
    if tfcNormalModelChanged in FStates then
    begin
      M := GetModelMatrix;
      NormalizeMatrix(M);
      SetMatrix(Result, M);
      SetMatrix(FNormalModelMatrix, M);
      Exclude(FStates, tfcNormalModelChanged);
    end
    else
    begin
      SetMatrix(M, FNormalModelMatrix);
      SetMatrix(Result, M);
    end;
  end;
end;

function TGLTransformation.GetViewProjectionMatrix: TMatrix;
begin
  with FStack[FStackPos] do
  begin
    if tfcViewProjChanged in FStates then
    begin
      Result := MatrixMultiply(GetViewMatrix, GetProjectionMatrix);
      SetMatrix(FViewProjectionMatrix, Result);
      Exclude(FStates, tfcViewProjChanged);
    end
    else
      SetMatrix(Result, FViewProjectionMatrix);
  end;
end;

function TGLTransformation.GetFrustum: TFrustum;
begin
  with FStack[FStackPos] do
  begin
    if tfcFrustum in FStates then
    begin
      FFrustum := ExtractFrustumFromModelViewProjection
        (GetViewProjectionMatrix);
      Exclude(FStates, tfcFrustum);
    end;
    Result := FFrustum;
  end;
end;

initialization

  SetLength(vIdentityHmgMatrix, 16);
  Move(IdentityHmgMatrix[0], vIdentityHmgMatrix[0], SizeOf(TMatrix));

end.

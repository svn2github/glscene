//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//

unit VXS.PipelineTransformation;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  System.Sysutils,
  FMX.Dialogs,
  VXS.VectorGeometry,
  VXS.VectorTypes;

const
  MAX_MATRIX_STACK_DEPTH = 128;

type

  TVXPipelineTransformationState =
  (
    trsModelViewChanged,
    trsInvModelViewChanged,
    trsInvModelChanged,
    trsNormalModelChanged,
    trsViewProjChanged,
    trsFrustum
  );

  TVXPipelineTransformationStates = set of TVXPipelineTransformationState;

const
  cAllStatesChanged = [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsViewProjChanged, trsNormalModelChanged, trsFrustum];

type

  PTransformationRec = ^TTransformationRec;
  TTransformationRec = record
    FStates: TVXPipelineTransformationStates;
    FModelMatrix: TMatrix;
    FViewMatrix: TMatrix;
    FProjectionMatrix: TMatrix;
    FInvModelMatrix: TMatrix;
    FNormalModelMatrix: TAffineMatrix;
    FModelViewMatrix: TMatrix;
    FInvModelViewMatrix: TMatrix;
    FViewProjectionMatrix: TMatrix;
    FFrustum: TFrustum;
  end;

type

  TOnMatricesPush = procedure() of object;

  TVXTransformation = class(TObject)
  private
    FStackPos: Integer;
    FStack: array of TTransformationRec;
    FLoadMatricesEnabled: Boolean;
    FOnPush: TOnMatricesPush;
    function GetModelMatrix: PMatrix; inline;
    function GetViewMatrix: PMatrix; inline;
    function GetProjectionMatrix: PMatrix; inline;
    function GetModelViewMatrix: PMatrix; inline;
    function GetInvModelViewMatrix: PMatrix; inline;
    function GetInvModelMatrix: PMatrix; inline;
    function GetNormalModelMatrix: PAffineMatrix; inline;
    function GetViewProjectionMatrix: PMatrix; inline;
    function GetFrustum: TFrustum; inline;
  protected
    procedure LoadModelViewMatrix; inline;
    procedure LoadProjectionMatrix; inline;
    procedure DoMatrcesLoaded; inline;
    property OnPush: TOnMatricesPush read FOnPush write FOnPush;
  public
    constructor Create;
    procedure SetModelMatrix(const AMatrix: TMatrix); inline;
    procedure SetViewMatrix(const AMatrix: TMatrix); inline;
    procedure SetProjectionMatrix(const AMatrix: TMatrix); inline;
    procedure IdentityAll; inline;
    procedure Push(AValue: PTransformationRec); overload;
    procedure Push(); overload; inline;
    procedure Pop;
    procedure ReplaceFromStack;
    function StackTop: TTransformationRec; inline;
    property ModelMatrix: PMatrix read GetModelMatrix;
    property ViewMatrix: PMatrix read GetViewMatrix;
    property ProjectionMatrix: PMatrix read GetProjectionMatrix;
    property InvModelMatrix: PMatrix read GetInvModelMatrix;
    property ModelViewMatrix: PMatrix read GetModelViewMatrix;
    property NormalModelMatrix: PAffineMatrix read GetNormalModelMatrix;
    property InvModelViewMatrix: PMatrix read GetInvModelViewMatrix;
    property ViewProjectionMatrix: PMatrix read GetViewProjectionMatrix;
    property Frustum: TFrustum read GetFrustum;
    property LoadMatricesEnabled: Boolean read FLoadMatricesEnabled write FLoadMatricesEnabled;
  end;

//=====================================================================
implementation
//=====================================================================

uses
  VXS.Context;

constructor TVXTransformation.Create;
begin
  FStackPos := 0;
  SetLength(FStack, MAX_MATRIX_STACK_DEPTH);
  IdentityAll;
end;

procedure TVXTransformation.LoadProjectionMatrix;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(PGLFloat(@FStack[FStackPos].FProjectionMatrix));
  glMatrixMode(GL_MODELVIEW);
end;

function TVXTransformation.GetModelViewMatrix: PMatrix;
begin
  if trsModelViewChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FModelViewMatrix :=
      MatrixMultiply(FStack[FStackPos].FModelMatrix, FStack[FStackPos].FViewMatrix);
    Exclude(FStack[FStackPos].FStates, trsModelViewChanged);
  end;
  Result := @FStack[FStackPos].FModelViewMatrix;
end;

procedure TVXTransformation.LoadModelViewMatrix;
begin
  glLoadMatrixf(PGLFloat(GetModelViewMatrix));
end;

procedure TVXTransformation.IdentityAll;
begin
  with FStack[FStackPos] do
  begin
    FModelMatrix := IdentityHmgMatrix;
    FViewMatrix := IdentityHmgMatrix;
    FProjectionMatrix := IdentityHmgMatrix;
    FStates := cAllStatesChanged;
  end;
  if LoadMatricesEnabled then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TVXTransformation.DoMatrcesLoaded;
begin
  if Assigned(FOnPush) then
    FOnPush();
end;

procedure TVXTransformation.Push;
var
  prevPos: Integer;
begin
  prevPos := FStackPos;
  Inc(FStackPos);
  FStack[FStackPos] := FStack[prevPos];
end;

procedure TVXTransformation.Push(AValue: PTransformationRec);
var
  prevPos: Integer;
begin

  if FStackPos > MAX_MATRIX_STACK_DEPTH then
  begin
    ShowMessage(Format('Transformation stack overflow, more then %d values',
      [MAX_MATRIX_STACK_DEPTH]));
  end;
  prevPos := FStackPos;
  Inc(FStackPos);

  if Assigned(AValue) then
  begin
    FStack[FStackPos] := AValue^;
    if LoadMatricesEnabled then
    begin
      LoadModelViewMatrix;
      LoadProjectionMatrix;
    end;
    DoMatrcesLoaded;
  end
  else
    FStack[FStackPos] := FStack[prevPos];
end;

procedure TVXTransformation.Pop;
begin
  if FStackPos = 0 then
  begin
    ShowMessage('Transformation stack underflow');
    exit;
  end;

  Dec(FStackPos);
  if LoadMatricesEnabled then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TVXTransformation.ReplaceFromStack;
var
  prevPos: Integer;
begin
  if FStackPos = 0 then
  begin
    ShowMessage('Transformation stack underflow');
    exit;
  end;
  prevPos := FStackPos - 1;
  FStack[FStackPos].FModelMatrix := FStack[prevPos].FModelMatrix;
  FStack[FStackPos].FViewMatrix:= FStack[prevPos].FViewMatrix;
  FStack[FStackPos].FProjectionMatrix:= FStack[prevPos].FProjectionMatrix;
  FStack[FStackPos].FStates := FStack[prevPos].FStates;
  if LoadMatricesEnabled then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;



function TVXTransformation.GetModelMatrix: PMatrix;
begin
  Result := @FStack[FStackPos].FModelMatrix;
end;

function TVXTransformation.GetViewMatrix: PMatrix;
begin
  Result := @FStack[FStackPos].FViewMatrix;
end;

function TVXTransformation.GetProjectionMatrix: PMatrix;
begin
  Result := @FStack[FStackPos].FProjectionMatrix;
end;

procedure TVXTransformation.SetModelMatrix(const AMatrix: TMatrix);
begin
  FStack[FStackPos].FModelMatrix := AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged];
  if LoadMatricesEnabled then
    LoadModelViewMatrix;
end;

procedure TVXTransformation.SetViewMatrix(const AMatrix: TMatrix);
begin
  FStack[FStackPos].FViewMatrix:= AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsModelViewChanged, trsInvModelViewChanged, trsViewProjChanged, trsFrustum];
  if LoadMatricesEnabled then
    LoadModelViewMatrix;
end;

function TVXTransformation.StackTop: TTransformationRec;
begin
  Result := FStack[FStackPos];
end;

procedure TVXTransformation.SetProjectionMatrix(const AMatrix: TMatrix);
begin
  FStack[FStackPos].FProjectionMatrix := AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsViewProjChanged, trsFrustum];
  if LoadMatricesEnabled then
    LoadProjectionMatrix;
end;


function TVXTransformation.GetInvModelViewMatrix: PMatrix;
begin
  if trsInvModelViewChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FInvModelViewMatrix := GetModelViewMatrix^;
    InvertMatrix(FStack[FStackPos].FInvModelViewMatrix);
    Exclude(FStack[FStackPos].FStates, trsInvModelViewChanged);
  end;
  Result := @FStack[FStackPos].FInvModelViewMatrix;
end;

function TVXTransformation.GetInvModelMatrix: PMatrix;
begin
  if trsInvModelChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FInvModelMatrix := MatrixInvert(FStack[FStackPos].FModelMatrix);
    Exclude(FStack[FStackPos].FStates, trsInvModelChanged);
  end;
  Result := @FStack[FStackPos].FInvModelMatrix;
end;

function TVXTransformation.GetNormalModelMatrix: PAffineMatrix;
var
  M: TMatrix;
begin
  if trsNormalModelChanged in FStack[FStackPos].FStates then
  begin
    M := FStack[FStackPos].FModelMatrix;
    NormalizeMatrix(M);
    SetMatrix(FStack[FStackPos].FNormalModelMatrix, M);
    Exclude(FStack[FStackPos].FStates, trsNormalModelChanged);
  end;
  Result := @FStack[FStackPos].FNormalModelMatrix;
end;

function TVXTransformation.GetViewProjectionMatrix: PMatrix;
begin
  if trsViewProjChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FViewProjectionMatrix :=
      MatrixMultiply(FStack[FStackPos].FViewMatrix, FStack[FStackPos].FProjectionMatrix);
    Exclude(FStack[FStackPos].FStates, trsViewProjChanged);
  end;
  Result := @FStack[FStackPos].FViewProjectionMatrix;
end;

function TVXTransformation.GetFrustum: TFrustum;
begin
  if trsFrustum in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FFrustum := ExtractFrustumFromModelViewProjection(GetViewProjectionMatrix^);
    Exclude(FStack[FStackPos].FStates, trsFrustum);
  end;
  Result := FStack[FStackPos].FFrustum;
end;

end.

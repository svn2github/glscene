//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ @HTML ( GLPipelineTransformation<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - JD - Imported from GLScene
 </ul></font>
}

unit DGLPipelineTransformation;

interface

{$I DGLEngine.inc}

uses
  classes,
  SysUtils,
  dglOpenGL,
  DGLBaseClasses,
  DGLVectorMaths,
  DGLVectorTypes,
  DGLSLog;

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

const
  cAllStatesChanged = [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsViewProjChanged, trsNormalModelChanged, trsFrustum];

type

  PTransformationRec = ^TTransformationRec;
  TTransformationRec = record
    FStates: TGLPipelineTransformationStates;
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

  // TDGLTransformation
  //
  TDGLTransformation = class(TObject)
  private
    FStackPos: Integer;
    FStack: array of TTransformationRec;
    FLoadMatricesEnabled: Boolean;
    FOnPush: TOnMatricesPush;
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
    procedure DoMatricesLoaded; {$IFDEF GLS_INLINE} inline; {$ENDIF}
    property OnPush: TOnMatricesPush read FOnPush write FOnPush;
  public
    constructor Create;

    procedure IdentityAll;
    procedure Push(AValue: PTransformationRec = nil);
    procedure Pop;
    procedure ReplaceFromStack;
    function StackTop: TTransformationRec;

    property ModelMatrix: TMatrix read GetModelMatrix write SetModelMatrix;
    property ViewMatrix: TMatrix read GetViewMatrix write SetViewMatrix;
    property ProjectionMatrix: TMatrix read GetProjectionMatrix write SetProjectionMatrix;

    property InvModelMatrix: TMatrix read GetInvModelMatrix;
    property ModelViewMatrix: TMatrix read GetModelViewMatrix;
    property NormalModelMatrix: TAffineMatrix read GetNormalModelMatrix;
    property InvModelViewMatrix: TMatrix read GetInvModelViewMatrix;
    property ViewProjectionMatrix: TMatrix read GetViewProjectionMatrix;
    property Frustum: TFrustum read GetFrustum;

    property LoadMatricesEnabled: Boolean read FLoadMatricesEnabled write FLoadMatricesEnabled;
  end;

// Prevent Lazaruses issue with checksumm chenging!
//type
//  TGLCall = function(): TGLExtensionsAndEntryPoints;
//var
//  vLocalGL: TGLCall;

implementation

procedure DisplayMatrix(Value:TMatrix;Name:String);
var
i,j:Integer;
tmp:String;
begin
  DGLSLogger.LogInfo('Display '+Name);
  for i:=0 to 3  do
  begin
    tmp:='[ ';
    for J := 0 to 3 do
    begin
      tmp:=tmp+ FloatToStrF(Value.V[I].V[J],ffFixed,5,3);
      if (J<3) then tmp:=tmp+', ';
    end;
    tmp:=tmp+' ]';
    DGLSLogger.LogInfo(tmp);
  end;

end;

// ------------------
{ TDGLTransformation }
{$IFDEF GLS_REGION}{$REGION 'TDGLTransformation'}{$ENDIF}

constructor TDGLTransformation.Create;
begin
  DGLSLogger.LogInfo('TDGLtranformation : Create');
  FStackPos := 0;
  SetLength(FStack, 1);
  IdentityAll;
end;

procedure TDGLTransformation.IdentityAll;
begin
  DGLSLogger.LogInfo('TDGLtranformation : IdentityAll');
  with FStack[FStackPos] do
  begin
    FModelMatrix := IdentityHmgMatrix;
    DisplayMatrix(FModelMatrix,'ModelMatrix');
    FViewMatrix := IdentityHmgMatrix;
    DisplayMatrix(FViewMatrix,'ViewMatrix');
    FProjectionMatrix := IdentityHmgMatrix;
    DisplayMatrix(FProjectionMatrix,'ProjectionMatrix');
    FStates := cAllStatesChanged;
  end;

  if LoadMatricesEnabled then
  begin
    DGLSLogger.LogInfo('TDGLtranformation : IdentityAll LoadMatricesEnabled');
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TDGLTransformation.Push(AValue: PTransformationRec);
var
  prevPos: Integer;
begin
  DGLSLogger.LogInfo('TDGLTranformation : Push');
  if FStackPos > MAX_MATRIX_STACK_DEPTH then
  begin
    DGLSLogger.LogWarningFmt('Transformation stack overflow, more then %d values',[MAX_MATRIX_STACK_DEPTH]);
  end;
  prevPos := FStackPos;
  Inc(FStackPos);
  if High(FStack) < FStackPos then
    SetLength(FStack, FStackPos+1);

  if Assigned(AValue) then
  begin
    FStack[FStackPos] := AValue^;
    if LoadMatricesEnabled then
    begin
      DGLSLogger.LogInfo('TDGLtranformation : Push LoadMatricesEnabled');
      LoadModelViewMatrix;
      LoadProjectionMatrix;
    end;
    DoMatricesLoaded;
  end
  else
    FStack[FStackPos] := FStack[prevPos];
end;

procedure TDGLTransformation.Pop;
begin
  DGLSLogger.LogInfo('TDGLtranformation : Pop');
  if FStackPos = 0 then
  begin
    DGLSLogger.LogError('Transformation stack underflow');
    exit;
  end;

  Dec(FStackPos);
  if LoadMatricesEnabled then
  begin
  DGLSLogger.LogInfo('TDGLtranformation : Pop LoadMatricesEnabled');
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TDGLTransformation.ReplaceFromStack;
var
  prevPos: Integer;
begin
  DGLSLogger.LogInfo('TDGLtranformation : ReplaceFromStack');
  if FStackPos = 0 then
  begin
    DGLSLogger.LogError('Transformation stack underflow');
    exit;
  end;
  //if FStackPos > 0 then
  prevPos := FStackPos - 1;
  // else prevPos:=0;
  FStack[FStackPos].FModelMatrix := FStack[prevPos].FModelMatrix;
  FStack[FStackPos].FViewMatrix:= FStack[prevPos].FViewMatrix;
  FStack[FStackPos].FProjectionMatrix:= FStack[prevPos].FProjectionMatrix;
  FStack[FStackPos].FStates := FStack[prevPos].FStates;
  if LoadMatricesEnabled then
  begin
    DGLSLogger.LogInfo('TDGLtranformation : ReplaceFromStack LoadMatricesEnabled');
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TDGLTransformation .LoadModelViewMatrix;
var
  M: TMatrix;
begin
  DGLSLogger.LogInfo('TDGLtranformation : LoadModelViewMatrix');
  M := GetModelViewMatrix;
//  vLocalGL.LoadMatrixf(PGLFloat(@M));
end;

procedure TDGLTransformation .LoadProjectionMatrix;
begin
   DGLSLogger.LogInfo('TDGLtranformation : LoadProjectionMatrix');
//  with vLocalGL do
//  begin
//    MatrixMode(GL_PROJECTION);
//    LoadMatrixf(PGLFloat(@FStack[FStackPos].FProjectionMatrix));
//    MatrixMode(GL_MODELVIEW);
//  end;
end;

function TDGLTransformation.GetModelMatrix: TMatrix;
begin
  DGLSLogger.LogInfo('TDGLtranformation : Get Model Matrix from pos '+inttostr(FStackPos));
  DisplayMatrix(FStack[FStackPos].FModelMatrix,'ModelMatrix');
  Result := FStack[FStackPos].FModelMatrix;
end;

function TDGLTransformation.GetViewMatrix: TMatrix;
begin
  DGLSLogger.LogInfo('TDGLtranformation : Get View Matrix from pos '+inttostr(FStackPos));
  DisplayMatrix(FStack[FStackPos].FViewMatrix,'ViewMatrix');
  Result := FStack[FStackPos].FViewMatrix;
end;

function TDGLTransformation.GetProjectionMatrix: TMatrix;
begin
  DGLSLogger.LogInfo('TDGLtranformation : Get Projection Matrix from pos '+inttostr(FStackPos));
  DisplayMatrix(FStack[FStackPos].FProjectionMatrix,'ProjectionMatrix');
  Result := FStack[FStackPos].FProjectionMatrix;
end;

procedure TDGLTransformation.SetModelMatrix(const AMatrix: TMatrix);
begin
  DGLSLogger.LogInfo('TDGLtranformation : Set Model Matrix at pos '+inttostr(FStackPos));
 // ReplaceFromStack;
  FStack[FStackPos].FModelMatrix := AMatrix;
  DisplayMatrix(AMatrix,'ModelMatrix');
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged];
  if LoadMatricesEnabled then
    LoadModelViewMatrix;
end;

procedure TDGLTransformation.SetViewMatrix(const AMatrix: TMatrix);
begin
  DGLSLogger.LogInfo('TDGLtranformation : Set View Matrix at pos '+inttostr(FStackPos));
 // ReplaceFromStack;
  FStack[FStackPos].FViewMatrix:= AMatrix;
  DisplayMatrix(AMatrix,'ViewMatrix');
  FStack[FStackPos].FStates := FStack[FStackPos].FStates + [trsModelViewChanged, trsInvModelViewChanged, trsViewProjChanged, trsFrustum];
  if LoadMatricesEnabled then
    LoadModelViewMatrix;
end;

function TDGLTransformation.StackTop: TTransformationRec;
begin
  Result := FStack[FStackPos];
end;

procedure TDGLTransformation.SetProjectionMatrix(const AMatrix: TMatrix);
begin
  DGLSLogger.LogInfo('TDGLtranformation : Set Projection Matrix at pos '+inttostr(FStackPos));
 // ReplaceFromStack;
  FStack[FStackPos].FProjectionMatrix := AMatrix;
  DisplayMatrix(AMatrix,'ProjectionMatrix');
  FStack[FStackPos].FStates := FStack[FStackPos].FStates + [trsViewProjChanged, trsFrustum];
  if LoadMatricesEnabled then LoadProjectionMatrix;
end;

function TDGLTransformation.GetModelViewMatrix: TMatrix;
begin
  if trsModelViewChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FModelViewMatrix := MatrixMultiply(FStack[FStackPos].FModelMatrix, FStack[FStackPos].FViewMatrix);
    Exclude(FStack[FStackPos].FStates, trsModelViewChanged);
  end;
  DGLSLogger.LogInfo('TDGLtranformation : Get ModelView Matrix from pos '+inttostr(FStackPos));
  DisplayMatrix(FStack[FStackPos].FModelViewMatrix,'ModelViewMatrix');
  Result := FStack[FStackPos].FModelViewMatrix;
end;

function TDGLTransformation.GetInvModelViewMatrix: TMatrix;
begin
  if trsInvModelViewChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FInvModelViewMatrix := GetModelViewMatrix;
    InvertMatrix(FStack[FStackPos].FInvModelViewMatrix);
    Exclude(FStack[FStackPos].FStates, trsInvModelViewChanged);
  end;
  DGLSLogger.LogInfo('TDGLtranformation : Get InvModelView Matrix from pos '+inttostr(FStackPos));
  DisplayMatrix(FStack[FStackPos].FInvModelViewMatrix,'InvModelViewMatrix');
  Result := FStack[FStackPos].FInvModelViewMatrix;
end;

function TDGLTransformation.GetInvModelMatrix: TMatrix;
begin
  if trsInvModelChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FInvModelMatrix := MatrixInvert(FStack[FStackPos].FModelMatrix);
    Exclude(FStack[FStackPos].FStates, trsInvModelChanged);
  end;
  DGLSLogger.LogInfo('TDGLtranformation : Get InvModel Matrix from pos '+inttostr(FStackPos));
  DisplayMatrix(FStack[FStackPos].FInvModelMatrix,'InvModelMatrix');
  Result := FStack[FStackPos].FInvModelMatrix;
end;

function TDGLTransformation.GetNormalModelMatrix: TAffineMatrix;
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
  DGLSLogger.LogInfo('TDGLtranformation : Get NormalModel Matrix from pos '+inttostr(FStackPos));
//  DisplayMatrix(FStack[FStackPos].FNormalModelMatrix,'NormalModelMatrix');
  Result := FStack[FStackPos].FNormalModelMatrix;
end;

function TDGLTransformation.GetViewProjectionMatrix: TMatrix;
begin
  if trsViewProjChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FViewProjectionMatrix :=
      MatrixMultiply(FStack[FStackPos].FViewMatrix, FStack[FStackPos].FProjectionMatrix);
    Exclude(FStack[FStackPos].FStates, trsViewProjChanged);
  end;
  DGLSLogger.LogInfo('TDGLtranformation : Get ViewProjection Matrix from pos '+inttostr(FStackPos));
  DisplayMatrix(FStack[FStackPos].FViewProjectionMatrix,'ViewProjectionMatrix');
  Result := FStack[FStackPos].FViewProjectionMatrix;
end;

procedure TDGLTransformation.DoMatricesLoaded;
begin
  if Assigned(FOnPush) then
    FOnPush();
end;

function TDGLTransformation.GetFrustum: TFrustum;
begin
  if trsFrustum in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FFrustum := ExtractFrustumFromModelViewProjection(GetViewProjectionMatrix);
    Exclude(FStack[FStackPos].FStates, trsFrustum);
  end;
  DGLSLogger.LogInfo('TDGLtranformation : Get Frustrum from pos '+inttostr(FStackPos));
  Result := FStack[FStackPos].FFrustum;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

end.

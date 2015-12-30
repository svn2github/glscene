//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{: GLPipelineTransformation<p>

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

  TDGLPipelineTransformationState =
  (
    trsModelViewChanged,
    trsInvModelViewChanged,
    trsInvModelChanged,
    trsNormalModelChanged,
    trsViewProjChanged,
    trsFrustum
  );

  TDGLPipelineTransformationStates = set of TDGLPipelineTransformationState;

const
  cAllStatesChanged = [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsViewProjChanged, trsNormalModelChanged, trsFrustum];

type

  PTransformationRec = ^TTransformationRec;
  TTransformationRec = record
    FStates: TDGLPipelineTransformationStates;
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
    FStack: TStackList; // array of TTransformationRec;
    //FLoadMatricesEnabled: Boolean;
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
//    procedure LoadModelViewMatrix; {$IFDEF GLS_INLINE} inline; {$ENDIF}
//    procedure LoadProjectionMatrix; {$IFDEF GLS_INLINE} inline; {$ENDIF}
    procedure DoMatricesLoaded; {$IFDEF GLS_INLINE} inline; {$ENDIF}
    property OnPush: TOnMatricesPush read FOnPush write FOnPush;
  public
    constructor Create;
    destructor Destroy; override;

    procedure IdentityAll;
    procedure Push(AValue: PTransformationRec = nil);
    procedure Pop;
    procedure ReplaceFromStack;
    function Current: TTransformationRec;

    property ModelMatrix: TMatrix read GetModelMatrix write SetModelMatrix;
    property ViewMatrix: TMatrix read GetViewMatrix write SetViewMatrix;
    property ProjectionMatrix: TMatrix read GetProjectionMatrix write SetProjectionMatrix;

    property InvModelMatrix: TMatrix read GetInvModelMatrix;
    property ModelViewMatrix: TMatrix read GetModelViewMatrix;
    property NormalModelMatrix: TAffineMatrix read GetNormalModelMatrix;
    property InvModelViewMatrix: TMatrix read GetInvModelViewMatrix;
    property ViewProjectionMatrix: TMatrix read GetViewProjectionMatrix;
    property Frustum: TFrustum read GetFrustum;

//    property LoadMatricesEnabled: Boolean read FLoadMatricesEnabled write FLoadMatricesEnabled;
  end;

// Prevent Lazaruses issue with checksumm chenging!
//type
//  TGLCall = function(): TGLExtensionsAndEntryPoints;
//var
//  vLocalGL: TGLCall;

implementation

// ------------------
{ TDGLTransformation }
{$IFDEF GLS_REGION}{$REGION 'TDGLTransformation'}{$ENDIF}

constructor TDGLTransformation.Create;
begin
  FStack := TStackList.Create;
//  FStackPos := 0;
//  SetLength(FStack, 1);
  IdentityAll;
end;

Destructor TDGLTransformation.Destroy;
begin
  FreeAndNil(FStack);
end;

procedure TDGLTransformation.IdentityAll;
begin
  with TTransformationRec(FStack.Top.Data^)  do
  begin
    FModelMatrix := IdentityHmgMatrix;
    FViewMatrix := IdentityHmgMatrix;
    FProjectionMatrix := IdentityHmgMatrix;
    FStates := cAllStatesChanged;
  end;
//  if LoadMatricesEnabled then
//  begin
//    LoadModelViewMatrix;
//    LoadProjectionMatrix;
//  end;
end;

procedure TDGLTransformation.Push(AValue: PTransformationRec);
begin

  if Assigned(AValue) then
  begin
    FStack.Push(AValue);
//    if LoadMatricesEnabled then
//    begin
//      LoadModelViewMatrix;
//      LoadProjectionMatrix;
//    end;
    DoMatricesLoaded;
  end
end;

procedure TDGLTransformation.Pop;
begin
  FStack.Pop;
//  if LoadMatricesEnabled then
//  begin
//    LoadModelViewMatrix;
//    LoadProjectionMatrix;
//  end;
end;

procedure TDGLTransformation.ReplaceFromStack;
Var p:Pointer;
begin
  P:=FStack.Top.Prev.Data;
  FStack.Top.Data := P;
//  if LoadMatricesEnabled then
//  begin
//    LoadModelViewMatrix;
//    LoadProjectionMatrix;
//  end;
end;

//procedure TDGLTransformation.LoadModelViewMatrix;
//var
//  M: TMatrix;
//begin
//  M := GetModelViewMatrix;
////  LoadMatrixf(PGLFloat(@M));
//end;

//procedure TDGLTransformation.LoadProjectionMatrix;
//begin
////    MatrixMode(GL_PROJECTION);
////    LoadMatrixf(PGLFloat(@FStack[FStackPos].FProjectionMatrix));
////    MatrixMode(GL_MODELVIEW);
//end;

function TDGLTransformation.GetModelMatrix: TMatrix;
begin
  Result := TTransformationRec(FStack.Top.Data^).FModelMatrix;
end;

function TDGLTransformation.GetViewMatrix: TMatrix;
begin
  Result := TTransformationRec(FStack.Top.Data^).FViewMatrix;
end;

function TDGLTransformation.GetProjectionMatrix: TMatrix;
begin
  Result := TTransformationRec(FStack.Top.Data^).FProjectionMatrix;
end;

procedure TDGLTransformation.SetModelMatrix(const AMatrix: TMatrix);
begin
  TTransformationRec(FStack.Top.Data^).FModelMatrix := AMatrix;
  TTransformationRec(FStack.Top.Data^).FStates := TTransformationRec(FStack.Top.Data^).FStates + [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged];

end;

procedure TDGLTransformation.SetViewMatrix(const AMatrix: TMatrix);
begin
  TTransformationRec(FStack.Top.Data^).FViewMatrix:= AMatrix;
  TTransformationRec(FStack.Top.Data^).FStates := TTransformationRec(FStack.Top.Data^).FStates +[trsModelViewChanged, trsInvModelViewChanged, trsViewProjChanged, trsFrustum];
//  if LoadMatricesEnabled then
//    LoadModelViewMatrix;
end;

function TDGLTransformation.Current: TTransformationRec;
begin
  Result := TTransformationRec(FStack.Top.Data^);
end;

procedure TDGLTransformation.SetProjectionMatrix(const AMatrix: TMatrix);
begin
  TTransformationRec(FStack.Top.Data^).FProjectionMatrix := AMatrix;
  TTransformationRec(FStack.Top.Data^).FStates := TTransformationRec(FStack.Top.Data^).FStates + [trsViewProjChanged, trsFrustum];
//  if LoadMatricesEnabled then
//    LoadProjectionMatrix;
end;

function TDGLTransformation.GetModelViewMatrix: TMatrix;
begin
  if trsModelViewChanged in TTransformationRec(FStack.Top.Data^).FStates then
  begin
    TTransformationRec(FStack.Top.Data^).FModelViewMatrix :=
      MatrixMultiply(TTransformationRec(FStack.Top.Data^).FModelMatrix, TTransformationRec(FStack.Top.Data^).FViewMatrix);
    Exclude(TTransformationRec(FStack.Top.Data^).FStates, trsModelViewChanged);
  end;
  Result := TTransformationRec(FStack.Top.Data^).FModelViewMatrix;
end;

function TDGLTransformation.GetInvModelViewMatrix: TMatrix;
begin
  if trsInvModelViewChanged in TTransformationRec(FStack.Top.Data^).FStates then
  begin
    TTransformationRec(FStack.Top.Data^).FInvModelViewMatrix := GetModelViewMatrix;
    InvertMatrix(TTransformationRec(FStack.Top.Data^).FInvModelViewMatrix);
    Exclude(TTransformationRec(FStack.Top.Data^).FStates, trsInvModelViewChanged);
  end;
  Result := TTransformationRec(FStack.Top.Data^).FInvModelViewMatrix;
end;

function TDGLTransformation.GetInvModelMatrix: TMatrix;
begin
  if trsInvModelChanged in TTransformationRec(FStack.Top.Data^).FStates then
  begin
    TTransformationRec(FStack.Top.Data^).FInvModelMatrix := MatrixInvert(TTransformationRec(FStack.Top.Data^).FModelMatrix);
    Exclude(TTransformationRec(FStack.Top.Data^).FStates, trsInvModelChanged);
  end;
  Result := TTransformationRec(FStack.Top.Data^).FInvModelMatrix;
end;

function TDGLTransformation.GetNormalModelMatrix: TAffineMatrix;
var
  M: TMatrix;
begin
  if trsNormalModelChanged in TTransformationRec(FStack.Top.Data^).FStates then
  begin
    M := TTransformationRec(FStack.Top.Data^).FModelMatrix;
    NormalizeMatrix(M);
    SetMatrix(TTransformationRec(FStack.Top.Data^).FNormalModelMatrix, M);
    Exclude(TTransformationRec(FStack.Top.Data^).FStates, trsNormalModelChanged);
  end;
  Result := TTransformationRec(FStack.Top.Data^).FNormalModelMatrix;
end;

function TDGLTransformation.GetViewProjectionMatrix: TMatrix;
begin
  if trsViewProjChanged in TTransformationRec(FStack.Top.Data^).FStates then
  begin
    TTransformationRec(FStack.Top.Data^).FViewProjectionMatrix :=
      MatrixMultiply(TTransformationRec(FStack.Top.Data^).FViewMatrix, TTransformationRec(FStack.Top.Data^).FProjectionMatrix);
    Exclude(TTransformationRec(FStack.Top.Data^).FStates, trsViewProjChanged);
  end;
  Result := TTransformationRec(FStack.Top.Data^).FViewProjectionMatrix;
end;

procedure TDGLTransformation.DoMatricesLoaded;
begin
  if Assigned(FOnPush) then
    FOnPush();
end;

function TDGLTransformation.GetFrustum: TFrustum;
begin
  if trsFrustum in TTransformationRec(FStack.Top.Data^).FStates then
  begin
    TTransformationRec(FStack.Top.Data^).FFrustum := ExtractFrustumFromModelViewProjection(GetViewProjectionMatrix);
    Exclude(TTransformationRec(FStack.Top.Data^).FStates, trsFrustum);
  end;
  Result := TTransformationRec(FStack.Top.Data^).FFrustum;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

end.

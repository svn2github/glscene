//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLScene.Objects.SpaceText<p>

  <b>History : </b><font size=-1><ul>
  <li>19/11/11 - Yar - Creation
  </ul></font>
}

unit GLScene.Objects.SpaceText;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,

  GLScene.Base.Classes,
  GLScene.Base.Context.Info,
  GLScene.Base.Transformation,
  GLScene.Base.GeometryBB,
  GLScene.Base.Vector.Lists,
  GLScene.Core,
  GLScene.Mesh,
  GLScene.DrawTechnique,
  GLScene.Platform,
  GLScene.Objects,
  GLScene.VectorFont;

type

  // TGLTextHorzAdjust
  //
  // Note: haAligned, haCentrically, haFitIn have not been implemented!
  //
  TGLTextHorzAdjust = (haLeft, haCenter, haRight, haAligned,
    haCentrically, haFitIn);

  // TGLTextVertAdjust
  //
  TGLTextVertAdjust = (vaTop, vaCenter, vaBottom, vaBaseLine);

  // TGLTextAdjust
  //
  TGLTextAdjust = class(TPersistent)
  private
    { Private Declarations }
    FHorz: TGLTextHorzAdjust;
    FVert: TGLTextVertAdjust;
    FOnChange: TNotifyEvent;
    procedure SetHorz(const Value: TGLTextHorzAdjust);
    procedure SetVert(const Value: TGLTextVertAdjust);

  public
    { public Declarations }
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  published
    { Published Declarations }
    property Horz: TGLTextHorzAdjust read FHorz write SetHorz default haLeft;
    property Vert: TGLTextVertAdjust read FVert write SetVert
      default vaBaseLine;
  end;

  TGLSpaceText = class(TGLSceneObjectEx)
  protected
    { Protected Declarations }
    FVectorFont: TGLCustomVectorFont;
    FAllowedDeviation: Single;
    FAdjust: TGLTextAdjust;
    FHSpace: Single;
    FTextHeight: Single;
    FEditableLines: TStringList;
    FVisualizedLines: TStringList;
    FBatches: TDrawBatchArray;
    FLocations: TAffineVectorList;
    FTransformations: array of TTransformationRec;
    FLocationChanged: Boolean;
    procedure BuildMesh; override; stdcall;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure OnTextChanged(Sender: TObject);
    procedure OnAdjustChanged(Sender: TObject);
    procedure FreeBatches;
    procedure CalcLocations;
  private

    { Private Declarations }
    procedure SetVectorFont(Value: TGLCustomVectorFont);
    function GetText: WideString;
    procedure SetLines(const Value: TStringList);
    procedure SetText(const Value: WideString);
    procedure SetAdjust(const Value: TGLTextAdjust);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    { Published Declarations }
    property VectorFont: TGLCustomVectorFont read FVectorFont
      write SetVectorFont;
    property Text: WideString read GetText write SetText stored False;
    property Lines: TStringList read FEditableLines write SetLines;
    property Adjust: TGLTextAdjust read FAdjust write SetAdjust;
    property HSpace: Single read FHSpace write FHSpace;
  end;

implementation

uses
  GLScene.Base.Vector.Geometry,
  GLScene.Base.Vector.Types;

{$REGION 'TGLTextAdjust'}
// ------------------
// ------------------ TGLTextAdjust ------------------
// ------------------

constructor TGLTextAdjust.Create;
begin
  inherited;
  FHorz := haLeft;
  FVert := vaBaseLine;
end;

procedure TGLTextAdjust.Assign(Source: TPersistent);
begin
  if Source is TGLTextAdjust then
  begin
    FHorz := TGLTextAdjust(Source).Horz;
    FVert := TGLTextAdjust(Source).Vert;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TGLTextAdjust.SetHorz(const Value: TGLTextHorzAdjust);
begin
  if FHorz <> Value then
  begin
    FHorz := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TGLTextAdjust.SetVert(const Value: TGLTextVertAdjust);
begin
  if Value <> FVert then
  begin
    FVert := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;
{$ENDREGION}
{$REGION 'TGLSpaceText'}
// ------------------
// ------------------ TGLSpaceText ------------------
// ------------------

procedure TGLSpaceText.BuildMesh;
var
  I: Integer;

  function IsLineDifferent: Boolean;
  begin
    if FVisualizedLines.Count <= I then
    begin
      FVisualizedLines.Add('');
      exit(True);
    end;
    Result := FEditableLines[I] <> FVisualizedLines[I];
  end;

  procedure CheckBatchesRange;
  begin
    if High(FBatches) < I then
    begin
      SetLength(FBatches, I+1);
      SetLength(FTransformations, I+1);
      with FBatches[I] do
      begin
        Mesh := TMeshAtom.Create;
        Mesh.Owner := Self;
        Mesh.TagName := ClassName + Format('-Line%d', [I]);
        Material := FMaterial;
        PickCallback := DoOnPicked;
      end;
    end;
  end;

begin
  FLocationChanged := False;
  if Assigned(FVectorFont) and (FEditableLines.Count > 0) then
  begin
    for I := 0 to FEditableLines.Count - 1 do
    begin
      CheckBatchesRange;
      if IsLineDifferent then
        FVectorFont.BuildString(FBatches[I], FEditableLines[I]);
    end;
    FVisualizedLines.Assign(FEditableLines);
    CalcLocations;
  end;

  inherited;
end;

procedure TGLSpaceText.CalcLocations;
var
  I: Integer;
  H, lineW, firstLineH, lineH, fullH: Single;
  LAABB: TAABB;
  Pos: TAffineVector;
begin
  FLocations.Count := FVisualizedLines.Count;
  if not Assigned(FVectorFont) then
    exit;

  FLocationChanged := False;

  H := 0;
  fullH := 0;
  firstLineH := 0;
  Pos := NullVector;

  // Horizontal adjust
  for I := 0 to FVisualizedLines.Count - 1 do
  begin
    LAABB := FVectorFont.GetAABB(FVisualizedLines[I]);
    lineW := LAABB.max[0] - LAABB.min[0];
    lineH := LAABB.max[1] - LAABB.min[1];
    fullH := fullH + lineH;
    case FAdjust.Horz of
      haLeft:
        ; // nothing
      haCenter:
        Pos[0] := -lineW * 0.5;
      haRight:
        Pos[0] := -lineW;
    end;
    if I > 0 then
      H := H - lineH - FHSpace
    else
      firstLineH := lineH;
    Pos[1] := H;
    FLocations[I] := Pos;
  end;

  fullH := fullH + FHSpace * (FVisualizedLines.Count - 1);

  case FAdjust.Vert of
    vaBaseLine:
      exit; // nothing;
    vaBottom:
      H := fullH - firstLineH;
    vaCenter:
      H := fullH * 0.5 - firstLineH;
    vaTop:
      H := -firstLineH;
  end;

  FLocations.Translate(AffineVectorMake(0, H, 0));
end;

constructor TGLSpaceText.Create(AOwner: TComponent);
begin
  inherited;
  FEditableLines := TStringList.Create;
  FVisualizedLines := TStringList.Create;
  FEditableLines.Add('ABC');
  FEditableLines.OnChange := OnTextChanged;
  FHSpace := 0.4;
  FAdjust := TGLTextAdjust.Create;
  FAdjust.OnChange := OnAdjustChanged;
  FLocations := TAffineVectorList.Create;
end;

destructor TGLSpaceText.Destroy;
begin
  VectorFont := nil;
  FEditableLines.Free;
  FVisualizedLines.Free;
  FreeBatches;
  FAdjust.Free;
  FLocations.Destroy;
  inherited;
end;

procedure TGLSpaceText.FreeBatches;
var
  I: Integer;
begin
  for I := High(FBatches) downto 0 do
    FBatches[I].Mesh.Free;
  SetLength(FBatches, 0);
  SetLength(FTransformations, 0);
end;

procedure TGLSpaceText.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  I: Integer;
  M: TMatrix;
begin
  inherited DoRender(ARci, False, False);

  if FLocationChanged then
    CalcLocations;

  if ARenderSelf and (FVisualizedLines.Count > 0) then
  begin
    M := ARci.PipelineTransformation.ModelMatrix;
    for I := 0 to FVisualizedLines.Count - 1 do
    begin
      if High(FBatches) < I then
        break;
      ARci.PipelineTransformation.Push;
      ARci.PipelineTransformation.ModelMatrix :=
        MatrixMultiply(M, CreateTranslationMatrix(FLocations[I]));
      FTransformations[I] := ARci.PipelineTransformation.StackTop;
      ARci.PipelineTransformation.Pop;
      FBatches[I].Transformation := @FTransformations[I];
      FBatches[I].Material := FBatch.Material;
      FBatches[I].PickingMaterial := FBatch.PickingMaterial;
      ARci.drawList.Add(@FBatches[I]);
    end;
  end;

  if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

function TGLSpaceText.GetText: WideString;
begin
  if FEditableLines.Count = 1 then
    Result := FEditableLines[0]
  else
    Result := FEditableLines.Text;
end;

procedure TGLSpaceText.SetAdjust(const Value: TGLTextAdjust);
begin
  FAdjust.Assign(Value);
  StructureChanged;
end;

procedure TGLSpaceText.SetLines(const Value: TStringList);
begin
  FEditableLines.Assign(Value);
end;

procedure TGLSpaceText.SetText(const Value: WideString);
begin
  if GetText <> Value then
    FEditableLines.Text := Value;
end;

procedure TGLSpaceText.SetVectorFont(Value: TGLCustomVectorFont);
begin
  if Value <> FVectorFont then
  begin
    if Assigned(FVectorFont) then
      FVectorFont.UnRegisterUser(Self);
    FVectorFont := Value;
    if Assigned(FVectorFont) then
    begin
      FVectorFont.RegisterUser(Self);
      FVectorFont.FreeNotification(Self);
    end;
    StructureChanged;
  end;
end;

procedure TGLSpaceText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FVectorFont) then
    VectorFont := nil;
  inherited;
end;

procedure TGLSpaceText.NotifyChange(Sender: TObject);
begin
  inherited NotifyChange(Sender);
  if Assigned(Sender) and (Sender = FVectorFont) then
  begin
    FVisualizedLines.Clear;
  end;
end;

procedure TGLSpaceText.OnTextChanged(Sender: TObject);
begin
  FLocationChanged := True;
  StructureChanged;
end;

procedure TGLSpaceText.OnAdjustChanged(Sender: TObject);
begin
  FLocationChanged := True;
end;

{$ENDREGION}

initialization

RegisterClass(TGLSpaceText);

end.

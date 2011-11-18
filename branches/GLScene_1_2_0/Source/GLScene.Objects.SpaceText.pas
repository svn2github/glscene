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
  GLScene.Mesh,
  GLScene.DrawTechnique,
  GLScene.Platform,
  GLScene.Objects,
  GLScene.VectorFont;

type

  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles = set of TFontStyle;
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
    FAspectRatio: Single;
    FOblique: Single;
    FHSpace: Single;
    FTextHeight: Single;
    FLines: TStringList;
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
  published
    { Published Declarations }
    property VectorFont: TGLCustomVectorFont read FVectorFont
      write SetVectorFont;
    property Text: WideString read GetText write SetText stored False;
    property Lines: TStringList read FLines write SetLines;
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
begin
  if Assigned(FVectorFont) and (FLines.Count > 0) then
  begin
    FVectorFont.BuildString(FBatch, FLines[0]);

    for I := 1 to FLines.Count - 1 do
    begin
      if Length(FBatches) < I then
      begin
        SetLength(FBatches, I);
        SetLength(FTransformations, I);
        with FBatches[I-1] do
        begin
          Mesh := TMeshAtom.Create;
          Mesh.Owner := Self;
          Mesh.TagName := ClassName+Format('-Line%d', [I]);
          Material := FMaterial;
          PickCallback := DoOnPicked;
        end;
        FVectorFont.BuildString(FBatches[I-1], FLines[I]);
      end;
    end;

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
  FLocations.Count := FLines.Count;
  if not Assigned(FVectorFont) then
    exit;

  FLocationChanged := False;

  H := 0;
  fullH := 0;
  firstLineH := 0;
  Pos := NullVector;

  // Horizontal adjust
  for I := 0 to FLines.Count - 1 do
  begin
    LAABB := FVectorFont.GetAABB(FLines[I]);
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

  fullH := fullH + FHSpace * (FLines.Count - 1);

  case FAdjust.Vert of
    vaBaseLine:
      exit; // nothing;
    vaBottom:
      H := fullH - firstLineH;
    vaCenter:
      H := fullH * 0.5 - firstLineH;
    vaTop:
      H := - firstLineH;
  end;

  FLocations.Translate(AffineVectorMake(0, H, 0));
end;

constructor TGLSpaceText.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TStringList.Create;
  FLines.Add('ABC');
  FLines.OnChange := OnTextChanged;
  FHSpace:= 0.4;
  Static := False;
  FAdjust := TGLTextAdjust.Create;
  FAdjust.OnChange := OnAdjustChanged;
  FLocations := TAffineVectorList.Create;
end;

destructor TGLSpaceText.Destroy;
begin
  VectorFont := nil;
  FLines.Free;
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
end;

procedure TGLSpaceText.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  I: Integer;
  M: TMatrix;
begin
  inherited DoRender(ARci, ARenderSelf, False);

  if FLocationChanged then
    CalcLocations;

  if ARenderSelf and (FLines.Count > 1) then
  begin
    M := ARci.PipelineTransformation.ModelMatrix;
    ARci.PipelineTransformation.Push;
    ARci.PipelineTransformation.ModelMatrix :=
      MatrixMultiply(M, CreateTranslationMatrix(FLocations[0]));
    FTransformation := ARci.PipelineTransformation.StackTop;
    ARci.PipelineTransformation.Pop;
    for I := 0 to FLines.Count - 2 do
    begin
      ARci.PipelineTransformation.Push;
      ARci.PipelineTransformation.ModelMatrix :=
        MatrixMultiply(M, CreateTranslationMatrix(FLocations[I+1]));
      FTransformations[I] := ARci.PipelineTransformation.StackTop;
      ARci.PipelineTransformation.Pop;
      FBatches[I].Transformation := @FTransformations[I];
      ARci.drawList.Add(@FBatches[I]);
    end;
  end;

  if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

function TGLSpaceText.GetText: WideString;
begin
  if FLines.Count = 1 then
    Result := FLines[0]
  else
    Result := FLines.Text;
end;

procedure TGLSpaceText.SetAdjust(const Value: TGLTextAdjust);
begin
  FAdjust.Assign(Value);
  StructureChanged;
end;

procedure TGLSpaceText.SetLines(const Value: TStringList);
begin
  FLines.Assign(Value);
end;

procedure TGLSpaceText.SetText(const Value: WideString);
begin
  if GetText <> Value then
    FLines.Text := Value;
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

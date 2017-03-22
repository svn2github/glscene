unit uTweener;
{Author: lampogolovii (lampogolovii.blogspot.com)}

interface

uses
  System.Classes,
  System.Math,
  GLVectorGeometry;

type
//==============================================================================
  TVarType = (vt_Integer, vt_Single, vt_Vector);
  PInteger = ^Integer;
  PSingle = ^Single;
  PVector = ^TVector;
//==============================================================================
  TTweenObject = TObject;
//==============================================================================
  // aUnitValue - from 0 to 1
  TEasingFunc = Function (aStartValue, aDiffValue, aUnitValue: Single): Single of object;
//==============================================================================
  TTweenStyle = (ts_ElasticEaseIn, ts_ElasticEaseOut, ts_ExpoEaseIn, ts_Bounce);
//==============================================================================
  TSetSingle = Procedure (aObject: TTweenObject; aValue: Single);
//==============================================================================
  TBaseTweenItem = class
  protected
    fPaused: Boolean;
    fTime: Single;
    fPauseOnStart: Single;
    fDone: Boolean;
    fDuration: Single;
  protected
    fEasingFunc: TEasingFunc;
    Function GetUnitValue: Single; virtual;
    Function ShouldChange: Boolean;
  public
    property Done: Boolean read fDone;
    Procedure Play; virtual;
    Procedure Pause; virtual;
    Procedure SetPause(const aPause: Boolean); virtual;
    Procedure Update(const aDeltaTime: Single); virtual;
    Constructor Create(aDuration: Single; aPauseOnStart: Single);
  end;
//==============================================================================
  TBaseSingleTweenItem = class (TBaseTweenItem)
  protected
    fStartValue, fFinishValue: Single;
  public
    Constructor Create(aStartValue, aFinishValue, aDuration: Single; aPauseOnStart: Single);
  end;
//==============================================================================
  TPSingleTweenItem = class (TBaseSingleTweenItem)
  protected
    fValue: PSingle;
  public
    Procedure Update(const aDeltaTime: Single); override;
    Constructor Create(aValue: PSingle; aStartValue, aFinishValue, aDuration: Single; aPauseOnStart: Single);
  end;
//==============================================================================
  TSingleTweenItem = class (TBaseSingleTweenItem)
  protected
    fSetSingleEvent: TSetSingle;
    fObject: TTweenObject;
  public
    property SetSingleEvent: TSetSingle read fSetSingleEvent write fSetSingleEvent;
    Procedure Update(const aDeltaTime: Single); override;
    Constructor Create(aObject: TTweenObject; aEvent: TSetSingle; aStartValue, aFinishValue, aDuration: Single; aPauseOnStart: Single);
  end;
//==============================================================================
  TBaseVectorTweenItem = class (TBaseTweenItem)
  protected
    fStartValue, fFinishValue: TVector;
  public
    Constructor Create(aStartValue, aFinishValue: TVector; aDuration: Single; aPauseOnStart: Single);
  end;
//==============================================================================
  TPVectorTweenItem = class (TBaseVectorTweenItem)
  protected
    fValue: PVector;
  public
    Procedure Update(const aDeltaTime: Single); override;
    Constructor Create(aValue: PVector; aStartValue, aFinishValue: TVector; aDuration: Single; aPauseOnStart: Single);
  end;
//==============================================================================
  TBaseEasingFunctions = class
  public
    class function expoEaseIn(aStartValue, aDiffValue, aUnitValue: Single): Single;
    class function quintEaseOut(aStartValue, aDiffValue, aUnitValue: Single): Single;
    class function elasticEaseIn(aStartValue, aDiffValue, aUnitValue: Single): Single;
  end;
//==============================================================================
  TTweener = class
  protected
    fTweenItems: TList;
    fEasingFunctions: TBaseEasingFunctions;
    Function GetTweenCount: Integer;
  public
    property TweenCount: Integer read GetTweenCount;
    // манипул€ци€ с элементами списка
    Function GetItemByIndex(const aIndex: integer): TBaseTweenItem;
    Procedure FreeByIndex(const aIndex: integer);
    Procedure FreeAll;
    Function AddTweenItem(aTweenItem: TBaseTweenItem; aTweenStyle: TTweenStyle): Integer; virtual;

    // добавление типовых элементов
    Function AddTweenPSingle(aVariable: PSingle;    aTweenStyle: TTweenStyle; const aStartValue, aFinishValue, aDuration: Single; const aPauseOnStart: Single = 0): TPSingleTweenItem;
    Function AddTweenPVector(aVariable: PVector;    aTweenStyle: TTweenStyle; const aStartValue, aFinishValue: TVector; aDuration: Single; const aPauseOnStart: Single = 0): TPVectorTweenItem;
    Function AddTweenSingle (aObject: TTweenObject; aSetValue: TSetSingle; aTweenStyle: TTweenStyle; const aStartValue, aFinishValue, aDuration: Single; const aPauseOnStart: Single = 0): TSingleTweenItem;

    Procedure Update(const aDeltaTime: Single);

    // конструктор + деструктор
    Constructor Create;
    Destructor Destroy; override;
  end;
//==============================================================================

var
  Tweener: TTweener;

implementation
uses
  SysUtils;
//==============================================================================
Function TBaseTweenItem.GetUnitValue: Single;
begin
  if fTime <= fPauseOnStart then
    result := 0
  else if fTime = fDuration + fPauseOnStart then
    result := 1
  else  
    result := (fTime - fPauseOnStart) / fDuration;
end;
//------------------------------------------------------------------------------
Function TBaseTweenItem.ShouldChange: Boolean;
begin
  result := fTime >= fPauseOnStart;
end;
//------------------------------------------------------------------------------
Procedure TBaseTweenItem.Play;
begin
  SetPause(false);
end;
//------------------------------------------------------------------------------
Procedure TBaseTweenItem.Pause;
begin
  SetPause(true);
end;
//------------------------------------------------------------------------------
Procedure TBaseTweenItem.SetPause(const aPause: Boolean);
begin
  fPaused := aPause;
end;
//------------------------------------------------------------------------------
Procedure TBaseTweenItem.Update(const aDeltaTime: Single);
begin
  if fPaused then
    exit;

  fTime := fTime + aDeltaTime;
  if fTime - fPauseOnStart >= fDuration then
  begin
    fTime := fDuration + fPauseOnStart;
    fDone := true;
    Pause;
  end;  
end;
//------------------------------------------------------------------------------
Constructor TBaseTweenItem.Create(aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create;
  fDuration := aDuration;
  fPauseOnStart := aPauseOnStart;
  fTime := 0;
  fDone := false;
end;
//==============================================================================
//==============================================================================
//==============================================================================
Constructor TBaseSingleTweenItem.Create(aStartValue, aFinishValue, aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aDuration, aPauseOnStart);
  fStartValue := aStartValue;
  fFinishValue := aFinishValue;
end;
//==============================================================================
//==============================================================================
//==============================================================================
Procedure TPSingleTweenItem.Update(const aDeltaTime: Single);
begin
  inherited Update(aDeltaTime);
  if Assigned(fEasingFunc) then
    fValue^ := fEasingFunc(fStartValue, fFinishValue - fStartValue, GetUnitValue);
end;
//------------------------------------------------------------------------------
Constructor TPSingleTweenItem.Create(aValue: PSingle; aStartValue, aFinishValue, aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aStartValue, aFinishValue, aDuration, aPauseOnStart);
  fValue := aValue;
end;
//==============================================================================
//==============================================================================
//==============================================================================
Procedure TSingleTweenItem.Update(const aDeltaTime: Single);
begin
  inherited Update(aDeltaTime);
  if Assigned(fSetSingleEvent) and Assigned(fEasingFunc) then
    fSetSingleEvent(fObject, fEasingFunc(fStartValue, fFinishValue - fStartValue, GetUnitValue));
end;
//------------------------------------------------------------------------------
Constructor TSingleTweenItem.Create(aObject: TTweenObject; aEvent: TSetSingle; aStartValue, aFinishValue, aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(fDuration, aStartValue, aFinishValue, aPauseOnStart);
  fObject := aObject;
  fSetSingleEvent := aEvent;
end;
//==============================================================================
//==============================================================================
//==============================================================================
Constructor TBaseVectorTweenItem.Create(aStartValue, aFinishValue: TVector; aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aDuration, aPauseOnStart);
  fStartValue := aStartValue;
  fFinishValue := aFinishValue;
end;
//==============================================================================
//==============================================================================
//==============================================================================
Procedure TPVectorTweenItem.Update(const aDeltaTime: Single);
begin
  inherited Update(aDeltaTime);
  if Assigned(fEasingFunc) and ShouldChange then
  begin
    fValue^.X := fEasingFunc(fStartValue.X, fFinishValue.X - fStartValue.X, GetUnitValue);
    fValue^.Y := fEasingFunc(fStartValue.Y, fFinishValue.Y - fStartValue.Y, GetUnitValue);
    fValue^.Z := fEasingFunc(fStartValue.Z, fFinishValue.Z - fStartValue.Z, GetUnitValue);
  end;
end;
//------------------------------------------------------------------------------
Constructor TPVectorTweenItem.Create(aValue: PVector; aStartValue, aFinishValue: TVector; aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aStartValue, aFinishValue, aDuration, aPauseOnStart);
  fValue := aValue;
end;
//==============================================================================
//==============================================================================
//==============================================================================
class function TBaseEasingFunctions.expoEaseIn(aStartValue, aDiffValue, aUnitValue: Single): Single;
begin
  if (aUnitValue = 1) then
    result := aStartValue + aDiffValue
  else
    result := aDiffValue * (-Power(2, -15*aUnitValue) + 1) + aStartValue;
end;
//------------------------------------------------------------------------------
class function TBaseEasingFunctions.quintEaseOut(aStartValue, aDiffValue, aUnitValue: Single): Single;
begin
  result := aStartValue;
end;
//------------------------------------------------------------------------------
class function TBaseEasingFunctions.elasticEaseIn(aStartValue, aDiffValue, aUnitValue: Single): Single;
begin
  if (aDiffValue = 0) or (aUnitValue = 0) or (aUnitValue = 1) then
  begin
    if (aUnitValue = 1) then
      result := aStartValue + aDiffValue
    else
      result := aStartValue;
    exit;
  end;
  result := (aDiffValue * Power(2, -10 * aUnitValue) * Sin((aUnitValue - 0.25/4)*(2*3.14)/0.25)) + aDiffValue + aStartValue;
end;
//==============================================================================
//==============================================================================
//==============================================================================
Function TTweener.GetTweenCount: Integer;
begin
  result := fTweenItems.Count;
end;
//------------------------------------------------------------------------------
Function TTweener.GetItemByIndex(const aIndex: integer): TBaseTweenItem;
begin
  result := TBaseTweenItem(fTweenItems[aIndex]);
end;
//------------------------------------------------------------------------------
Procedure TTweener.FreeByIndex(const aIndex: integer);
var
  TweenItem: TBaseTweenItem;
begin
  TweenItem := GetItemByIndex(aIndex);
  FreeAndNil(TweenItem);
  fTweenItems.Delete(aIndex);
end;
//------------------------------------------------------------------------------
Procedure TTweener.FreeAll;
begin
  while fTweenItems.Count > 0 do
    FreeByIndex(0);  
end;
//------------------------------------------------------------------------------
Function TTweener.AddTweenItem(aTweenItem: TBaseTweenItem; aTweenStyle: TTweenStyle): Integer;
begin
  case aTweenStyle of
    ts_ElasticEaseIn:  aTweenItem.fEasingFunc := fEasingFunctions.elasticEaseIn;
    ts_ElasticEaseOut: aTweenItem.fEasingFunc := fEasingFunctions.elasticEaseIn;
    ts_ExpoEaseIn:     aTweenItem.fEasingFunc := fEasingFunctions.expoEaseIn;
    ts_Bounce:         aTweenItem.fEasingFunc := fEasingFunctions.elasticEaseIn;
  end;
  result := fTweenItems.Add(aTweenItem);
end;
//------------------------------------------------------------------------------
Function TTweener.AddTweenPSingle(aVariable: PSingle; aTweenStyle: TTweenStyle; const aStartValue, aFinishValue, aDuration: Single; const aPauseOnStart: Single = 0): TPSingleTweenItem;
begin
  result := TPSingleTweenItem.Create(aVariable, aStartValue, aFinishValue, aDuration, aPauseOnStart);
  AddTweenItem(result, aTweenStyle);
end;
//------------------------------------------------------------------------------
Function TTweener.AddTweenPVector(aVariable: PVector; aTweenStyle: TTweenStyle; const aStartValue, aFinishValue: TVector; aDuration: Single; const aPauseOnStart: Single = 0): TPVectorTweenItem;
begin
  result := TPVectorTweenItem.Create(aVariable, aStartValue, aFinishValue, aDuration, aPauseOnStart);
  AddTweenItem(result, aTweenStyle);
end;
//------------------------------------------------------------------------------
Function TTweener.AddTweenSingle(aObject: TTweenObject; aSetValue: TSetSingle; aTweenStyle: TTweenStyle; const aStartValue, aFinishValue, aDuration: Single; const aPauseOnStart: Single = 0): TSingleTweenItem;
begin
  result := TSingleTweenItem.Create(aObject, aSetValue, aStartValue, aFinishValue, aDuration, aPauseOnStart);
  AddTweenItem(result, aTweenStyle);
end;
//------------------------------------------------------------------------------
Procedure TTweener.Update(const aDeltaTime: Single);
var
  i: integer;
  item: TBaseTweenItem;
begin
  i := 0;
  while (i < TweenCount)do
  begin
    item := GetItemByIndex(i);
    if item.Done then
      FreeByIndex(i)
    else
    begin
      item.Update(aDeltaTime);
      inc(i);
    end;
  end;
end;
//------------------------------------------------------------------------------
Constructor TTweener.Create;
begin
  inherited Create;
  fTweenItems := TList.Create;
  fEasingFunctions := TBaseEasingFunctions.Create;
end;
//------------------------------------------------------------------------------
Destructor TTweener.Destroy;
begin
  FreeAll;
  FreeAndNil(fEasingFunctions);
  FreeAndNil(fTweenItems);
  inherited Destroy;
end;
//==============================================================================

initialization
  Tweener := TTweener.Create;

finalization
  Tweener.Free;

end.

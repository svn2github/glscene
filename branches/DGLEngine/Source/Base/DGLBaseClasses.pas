//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : Base classes for GLScene.<p>

  <b>History : </b><font size=-1><ul>
  <li>22/12/15 - JD - Imported From GLScene
  </ul></font>
}

unit DGLBaseClasses;

interface

uses
  System.Classes, System.SysUtils,
  // DGLE
  DGLSLog,
  DGLResStrings,
  DGLPersistentClasses,
  DGLCrossPlatform;

type

  // TProgressTimes
  //
  TProgressTimes = record
    deltaTime, newTime: Double end;

    // TGLProgressEvent
    //
    { : Progression event for time-base animations/simulations.<p>
      deltaTime is the time delta since last progress and newTime is the new
      time after the progress event is completed. }
    TDGLProgressEvent = procedure(Sender: TObject; const deltaTime, newTime: Double) of object;

  IDGLNotifyAble = interface(IInterface)
    ['{A9C11C23-7789-4AAC-A950-372C3EE9E47E}']
    procedure NotifyChange(Sender: TObject);
  end;

  IDGLProgessAble = interface(IInterface)
    ['{AB913A82-443B-49D6-8AC7-659FD4DC3B9E}']
    procedure DoProgress(const progressTime: TProgressTimes);
  end;

  // TDGLUpdateAbleObject
  //
  { : An abstract class describing the "update" interface.<p> }
  TDGLUpdateAbleObject = class(TDGLInterfacedPersistent, IDGLNotifyAble)
  private
    { Private Declarations }
    FOwner:          TPersistent;
    FUpdating:       Integer;
    FOnNotifyChange: TNotifyEvent;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); virtual;

    procedure NotifyChange(Sender: TObject); virtual;
    procedure Notification(Sender: TObject; Operation: TOperation); virtual;
    function GetOwner: TPersistent; override;

    property Updating: Integer read FUpdating;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner: TPersistent read FOwner;
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

  // TDGLCadenceAbleComponent
  //
  { : A base class describing the "cadenceing" interface.<p> }
  TDGLCadenceAbleComponent = class(TDGLComponent, IDGLProgessAble)
  public
    { Public Declarations }
    procedure DoProgress(const progressTime: TProgressTimes); virtual;
  end;

  // TDGLUpdateAbleComponent
  //
  { : A base class describing the "update" interface.<p> }
  TDGLUpdateAbleComponent = class(TDGLCadenceAbleComponent, IDGLNotifyAble)
  public
    { Public Declarations }
    procedure NotifyChange(Sender: TObject); virtual;
  end;

  // TNotifyCollection
  //
  TNotifyCollection = class(TOwnedCollection)
  private
    { Private Declarations }
    FOnNotifyChange: TNotifyEvent;

  protected
    { Protected Declarations }
    procedure Update(item: TCollectionItem); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

  PStackElement = ^TStackElement;

  TStackElement = record
    Data: pointer;
    Prev: PStackElement;
    Next: PStackElement;
  end;

  TStackEnumProc = procedure(Data: pointer; var Break: boolean);

  // TStackList (LIFO)
  //
  TStackList = class
  private
    FRoot:               PStackElement;
    FTop:                PStackElement;
    FMaxElement, FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Push(Value: pointer): PStackElement;
    function Pop: Pointer;

    function Find(Value: pointer): PStackElement;
    procedure Remove(Element: PStackElement);
    procedure Enum(EnumProc: TStackEnumProc);

    property Root: PStackElement read FRoot;
    property Top: PStackElement read FTop;
    property MaxElement: Integer read FMaxElement write FMaxElement;
    property Count: Integer read FCount;
  end;

implementation

// ------------------
// { TDGLUpdateAbleObject }
{$IFDEF GLS_REGIONS}{$REGION 'TDGUpdateAbleObject'}{$ENDIF}


// Create
//

constructor TDGLUpdateAbleObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

// NotifyChange
//

procedure TDGLUpdateAbleObject.NotifyChange(Sender: TObject);
begin
  if FUpdating = 0 then
  begin
    if Assigned(Owner) then
    begin
      if Owner is TDGLUpdateAbleObject then
        TDGLUpdateAbleObject(Owner).NotifyChange(Self)
      else if Owner is TDGLUpdateAbleComponent then
        TDGLUpdateAbleComponent(Owner).NotifyChange(Self);
    end;
    if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
  end;
end;

// Notification
//

procedure TDGLUpdateAbleObject.Notification(Sender: TObject; Operation: TOperation);
begin
end;

// GetOwner
//

function TDGLUpdateAbleObject.GetOwner: TPersistent;
begin
  Result := Owner;
end;

// BeginUpdate
//

procedure TDGLUpdateAbleObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

// EndUpdate
//

procedure TDGLUpdateAbleObject.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating <= 0 then
  begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// { TDGLCadenceAbleComponent }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLCadenceAbleComponent'}{$ENDIF}


// DoProgress
//

procedure TDGLCadenceAbleComponent.DoProgress(const progressTime: TProgressTimes);
begin
  // nothing
end;

// ------------------
// ------------------TDGLUpdateAbleObject ------------------
// ------------------

// NotifyChange
//

procedure TDGLUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TDGLUpdateAbleComponent) then
      (Owner as TDGLUpdateAbleComponent).NotifyChange(Self);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// { TNotifyCollection }
{$IFDEF GLS_REGIONS}{$REGION 'TNotifyCollection'}{$ENDIF}
// Create
//

constructor TNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  if Assigned(AOwner) and (AOwner is TDGLUpdateAbleComponent) then
    OnNotifyChange := TDGLUpdateAbleComponent(AOwner).NotifyChange;
end;

// Update
//

procedure TNotifyCollection.Update(item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TStackList }
{$IFDEF GLS_REGIONS}{$REGION 'TStackList'}{$ENDIF}

function TStackList.Pop: pointer;
begin
  DGLSLogger.LogInfo('Pop Stack Element');
  if Assigned(FTop) and (FCount > 1) then
  begin
    Dec(FCount);
    Result := FTop.Data;
    Remove(FTop);
  end
  else
  begin
    //DGLSLogger.LogError('Transformation stack underflow');
    Result := FTop.Data;
  end;
end;

function TStackList.Push(Value: pointer): PStackElement;
var
  Element: PStackElement;
begin
  DGLSLogger.LogInfo('Push Stack Element');
  Inc(FCount);
  if FCount > FMaxElement then
  begin
    DGLSLogger.LogWarningFmt('Transformation stack overflow, more then %d values', [FMaxElement]);
    result:=nil;
    exit;
  end;

  new(Element);
  Result       := Element;
  Element.Data := Value;
  Element.Next := nil;
  if not Assigned(FRoot) then
  begin
    DGLSLogger.LogInfo('Assign Root Stack Element');
    FRoot      := Element;
    FTop       := FRoot;
    FRoot.Prev := nil;
  end
  else
  begin
    DGLSLogger.LogInfo('New on Top Stack Element');
    FTop.Next    := Element;
    Element.Prev := FTop;
    FTop         := Element;
  end;
end;

constructor TStackList.Create;
begin
  inherited;
  FRoot       := nil;
  FTop        := FRoot;
  FMaxElement := 10;
  FCount      := 0;
end;

destructor TStackList.Destroy;
begin
  while Assigned(FRoot) do
  begin
    dispose(FTop.data);
    Remove(FTop);
  end;
  inherited;
end;

procedure TStackList.Enum(EnumProc: TStackEnumProc);
var
  curr: PStackElement;
  br:   boolean;
begin
  curr := FRoot;
  br   := false;
  while Assigned(curr) and (not br) do
  begin
    EnumProc(curr.Data, br);
    curr := curr.Next;
  end;
end;

function TStackList.Find(Value: pointer): PStackElement;
var
  curr: PStackElement;
begin
  curr   := FRoot;
  Result := nil;
  while Assigned(curr) do
  begin
    if curr.Data = Value then
    begin
      Result := curr;
      exit;
    end
    else
      curr := curr.Next;
  end;
end;

procedure TStackList.Remove(Element: PStackElement);
begin
  if Assigned(Element.Next) then
    Element.Next.Prev := Element.Prev
  else
    FTop := Element.Prev;
  if Assigned(Element.Prev) then
    Element.Prev.Next := Element.Next
  else
    FRoot := Element.Next;
  if FRoot = FTop then
    FRoot := nil;
  FTop    := nil;
  Dispose(Element);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

end.

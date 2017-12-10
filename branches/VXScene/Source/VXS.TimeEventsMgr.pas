//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Time based events mannager using the Cadencer
   can be useful to make animations
}

unit VXS.TimeEventsMgr;

interface

uses
  System.Classes,
  System.SysUtils,

  VXS.Cadencer,
  VXS.BaseClasses;

type

  TVXTimeEvent = class;
  TVXTimeEvents = class;

  TVXTimeEventsMGR = class(TVXUpdateAbleComponent)
  private
    FCadencer: TVXCadencer;
    FEnabled: boolean;
    FFreeEventOnEnd: boolean;
    FEvents: TVXTimeEvents;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCadencer(const val: TVXCadencer);
    procedure SetEvents(const val: TVXTimeEvents);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    procedure Reset();
  published
    property Cadencer: TVXCadencer read FCadencer write SetCadencer;
    property Enabled: boolean read FEnabled write FEnabled default True;
    property FreeEventOnEnd: boolean read FFreeEventOnEnd write FFreeEventOnEnd default False;
    property Events: TVXTimeEvents read FEvents write SetEvents;
  end;

  TVXTimeEvents = class(TCollection)
  protected
    Owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVXTimeEvent);
    function GetItems(index: Integer): TVXTimeEvent;
  public
    constructor Create(aOwner: TComponent);
    function Add: TVXTimeEvent;
    function FindItemID(ID: Integer): TVXTimeEvent;
    function EventByName(name: String): TVXTimeEvent;
    property Items[index: Integer]: TVXTimeEvent read GetItems write SetItems; default;
  end;

  TVXTimeEventType = (etOneShot, etContinuous, etPeriodic);
  TVXTimeEventProc = procedure(event: TVXTimeEvent) of object;

  TVXTimeEvent = class(TCollectionItem)
  private
    FName: String;
    FStartTime, FEndTime, FElapsedTime: Double;
    FPeriod: Double;
    FEventType: TVXTimeEventType;
    FOnEvent: TVXTimeEventProc;
    FEnabled: boolean;
    FTickCount: Cardinal;
    procedure SetEnabled(const Value: boolean);
  protected
    function GetDisplayName: String; override;
    procedure SetName(val: String);
    procedure DoEvent(const curTime: Double);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // Number of times the event was triggered since activation
    property TickCount: Cardinal read FTickCount;
    // Elapsed time since the event was activated
    property ElapsedTime: Double read FElapsedTime;
  published
    property Name: String read FName write SetName;
    property StartTime: Double read FStartTime write FStartTime;
    property EndTime: Double read FEndTime write FEndTime;
    property Period: Double read FPeriod write FPeriod;
    property EventType: TVXTimeEventType read FEventType write FEventType default etOneShot;
    property OnEvent: TVXTimeEventProc read FOnEvent write FOnEvent;
    property Enabled: boolean read FEnabled write SetEnabled default True;
  end;

//---------------------------------------------------------
implementation
//---------------------------------------------------------

// ------------------
// ------------------ TVXTimeEventsMGR ------------------
// ------------------

constructor TVXTimeEventsMGR.Create(aOwner: TComponent);
begin
  inherited;
  FEnabled := True;
  FFreeEventOnEnd := False;
  FEvents := TVXTimeEvents.Create(self);
end;

destructor TVXTimeEventsMGR.Destroy;
begin
  Cadencer := nil;
  FEvents.Free;
  inherited Destroy;
end;

procedure TVXTimeEventsMGR.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Cadencer) then
    FCadencer := nil;
  inherited;
end;

procedure TVXTimeEventsMGR.SetCadencer(const val: TVXCadencer);
begin
  if FCadencer <> val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(self);
  end;
end;

procedure TVXTimeEventsMGR.SetEvents(const val: TVXTimeEvents);
begin
  FEvents.Assign(val);
end;

procedure TVXTimeEventsMGR.DoProgress(const progressTime: TVXProgressTimes);
var
  i: Integer;
begin
  if not Enabled then
    Exit;

  i := 0;
  with progressTime do
    while i <= Events.Count - 1 do
      with Events.Items[i] do
      begin
        if Enabled and Assigned(FOnEvent) then
        begin
          case EventType of
            etOneShot:
              if (newTime >= StartTime) and (TickCount = 0) then
                DoEvent(newTime);
            etContinuous:
              if (newTime >= StartTime) and ((newTime <= EndTime) or (EndTime <= 0)) then
                DoEvent(newTime);
            etPeriodic:
              if (newTime >= StartTime + TickCount * Period) and ((newTime <= EndTime) or (EndTime <= 0)) then
                DoEvent(newTime);
          else
            Assert(False);
          end;
        end;
        if FreeEventOnEnd and (((EventType <> etOneShot) and (newTime > EndTime) and (EndTime >= 0)) or
          ((EventType = etOneShot) and (TickCount > 0))) then
          Events[i].Free
        else
        begin
          // if we delete current event, the next will have same index
          // so increment only if we don't delete
          Inc(i);
        end;
      end;
end;

procedure TVXTimeEventsMGR.Reset;
var
  i: Integer;
begin
  if FEvents.Count <> 0 then
    for i := 0 to FEvents.Count - 1 do
      FEvents[i].FTickCount := 0;
end;


// ------------------
// ------------------ TVXTimeEvents ------------------
// ------------------

constructor TVXTimeEvents.Create(aOwner: TComponent);
begin
  Owner := aOwner;
  inherited Create(TVXTimeEvent);
end;

function TVXTimeEvents.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TVXTimeEvents.SetItems(index: Integer; const val: TVXTimeEvent);
begin
  inherited Items[index] := val;
end;

function TVXTimeEvents.GetItems(index: Integer): TVXTimeEvent;
begin
  Result := TVXTimeEvent(inherited Items[index]);
end;

function TVXTimeEvents.Add: TVXTimeEvent;
begin
  Result := (inherited Add) as TVXTimeEvent;
end;

function TVXTimeEvents.FindItemID(ID: Integer): TVXTimeEvent;
begin
  Result := (inherited FindItemID(ID)) as TVXTimeEvent;
end;

function TVXTimeEvents.EventByName(name: String): TVXTimeEvent;
var
  i: Integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FName <> name) do
    Inc(i);

  if i = Count then
    Result := nil
  else
    Result := Items[i];
end;

// ------------------
// ------------------ TVXTimeEvent ------------------
// ------------------

constructor TVXTimeEvent.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEventType := etOneShot;
  FName := Format('Event%d', [index]); // give a default name different for each event
  FEnabled := True;
end;

destructor TVXTimeEvent.Destroy;
begin
  inherited Destroy;
end;

function TVXTimeEvent.GetDisplayName: String;
begin
  case EventType of
    etOneShot:
      Result := Name + Format(' (OneShot ST=%g)', [StartTime]);
    etContinuous:
      Result := Name + Format(' (Continuous ST=%g ET=%g)', [StartTime, EndTime]);
    etPeriodic:
      Result := Name + Format(' (Periodic ST=%g ET=%g P=%g)', [StartTime, EndTime, Period]);
  end;
end;

procedure TVXTimeEvent.SetName(val: String);
var
  i: Integer;
  ok: boolean;
begin
  ok := True;
  with self.Collection as TVXTimeEvents do // we mustn't have 2 events with the same name (for EventByName)
    for i := 0 to Count - 1 do
      if Items[i].FName = val then
        ok := False;

  if ok and (val <> '') then
    FName := val;
end;

procedure TVXTimeEvent.DoEvent(const curTime: Double);
begin
  if Assigned(FOnEvent) then
  begin
    FElapsedTime := curTime - StartTime;
    FOnEvent(self);
  end;
  Inc(FTickCount);
end;

procedure TVXTimeEvent.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
  FStartTime := ((GetOwner as TVXTimeEvents).Owner as TVXTimeEventsMGR).Cadencer.CurrentTime;
end;

end.

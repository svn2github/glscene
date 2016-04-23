//
// VKScene project, http://glscene.sourceforge.net 
//
{
  Asynchronous timer component (actual 1 ms resolution). 
  This component is based on ThreadedTimer by Carlos Barbosa. 
   
}
unit VKS.AsyncTimer;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  VKS.CrossPlatform;


const
  cDEFAULT_TIMER_INTERVAL = 1000;

type

  // TVKAsyncTimer
  //
  { Asynchronous timer component (actual 1 ms resolution, if CPU fast enough). 
    Keep in mind timer resolution is obtained <i>in-between</i> events, but
    events are not triggered every x ms. For instance if you set the interval to
    5 ms, and your Timer event takes 1 ms to complete, Timer events will actually
    be triggered every 5+1=6 ms (that's why it's "asynchronous"). 
    This component is based on ThreadedTimer by Carlos Barbosa. }
  TVKAsyncTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FOnTimer: TNotifyEvent;
    FTimerThread: TThread;
    FMutex: TCriticalSection;
  protected
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Word;
    procedure SetInterval(Value: Word);
    function GetThreadPriority: TThreadPriority;
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure DoTimer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Word read GetInterval write SetInterval
      default cDEFAULT_TIMER_INTERVAL;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property ThreadPriority: TThreadPriority read GetThreadPriority
      write SetThreadPriority default tpTimeCritical;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type

  // TTimerThread
  //
  TTimerThread = class(TThread)
  private
    FOwner: TVKAsyncTimer;
    FInterval: Word;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
  end;

  // Create
  //
constructor TTimerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

// Execute
//
procedure TTimerThread.Execute;
var
  lastTick, nextTick, curTick, perfFreq: Int64;
begin
  QueryPerformanceFrequency(perfFreq);
  QueryPerformanceCounter(lastTick);
  nextTick := lastTick + (FInterval * perfFreq) div 1000;
  while not Terminated do
  begin
    FOwner.FMutex.Acquire;
    FOwner.FMutex.Release;
    while not Terminated do
    begin
      QueryPerformanceCounter(lastTick);
      if lastTick >= nextTick then
        break;
      Sleep(1);
    end;
    if not Terminated then
    begin
      // if time elapsed run user-event
      Synchronize(FOwner.DoTimer);
      QueryPerformanceCounter(curTick);
      nextTick := lastTick + (FInterval * perfFreq) div 1000;
      if nextTick <= curTick then
      begin
        // CPU too slow... delay to avoid monopolizing what's left
        nextTick := curTick + (FInterval * perfFreq) div 1000;
      end;
    end;
  end;
end;

{ TVKAsyncTimer }

// Create
//
constructor TVKAsyncTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // create timer thread
  FMutex := TCriticalSection.Create;
  FMutex.Acquire;
  FTimerThread := TTimerThread.Create(False);

  with TTimerThread(FTimerThread) do
  begin
    FOwner := Self;
    FreeOnTerminate := False;
    Priority := tpTimeCritical;
    FInterval := cDEFAULT_TIMER_INTERVAL;
  end;
end;

// Destroy
//
destructor TVKAsyncTimer.Destroy;
begin
  Enabled := False;
  FTimerThread.Terminate;
  FMutex.Release;
  CheckSynchronize;
  // wait & free
  FTimerThread.WaitFor;
  FTimerThread.Free;
  FMutex.Free;
  inherited Destroy;
end;

// DoTimer
//
procedure TVKAsyncTimer.DoTimer;
begin
  if Enabled and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

// SetEnabled
//
procedure TVKAsyncTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      // When enabled resume thread
      if TTimerThread(FTimerThread).FInterval > 0 then
        FMutex.Release;
    end
    else
      FMutex.Acquire;
  end;
end;

function TVKAsyncTimer.GetInterval: Word;
begin
  Result := TTimerThread(FTimerThread).FInterval;
end;

procedure TVKAsyncTimer.SetInterval(Value: Word);
begin
  if Value <> TTimerThread(FTimerThread).FInterval then
  begin
    TTimerThread(FTimerThread).FInterval := Value;
  end;
end;

function TVKAsyncTimer.GetThreadPriority: TThreadPriority;
begin
  Result := FTimerThread.Priority;
end;

procedure TVKAsyncTimer.SetThreadPriority(Value: TThreadPriority);
begin
  FTimerThread.Priority := Value;
end;

initialization

  RegisterClass(TVKAsyncTimer);


end.

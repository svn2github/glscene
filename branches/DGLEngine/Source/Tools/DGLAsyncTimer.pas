//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : GLAsyncTimer - asynchronous timer component (actual 1 ms resolution).<p>

  This component is based on ThreadedTimer by Carlos Barbosa.<p>

  <b>History : </b><font size=-1><ul>
    <li>31/12/15 - JD - Imported from GLScene
  </ul></font>
}
unit DGLAsyncTimer;

interface

{$I DGLEngine.inc}

uses
  Classes, SysUtils, SyncObjs,
  DGLCrossPlatform;


const
  cDEFAULT_TIMER_INTERVAL = 1000;

type
  // ****************************************************************************************
  // TDGLAsyncTimer
  //
  { : Asynchronous timer component (actual 1 ms resolution, if CPU fast enough).<p>
    Keep in mind timer resolution is obtained <i>in-between</i> events, but
    events are not triggered every x ms. For instance if you set the interval to
    5 ms, and your Timer event takes 1 ms to complete, Timer events will actually
    be triggered every 5+1=6 ms (that's why it's "asynchronous").<p>
    This component is based on ThreadedTimer by Carlos Barbosa. }
  TDGLAsyncTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FOnTimer: TNotifyEvent;
    FTimerThread: TThread;
    FMutex: TCriticalSection;
  protected
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Word;
    procedure SetInterval(Value: Word);
    {$IFDEF MSWINDOWS}
    function GetThreadPriority: TThreadPriority;
    procedure SetThreadPriority(Value: TThreadPriority);
    {$ENDIF}
    procedure DoTimer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Word read GetInterval write SetInterval default cDEFAULT_TIMER_INTERVAL;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    {$IFDEF MSWINDOWS}
    property ThreadPriority: TThreadPriority read GetThreadPriority write SetThreadPriority default tpTimeCritical;
    {$ENDIF}
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
    FOwner: TDGLAsyncTimer;
    FInterval: Word;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
  end;

// ------------------
{ TTimerThread. }
{$IFDEF GLS_REGION}{$REGION 'TTimerThread.'}{$ENDIF}

constructor TTimerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

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

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAsyncTimer }
{$IFDEF GLS_REGION}{$REGION 'TDGLAsyncTimer'}{$ENDIF}

constructor TDGLAsyncTimer.Create(AOwner: TComponent);
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
    {$IFDEF MSWINDOWS}
    Priority := tpTimeCritical;
    {$ENDIF}
    FInterval := cDEFAULT_TIMER_INTERVAL;
  end;
end;

destructor TDGLAsyncTimer.Destroy;
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

procedure TDGLAsyncTimer.DoTimer;
begin
  if Enabled and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TDGLAsyncTimer.SetEnabled(Value: Boolean);
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

function TDGLAsyncTimer.GetInterval: Word;
begin
  Result := TTimerThread(FTimerThread).FInterval;
end;

procedure TDGLAsyncTimer.SetInterval(Value: Word);
begin
  if Value <> TTimerThread(FTimerThread).FInterval then
  begin
    TTimerThread(FTimerThread).FInterval := Value;
  end;
end;

{$IFDEF MSWINDOWS}
function TDGLAsyncTimer.GetThreadPriority: TThreadPriority;
begin
  Result := FTimerThread.Priority;
end;

procedure TDGLAsyncTimer.SetThreadPriority(Value: TThreadPriority);
begin
  FTimerThread.Priority := Value;
end;
{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

  RegisterClass(TDGLAsyncTimer);


end.

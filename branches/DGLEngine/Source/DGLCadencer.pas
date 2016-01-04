//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ @HTML ( GLCadencer<p>

 Cadencing composant for DGLEngine (ease Progress processing)<p>

 <b>History: </b><font size=-1><ul>
      <li>21/12/15 - JD -  Imported From GLScene
 </ul></font>
}
unit DGLCadencer;

interface

{$I DGLEngine.inc}

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.Types, VCL.Forms,
  //DGLE
  DGLCrossPlatform, DGLBaseClasses, DGLScene;

type

  // TDGLCadencerMode
  //
  { @HTML ( Determines how the TDGLCadencer operates.<p>
   - cmManual : you must trigger progress manually (in your code)<br>
   - cmASAP : progress is triggered As Soon As Possible after a previous
    progress (uses windows messages).
       - cmApplicationIdle : will hook Application.OnIdle, this will overwrite
          any previous event handle, and only one cadencer may be in this mode. }
  TDGLCadencerMode = (cmManual, cmASAP, cmApplicationIdle);

  // TDGLCadencerTimeReference
  //
  { @HTML ( Determines which time reference the TDGLCadencer should use.<p>
   - cmRTC : the Real Time Clock is used (precise over long periods, but
    not accurate to the millisecond, may limit your effective framerate
          to less than 50 FPS on some systems)<br>
   - cmPerformanceCounter : the windows performance counter is used (nice
    precision, may derive over long periods, this is the default option
    as it allows the smoothest animation on fast systems)<br>
   - cmExternal : the CurrentTime property is used }
  TDGLCadencerTimeReference = (cmRTC, cmPerformanceCounter, cmExternal);

  // ****************************************************************************************
  // TDGLCadencer
  //
  { @HTML ( This component allows auto-progression of animation.<p>
   Basicly dropping this component and linking it to your TGLScene will send
   it real-time progression events (time will be measured in seconds) while
   keeping the CPU 100% busy if possible (ie. if things change in your scene).<p>
   The progression time (the one you'll see in you progression events)
   is calculated using  (CurrentTime-OriginTime)*TimeMultiplier,
   CurrentTime being either manually or automatically updated using
   TimeReference (setting CurrentTime does NOT trigger progression). }
  TDGLCadencer = class(TComponent)
  private
    { Private Declarations }
    FSubscribedCadenceableComponents: TList;
    FScene: TDGLScene;
    FTimeMultiplier: Double;
    lastTime, downTime, lastMultiplier: Double;
    FEnabled: Boolean;
    FSleepLength: Integer;
    FMode: TDGLCadencerMode;
    FTimeReference: TDGLCadencerTimeReference;
    FCurrentTime: Double;
    FOriginTime: Double;
    FMaxDeltaTime, FMinDeltaTime, FFixedDeltaTime: Double;
  	FOnProgress, FOnTotalProgress : TDGLProgressEvent;
    FProgressing: Integer;
    procedure SetCurrentTime(const Value: Double);

  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function StoreTimeMultiplier: Boolean;
    procedure SetEnabled(const val: Boolean);
    procedure SetScene(const val: TDGLScene);
    procedure SetMode(const val: TDGLCadencerMode);
    procedure SetTimeReference(const val: TDGLCadencerTimeReference);
    procedure SetTimeMultiplier(const val: Double);
    { @HTML ( Returns raw ref time (no multiplier, no offset) }
    function GetRawReferenceTime: Double;
    procedure RestartASAP;
    procedure Loaded; override;

    procedure OnIdleEvent(Sender: TObject; var Done: Boolean);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Subscribe(aComponent: TDGLCadenceAbleComponent);
    procedure UnSubscribe(aComponent: TDGLCadenceAbleComponent);

    { @HTML ( Allows to manually trigger a progression.<p>
     Time stuff is handled automatically.<br>
     If cadencer is disabled, this functions does nothing. }
    procedure Progress;

    { @HTML ( Adjusts CurrentTime if necessary, then returns its value. }
    function GetCurrenttime: Double;

    { @HTML ( Returns True if a "Progress" is underway.<p>
       Be aware that as long as IsBusy is True, the Cadencer may be
       sending messages and progression calls to cadenceable components
       and scenes. }
    function IsBusy: Boolean;

    { @HTML ( Reset the time parameters and returns to zero.<p>}
    procedure Reset;

    { @HTML ( Value soustracted to current time to obtain progression time. }
    property OriginTime: Double read FOriginTime write FOriginTime;
    { @HTML ( Current time (manually or automatically set, see TimeReference). }
    property CurrentTime: Double read FCurrentTime write SetCurrentTime;

  published
    { Published Declarations }
    { @HTML ( The TGLScene that will be cadenced (progressed). }
    property Scene: TDGLScene read FScene write SetScene;
    { @HTML ( Enables/Disables cadencing.<p>
     Disabling won't cause a jump when restarting, it is working like
     a play/pause (ie. may modify OriginTime to keep things smooth). }
    property Enabled: Boolean read FEnabled write SetEnabled default True;

    { @HTML ( Defines how CurrentTime is updated.<p>
     See TDGLCadencerTimeReference.<br>
     Dynamically changeing the TimeReference may cause a "jump".  }
    property TimeReference: TDGLCadencerTimeReference read FTimeReference write
      SetTimeReference default cmPerformanceCounter;

    { @HTML ( Multiplier applied to the time reference.<p>
      Zero isn't an allowed value, and be aware that if negative values
      are accepted, they may not be supported by other GLScene objects.<br>
     Changing the TimeMultiplier will alter OriginTime. }
    property TimeMultiplier: Double read FTimeMultiplier write SetTimeMultiplier
      stored StoreTimeMultiplier;

    { @HTML ( Maximum value for deltaTime in progression events.<p>
       If null or negative, no max deltaTime is defined, otherwise, whenever
       an event whose actual deltaTime would be superior to MaxDeltaTime
       occurs, deltaTime is clamped to this max, and the extra time is hidden
       by the cadencer (it isn't visible in CurrentTime either).<br>
       This option allows to limit progression rate in simulations where
       high values would result in errors/random behaviour. }
    property MaxDeltaTime: Double read FMaxDeltaTime write FMaxDeltaTime;

    { @HTML ( Minimum value for deltaTime in progression events.<p>
       If superior to zero, this value specifies the minimum time step
       between two progression events.<br>
       This option allows to limit progression rate in simulations where
       low values would result in errors/random behaviour. }
    property MinDeltaTime: Double read FMinDeltaTime write FMinDeltaTime;

    { @HTML ( Fixed time-step value for progression events.<p>
       If superior to zero, progression steps will happen with that fixed
       delta time. The progression remains time based, so zero to N events
       may be fired depending on the actual deltaTime (if deltaTime is
       inferior to FixedDeltaTime, no event will be fired, if it is superior
       to two times FixedDeltaTime, two events will be fired, etc.).<br>
       This option allows to use fixed time steps in simulations (while the
       animation and rendering itself may happen at a lower or higher
       framerate). }
    property FixedDeltaTime: Double read FFixedDeltaTime write FFixedDeltaTime;

    { @HTML ( Adjusts how progression events are triggered.<p>
     See TDGLCadencerMode. }
    property Mode: TDGLCadencerMode read FMode write SetMode default cmASAP;

    { @HTML ( Allows relinquishing time to other threads/processes.<p>
     A "sleep" is issued BEFORE each progress if SleepLength>=0 (see
     help for the "sleep" procedure in delphi for details). }
    property SleepLength: Integer read FSleepLength write FSleepLength default
      -1;

    { @HTML ( Happens AFTER scene was progressed. }
    property OnProgress: TDGLProgressEvent read FOnProgress write FOnProgress;
    { @HTML ( Happens AFTER all iterations with fixed delta time. }
    property OnTotalProgress : TDGLProgressEvent read FOnTotalProgress write FOnTotalProgress;
  end;

  // ****************************************************************************************
  // TDGLCustomCadencedComponent
  //
  { @HTML ( Adds a property to connect/subscribe to a cadencer.<p> }
  TDGLCustomCadencedComponent = class(TDGLUpdateAbleComponent)
  private
    { Private Declarations }
    FCadencer: TDGLCadencer;

  protected
    { Protected Declarations }
    procedure SetCadencer(const val: TDGLCadencer);

    property Cadencer: TDGLCadencer read FCadencer write SetCadencer;

  public
    { Public Declarations }
    destructor Destroy; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  end;

  // ****************************************************************************************
  // TGLCadencedComponent
  //
  TGLCadencedComponent = class(TDGLCustomCadencedComponent)
  published
    { Published Declarations }
    property Cadencer;
  end;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses SysUtils;

const
  cTickGLCadencer = 'TickGLCadencer';

type
  { TASAPHandler }
  TASAPHandler = class
  private
    FTooFastCounter: Integer;
    FTimer: Cardinal;
    FWindowHandle: HWND;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  vWMTickCadencer: Cardinal;
  vASAPCadencerList: TList;
  vHandler: TASAPHandler;
  vCounterFrequency: Int64;
  vWndProcInLoop: Boolean;


// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

procedure RegisterASAPCadencer(aCadencer: TDGLCadencer);
begin
  if aCadencer.Mode = cmASAP then
  begin
    if not Assigned(vASAPCadencerList) then
      vASAPCadencerList := TList.Create;
    if vASAPCadencerList.IndexOf(aCadencer) < 0 then
    begin
      vASAPCadencerList.Add(aCadencer);
      if not Assigned(vHandler) then
        vHandler := TASAPHandler.Create;
    end;
  end
  else if aCadencer.Mode = cmApplicationIdle then
    Application.OnIdle := aCadencer.OnIdleEvent;
end;

procedure UnRegisterASAPCadencer(aCadencer: TDGLCadencer);
var
  i: Integer;
begin
  if aCadencer.Mode = cmASAP then
  begin
    if Assigned(vASAPCadencerList) then
    begin
      i := vASAPCadencerList.IndexOf(aCadencer);
      if i >= 0 then
        vASAPCadencerList[i] := nil;
    end;
  end
  else if aCadencer.Mode = cmApplicationIdle then
    Application.OnIdle := nil;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TASAPHandler }
{$IFDEF GLS_REGION}{$REGION 'TASAPHandler'}{$ENDIF}

constructor TASAPHandler.Create;
begin
  inherited Create;
  FWindowHandle := AllocateHWnd(WndProc);
  PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
end;

destructor TASAPHandler.Destroy;
begin
  if FTimer <> 0 then
    KillTimer(FWindowHandle, FTimer);
  DeallocateHWnd(FWindowHandle);

  inherited Destroy;
end;

procedure TASAPHandler.WndProc(var Msg: TMessage);
var
  i: Integer;
  cad: TDGLCadencer;
begin
  //   Windows.Beep(440, 10);
  with Msg do
  begin
    if Msg = WM_TIMER then
    begin
      KillTimer(FWindowHandle, FTimer);
      FTimer := 0;
    end;
    if (Msg <> WM_TIMER) and (Cardinal(GetMessageTime) = GetTickCount) then
    begin
      // if we're going too fast, "sleep" for 1 msec
      Inc(FTooFastCounter);
      if FTooFastCounter > 5000 then
      begin
        if FTimer = 0 then
          FTimer := SetTimer(FWindowHandle, 1, 1, nil);
        FTooFastCounter := 0;
      end;
    end
    else
      FTooFastCounter := 0;
    if FTimer <> 0 then
    begin
      Result := 0;
      Exit;
    end;
    if not vWndProcInLoop then
    begin
      vWndProcInLoop := True;
      try
        if (Msg = vWMTickCadencer) or (Msg = WM_TIMER) then
        begin
          // Progress
          for i := vASAPCadencerList.Count - 1 downto 0 do
          begin
            cad := TDGLCadencer(vASAPCadencerList[i]);
            if Assigned(cad) and (cad.Mode = cmASAP)
              and cad.Enabled and (cad.FProgressing = 0) then
            begin
              if Application.Terminated then
              begin
                // force stop
                cad.Enabled := False
              end
              else
              begin
                try
                  // do stuff
                  cad.Progress;
                except
                  Application.HandleException(Self);
                  // it faulted, stop it
                  cad.Enabled := False
                end
              end;
            end;
          end;
          // care for nils
          vASAPCadencerList.Pack;
          if vASAPCadencerList.Count = 0 then
          begin
            vASAPCadencerList.Free;
            vASAPCadencerList := nil;
            vHandler.Free;
            vHandler := nil;
          end
          else
          begin
            // Prepare the return of the infernal loop...
            PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
          end;
        end;
      finally
        vWndProcInLoop := False;
      end;
    end;
    Result := 0;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLCadencer }
{$IFDEF GLS_REGION}{$REGION 'TDGLCadencer'}{$ENDIF}

constructor TDGLCadencer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeReference := cmPerformanceCounter;
  downTime := GetRawReferenceTime;
  FOriginTime := downTime;
  FTimeMultiplier := 1;
  FSleepLength := -1;
  Mode := cmASAP;
  Enabled := True;
end;

destructor TDGLCadencer.Destroy;
begin
  Assert(FProgressing = 0);
  UnRegisterASAPCadencer(Self);
  FSubscribedCadenceableComponents.Free;
  FSubscribedCadenceableComponents := nil;
  inherited Destroy;
end;

procedure TDGLCadencer.Subscribe(aComponent: TDGLCadenceAbleComponent);
begin
  if not Assigned(FSubscribedCadenceableComponents) then
    FSubscribedCadenceableComponents := TList.Create;
  if FSubscribedCadenceableComponents.IndexOf(aComponent) < 0 then
  begin
    FSubscribedCadenceableComponents.Add(aComponent);
    aComponent.FreeNotification(Self);
  end;
end;

procedure TDGLCadencer.UnSubscribe(aComponent: TDGLCadenceAbleComponent);
var
  i: Integer;
begin
  if Assigned(FSubscribedCadenceableComponents) then
  begin
    i := FSubscribedCadenceableComponents.IndexOf(aComponent);
    if i >= 0 then
    begin
      FSubscribedCadenceableComponents.Delete(i);
      aComponent.RemoveFreeNotification(Self);
    end;
  end;
end;

procedure TDGLCadencer.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FScene then
      FScene := nil;
    if Assigned(FSubscribedCadenceableComponents) then
      FSubscribedCadenceableComponents.Remove(AComponent);
  end;
  inherited;
end;

procedure TDGLCadencer.Loaded;
begin
  inherited Loaded;
  RestartASAP;
end;

procedure TDGLCadencer.OnIdleEvent(Sender: TObject; var Done: Boolean);
begin
  Progress;
  Done := False;
end;

procedure TDGLCadencer.RestartASAP;
begin
  if not (csLoading in ComponentState) then
  begin
    if (Mode in [cmASAP, cmApplicationIdle]) and (not (csDesigning in
      ComponentState))
      and Assigned(FScene) and Enabled then
      RegisterASAPCadencer(Self)
    else
      UnRegisterASAPCadencer(Self);
  end;
end;

procedure TDGLCadencer.SetEnabled(const val: Boolean);
begin
  if FEnabled <> val then
  begin
    FEnabled := val;
    if not (csDesigning in ComponentState) then
    begin
      if Enabled then
        FOriginTime := FOriginTime + GetRawReferenceTime - downTime
      else
        downTime := GetRawReferenceTime;
      RestartASAP;
    end;
  end;
end;

procedure TDGLCadencer.SetScene(const val: TDGLScene);
begin
  if FScene <> val then
  begin
    if Assigned(FScene) then
      FScene.RemoveFreeNotification(Self);
    FScene := val;
    if Assigned(FScene) then
      FScene.FreeNotification(Self);
    RestartASAP;
  end;
end;

procedure TDGLCadencer.SetTimeMultiplier(const val: Double);
var
  rawRef: Double;
begin
  if val <> FTimeMultiplier then
  begin
    if val = 0 then
    begin
      lastMultiplier := FTimeMultiplier;
      Enabled := False;
    end
    else
    begin
      rawRef := GetRawReferenceTime;
      if FTimeMultiplier = 0 then
      begin
        Enabled := True;
        // continuity of time:
        // (rawRef-newOriginTime)*val = (rawRef-FOriginTime)*lastMultiplier
        FOriginTime := rawRef - (rawRef - FOriginTime) * lastMultiplier / val;
      end
      else
      begin
        // continuity of time:
        // (rawRef-newOriginTime)*val = (rawRef-FOriginTime)*FTimeMultiplier
        FOriginTime := rawRef - (rawRef - FOriginTime) * FTimeMultiplier / val;
      end;
    end;
    FTimeMultiplier := val;
  end;
end;

function TDGLCadencer.StoreTimeMultiplier: Boolean;
begin
  Result := (FTimeMultiplier <> 1);
end;

procedure TDGLCadencer.SetMode(const val: TDGLCadencerMode);
begin
  if FMode <> val then
  begin
    if FMode <> cmManual then
      UnRegisterASAPCadencer(Self);
    FMode := val;
    RestartASAP;
  end;
end;

procedure TDGLCadencer.SetTimeReference(const val: TDGLCadencerTimeReference);
begin
  // nothing more, yet
  FTimeReference := val;
end;

procedure TDGLCadencer.Progress;
var
  deltaTime, newTime, totalDelta: Double;
  fullTotalDelta, firstLastTime : Double;
  i: Integer;
  pt: TProgressTimes;
begin
  // basic protection against infinite loops,
    // shall never happen, unless there is a bug in user code
  if FProgressing < 0 then Exit;
  if Enabled then
  begin
    // avoid stalling everything else...
    if SleepLength >= 0 then
      Sleep(SleepLength);
    // in manual mode, the user is supposed to make sure messages are handled
    // in Idle mode, this processing is implicit
    if Mode = cmASAP then
    begin
      Application.ProcessMessages;
      if (not Assigned(vASAPCadencerList)) or (vASAPCadencerList.IndexOf(Self) < 0) then Exit;
    end;
  end;
  Inc(FProgressing);
  try
    if Enabled then
    begin
      // One of the processed messages might have disabled us
      if Enabled then
      begin
        // ...and progress !
        newTime := GetCurrenttime;
        deltaTime := newTime - lastTime;
        if (deltaTime >= MinDeltaTime) and (deltaTime >= FixedDeltaTime) then
        begin
          if FMaxDeltaTime > 0 then
          begin
            if deltaTime > FMaxDeltaTime then
            begin
              FOriginTime := FOriginTime + (deltaTime - FMaxDeltaTime) / FTimeMultiplier;
              deltaTime := FMaxDeltaTime;
              newTime := lastTime + deltaTime;
            end;
          end;
          totalDelta := deltaTime;
          fullTotalDelta := totalDelta;
          firstLastTime := lastTime;
          if FixedDeltaTime > 0 then
            deltaTime := FixedDeltaTime;
          while totalDelta >= deltaTime do
          begin
            lastTime := lastTime + deltaTime;
            if Assigned(FScene) and (deltaTime <> 0) then
            begin
              FProgressing := -FProgressing;
              try
                FScene.Progress(deltaTime, lastTime);
              finally
                FProgressing := -FProgressing;
              end;
            end;
            pt.deltaTime := deltaTime;
            pt.newTime := lastTime;
            i := 0;
            while Assigned(FSubscribedCadenceableComponents) and
              (i <= FSubscribedCadenceableComponents.Count - 1) do
            begin
              TDGLCadenceAbleComponent(FSubscribedCadenceableComponents[i]).DoProgress(pt);
              i := i + 1;
            end;
            if Assigned(FOnProgress) and (not (csDesigning in ComponentState))
              then
              FOnProgress(Self, deltaTime, newTime);
            if deltaTime <= 0 then Break;
            totalDelta := totalDelta - deltaTime;
          end;
          if Assigned(FOnTotalProgress) and (not (csDesigning in ComponentState)) then
            FOnTotalProgress(Self, fullTotalDelta, firstLastTime);
        end;
      end;
    end;
  finally
    Dec(FProgressing);
  end;
end;

function TDGLCadencer.GetRawReferenceTime: Double;
var
  counter: Int64;
begin
  case FTimeReference of
    cmRTC: // Real Time Clock
      Result := Now * (3600 * 24);
    cmPerformanceCounter:
      begin // HiRes Performance Counter
        QueryPerformanceCounter(counter);
        Result := counter / vCounterFrequency;
      end;
    cmExternal: // User defined value
      Result := FCurrentTime;
  else
    Result := 0;
    Assert(False);
  end;
end;

function TDGLCadencer.GetCurrenttime: Double;
begin
  Result := (GetRawReferenceTime - FOriginTime) * FTimeMultiplier;
  FCurrentTime := Result;
end;

function TDGLCadencer.IsBusy: Boolean;
begin
  Result := (FProgressing <> 0);
end;

procedure TDGLCadencer.Reset;
begin
  lasttime := 0;
  downTime := GetRawReferenceTime;
  FOriginTime := downTime;
end;

procedure TDGLCadencer.SetCurrentTime(const Value: Double);
begin
  LastTime := Value - (FCurrentTime - LastTime);
  FOriginTime := FOriginTime + (FCurrentTime - Value);
  FCurrentTime := Value;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLCustomCadencedComponent }
{$IFDEF GLS_REGION}{$REGION 'TDGLCustomCadencedComponent'}{$ENDIF}

destructor TDGLCustomCadencedComponent.Destroy;
begin
  Cadencer := nil;
  inherited Destroy;
end;

procedure TDGLCustomCadencedComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FCadencer) then
    Cadencer := nil;
  inherited;
end;

procedure TDGLCustomCadencedComponent.SetCadencer(const val: TDGLCadencer);
begin
  if FCadencer <> val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
initialization

  RegisterClasses([TDGLCadencer]);

  // Get our Windows message ID
  vWMTickCadencer := RegisterWindowMessage(cTickGLCadencer);

  // Preparation for high resolution timer
  if not QueryPerformanceFrequency(vCounterFrequency) then
    vCounterFrequency := 0;

finalization
  FreeAndNil(vHandler);
  FreeAndNil(vASAPCadencerList);

end.

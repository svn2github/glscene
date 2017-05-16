// Joystick
{: Egg<p>

	Component for handling joystick messages<p>

	<b>Historique : </b><font size=-1><ul>
      <li>01/06/03 - AH - Added buttons 5 to 8, axes Z (throttle) and R,
                          and point-of-view button (hat). Z, R and POV can only be accessed
                          through properties.
      <li>29/01/02 - Egg - Added NoCaptureErrors
      <li>18/12/00 - Egg - Fix for supporting 2 joysticks simultaneously
      <li>14/04/00 - Egg - Various minor to major fixes, the component should
                           now work properly for the 4 first buttons and XY axis
	   <li>20/03/00 - Egg - Creation from GLScene's TGLJoystick
	</ul></font>
}
unit Joystick;

interface

{$i GLScene.inc}
{$IFDEF LINUX}{$Message Error 'Unit not supported'}{$ENDIF LINUX}

uses Windows, Forms, Classes, Controls, Messages;

type

   TJoystickButton = (jbButton1, jbButton2, jbButton3, jbButton4, jbButton5,
                      jbButton6, jbButton7, jbButton8);
   TJoystickButtons = set of TJoystickButton;

   TJoystickID = (jidNoJoystick, jidJoystick1, jidJoystick2);
   TJoystickDesignMode = (jdmInactive, jdmActive);
   TJoyPos = (jpMin, jpCenter, jpMax);
   TJoyAxis = (jaX, jaY, jaZ, jaR, jaU, jaV);

   TJoystickEvent = procedure(Sender: TObject; JoyID: TJoystickID; Buttons: TJoystickButtons;
                              XDeflection, YDeflection: Integer) of Object;

	// TJoystick
	//
   {: A component interfacing the Joystick via the (regular) windows API. }
	TJoystick = class (TComponent)
	   private
	       
         FWindowHandle : HWND;
         FNumButtons, FLastX, FLastY, FLastZ, FLastR : Integer;
         FThreshold, FInterval : Cardinal;
         FCapture, FNoCaptureErrors : Boolean;
         FJoystickID : TJoystickID;
         FMinMaxInfo : array[TJoyAxis, TJoyPos] of Integer;
         FXPosInfo, FYPosInfo, FZPosInfo,FRPosInfo : array[0..4] of Integer;
         FOnJoystickButtonChange, FOnJoystickMove : TJoystickEvent;
         FXPosition, FYPosition, FZPosition,FRPosition : Integer;
         FJoyButtons : TJoystickButtons;
         FPointOfView: integer;

         procedure SetCapture(AValue: Boolean);
         procedure SetInterval(AValue: Cardinal);
         procedure SetJoystickID(AValue: TJoystickID);
         procedure SetThreshold(AValue: Cardinal);

	   protected
	      { Protected Declarations }
         function MakeJoyButtons(Param: UINT): TJoystickButtons;
         procedure DoChange;    // Fires an event
         procedure DoGetPosition; // Updates joystick states
         procedure DoJoystickCapture(AHandle: HWND; AJoystick: TJoystickID);
         procedure DoJoystickRelease(AJoystick: TJoystickID);
         procedure ReapplyCapture(AJoystick: TJoystickID);
         procedure WndProc(var Msg: TMessage);
         procedure Loaded; override;

      public
	       
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;

         property JoyButtons : TJoystickButtons read FJoyButtons;
         property XPosition : Integer read FXPosition;
         property YPosition : Integer read FYPosition;
         property ZPosition : Integer read FZPosition;
         property RPosition : Integer read FRPosition;
         property PointOfView:integer read FPointOfView;
         procedure GetPosition(var XDeflection,YDeflection,ZDeflection,RDeflection:integer;
                               var Buttons:TJoystickButtons; var PointOfView:Integer); overload;
         procedure GetPosition; overload;

	   published
	       
         {: When set to True, the component attempts to capture the joystick.<p>
            If capture is successfull, retrieving joystick status is possible,
            if not, an error message is triggered. }
         property Capture : Boolean read FCapture write SetCapture default False;
         {: If true joystick capture errors do not result in exceptions. }
         property NoCaptureErrors : Boolean read FNoCaptureErrors write FNoCaptureErrors default True;
         {: Polling frequency (milliseconds) }
         property Interval : Cardinal read FInterval write SetInterval default 100;
         property JoystickID: TJoystickID read FJoystickID write SetJoystickID default jidNoJoystick;
         property Threshold: Cardinal read FThreshold write SetThreshold default 1000;
	      property OnJoystickButtonChange: TJoystickEvent read FOnJoystickButtonChange write FOnJoystickButtonChange;
	      property OnJoystickMove: TJoystickEvent read FOnJoystickMove write FOnJoystickMove;

	end;

procedure Register;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses SysUtils, MMSystem;

const
  cJoystickIDToNative : array [jidNoJoystick..jidJoystick2] of Byte =
                        (9, JOYSTICKID1, JOYSTICKID2);

resourcestring
  glsNoJoystickDriver   = 'There''s no joystick driver present';
  glsConnectJoystick    = 'Joystick is not connected to your system';
  glsJoystickError      = 'Your system reports a joystick error, can''t do anything about it';

procedure Register;
begin
  RegisterComponents('GLScene', [TJoystick]);
end;

// ------------------
// ------------------ TJoystick ------------------
// ------------------

// Create
//
constructor TJoystick.Create(AOwner: TComponent);
begin
   inherited;
   FWindowHandle := AllocateHWnd(WndProc);
   FInterval := 100;
   FThreshold := 1000;
   FJoystickID := jidNoJoystick;
   FLastX := -1;
   FLastY := -1;
   FLastZ := -1;
   FLastR := -1;
   FNoCaptureErrors := True;
end;

// Destroy
//
destructor TJoystick.Destroy;
begin
   DeallocateHWnd(FWindowHandle);
   inherited;
end;

// WndProc
//
procedure TJoystick.WndProc(var Msg: TMessage);
begin
   with Msg do begin
      case FJoystickID of
         jidJoystick1 : // check only 1st stick
            case Msg of
               MM_JOY1MOVE :
                 DoGetPosition;
               MM_JOY1ZMOVE :
                 DoGetPosition;
               MM_JOY1BUTTONDOWN :
                 DoGetPosition;
               MM_JOY1BUTTONUP :
                 DoGetPosition;
            end;
         jidJoystick2 : // check only 2nd stick
            case Msg of
               MM_JOY2MOVE :
                 DoGetPosition;
               MM_JOY2ZMOVE :
                 DoGetPosition;
               MM_JOY2BUTTONDOWN :
                 DoGetPosition;
               MM_JOY2BUTTONUP :
                 DoGetPosition;
            end;
         jidNoJoystick : ; // ignore
      else
         Assert(False);
      end;
      Result:=0;
   end;
end;

// Loaded
//
procedure TJoystick.Loaded;
begin
   inherited;
   ReapplyCapture(FJoystickID);
end;

// Assign
//
procedure TJoystick.Assign(Source: TPersistent);
begin
   if Source is TJoystick then begin
      FInterval := TJoystick(Source).FInterval;
      FThreshold := TJoystick(Source).FThreshold;
      FCapture := TJoystick(Source).FCapture;
      FJoystickID := TJoystick(Source).FJoystickID;
      try
         ReapplyCapture(FJoystickID);
      except
         FJoystickID := jidNoJoystick;
         FCapture := False;
         raise;
      end;
   end else inherited Assign(Source);
end;

// MakeJoyButtons
//
function TJoystick.MakeJoyButtons(Param: UINT): TJoystickButtons;
begin
   Result := [];
   if (Param and JOY_BUTTON1) > 0 then Include(Result, jbButton1);
   if (Param and JOY_BUTTON2) > 0 then Include(Result, jbButton2);
   if (Param and JOY_BUTTON3) > 0 then Include(Result, jbButton3);
   if (Param and JOY_BUTTON4) > 0 then Include(Result, jbButton4);
   if (Param and JOY_BUTTON5) > 0 then Include(Result, jbButton5);
   if (Param and JOY_BUTTON6) > 0 then Include(Result, jbButton6);
   if (Param and JOY_BUTTON7) > 0 then Include(Result, jbButton7);
   if (Param and JOY_BUTTON8) > 0 then Include(Result, jbButton8);
   FJoyButtons:=Result;
end;

// DoScale
//
function DoScale(aValue : Integer) : Integer;
begin
  Result:=Round(AValue/1);
end;

// ReapplyCapture
//
procedure TJoystick.ReapplyCapture(AJoystick: TJoystickID);
var
   jc : TJoyCaps;
begin
   DoJoystickRelease(AJoystick);
   if FCapture and (not (csDesigning in ComponentState)) then with JC do begin
      joyGetDevCaps(cJoystickIDToNative[FJoystickID], @JC, SizeOf(JC));
      FNumButtons := wNumButtons;
      FMinMaxInfo[jaX, jpMin] := DoScale(wXMin);
      FMinMaxInfo[jaX, jpCenter] := DoScale((wXMin + wXMax) div 2); FMinMaxInfo[jaX, jpMax] := DoScale(wXMax);
      FMinMaxInfo[jaY, jpMin] := DoScale(wYMin); FMinMaxInfo[jaY, jpCenter] := DoScale((wYMin + wYMax) div 2); FMinMaxInfo[jaY, jpMax] := DoScale(wYMax);
      FMinMaxInfo[jaZ, jpMin] := DoScale(wZMin); FMinMaxInfo[jaZ, jpCenter] := DoScale((wZMin + wZMax) div 2); FMinMaxInfo[jaZ, jpMax] := DoScale(wZMax);
      FMinMaxInfo[jaR, jpMin] := DoScale(wRMin); FMinMaxInfo[jaR, jpCenter] := DoScale((wRMin + wRMax) div 2); FMinMaxInfo[jaR, jpMax] := DoScale(wRMax);
      FMinMaxInfo[jaU, jpMin] := DoScale(wUMin); FMinMaxInfo[jaU, jpCenter] := DoScale((wUMin + wUMax) div 2); FMinMaxInfo[jaU, jpMax] := DoScale(wUMax);
      FMinMaxInfo[jaV, jpMin] := DoScale(wVMin); FMinMaxInfo[jaV, jpCenter] := DoScale((wVMin + wVMax) div 2); FMinMaxInfo[jaV, jpMax] := DoScale(wVMax);
      DoJoystickCapture(FWindowHandle, AJoystick)
   end;
end;

// DoJoystickCapture
//
procedure TJoystick.DoJoystickCapture(AHandle: HWND; AJoystick: TJoystickID);
var
   res : Cardinal;
begin
   res:=joySetCapture(AHandle, cJoystickIDToNative[AJoystick], FInterval, True);
   if res<>JOYERR_NOERROR then begin
      FCapture:=False;
      if not NoCaptureErrors then begin
         case res of
            MMSYSERR_NODRIVER : raise Exception.Create(glsNoJoystickDriver);
            JOYERR_UNPLUGGED :  raise Exception.Create(glsConnectJoystick);
            JOYERR_NOCANDO :    raise Exception.Create(glsJoystickError);
         else
            raise Exception.Create(glsJoystickError);
         end;
      end;
   end else joySetThreshold(cJoystickIDToNative[AJoystick], FThreshold);
end;

// DoJoystickRelease
//
procedure TJoystick.DoJoystickRelease(AJoystick: TJoystickID);
begin
   if AJoystick <> jidNoJoystick then
      joyReleaseCapture(cJoystickIDToNative[AJoystick]);
end;

// SetCapture
//
procedure TJoystick.SetCapture(AValue: Boolean);
begin
   if FCapture <> AValue then begin
      FCapture := AValue;
      if not (csReading in ComponentState) then begin
         try
            ReapplyCapture(FJoystickID);
         except
            FCapture := False;
            raise;
         end;
      end;
   end;
end;

// SetInterval
//
procedure TJoystick.SetInterval(AValue: Cardinal);
begin
   if FInterval <> AValue then begin
      FInterval := AValue;
      if not (csReading in ComponentState) then
         ReapplyCapture(FJoystickID);
   end;
end;

// SetJoystickID
//
procedure TJoystick.SetJoystickID(AValue: TJoystickID);
begin
   if FJoystickID <> AValue then begin
      try
         if not (csReading in ComponentState) then
            ReapplyCapture(AValue);
         FJoystickID := AValue;
      except
         on E: Exception do begin
            ReapplyCapture(FJoystickID);
            Application.ShowException(E);
         end;
      end;
   end;
end;

//------------------------------------------------------------------------------

procedure TJoystick.SetThreshold(AValue: Cardinal);

begin
  if FThreshold <> AValue then
  begin
    FThreshold := AValue;
    if not (csReading in ComponentState) then ReapplyCapture(FJoystickID);
  end;
end;

//------------------------------------------------------------------------------

function Approximation(Data: array of Integer): Integer;

// calculate a better estimation of the last value in the given data, depending
// on the other values (used to approximate a smoother joystick movement)
//
// based on Gauss' principle of smallest squares in Maximum-Likelihood and
// linear normal equations

var
  SumX, SumY, SumXX, SumYX: Double;
  I, Comps: Integer;
  a0, a1: Double;

begin
  SumX := 0;
  SumY := 0;
  SumXX := 0;
  SumYX := 0;
  Comps := High(Data) + 1;
  for I := 0 to High(Data) do
  begin
    SumX := SumX + I;
    SumY := SumY + Data[I];
    SumXX := SumXX + I * I;
    SumYX := SumYX + I * Data[I];
  end;
  a0 := (SumY * SumXX - SumX * SumYX) / (Comps * SumXX - SumX * SumX);
  a1 := (Comps * SumYX - SumY * SumX) / (Comps * SumXX - SumX * SumX);
  Result := Round(a0 + a1 * High(Data));
end;

procedure TJoystick.DoGetPosition;
var
   i				:Integer;
   Joy				:TJoyInfoEx;
   dX,dY,dZ,dR		:Integer;
   XPos,YPos,ZPos,RPos	:integer; // Axes: Roll, Pitch, Throttle, Yaw (rudder)
   POV				:integer; // Point of view button
begin
  {Read infos}
  with Joy do begin
    dwFlags:=JOY_RETURNALL;
    dwSize:=SizeOf(Joy);
    joyGetPosEx(cJoystickIDToNative[FJoystickID], @Joy);
    XPos:=DoScale(wXpos);
    YPos:=DoScale(wYpos);
    ZPos:=DoScale(wZpos);
    RPos:=DoScale(dwRpos);
    POV:=DoScale(dwPOV);
    MakeJoyButtons(wButtons);
  end;//with

  {Initialisation}
  if (FLastX=-1)or(FLastY=-1)or(FLastZ=-1)or(FLastR=-1) then begin
    FLastX:=XPos;
    FLastY:=YPos;
    FLastZ:=ZPos;
    FLastR:=RPos;
    for I:=0 to 4 do begin
      FXPosInfo[I]:=XPos;
      FYPosInfo[I]:=YPos;
      FZPosInfo[I]:=ZPos;
      FRPosInfo[I]:=RPos;
    end;

  {Smoothing output}
  end else begin
    Move(FXPosInfo[1], FXPosInfo[0], 16);
    FXPosInfo[4] := XPos;
    XPos := Approximation(FXPosInfo);
    Move(FYPosInfo[1], FYPosInfo[0], 16);
    FYPosInfo[4] := YPos;
    YPos := Approximation(FYPosInfo);
    Move(FZPosInfo[1], FZPosInfo[0], 16);
    FZPosInfo[4] := ZPos;
    ZPos := Approximation(FZPosInfo);
    Move(FRPosInfo[1], FRPosInfo[0], 16);
    FRPosInfo[4] := RPos;
    RPos := Approximation(FRPosInfo);

    {Scaling output in [-100,100] }
    dX:=Round((XPos-FMinMaxInfo[jaX,jpCenter])*100/FMinMaxInfo[jaX,jpCenter]);
    dY:=Round((YPos-FMinMaxInfo[jaY,jpCenter])*100/FMinMaxInfo[jaY,jpCenter]);
    dZ:=Round((ZPos-FMinMaxInfo[jaZ,jpCenter])*100/FMinMaxInfo[jaZ,jpCenter]);
    dR:=Round((RPos-FMinMaxInfo[jaR,jpCenter])*100/FMinMaxInfo[jaR,jpCenter]);

    {Point of view}
    if POV>=0 then FPointOfView:=Round(POV/100) // Point of view in degrees.
    else FPointOfView:=POV;                     // -1 if centered

    {Updating joystick states }
    FXPosition:=dX;
    FYPosition:=dY;
    FZPosition:=dZ;
    FRPosition:=dR;
    FLastX:=XPos;
    FLastY:=YPos;
    FLastZ:=ZPos;
    FLastR:=RPos;
  end;
end;

procedure TJoystick.DoChange;
{ Only buttons, X and Y axes provided to the event handler. Other axes will have
  to be got through properties}
begin
  GetPosition;
  if Assigned(FOnJoystickMove)
  then FOnJoystickMove(Self, FJoystickID, FJoyButtons, FXPosition, FYPosition);
end;

procedure TJoystick.GetPosition(var XDeflection, YDeflection, ZDeflection,
  RDeflection: integer; var Buttons: TJoystickButtons;
  var PointOfView: Integer);
{Updates the stats (R-axis and POV button do not fire a WinMessage and
 provides them in the parameters}
begin
  DoGetPosition;
  XDeflection:=FXPosition;
  YDeflection:=FYPosition;
  ZDeflection:=FZPosition;
  RDeflection:=FRPosition;
  Buttons:=FJoyButtons;
  PointOfView:=FPointOfView;
end;

procedure TJoystick.GetPosition;
begin
  DoGetPosition;
end;

end.

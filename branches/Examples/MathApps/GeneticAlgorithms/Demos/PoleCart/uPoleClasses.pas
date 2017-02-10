unit uPoleClasses;

interface

uses
  SysUtils, Math, Graphics, Forms, StdCtrls;

const
  one_degree = 0.0174532;	{ 2pi/360 }
  six_degrees = 0.1047192;
  twelve_degrees = 0.2094384;
  thirtysix_degrees = twelve_degrees * 3;
  fifty_degrees = 0.87266;

  MUP = 0.000002;
  MUC = 0.0005;

  MAX_STEPS = 1000;
  GRAVITY = -9.81;
  RAIL_LENGTH = 4.8;

  FOURTHIRDS = 1.3333333333333;

  USE_RK4 = true;

type
  //TSingleArray = array[-2..7] of single;
  TSingleArray = array[0..5] of single;

  TPoleCart = class
    Position, Speed  :  single;
    Angle, AngleSpeed : single;
    ShortAngle, ShortAngleSpeed : single;

    CartMass, PoleMass, ShortPoleMass, TotalMass : single;
    PoleLength, PoleMassLength : single;
    ShortPoleLength, ShortPoleMassLength : single;
    MaxForce : single;
    LastForce : single;

    State : TSingleArray;

    Step, MaxSteps : integer;

    DrawScale : single;
    DrawOffsetX, DrawOffsetY : integer;
    DrawBoxX, DrawBoxY : single;

    TimeLabel : TLabel;

    Canvas : TCanvas;

    Visible : boolean;

    WiggleCount : single;

    TwoPoles : boolean;

    TAU : single;
    EULER_TAU : single;

    TimeArray : array[0..MAX_STEPS] of single;

    procedure Draw;
    procedure Randomize;
    procedure SetToZero;

    procedure CopyStateToVariables;

    function OutsideBounds : boolean;
    function BalancePole(MaxSteps : integer) : integer;
    procedure ApplyForce(Force : single); virtual;
    procedure ApplyForceOnePole(Force : single); virtual;
    procedure ApplyForceTwoPoles(Force : single); virtual;
    function GetForceActivator : single; virtual;

    constructor Create(TwoPoles : boolean);
    procedure Initialize;
  end;

implementation

procedure ClearSingleArray(var a : TSingleArray);
var
  i : integer;
begin
  for i := 0 to 5 do
    a[i] := 0;
end;

{ TPoleCart }

(*cart_pole(action, x, x_dot, theta, theta_dot)
int action;
float *x, *x_dot, *theta, *theta_dot;
{
    float xacc,thetaacc,force,costheta,sintheta,temp;

    force = (action>0)? FORCE_MAG : -FORCE_MAG;
    costheta = cos(*theta);
    sintheta = sin(*theta);

    temp = (force + POLEMASS_LENGTH * *theta_dot * *theta_dot * sintheta)
		         / TOTAL_MASS;

    thetaacc = (GRAVITY * sintheta - costheta* temp)
	       / (LENGTH * (FOURTHIRDS - MASSPOLE * costheta * costheta / TOTAL_MASS));

    xacc  = temp - POLEMASS_LENGTH * thetaacc* costheta / TOTAL_MASS;

/*** Update the four state variables, using Euler's method. ***/

    *x  += TAU * *x_dot;
    *x_dot += TAU * xacc;
    *theta += TAU * *theta_dot;
    *theta_dot += TAU * thetaacc;
  }
*)

procedure TPoleCart.ApplyForce(Force: single);
begin
  if TwoPoles then
    ApplyForceTwoPoles(Force)
  else
    ApplyForceOnePole(Force);
end;

procedure TPoleCart.ApplyForceOnePole(Force: single);
var
  xAcc, AngleAcc, cosAngle, sinAngle, temp, divisor : single;
begin
  cosAngle := cos(angle);
  sinAngle := sin(angle);

  // temp = (force + POLEMASS_LENGTH * *theta_dot * *theta_dot * sintheta) / TOTAL_MASS;
  temp := (force + PoleLength * sqr(anglespeed) * sinAngle) / TotalMass;

  // /          (LENGTH * (FOURTHIRDS - MASSPOLE * costheta * costheta / TOTAL_MASS));
  divisor := (PoleLength/2) * (FOURTHIRDS - PoleMass * sqr(cosAngle) / TotalMass);

  //  thetaacc = (GRAVITY * sintheta - costheta* temp) / divisor
  AngleAcc := (-GRAVITY * sinAngle - cosAngle * temp) / divisor;

  // xacc  = temp - POLEMASS_LENGTH * thetaacc* costheta / TOTAL_MASS;
  xAcc := temp - PoleMassLength * AngleAcc * cosAngle / TotalMass;

  // update the four state variables

  // CART!
  // *x  += TAU * *x_dot;
  Position := Position + TAU * Speed;

  // *x_dot += TAU * xacc;
  Speed := Speed + TAU * xAcc;

  // POLE!
  // *theta += TAU * *theta_dot;
  Angle := Angle + TAU * AngleSpeed;
  
  // *theta_dot += TAU * thetaacc;
  AngleSpeed := AngleSpeed + TAU * AngleAcc;
end;

(*
int go_cart(net,max_steps)
  network *net;
  int max_steps;

{
   float x,			/* cart position, meters */
         x_dot,			/* cart velocity */
         theta,			/* pole angle, radians */
         theta_dot;		/* pole angular velocity */
   int steps=0,y;

  if (RANDOM_START) {
    /*set up random start state*/
    x = (lrand48()%4800)/1000.0 - 2.4;
    x_dot = (lrand48()%2000)/1000.0 - 1;
    theta = (lrand48()%400)/1000.0 - .2;
    theta_dot = (lrand48()%3000)/1000.0 - 1.5;
    }
  else
    x = x_dot = theta = theta_dot = 0.0;

  /*--- Iterate through the action-learn loop. ---*/
  while (steps++ < max_steps)
    {

      /*-- setup the input layer based on the four iputs --*/
      setup_input(net,x,x_dot,theta,theta_dot);
      activate_net(net);   /*-- activate the network based on the input --*/

      /*-- decide which way to push via which output unit is greater --*/
      if (net->output_unit[0].sum > net->output_unit[1].sum)
        y = 0;
      else
        y = 1;

      /*--- Apply action to the simulated cart-pole ---*/
      cart_pole(y, &x, &x_dot, &theta, &theta_dot);

      /*--- Check for failure.  If so, return steps ---*/
      if (x < -2.4 || x > 2.4  || theta < -twelve_degrees ||
          theta > twelve_degrees)
         return steps;
    }
  return steps;
}
*)

procedure TPoleCart.ApplyForceTwoPoles(Force: single);
(*

void CartPole::step(double action, double *st, double *derivs)
{
    double force,costheta_1,costheta_2,sintheta_1,sintheta_2,
          gsintheta_1,gsintheta_2,temp_1,temp_2,ml_1,ml_2,fi_1,fi_2,mi_1,mi_2;

    force =  (action - 0.5) * FORCE_MAG * 2;
    costheta_1 = cos(st[2]);
    sintheta_1 = sin(st[2]);
    gsintheta_1 = GRAVITY * sintheta_1;
    costheta_2 = cos(st[4]);
    sintheta_2 = sin(st[4]);
    gsintheta_2 = GRAVITY * sintheta_2;

    ml_1 = LENGTH_1 * MASSPOLE_1;
    ml_2 = LENGTH_2 * MASSPOLE_2;
    temp_1 = MUP * st[3] / ml_1;
    temp_2 = MUP * st[5] / ml_2;
    fi_1 = (ml_1 * st[3] * st[3] * sintheta_1) +
           (0.75 * MASSPOLE_1 * costheta_1 * (temp_1 + gsintheta_1));
    fi_2 = (ml_2 * st[5] * st[5] * sintheta_2) +
           (0.75 * MASSPOLE_2 * costheta_2 * (temp_2 + gsintheta_2));
    mi_1 = MASSPOLE_1 * (1 - (0.75 * costheta_1 * costheta_1));
    mi_2 = MASSPOLE_2 * (1 - (0.75 * costheta_2 * costheta_2));

    derivs[1] = (force + fi_1 + fi_2)
                 / (mi_1 + mi_2 + MASSCART);

    derivs[3] = -0.75 * (derivs[1] * costheta_1 + gsintheta_1 + temp_1)
                 / LENGTH_1;
    derivs[5] = -0.75 * (derivs[1] * costheta_2 + gsintheta_2 + temp_2)
                  / LENGTH_2;

}

*)

  procedure Step(force : single; st : TSingleArray; var derivs : TSingleArray);
  var
    costheta_1,costheta_2,sintheta_1,sintheta_2,
    gsintheta_1,gsintheta_2,temp_1,temp_2,ml_1,ml_2,fi_1,fi_2,mi_1,mi_2 : single;

    LENGTH_1, MASSPOLE_1,
    LENGTH_2, MASSPOLE_2, MASSCART : single;
  begin
    LENGTH_1 := PoleLength/2;
    MASSPOLE_1 := PoleMass;
    LENGTH_2 := ShortPoleLength/2;
    MASSPOLE_2 := ShortPoleMass;
    MASSCART := CartMass;

    costheta_1 := cos(st[2]);
    sintheta_1 := sin(st[2]);
    gsintheta_1 := GRAVITY * sintheta_1;

    costheta_2 := cos(st[4]);
    sintheta_2 := sin(st[4]);
    gsintheta_2 := GRAVITY * sintheta_2;

    ml_1 := LENGTH_1 * MASSPOLE_1;
    ml_2 := LENGTH_2 * MASSPOLE_2;
    temp_1 := MUP * st[3] / ml_1;
    temp_2 := MUP * st[5] / ml_2;
    fi_1 := (ml_1 * st[3] * st[3] * sintheta_1) +
           (0.75 * MASSPOLE_1 * costheta_1 * (temp_1 + gsintheta_1));
    fi_2 := (ml_2 * st[5] * st[5] * sintheta_2) +
           (0.75 * MASSPOLE_2 * costheta_2 * (temp_2 + gsintheta_2));
    mi_1 := MASSPOLE_1 * (1 - (0.75 * costheta_1 * costheta_1));
    mi_2 := MASSPOLE_2 * (1 - (0.75 * costheta_2 * costheta_2));

    derivs[1] := (force + fi_1 + fi_2)
                 / (mi_1 + mi_2 + MASSCART);

    derivs[3] := -0.75 * (derivs[1] * costheta_1 + gsintheta_1 + temp_1)
                 / LENGTH_1;
    derivs[5] := -0.75 * (derivs[1] * costheta_2 + gsintheta_2 + temp_2)
                  / LENGTH_2;

  end;

(*void CartPole::rk4(double f, double y[], double dydx[], double yout[])
{

	int i;

	double hh,h6,dym[6],dyt[6],yt[6];


	hh=TAU*0.5;
	h6=TAU/6.0;
	for (i=0;i<=5;i++) yt[i]=y[i]+hh*dydx[i];
	step(f,yt,dyt);
	dyt[0] = yt[1];
	dyt[2] = yt[3];
	dyt[4] = yt[5];
	for (i=0;i<=5;i++) yt[i]=y[i]+hh*dyt[i];
	step(f,yt,dym);
	dym[0] = yt[1];
	dym[2] = yt[3];
	dym[4] = yt[5];
	for (i=0;i<=5;i++) {
		yt[i]=y[i]+TAU*dym[i];
		dym[i] += dyt[i];
	}
	step(f,yt,dyt);
	dyt[0] = yt[1];
	dyt[2] = yt[3];
	dyt[4] = yt[5];
	for (i=0;i<=5;i++)
		yout[i]=y[i]+h6*(dydx[i]+dyt[i]+2.0*dym[i]);
}*)
  procedure RK4(f : single; var y, dydx, yout : TSingleArray);
  var
	  i : integer;

	  hh,h6 : single;
    dym,dyt,yt : TSingleArray;
  begin
    hh:=TAU*0.5;
    h6:=TAU/6.0;

    //for (i=0;i<=5;i++)
    for i := 0 to 5 do
      yt[i]:=y[i]+hh*dydx[i];

    step(f,yt,dyt);

    dyt[0] := yt[1];
    dyt[2] := yt[3];
    dyt[4] := yt[5];

    //for (i=0;i<=5;i++)
    for i := 0 to 5 do
      yt[i]:=y[i]+hh*dyt[i];

    step(f,yt,dym);
    dym[0] := yt[1];
    dym[2] := yt[3];
    dym[4] := yt[5];
    //for (i=0;i<=5;i++)
    for i := 0 to 5 do
    begin
      yt[i]:=y[i]+TAU*dym[i];
      dym[i] := dym[i]+dyt[i];
    end;

    step(f,yt,dyt);
    dyt[0] := yt[1];
    dyt[2] := yt[3];
    dyt[4] := yt[5];

    //for (i=0;i<=5;i++)
    for i := 0 to 5 do
      yout[i]:=y[i]+h6*(dydx[i]+dyt[i]+2.0*dym[i]);
  end;

(*
#define RK4 1
#define EULER_TAU (TAU/4)
void CartPole::performAction(const vector<double> &output)
{ 
  
  int i;
  double  dydx[6];

  /*random start state for long pole*/
  /*state[2]= drand48();   */


  /*--- Apply action to the simulated cart-pole ---*/

  if(RK4){
    for(i=0;i<2;++i){
      dydx[0] = state[1];
      dydx[2] = state[3];
      dydx[4] = state[5];
      step(output[0],state,dydx);
      rk4(output[0],state,dydx,state);
    }
  }
  else{
    for(i=0;i<8;++i){
      step(output[0],state,dydx);
      state[0] += EULER_TAU * dydx[0];
      state[1] += EULER_TAU * dydx[1];
      state[2] += EULER_TAU * dydx[2];
      state[3] += EULER_TAU * dydx[3];
      state[4] += EULER_TAU * dydx[4];
      state[5] += EULER_TAU * dydx[5];
    }
  }
}
*)
var
  i : integer;
  dydx : TSingleArray;
begin
  if USE_RK4 then
  begin
    for i := 0 to 1 do
    begin
      dydx[0] := state[1];
      dydx[2] := state[3];
      dydx[4] := state[5];
      step(force,state,dydx);
      rk4(force,state,dydx,state);
    end;
  end else
  begin
    dydx[0] := state[1];
    dydx[2] := state[3];
    dydx[4] := state[5];//}

    for i := 0 to 7 do
    begin
      Step(Force, State, dydx);

      state[0] := state[0] + EULER_TAU * dydx[0];
      state[1] := state[1] + EULER_TAU * dydx[1];
      state[2] := state[2] + EULER_TAU * dydx[2];
      state[3] := state[3] + EULER_TAU * dydx[3];
      state[4] := state[4] + EULER_TAU * dydx[4];
      state[5] := state[5] + EULER_TAU * dydx[5];//}
    end;
  end;

  CopyStateToVariables;
end;

function TPoleCart.BalancePole(MaxSteps : integer): integer;
const
  // DON'T USE THIS! DON'T SET IT TO TRUE
  NEAT_FITNESS = false;
var
  i : integer;
  Force : single;
  CurrentWiggle : single;
begin
  // Set the step counter to zero
  Step := 0;

  // Reset the wiggle counter
  WiggleCount := 0;

  while Step<MaxSteps do
  begin
    // Calculate the force that the strategy wants to apply
    Force := GetForceActivator * MaxForce;

    // Keep the last excerted force, for drawing purposes
    LastForce := Force;

    // Apply the force to the wagon
    ApplyForce(Force);

    // Draw the cart if it's visible!
    if Visible then
    begin
      Draw;
      Sleep(trunc(TAU*1000/10));
    end;

    if TwoPoles then
      CurrentWiggle := abs(State[0])+abs(State[1])+abs(State[2])+abs(State[3])
    else
      CurrentWiggle := abs(Angle) + abs(AngleSpeed) + abs(Position) + abs(Speed);


    // Add up the wiggle counter
    if NEAT_FITNESS then
    begin
      if Step>=900 then
        WiggleCount := WiggleCount + CurrentWiggle;//}
    end;

    if Step<= High(TimeArray) then
      TimeArray[Step] := CurrentWiggle;


    // Check for failure
    if OutsideBounds then
      break;

    // Terminated?
    if Application.Terminated then
      break;

    // Increase the step counter
    inc(Step);
  end;

  // Calculate the wigglecount for the last 100 timesteps
  if not NEAT_FITNESS then
  begin
    if (Step <= High(TimeArray)) and (Step>=100) then
      for i := 1 to 100 do
        WiggleCount := WiggleCount + TimeArray[Step-i];//}
  end;

  // Return the number of steps completed
  result := Step;
end;


procedure TPoleCart.CopyStateToVariables;
begin
  Position := state[0];
  Speed := state[1];

  Angle := state[2];
  AngleSpeed := state[3];

  ShortAngle := state[4];
  ShortAngleSpeed := State[5];
end;

constructor TPoleCart.Create(TwoPoles : boolean);
begin
  MaxSteps := MAX_STEPS;

  CartMass := 1.0;
  PoleMass := 0.1;

  PoleLength := 1;
  MaxForce := 10.0;

  ShortPoleMass := PoleMass * 0.1;
  ShortPoleLength := PoleLength*0.1;

  DrawScale := 105;
  DrawBoxX := 0.5;
  DrawBoxY := 0.2;
  DrawOffsetX := 50;
  DrawOffsetY := 150;

  self.TwoPoles := TwoPoles;

  if TwoPoles then
  begin
    TAU := 0.01;
    EULER_TAU := TAU/4;
  end else
  begin
    TAU := 0.02;
  end;

  Visible := true;

  Initialize;
end;

procedure TPoleCart.Draw;
var
  cx, cy, HalfBox,ex,ey : integer;
begin
  // Make sure there's a canvas
  if Canvas=nil then
    exit;

  with Canvas do
  begin
    // Clear the canvas
    Rectangle(ClipRect);

    // Draw the rail
    HalfBox := trunc(DrawBoxX*DrawScale/2);
    Pen.Color := clBlack;
    Rectangle(
      DrawOffsetX-HalfBox,
      DrawOffsetY-1,
      DrawOffsetX+trunc(RAIL_LENGTH*DrawScale)+ HalfBox,
      DrawOffsetY+5);

    // Draw the box
    Pen.Color := clBlack;
    cx := DrawOffsetX+trunc(((Position+RAIL_LENGTH/2))*DrawScale);
    cy := DrawOffsetY-trunc(DrawBoxY*DrawScale);
    Rectangle(
      trunc(cx - (DrawBoxX/2)*DrawScale),
      cy,
      trunc(cx + (DrawBoxX/2)*DrawScale),
      DrawOffsetY);


    // Draw the poles
    if TwoPoles then
    begin
      // Draw the pole
      Pen.Width := 4;
      MoveTo(cx-HalfBox, cy);
      ex := cx+trunc(DrawScale*(sin(angle)*PoleLength))-HalfBox;
      ey := cy-trunc(DrawScale*(cos(angle)*PoleLength));
      LineTo(ex, ey);
      Pen.Width := 1;

      Pen.Color := clBlue;
      MoveTo(cx+HalfBox, cy);
      ex := cx+trunc(DrawScale*(sin(ShortAngle)*ShortPoleLength*5))+HalfBox;
      ey := cy-trunc(DrawScale*(cos(ShortAngle)*ShortPoleLength*5));
      LineTo(ex, ey);

      Pen.Color := clBlack;
      Pen.Width := 4;
      MoveTo(cx+HalfBox, cy);
      ex := cx+trunc(DrawScale*(sin(ShortAngle)*ShortPoleLength))+HalfBox;
      ey := cy-trunc(DrawScale*(cos(ShortAngle)*ShortPoleLength));
      LineTo(ex, ey);
      Pen.Width := 1;
    end else
    begin
      // Draw the pole
      Pen.Width := 4;
      MoveTo(cx, cy);
      ex := cx+trunc(DrawScale*(sin(angle)*PoleLength));
      ey := cy-trunc(DrawScale*(cos(angle)*PoleLength));
      LineTo(ex, ey);
      Pen.Width := 1;
    end;

    // Draw the force
    Pen.Width := 1;
    Pen.Color := clRed;
    Pen.Width := 2;
    MoveTo(cx,cy+trunc(DrawBoxY*DrawScale/2));
    LineTo(cx+Trunc(3*DrawBoxX*DrawScale*LastForce/MaxForce),cy+trunc(DrawBoxY*DrawScale/2));
    Pen.Width := 1;
    Pen.Color := clBlack;

    if TimeLabel<>nil then
      TimeLabel.Caption := Format('%f s',[Step*TAU]);

    Refresh;
  end;

  Application.ProcessMessages;
end;

function TPoleCart.GetForceActivator: single;
begin
  result := angle*2+AngleSpeed;
end;

procedure TPoleCart.Initialize;
begin
  if TwoPoles then
  begin
    TotalMass := PoleMass+CartMass+ShortPoleMass;
    PoleMassLength := PoleMass * PoleLength / 2;
  end else
  begin
    TotalMass := PoleMass+CartMass;
    PoleMassLength := PoleMass * PoleLength / 2;
  end;
end;

function TPoleCart.OutsideBounds: boolean;
begin
  if TwoPoles then
  begin
    result :=
      (abs(state[0])>2.4) or
      (abs(state[2])>thirtysix_degrees) or
      (abs(state[4])>thirtysix_degrees);
  end else
  begin
    // Check for failure - outside rails
    result :=
      (abs(Position)>2.4) or
      (abs(Angle)>thirtysix_degrees);
  end;
end;

procedure TPoleCart.Randomize;
begin
  // set up random start state
{    x = (lrand48()%4800)/1000.0 - 2.4;
    x_dot = (lrand48()%2000)/1000.0 - 1;
    theta = (lrand48()%400)/1000.0 - .2;
    theta_dot = (lrand48()%3000)/1000.0 - 1.5;//}

  Position := random * RAIL_LENGTH - RAIL_LENGTH/2;
  Speed := random*2-1;
  Angle := random*0.4-0.2;
  AngleSpeed := random*3-1.5;
end;

procedure TPoleCart.SetToZero;
begin
  // set up zero start state
  Position := 0;
  Speed := 0;
  angle := 0;
  AngleSpeed := 0;
end;
end.

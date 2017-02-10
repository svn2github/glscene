unit uObstacleWorld;

interface

uses
  Math, Classes, Graphics, SysUtils;

const
  CIRCLE_SIZE = 2;

type
  TObstacleWorld = class;

  TBorder = class
    x1,y1,x2,y2,dx,dy,k,m : single;
    ObstacleWorld : TObstacleWorld;
    procedure Draw(Canvas : TCanvas);
    procedure Prepare;
    function IntersectionDistance(x,y,ldx,ldy, ang : single; Canvas : TCanvas) : single;
    function Distance(x,y : single ; Canvas : TCanvas) : single;
  end;

  TObstacleWorld = class
  private
    function GetBorder(i: integer): TBorder;
  public
    MaxSensorRange  : single;
    BorderList      : TList;
    DrawScale       : single;
    TurtleX, TurtleY, TurtleAngle : single;

    StartingX, StartingY, StartingDir, StepsToFinish : single;

    function Trans(a : single) : integer;
    procedure TurnTurtleLeft;
    procedure TurnTurtleRight;
    procedure ReverseTurtle;

    procedure D(length:single);
    procedure L;
    procedure R;

    procedure MoveTurtle(length : single);
    procedure DrawTurtle(length : single);
    procedure TurnTurtle(ang : single);
    procedure SetTurtleAngle(angle : single);

    procedure PositionTurtle(x,y : single);
    procedure DrawTurtleTo(x,y : single);

    procedure Draw(Canvas : TCanvas);

    function CheckObstacleImpact(x,y : single; Canvas : TCanvas) : single;

    function SenseDistance(x,y,angle : single; Canvas :TCanvas) : single;
    procedure AddBorder(x1,y1,x2,y2 : single);
    constructor Create;
    destructor Destroy; override;

    property Border[i : integer] : TBorder read GetBorder;
  end;

  function FastDistance(dx, dy : single) : single;

implementation

function FastDistance(dx, dy : single) : single;
begin
  dx := abs(dx);
  dy := abs(dy);
//  result := max(dx,dy)+min(dx,dy)/2;
  result := sqrt(sqr(dx)+sqr(dy));//}
end;

{ TObstacleWorld }

procedure TObstacleWorld.AddBorder(x1, y1, x2, y2 : single);
var
  Border : TBorder;
begin
  Border := TBorder.Create;

  Border.x1 := x1;
  Border.y1 := y1;
  Border.x2 := x2;
  Border.y2 := y2;
  Border.ObstacleWorld := self;
  Border.Prepare;

  BorderList.Add(Border);
end;

constructor TObstacleWorld.Create;
begin
  BorderList := TList.Create;

  PositionTurtle(10,10);
  SetTurtleAngle(pi/2);

  DrawScale := 2.5;
  MaxSensorRange := 30;
end;

destructor TObstacleWorld.Destroy;
var
  i : integer;
begin
  for i := 0 to BorderList.Count - 1 do
    Border[i].Free;

  BorderList.Free;

  inherited;
end;

procedure TObstacleWorld.Draw(Canvas: TCanvas);
var
  i : integer;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Canvas.ClipRect);
  Canvas.Pen.Width := 2;
  for i := 0 to BorderList.Count - 1 do
    Border[i].Draw(Canvas);
end;

procedure TObstacleWorld.DrawTurtle(length: single);
begin
  AddBorder(TurtleX, TurtleY, TurtleX+cos(TurtleAngle)*length, TurtleY+sin(TurtleAngle)*length);
  MoveTurtle(length);
end;

function TObstacleWorld.GetBorder(i: integer): TBorder;
begin
  result := TBorder(BorderList[i]);
end;

procedure TObstacleWorld.MoveTurtle(length: single);
begin
  TurtleX := TurtleX + cos(TurtleAngle)*length;
  TurtleY := TurtleY + sin(TurtleAngle)*length;
end;

procedure TObstacleWorld.PositionTurtle(x, y: single);
begin
  TurtleX := x;
  TurtleY := y;
end;

procedure TObstacleWorld.ReverseTurtle;
begin
  TurnTurtleLeft;
  TurnTurtleLeft;
end;

procedure TObstacleWorld.SetTurtleAngle(angle: single);
begin
  TurtleAngle := angle;
end;

function TObstacleWorld.SenseDistance(x, y, angle: single; Canvas: TCanvas) : single;
var
  i       : integer;
  px,py,ldx,ldy   : single;
  dist, testdist    : single;
  d : single;
begin
  dist := 10e5;
  ldx := cos(angle);
  ldy := sin(angle);

  for i := 0 to BorderList.Count-1 do
    with Border[i] do
    begin
      testdist := IntersectionDistance(x,y,ldx,ldy,angle, Canvas);

      if testdist < dist then
      begin
        dist := testdist;
      end;
    end;

  if Canvas <> nil then
  begin
    if dist < MaxSensorRange then
      d := dist
    else
      d := MaxSensorRange;

    Canvas.Pen.Width := 1;
    Canvas.MoveTo(trans(x), trans(y));
    Canvas.LineTo(trans(x+cos(angle)*d), trans(y+sin(angle)*d));

    px := x+cos(angle)*dist;
    py := y+sin(angle)*dist;

    if dist < MaxSensorRange then
      Canvas.Ellipse(trans(px)-CIRCLE_SIZE, trans(py)-CIRCLE_SIZE,trans(px)+CIRCLE_SIZE, trans(py)+CIRCLE_SIZE);//}
  end;

  if dist < MaxSensorRange then
    result := dist/MaxSensorRange
  else
    result := 1;
end;

function TObstacleWorld.Trans(a: single): integer;
begin
  result := trunc(a*DrawScale);
end;

procedure TObstacleWorld.TurnTurtle(ang: single);
begin
  TurtleAngle := TurtleAngle + ang;
end;

procedure TObstacleWorld.TurnTurtleLeft;
begin
  TurtleAngle := TurtleAngle-pi/2;
end;

procedure TObstacleWorld.TurnTurtleRight;
begin
  TurtleAngle := TurtleAngle+pi/2;
end;

function TObstacleWorld.CheckObstacleImpact(x,y : single; Canvas : TCanvas) : single;
var
  i : integer;
  Distance  : single;
begin
  Distance := 100;

  for i := 0 to BorderList.Count - 1 do
    Distance := min(Distance, Border[i].Distance(x,y, nil));

  if Canvas <> nil then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Ellipse(
      trans(x+distance),
      trans(y+distance),
      trans(x-distance),
      trans(y-distance));
    Canvas.Brush.Style := bsSolid;
  end;

  result := distance;
end;

procedure TObstacleWorld.DrawTurtleTo(x, y: single);
begin
  AddBorder(TurtleX, TurtleY, x,y);
  TurtleX := x;
  TurtleY := y;
end;

procedure TObstacleWorld.D(length: single);
begin
  DrawTurtle(length);
end;

procedure TObstacleWorld.L;
begin
  TurnTurtleLeft;
end;

procedure TObstacleWorld.R;
begin
  TurnTurtleRight;
end;

{ TBorder }

function TBorder.Distance(x, y : single ; Canvas: TCanvas): single;
var
  dist : single;
begin
  dist := 0;
  if dx=0 then
  begin
    if y<y1 then
    begin
      dist := FastDistance(x-x1,y-y1);
    end else
    if y>y2 then
    begin
      dist := FastDistance(x-x2,y-y2);
    end else
      dist := abs(x-x1);
  end else

  if dy=0 then
  begin
    if x<x1 then
    begin
      dist := FastDistance(x-x1,y-y1);
    end else
    if x>x2 then
    begin
      dist := FastDistance(x-x2,y-y2);
    end else
      dist := abs(y-y1);
  end;

  result := dist;

  if (dist<20) and (Canvas<>nil) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Ellipse(
      ObstacleWorld.trans(x+dist),
      ObstacleWorld.trans(y+dist),
      ObstacleWorld.trans(x-dist),
      ObstacleWorld.trans(y-dist));
  end;
end;

procedure TBorder.Draw(Canvas: TCanvas);
begin
  Canvas.MoveTo(ObstacleWorld.trans(x1), ObstacleWorld.trans(y1));
  Canvas.LineTo(ObstacleWorld.trans(x2), ObstacleWorld.trans(y2));
end;

function TBorder.IntersectionDistance(x, y,ldx,ldy, ang: single; Canvas : TCanvas): single;
var
  t : single;
  mx, my     : single;
begin
  result := 10e5;

  if ldx=0 then ldx := 0.00001;
  if ldy=0 then ldy := 0.00001;

  if dx=0 then
  begin
    // Vertical borders
    t := (x1-x)/ldx;

    mx := ldx*t + x;
    my := ldy*t + y;

    if (my<Min(y1,y2)) or (my>Max(y1,y2)) then exit;//}

  end else
  begin
    // Horizontal borders

    t := (y1-y)/ldy;

    mx := ldx*t + x;
    my := ldy*t + y;

    if (mx<x1) or (mx>x2) then exit;
  end;

  if t<0 then exit;

  {tx := ObstacleWorld.Trans(mx);
  ty := ObstacleWorld.Trans(my);//}

//  result := sqrt(sqr(x-mx)+sqr(y-my));
  result := FastDistance(x-mx,y-my);
end;

procedure TBorder.Prepare;
var
  t    : single;
begin
  dx := x2-x1;
  dy := y2-y1;

  if abs(dx)<0.01 then
  begin
    x2 := x1;
    dx := 0;
  end;

  if abs(dy)<0.01 then
  begin
    y2 := y1;
    dy := 0;
  end;

  if (dx<>0) and (x2<x1) or
     (dy<>0) and (y2<y1) then
  begin
    t := x1;
    x1 := x2;
    x2 := t;

    t := y1;
    y1 := y2;
    y2 := t;
  end;
end;
end.

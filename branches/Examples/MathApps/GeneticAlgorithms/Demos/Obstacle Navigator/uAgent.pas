unit uAgent;

interface

uses
  Graphics, uObstacleWorld;

const
  TURN_SPEED = 0.018;
  SPEED = 0.1;
  SIZE = 2;

type
  TAgent = class
    Direction : double;
    X,Y       : single;
    ObstacleWorld : TObstacleWorld;
    Canvas    : TCanvas;
    DeltaDir  : single;

    function SenseDistance(delta : double; Canvas : TCanvas) : single;
    procedure Turn(d : double);
    procedure TurnLeft(d : double);
    procedure TurnRight(d : double);
    procedure Move;virtual;
    procedure PrepareToMove;virtual;
    procedure Draw(Canvas : TCanvas);
  end;

  TAgentScanner = class(TAgent)
    procedure PrepareToMove;override;
  end;

  TAgentRandomScanner = class(TAgent)
    procedure PrepareToMove;override;
  end;

  TAgentScanner2 = class(TAgent)
    procedure PrepareToMove;override;
  end;

  TAgentRandomScanner2 = class(TAgent)
    procedure PrepareToMove;override;
  end;

implementation

{ TAgent }

procedure TAgent.Draw(Canvas: TCanvas);
var
  dx, dy  : single;
begin
  dx := cos(direction)*SIZE;
  dy := sin(direction)*SIZE;
  Canvas.MoveTo(ObstacleWorld.Trans(x-dx/2),ObstacleWorld.Trans(y-dy/2));
  Canvas.LineTo(ObstacleWorld.Trans(x+dx/2),ObstacleWorld.Trans(y+dy/2));
  Canvas.Brush.Style := bsClear;
  Canvas.Ellipse(ObstacleWorld.Trans(x+SIZE), ObstacleWorld.Trans(y+Size), ObstacleWorld.Trans(x-SIZE), ObstacleWorld.Trans(y-SIZE));
  Canvas.Brush.Style := bsSolid;
end;

procedure TAgent.Move;
begin
  if DeltaDir>TURN_SPEED then DeltaDir := TURN_SPEED;
  if DeltaDir<-TURN_SPEED then DeltaDir := -TURN_SPEED;

  Direction := Direction+DeltaDir;

  x := x + speed * cos(Direction);
  y := y + speed * sin(Direction);

  DeltaDir := 0;
end;

procedure TAgent.PrepareToMove;
var
  w : single;
  dleft, dright : single;
begin
  w := 0.20;

  dleft := ObstacleWorld.SenseDistance(x,y,Direction-w,Canvas);
  dright := ObstacleWorld.SenseDistance(x,y,Direction+w,Canvas);

  if dleft < dright then
  begin
    if dleft < 0.15 then
      TurnRight(1);

    if dleft < 0.35 then
      TurnRight(0.5);//}
  end else
  begin
    if dright < 0.15 then
      TurnLeft(1);

    if dright < 0.35 then
      TurnLeft(0.5);//}
  end;
end;

function TAgent.SenseDistance(delta: double; Canvas : TCanvas): single;
begin
  result := ObstacleWorld.SenseDistance(x,y,Direction+delta,Canvas);
end;

procedure TAgent.Turn(d: double);
begin
  DeltaDir := DeltaDir+d*TURN_SPEED;
end;

procedure TAgent.TurnLeft(d : double);
begin
  DeltaDir := DeltaDir-d*TURN_SPEED;
end;

procedure TAgent.TurnRight(d : double);
begin
  DeltaDir := DeltaDir+d*TURN_SPEED;
end;

{ TAgentScanner }

procedure TAgentScanner.PrepareToMove;
var
  TestDir, TestDist,
  BestDir, BestDist : single;
begin
  TestDir := -TURN_SPEED*10;
  BestDist := 0;
  BestDir := 0;

  while TestDir < TURN_SPEED*10 do
  begin
    TestDist := ObstacleWorld.SenseDistance(x,y,Direction+TestDir,Canvas);

    if TestDist>BestDist then
    begin
      BestDist := TestDist;
      BestDir := TestDir;
    end;

    TestDir := TestDir + 0.1;
  end;

  DeltaDir := BestDir;
end;

{ TAgentRandomScanner }

procedure TAgentRandomScanner.PrepareToMove;
var
  TestDir, TestDist,
  BestDir, BestDist : single;
  cnt : integer;
begin
  BestDist := 0;
  BestDir := 0;
  cnt := 0;

  while cnt < 2 do
  begin
    TestDir := random*TURN_SPEED*20-TURN_SPEED*10;

    TestDist := ObstacleWorld.SenseDistance(x,y,Direction+TestDir,Canvas);

    if TestDist>BestDist then
    begin
      BestDist := TestDist;
      BestDir := TestDir;
    end;

    inc(cnt);
  end;

  DeltaDir := BestDir;
end;
{ TAgentRandomScanner2 }

procedure TAgentRandomScanner2.PrepareToMove;
var
  TestDir, TestDist,
  WorstDir, WorstDist : single;
  cnt : integer;
  w : single;
begin
  WorstDist := 1;
  WorstDir := 0;
  cnt := 0;

  w := pi;

  while cnt < 5 do
  begin
    TestDir := random*w-w/2;

    TestDist := ObstacleWorld.SenseDistance(x,y,Direction+TestDir,Canvas);

    if TestDist<WorstDist then
    begin
      WorstDist := TestDist;
      WorstDir := TestDir;
    end;

    inc(cnt);
  end;

  DeltaDir := -WorstDir*(1-WorstDist);
{  if WorstDir<0 then
    DeltaDir := TURN_SPEED;

  if WorstDir>0 then
    DeltaDir := -TURN_SPEED;//}
end;

{ TAgentScanner2 }

procedure TAgentScanner2.PrepareToMove;
var
  TestDir, TestDist,
  WorstDir, WorstDist : single;
  w : single;
begin
  w := TURN_SPEED*50;
  TestDir := -w;
  WorstDir := 0;
  WorstDist := 1;

  while TestDir < w do
  begin
    TestDist := ObstacleWorld.SenseDistance(x,y,Direction+TestDir,Canvas);

    if TestDist<WorstDist then
    begin
      WorstDist := TestDist;
      WorstDir := TestDir;
    end;

    TestDir := TestDir + w/5*2;
  end;

  if WorstDir>0 then
    DeltaDir := -TURN_SPEED
  else
    DeltaDir := TURN_SPEED;
end;

end.

unit uWorlds;

interface

uses
  uObstacleWorld, Classes;

var
  Worlds  : TList;
  ObstacleWorld : TObstacleWorld;

implementation

begin
  Worlds  := TList.Create;

  ObstacleWorld := TObstacleWorld.Create;
  with ObstacleWorld do
  begin
    // Settings
    StartingX := 15;
    StartingY := 100;
    StartingDir := -pi/2+0.5;
    StepsToFinish := 1150;

    // Walls
    PositionTurtle(10,10);
    DrawTurtleTo(10,100);

    PositionTurtle(10,60);
    DrawTurtleTo(30,60);
    DrawTurtleTo(30,70);

    PositionTurtle(20,10);
    DrawTurtleTo(20,50);
    DrawTurtleTo(40,50);
    DrawTurtleTo(40,80);
    DrawTurtleTo(20,80);

    PositionTurtle(20,70);
    DrawTurtleTo(20,100);

    Worlds.Add(ObstacleWorld);
  end;

  ObstacleWorld := TObstacleWorld.Create;
  with ObstacleWorld do
  begin
    // Settings
    StartingX := 25;
    StartingY := 100;
    StartingDir := -pi/2+0.5;
    StepsToFinish := 1150;

    // Walls
    PositionTurtle(10,10);
    DrawTurtleTo(10,60);
    DrawTurtleTo(20,60);
    DrawTurtleTo(20,100);

    PositionTurtle(20,10);
    DrawTurtleTo(20,50);
    DrawTurtleTo(30,50);
    DrawTurtleTo(30,100);

    Worlds.Add(ObstacleWorld);
  end;

  ObstacleWorld := TObstacleWorld.Create;
  with ObstacleWorld do
  begin
    // Settings
    StartingX := 15;
    StartingY := 10;
    StartingDir := pi/2+0.5;
    StepsToFinish := 950;

    // Walls
    PositionTurtle(10,10);
    DrawTurtleTo(10,60);
    DrawTurtleTo(30,60);
    DrawTurtleTo(30,10);

    PositionTurtle(20,10);
    DrawTurtleTo(20,50);

    Worlds.Add(ObstacleWorld);
  end;//}

  ObstacleWorld := TObstacleWorld.Create;
  with ObstacleWorld do
  begin
    // Settings
    StartingX := 15;
    StartingY := 10;
    StartingDir := pi/2+0.5;
    StepsToFinish := 1000;

    // Walls
    PositionTurtle(10,10);
    DrawTurtleTo(10,50);
    DrawTurtleTo(30,50);

    PositionTurtle(30,60);
    DrawTurtleTo(30,30);

    PositionTurtle(20,10);
    DrawTurtleTo(20,40);

    PositionTurtle(20,20);
    DrawTurtleTo(40,20);
    DrawTurtleTo(40,60);

    Worlds.Add(ObstacleWorld);
  end;//}

  ObstacleWorld := TObstacleWorld.Create;
  with ObstacleWorld do
  begin
    // Settings
    StartingX := 15;
    StartingY := 10;
    StartingDir := pi/2+0.5;
    StepsToFinish := 1700;


    // Outer wall!
    PositionTurtle(10,10);
    SetTurtleAngle(pi/2);
    DrawTurtle(20);
    TurnTurtleLeft;
    DrawTurtle(10);
    TurnTurtleRight;
    DrawTurtle(10);
    TurnTurtleRight;
    DrawTurtle(10);
    TurnTurtleLeft;
    DrawTurtle(30);
    TurnTurtleLeft;
    DrawTurtle(40);
    TurnTurtleLeft;
    DrawTurtle(20);
    TurnTurtleRight;
    DrawTurtle(10);
    TurnTurtleLeft;
    DrawTurtle(30);
    TurnTurtleLeft;
    DrawTurtle(20);
    TurnTurtleRight;
    DrawTurtle(10);
    TurnTurtleLeft;
    DrawTurtle(10);

    // Inner wall!
    PositionTurtle(20,10);
    SetTurtleAngle(pi/2);
    DrawTurtle(10);
    TurnTurtleLeft;
    DrawTurtle(10);
    TurnTurtleRight;
    DrawTurtle(30);
    TurnTurtleRight;
    DrawTurtle(10);
    TurnTurtleLeft;
    DrawTurtle(10);
    TurnTurtleLeft;
    DrawTurtle(20);
    TurnTurtleLeft;
    DrawTurtle(20);
    TurnTurtleRight;
    DrawTurtle(10);
    TurnTurtleLeft;
    DrawTurtle(10);
    TurnTurtleLeft;
    DrawTurtle(20);

    Worlds.Add(ObstacleWorld);
  end;//}

  ObstacleWorld := TObstacleWorld.Create;
  with ObstacleWorld do
  begin
    // Settings
    StartingX := 35;
    StartingY := 10;
    StartingDir := pi/2+0.5;
    StepsToFinish := 1300;


    // Outer wall!
    PositionTurtle(30,10);
    SetTurtleAngle(pi/2);
    d(20);r;d(20);l;d(20);l;d(60);l;d(20);l;d(20);r;d(20);

    PositionTurtle(40,10);
    SetTurtleAngle(pi/2);
    d(30);l;
    MoveTurtle(20);
    ReverseTurtle;
    d(40);

    Worlds.Add(ObstacleWorld);
  end;//}
end.

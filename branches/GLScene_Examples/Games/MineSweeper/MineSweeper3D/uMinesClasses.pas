unit uMinesClasses;

interface

uses
  Classes, SysUtils, mmSystem, math;

type
  TMines = class;
  TSquareList = class;

  TSquareStatus = (ssHidden, ssRevealed, ssQuestion, ssFlag, ssErrorFlag, ssExploded);

  TSquare = class
  private
    FX, FY : integer;
    FNeighbouringMines : integer;
    FMines : TMines;
    FStatus: TSquareStatus;
    FSignalled: boolean;
    procedure SetStatus(const Value: TSquareStatus);
    procedure SetSignalled(const Value: boolean);
  public
    Mine : boolean;
    Neighbours : TSquareList;
    Data : TObject;

    Changed : boolean;
    RevealRecurseLevel : integer;

    procedure CountNeighbouringMines;
    function CountNeighbouring(SquareStatus : TSquareStatus) : integer;
    function AsString : string;

    procedure ClickedReveal(Wide : boolean);
    procedure ClickedRevealWide;
    procedure ClickedFlag;

    procedure RecursiveReveal(Level : integer);
    procedure Signal;
    procedure SignalWide;

    constructor Create(X, Y : integer; Mines : TMines);
    destructor Destroy;override;

    property X : integer read FX;
    property Y : integer read FY;
    property NeighbouringMines : integer read FNeighbouringMines;
    property Mines : TMines read FMines;
    property Status : TSquareStatus read FStatus write SetStatus;
    property Signalled : boolean read FSignalled write SetSignalled;
  end;

  TSquareList = class(TList)
  private
    function GetItems(i: integer): TSquare;
    procedure SetItems(i: integer; const Value: TSquare);
  public
    property Items[i : integer] : TSquare read GetItems write SetItems;default;
  end;

  TGameState = (gsWaiting, gsActive, gsWon, gsLost);
  TMapSize = (msBeginner, msIntermediate, msAdvanced);
  TMines = class
  private
    FCountX: integer;
    FCountY: integer;
    FMineCount: integer;
    FGameState: TGameState;
    FOnGameStarted: TNotifyEvent;
    FOnGameReset: TNotifyEvent;
    FOnGameWon: TNotifyEvent;
    FOnGameLost: TNotifyEvent;
    FOnAfterUpdate: TNotifyEvent;
    FOnRevealedFailed: TNotifyEvent;
    procedure SetCountX(const Value: integer);
    procedure SetCountY(const Value: integer);
    procedure SetMineCount(const Value: integer);
    procedure SetOnGameLost(const Value: TNotifyEvent);
    procedure SetOnGameReset(const Value: TNotifyEvent);
    procedure SetOnGameStarted(const Value: TNotifyEvent);
    procedure SetOnGameWon(const Value: TNotifyEvent);
    procedure SetOnAfterUpdate(const Value: TNotifyEvent);
    procedure SetOnRevealedFailed(const Value: TNotifyEvent);
  public
    SquareList : TSquareList;
    MinesList : TSquareList;
    HiddenCount : integer;
    RevealedCount : integer;
    GameStartedTime : cardinal;
    GameStoppedTime : cardinal;
    FlagsPlaced : integer;
    MoveHistory : TStringList;
    UseWarnings : boolean;
    SafeFirstReveal : boolean;

    procedure AddMoveInformation(s : string);
    constructor Create;
    destructor Destroy;override;

    function GameTimePassed : single;

    procedure ClearSquareList;
    procedure BuildRandomMap(MineCount, CountX, CountY : integer);
    procedure BuildRandomMapSpecified(MapSize : TMapSize);
    procedure BuildRandomSameSize;

    function GetSquareForXY(X, Y : integer) : TSquare;
    procedure TurnOfSignals;

    procedure StartGame;
    procedure FailGame;
    procedure MineRevealed;
    procedure WinGame;
  published
    property GameState : TGameState read FGameState;
    property MineCount : integer read FMineCount write SetMineCount;
    property CountX : integer read FCountX write SetCountX;
    property CountY : integer read FCountY write SetCountY;

    // An easy way to plug into the game engine
    property OnGameWon : TNotifyEvent read FOnGameWon write SetOnGameWon;
    property OnGameLost : TNotifyEvent read FOnGameLost write SetOnGameLost;
    property OnGameStarted : TNotifyEvent read FOnGameStarted write SetOnGameStarted;
    property OnGameReset : TNotifyEvent read FOnGameReset write SetOnGameReset;
    property OnAfterUpdate : TNotifyEvent read FOnAfterUpdate write SetOnAfterUpdate;
    property OnRevealedFailed : TNotifyEvent read FOnRevealedFailed write SetOnRevealedFailed;
  end;


implementation

{ TSquareList }

function TSquareList.GetItems(i: integer): TSquare;
begin
  result := Get(i);
end;

procedure TSquareList.SetItems(i: integer; const Value: TSquare);
begin
  Put(i, Value);
end;

{ TSquare }

function TSquare.AsString: string;
begin
  result := Format('%d|%d',[x,y]);
end;

procedure TSquare.ClickedFlag;
begin
  // If the game is either won or lost, the clicking serves no purpose
  if not (Mines.GameState in [gsActive, gsWaiting]) then
    exit;

  // This method is fired whenever the user actually RIGHT-CLICKS on a square.
  // Only these statuses make sense for right clicking
  if not (Status in [ssFlag, ssHidden, ssQuestion]) then
    exit;

  Mines.AddMoveInformation(Format('clicked flag|%s',[AsString]));

  if Status = ssHidden then
    Status := ssFlag else
  if Status = ssFlag then
    Status := ssQuestion else
  if Status = ssQuestion then
    Status := ssHidden;

  if (not Mines.UseWarnings) and (Status=ssQuestion) then
    Status := ssHidden;
end;

procedure TSquare.ClickedReveal(Wide : boolean);
begin
  // If the game is either won or lost, the clicking serves no purpose
  // Clicking a square is the only way to start a game!
  if Mines.GameState = gsWaiting then
    Mines.StartGame;

  if Mines.GameState <> gsActive then
    exit;

  // This method is fired whenever the user actually CLICKS on a square.
  // These statuses makes clicking pointless
  if Status in [ssFlag, ssExploded, ssRevealed] then
  begin
    if Assigned(Mines.OnRevealedFailed) and not Wide then
      Mines.OnRevealedFailed(Mines);
    exit;
  end;

  Mines.AddMoveInformation(Format('reveal|%s',[AsString]));

  // If it's a mine, then we're in trouble!
  if Mine then
  begin
    // Are we safe?
    if Mines.SafeFirstReveal and (Mines.RevealedCount=0) then
    begin
      // Flag it instead!
      ClickedFlag;
    end else
    begin
      Status := ssExploded;
      Mines.FailGame;
      exit;
    end;
  end;

  // Reveal the hidden, and it's neighbours!
  if (Status in [ssHidden, ssQuestion]) then
  begin
    RecursiveReveal(0);
    exit;
  end;

  //TSquareStatus = (ssHidden, ssRevealed, ssQuestion, ssFlag, ssExploded);
end;

procedure TSquare.ClickedRevealWide;
var
  i : integer;
begin
  // This is the same as clicking all the neighbouring squares. It can only
  // be done when the following conditions are met;
  // 1. It's revealed
  if Status<>ssRevealed then
    exit;

  // 2. The number on the revealed square is equal to the number of flags
  //    that neighbour it
  if CountNeighbouring(ssFlag)<>FNeighbouringMines then
    exit;

  // Now, click them all!
  for i := 0 to Neighbours.Count-1 do
    Neighbours[i].ClickedReveal(true);
end;

function TSquare.CountNeighbouring(SquareStatus: TSquareStatus): integer;
var
  i : integer;
  cnt : integer;
begin
  cnt := 0;

  for i := 0 to Neighbours.Count-1 do
    if (Neighbours[i].Status = SquareStatus) then inc(cnt);

  result := cnt;
end;

procedure TSquare.CountNeighbouringMines;
var
  i : integer;
begin
  FNeighbouringMines := 0;

  for i := 0 to Neighbours.Count-1 do
    if Neighbours[i].Mine then inc(FNeighbouringMines);
end;

constructor TSquare.Create(X, Y : integer; Mines : TMines);
begin
  Neighbours := TSquareList.Create;
  Status := ssHidden;

  FX := x;
  FY := y;
  FMines := Mines;
  Changed := false;
end;

destructor TSquare.Destroy;
begin
  Neighbours.Free;

  inherited;
end;

// WIDTH FIRST REVEAL
procedure TSquare.RecursiveReveal(Level : integer);
var
  j,i  : integer;

  Revealer : TSquare;
  RevealedSquares : TSquareList;

  procedure DoReveal(Square : TSquare);
  begin
    // If it's allready been revealed, just get out of here
    if (Square.Status=ssRevealed) or (Square.Status=ssFlag) then exit;

{    if RevealedSquares.IndexOf(Square)<>-1 then
      exit;//}

    // Reveal this node
    Square.Status := ssRevealed;
    Square.RevealRecurseLevel := Revealer.RevealRecurseLevel+1;

    RevealedSquares.Add(Square);
  end;

begin
  // If it's allready been revealed, just get out of here
  if (Status=ssRevealed) or (Status=ssFlag) then exit;

  // Reveal this node
  Status := ssRevealed;
  RevealRecurseLevel := 0;

  RevealedSquares := TSquareList.Create;
  RevealedSquares.Add(self);
  j := 0;

  try
    while j < RevealedSquares.Count do
    begin
      Revealer := RevealedSquares[j];

      if Revealer.NeighbouringMines=0 then
        for i := 0 to Revealer.Neighbours.Count-1 do
          DoReveal(Revealer.Neighbours[i]);

      inc(j);
    end;
  finally
    RevealedSquares.Free;
  end;
end;//}

{
// DEPTH FIRST REVEAL
procedure TSquare.RecursiveReveal(Level : integer);
var
  i  : integer;
begin
  // If it's allready been revealed, just get out of here
  if (Status=ssRevealed) or (Status=ssFlag) then exit;

  // Reveal this node
  Status := ssRevealed;
  RevealRecurseLevel := Level;

  // If it doesn't have any neighbouring bombs, start revealing the neighbours
  if (NeighbouringMines=0) then
    for i := 0 to Neighbours.Count-1 do
      Neighbours[i].RecursiveReveal(Level + 1);
end;//}

// DEPTH FIRST REVEAL 2
{procedure TSquare.RecursiveReveal(Level : integer);
var
  i  : integer;
begin
  // Reveal this node
  Status := ssRevealed;
  RevealRecurseLevel := Level;

  // If it doesn't have any neighbouring bombs, start revealing the neighbours
  if (NeighbouringMines=0) then
    for i := 0 to Neighbours.Count-1 do
      if not (Neighbours[i].Status in [ssRevealed, ssFlag]) then
        Neighbours[i].RecursiveReveal(Level + 1);
end;//}


procedure TSquare.SetSignalled(const Value: boolean);
begin
  if Value <> FSignalled then
    Changed := true;

  FSignalled := Value;
end;

procedure TSquare.SetStatus(const Value: TSquareStatus);
begin
  // Are we revealing a hidden non-bomb?
  if (Status<>ssRevealed) and (Value=ssRevealed) then
    Mines.MineRevealed;

  if (Value=ssFlag) then inc(Mines.FlagsPlaced);
  if (FStatus=ssFlag) then dec(Mines.FlagsPlaced);

  FStatus := Value;
  Changed := true;
end;

procedure TSquare.Signal;
begin
  if Mines.GameState in [gsWon, gsLost] then
    exit;

  // Signal only works if the square if question or hidden
  if (Status=ssHidden) or (Status=ssQuestion) then
    Signalled := true;
end;

procedure TSquare.SignalWide;
var
  i : integer;
begin
  if Mines.GameState in [gsWon, gsLost] then
    exit;

  // Signal self
  Signal;

  // Signal neighbours!
  for i := 0 to Neighbours.Count-1 do
    Neighbours[i].Signal;
end;

{ TMines }

constructor TMines.Create;
begin
  SquareList := TSquareList.Create;
  MinesList := TSquareList.Create;
  MoveHistory := TStringList.Create;
  GameStoppedTime := 0;
  GameStartedTime := 0;
  UseWarnings := true;
  SafeFirstReveal := false;
end;

procedure TMines.BuildRandomMap(MineCount, CountX, CountY: integer);
var
  i,x,y, tries, CurrentMines : integer;
  Square, NSquare : TSquare;
begin
  MoveHistory.Clear;
  AddMoveInformation(Format('new map|%d|%d|%d',[MineCount, x, y]));
  AddMoveInformation(Format('rand seed|%d',[RandSeed]));

  Assert(MineCount<CountX*CountY,'There''s no room for that many mines!');
  Assert(CountX>3,'CountX must be larger than 3!');
  Assert(CountY>3,'CountY must be larger than 3!');

  ClearSquareList;

  self.MineCount := MineCount;
  self.CountX := CountX;
  self.CountY := CountY;

  // Create the squares
  for x := 0 to CountX-1 do
    for y := 0 to CountY-1 do
    begin
      Square := TSquare.Create(x,y, self);
      SquareList.Add(Square);
    end;

  // Connect them to their neigbours!
  for i := 0 to Squarelist.Count-1 do
  begin
    Square := Squarelist[i];
    for x := -1 to 1 do
      for y := -1 to 1 do
      begin
        // Not neighbour with self!
        if (x=0) and (y=0) then
          continue;

        NSquare := GetSquareForXY(Square.X+X, Square.Y+Y);

        if NSquare <> nil then
          Square.Neighbours.Add(NSquare);
      end;
  end;

  // Assign random mines!
  tries := 10*MineCount;
  CurrentMines := 0;
  while (tries>0) and (CurrentMines<MineCount) do
  begin
    // Pick a random square
    Square := SquareList[random(SquareList.Count)];

    // If it's allready a mine, don't use it!
    if not Square.Mine then
    begin
      // Make it a mine
      Square.Mine := true;

      // Add to the mines list
      MinesList.Add(Square);

      // Increase the currentmine count
      inc(CurrentMines);
    end;

    dec(tries);
  end;

  // Update the mine count of the squares
  for i := 0 to SquareList.Count-1 do
    SquareList[i].CountNeighbouringMines;

  // Set game state to waiting
  FGameState := gsWaiting;

  // The number of hidden squares, that are NOT mines
  HiddenCount := SquareList.Count-MinesList.Count;

  GameStoppedTime := 0;
  GameStartedTime := 0;
  FlagsPlaced := 0;
  RevealedCount := 0;

  if Assigned(OnGameReset) then
    OnGameReset(self);
end;

destructor TMines.Destroy;
begin
  ClearSquareList;
  SquareList.Free;
  MinesList.Free;
  MoveHistory.Free;

  inherited;
end;

procedure TMines.SetCountX(const Value: integer);
begin
  FCountX := Value;
end;

procedure TMines.SetCountY(const Value: integer);
begin
  FCountY := Value;
end;

procedure TMines.SetMineCount(const Value: integer);
begin
  FMineCount := Value;
end;

procedure TMines.ClearSquareList;
var
  i : integer;
begin
  for i := 0 to SquareList.Count-1 do
    SquareList[i].Free;

  SquareList.Clear;
  MinesList.Clear;
end;

function TMines.GetSquareForXY(X, Y: integer): TSquare;
var
  i : integer;
begin
  result := nil;

  for i := 0 to SquareList.Count-1 do
    if (SquareList[i].X=X) and (SquareList[i].Y=Y) then
    begin
      result := SquareList[i];
      exit;
    end;
end;

procedure TMines.FailGame;
var
  i : integer;
begin
  GameStoppedTime := TimeGetTime;

  for i := 0 to MinesList.Count-1 do
    if MinesList[i].Status<>ssFlag then
      MinesList[i].Status := ssExploded;

  for i := 0 to SquareList.Count-1 do
    if (SquareList[i].Status=ssFlag) and not
       (SquareList[i].Mine) then
      SquareList[i].Status := ssErrorFlag;

  FGameState := gsLost;

  if Assigned(OnGameLost) then
    OnGameLost(self);

  TurnOfSignals;

  AddMoveInformation('game failed');
end;

procedure TMines.MineRevealed;
begin
  dec(HiddenCount);
  inc(RevealedCount);
  if HiddenCount=0 then
    WinGame;
end;

procedure TMines.WinGame;
var
  i : integer;
begin
  GameStoppedTime := TimeGetTime;

  // Yay
  FGameState := gsWon;

  for i := 0 to MinesList.Count-1 do
    MinesList[i].Status := ssFlag;

  if Assigned(OnGameWon) then
    OnGameWon(self);

  AddMoveInformation('game won');
end;

procedure TMines.BuildRandomMapSpecified(MapSize: TMapSize);
begin
  case MapSize of
    msBeginner : BuildRandomMap(10,9,9);
    msIntermediate : BuildRandomMap(40,16,16);
    msAdvanced : BuildRandomMap(99,30,16);
  end;
end;
procedure TMines.TurnOfSignals;
var
  i : integer;
begin
  for i := 0 to SquareList.Count-1 do
    SquareList[i].Signalled := false;
end;

procedure TMines.SetOnGameLost(const Value: TNotifyEvent);
begin
  FOnGameLost := Value;
end;

procedure TMines.SetOnGameReset(const Value: TNotifyEvent);
begin
  FOnGameReset := Value;
end;

procedure TMines.SetOnGameStarted(const Value: TNotifyEvent);
begin
  FOnGameStarted := Value;
end;

procedure TMines.SetOnGameWon(const Value: TNotifyEvent);
begin
  FOnGameWon := Value;
end;

procedure TMines.StartGame;
begin
  GameStartedTime := timeGetTime;

  FGameState := gsActive;

  if Assigned(OnGameStarted) then
    OnGameStarted(self);

  AddMoveInformation('game started');
end;

function TMines.GameTimePassed: single;
begin
  if GameState=gsActive then
    result := (timeGetTime-GameStartedTime)/1000
  else
    result := (GameStoppedTime-GameStartedTime)/1000;
end;

procedure TMines.SetOnAfterUpdate(const Value: TNotifyEvent);
begin
  FOnAfterUpdate := Value;
end;

procedure TMines.AddMoveInformation(s: string);
begin
  MoveHistory.Add(Format('%f|%s',[GameTimePassed, s]));

  if Assigned(OnAfterUpdate) then
    OnAfterUpdate(self);
end;

procedure TMines.BuildRandomSameSize;
begin
  BuildRandomMap(MineCount, CountX, CountY);
end;

procedure TMines.SetOnRevealedFailed(const Value: TNotifyEvent);
begin
  FOnRevealedFailed := Value;
end;

end.

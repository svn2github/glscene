unit uFighterControl;

interface

uses
  Contnrs,
  uSpacefighter;

type
  TdfFighterControl = class
  protected
    FEnabled: Boolean;
    FSpaceFighter: TdfSpaceFighter;
    procedure Shoot; virtual; abstract;
    procedure SetMouseMovement; virtual; abstract;
    procedure Accelerate; virtual; abstract;
    procedure Brake; virtual; abstract;
    procedure NextTarget; virtual; abstract;
  public
    property Enabled: Boolean read FEnabled write FEnabled;

    procedure Update(deltaTime: Double; X, Y: Integer); virtual; abstract;

//    procedure SetEnemyList(aList: TObjectList); virtual; abstract;
  end;

implementation

end.

{Based on lampogolovii' simple button}
unit uGUIButton;

interface

uses
  GLScene, GLHUDObjects;

type
  TdfGUIButton = class;

  TdfMouseButton = (mbNone, mbLeft, mbRight, mbMiddle);
  TdfMouseState =  (msIn, msOut);

  TdfMouseClick = procedure(Sender: TdfGUIButton; MouseButton: TdfMouseButton) of object;
  TdfMouseEvent = procedure(Sender: TdfGUIButton; OldMS, NewMS: TdfMouseState) of object;

  TdfGUIButton = class(TGLHUDSprite)
  private
  protected
    FState: TdfMouseState;
    FOnMouseClick: TdfMouseClick;
    FOnMouseEvent: TdfMouseEvent;
    FLPressed, FRPressed, FMPressed: Boolean;
    procedure Init(); virtual;
    function IsHit(X, Y: Integer): Boolean;
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;
    destructor Destroy; override;

    property OnMouseClick: TdfMouseClick read FOnMouseClick write FOnMouseClick;
    property OnMouseEvent: TdfMouseEvent read FOnMouseEvent write FOnMouseEvent;

    procedure UpdateMouseInfo(MouseX, MouseY: Integer; MouseButton: TdfMouseButton);
  end;

implementation

{ TdfGUUButton }

constructor TdfGUIButton.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
  Init();
end;

destructor TdfGUIButton.Destroy;
begin

  inherited;
end;

procedure TdfGUIButton.Init;
begin
  FState := msOut;
end;

function TdfGUIButton.IsHit(X, Y: Integer): Boolean;
begin
  Result := (Visible) and (X >= Position.X - Width / 2)
                      and (X <= Position.X + Width / 2)
                      and (Y >= Position.Y - Height / 2)
                      and (Y <= Position.Y + Height / 2);
end;

procedure TdfGUIButton.UpdateMouseInfo(MouseX, MouseY: Integer;
  MouseButton: TdfMouseButton);
begin
  if IsHit(MouseX, MouseY) then
  begin
    case MouseButton of
      mbNone:
        if Assigned(FOnMouseEvent) then
        begin
          FOnMouseEvent(Self, FState, msIn);
          FState := msIn;
        end;
      mbLeft:
        if Assigned(FOnMouseClick) and not FLPressed then
        begin
          FOnMouseClick(Self, mbLeft);
          FLPressed := True;
          FRPressed := False;
          FMPressed := False;
        end;
      mbRight:
        if Assigned(FOnMouseClick) and not FRPressed then
        begin
          FOnMouseClick(Self, mbRight);
          FLPressed := False;
          FRPressed := True;
          FMPressed := False;
        end;
      mbMiddle:
        if Assigned(FOnMouseClick) and not FMPressed then
        begin
          FOnMouseClick(Self, mbMiddle);
          FLPressed := False;
          FRPressed := False;
          FMPressed := True;
        end;
    end;
  end
  else
  begin
    if Assigned(FOnMouseEvent) then
      FOnMouseEvent(Self, FState, msOut);
    FState := msOut;
    FLPressed := MouseButton = mbLeft;
    FRPressed := MouseButton = mbRight;
    FMPressed := MouseButton = mbMiddle;
  end;
end;

end.

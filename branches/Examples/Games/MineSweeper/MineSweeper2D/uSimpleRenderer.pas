unit uSimpleRenderer;

interface

uses
  Classes, Dialogs, Math, Controls, ExtCtrls, SysUtils, Graphics,
  Types, Forms, uMinesClasses;

type
  TMinesRenderer=class
    Mines : TMines;
    Panel : TPanel;
    PanelList : TList;
    PanelsUpdated : integer;
    LastFocusPanel : TPanel;

    SquareSizeX, SquareSizeY : integer;

    constructor Create(Mines : TMines; Panel : TPanel);

    procedure Clear;
    procedure Prepare;
    procedure Render(OnlyChanged : boolean);

    // THE EVENTS!
    procedure SquarePanelClick(Sender: TObject);

    procedure SquarePanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure SquarePanelRightClick(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);

    procedure SquarePanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);

    procedure SquarePanelDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  end;

implementation

{ TMinesRenderer }

procedure TMinesRenderer.Clear;
var
  i : integer;
begin
  for i := 0 to PanelList.Count-1 do
    TPanel(PanelList[i]).Free;

  PanelList.Clear;
end;

constructor TMinesRenderer.Create(Mines: TMines; Panel: TPanel);
begin
  self.Mines := Mines;
  self.Panel := Panel;
  PanelList := TList.Create;
  SquareSizeX := 16;
  SquareSizeY := 16;
end;

procedure TMinesRenderer.Prepare;
var
  i : integer;
  Square : TSquare;
  sPanel : TPanel;
  maxRight, maxBottom : integer;
begin
  Panel.Hide;

  try
    Clear;

    maxRight := 0;
    maxBottom := 0;
    for i := 0 to Mines.SquareList.Count-1 do
    begin
      Square := Mines.SquareList[i];
      sPanel := TPanel.Create(Panel);
      PanelList.Add(sPanel);
      sPanel.Left := Square.X * SquareSizeX+2;
      sPanel.Width := SquareSizeX;

      sPanel.Top := Square.Y * SquareSizeY+2;
      sPanel.Height := SquareSizeY;
      sPanel.Parent := Panel;

      sPanel.Font.Style := [fsBold];
      sPanel.Font.Name := 'System';

      Square.Data := sPanel;
      sPanel.Tag := integer(Square);

      // EVENTS
      //sPanel.OnClick := SquarePanelClick;
      sPanel.OnMouseUp := SquarePanelMouseUp;
      sPanel.OnContextPopup := SquarePanelRightClick;
      sPanel.OnMouseMove := SquarePanelMouseMove;
      sPanel.OnDragOver := SquarePanelDragOver;
      sPanel.DragMode := dmAutomatic;

      maxRight := max(maxRight, sPanel.Left+sPanel.Width);
      maxBottom := max(maxBottom, sPanel.Top+sPanel.Height);
    end;

    Panel.Width := maxRight+2;
    Panel.Height := maxBottom+2;
  finally
    Panel.Show;
  end;
end;

procedure TMinesRenderer.Render(OnlyChanged : boolean);
  procedure StandardSetting(Panel : TPanel);
  begin
    Panel.BevelOuter := bvRaised;
    Panel.Caption := '';
    Panel.BevelWidth := 2;
  end;
var
  i : integer;
  Square : TSquare;
  sPanel : TPanel;
begin
  PanelsUpdated := 0;
  for i := 0 to Mines.SquareList.Count-1 do
  begin
    Square := Mines.SquareList[i];

    if OnlyChanged and not Square.Changed then
      continue;

    inc(PanelsUpdated);

    sPanel := TPanel(Square.Data);

    case Square.Status of
      ssHidden :
      begin
        StandardSetting(sPanel);
      end;

      ssFlag :
      begin
        StandardSetting(sPanel);
        sPanel.Caption := 'F';
      end;

      ssQuestion :
      begin
        StandardSetting(sPanel);
        sPanel.Caption := '?';
      end;

      ssRevealed :
      begin
        sPanel.BevelOuter := bvNone;
{        sPanel.BorderWidth := 1;
        sPanel.BorderStyle := bsSingle;
        sPanel.Ctl3D := false;//}

        if Square.NeighbouringMines>0 then
        begin
          if Square.NeighbouringMines=1 then
            sPanel.Font.Color := clBlue;

          if Square.NeighbouringMines=2 then
            sPanel.Font.Color := clGreen;

          if Square.NeighbouringMines=3 then
            sPanel.Font.Color := clRed;

          if Square.NeighbouringMines=4 then
            sPanel.Font.Color := clNavy;

          if Square.NeighbouringMines=5 then
            sPanel.Font.Color := clTeal;

          if Square.NeighbouringMines=6 then
            sPanel.Font.Color := clTeal;

          if Square.NeighbouringMines=7 then
            sPanel.Font.Color := clTeal;

          if Square.NeighbouringMines=8 then
            sPanel.Font.Color := clTeal;

          sPanel.Caption := IntToStr(Square.NeighbouringMines);
        end;
      end;

      ssExploded :
      begin
        sPanel.BevelOuter := bvNone;
        sPanel.Font.Color := clRed;
        sPanel.Caption := 'X';
      end;

      ssErrorFlag :
      begin
        sPanel.Font.Color := clRed;
        sPanel.Caption := 'f';
      end;
    end;

    if Square.Signalled then
      sPanel.BevelOuter := bvNone;

    Square.Changed := false;
  end;

end;

procedure TMinesRenderer.SquarePanelClick(Sender: TObject);
var
  Square : TSquare;
  sPanel : TPanel;
begin
  sPanel := TPanel(Sender);
  Square := TSquare(sPanel.Tag);

  Square.ClickedReveal(True);

  Render(true);
end;

procedure TMinesRenderer.SquarePanelDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  // This is needed to allow the user to shift around the cursor while clicked
  TControl(Sender).EndDrag(false);
end;

procedure TMinesRenderer.SquarePanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Square : TSquare;
  sPanel : TPanel;
begin
  sPanel := TPanel(Sender);
  Square := TSquare(sPanel.Tag);

  //if LastFocusPanel <> sPanel then
  begin
    LastFocusPanel := sPanel;
    Mines.TurnOfSignals;

    if ssLeft in Shift then
    begin
      if not (ssShift in Shift) then
        Square.Signal
      else
        Square.SignalWide;
    end;

    Render(true);
    Application.ProcessMessages;
  end;
end;

procedure TMinesRenderer.SquarePanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Square : TSquare;
  sPanel : TPanel;
begin
  Mines.TurnOfSignals;

  if Button = mbLeft then
  begin
    sPanel := TPanel(Sender);
    Square := TSquare(sPanel.Tag);

    if ssShift in Shift then
      Square.ClickedRevealWide
    else
      Square.ClickedReveal(True);

    Render(true);
  end;
end;

procedure TMinesRenderer.SquarePanelRightClick(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  Square : TSquare;
  sPanel : TPanel;
begin
  Handled := true;

  sPanel := TPanel(Sender);
  Square := TSquare(sPanel.Tag);

  Square.ClickedFlag;

  Render(true);
end;
end.

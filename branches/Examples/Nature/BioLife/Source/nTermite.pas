{*******************************************************}
{                                                       }
{                      Tachyon Unit                     }
{    Vector Raster Geographic Information Synthesis     }
{                     VOICE  ..  Tracer                 }
{                     GRIP ICE .. Tongs                 }
{                Digital Terrain Mapping                }
{               Image Locatable Holographics            }
{                          SOS MAP                      }
{  Surreal Object Synthesis Multimedia Analysis Product }
{                   Fractal3D  Life MOW                 }
{       Copyright (c) 1995,2006  Ivan Lee Herring       }
{                                                       }
{*******************************************************}
unit nTermite;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TTermitesForm = class(TForm)
    TermitePanel: TPanel;
    Image1: TImage;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    Label2: TLabel;
    TermitesEdit: TEdit;
    TermitesPerEdit: TEdit;
    Label3: TLabel;
    TermChipEdit: TEdit;
    TermChipPerEdit: TEdit;
    TermiteSizeRG: TRadioGroup;
    HelpBtn: TSpeedButton;
    MajiciansCB: TCheckBox;
    CancelBtn: TButton;
    TermitesOKBtn: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReallyClose;
    procedure FormCreate(Sender: TObject);
    procedure TermitesOKBtnClick(Sender: TObject);
{procedure NewStep(TX,TY,TD:Integer);}
    procedure TermitesRule;

    procedure HelpBtnClick(Sender: TObject);
    procedure CancelBitBtnClick(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
     
  public
     
  end;

var
  TermitesForm: TTermitesForm;
{var TX,TY,TD:Integer;  }
  OrthoClor: Boolean;
  TermWide, TermHeight, Termites, Chipset: Integer;

implementation
uses nUGlobal;
{$R *.DFM}


procedure TTermitesForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  TermiteFormY := TermitesForm.top;
  TermiteFormX := TermitesForm.left;
  OrthoClor := True;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TTermitesForm.ReallyClose;
begin
  OrthoClor := True;
{     SetLength(TermiteWorld, 0,0);
     SetLength(TermX,0);
     SetLength(TermY,0);
     SetLength(TermDX,0);
     SetLength(TermDY,0);}
  Close;
end;

procedure TTermitesForm.FormCreate(Sender: TObject);
begin
  top := TermiteFormY;
  left := TermiteFormX;
  OrthoClor := True;
end;

procedure TTermitesForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(6000);
end;

procedure TTermitesForm.CancelBitBtnClick(Sender: TObject);
begin
  OrthoClor := True;
  TermitesOKBtn.Enabled := True;
end;

procedure TTermitesForm.TermitesOKBtnClick(Sender: TObject);
var
  OKtogo: Boolean;
  TermitesPer, ChipPerset, CodeVx: Integer;
  CheckString: string;
begin
  OKtogo := True;
{Translate the Edits into Variables}
  val(TermitesEdit.Text, Termites, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(TermitesEdit.Text, CodeVx); end;
  if Termites = 0 then Termites := 1;
  val(TermitesPerEdit.Text, TermitesPer, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(TermitesPerEdit.Text, CodeVx); end;
  if TermitesPer = 0 then TermitesPer := 1
  else if TermitesPer > 99 then TermitesPer := 99;
  val(TermChipEdit.Text, Chipset, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(TermChipEdit.Text, CodeVx); end;
  if Chipset = 0 then Chipset := 1;
  val(TermChipPerEdit.Text, ChipPerset, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(TermChipPerEdit.Text, CodeVx); end;
  if ChipPerset = 0 then ChipPerset := 1
  else if ChipPerset > 99 then ChipPerset := 99;
  if OKtogo then
  begin
    case TermiteSizeRG.ItemIndex of
      0: begin
          TermWide := 148; {pixels}
          TermHeight := 228; {pixels}
        end;
      1: begin
          TermWide := 540; {639}
          TermHeight := 452; {480}
        end;
      2: begin
          TermWide := 135; {639}
          TermHeight := 113; {480}
        end;
      3: begin
          TermWide := 176; {799}
          TermHeight := 141; {600}
        end;
    end; {case}
    if (Termites > ((TermWide * TermHeight) div Round(100 /
      TermitesPer)))
      then
        Termites := ((TermWide * TermHeight) div Round(100 /
        TermitesPer));
    str(Termites, CheckString);
    TermitesEdit.Text := CheckString;
    if (Chipset > ((TermWide * TermHeight) div Round(100 /
      ChipPerset)))
      then
        Chipset := ((TermWide * TermHeight) div Round(100 /
        ChipPerset));
    str(Chipset, CheckString);
    TermChipEdit.Text := CheckString;
    TermitesOKBtn.Enabled := False;
    TermitesRule;
  end else ShowMessage('Input parameters dry rotted');
{OKtogo}
end;
(*
procedure TTermitesForm.NewStep(TX,TY,TD:Integer);
Begin
  // Rotate 45 degrees left or right, or continue in the same direction.
  TD := (TD + (random(3)  - 1));
  If (TD<-1) then TD:=-1 else If (TD>1) then TD:=1;
  // Move in the proper direction.
{  TX := (TX + dir[TD,0]+TermWide)mod TermWide;
  TY := (TY + dir[TD,1]+TermHeight)mod TermHeight;}
  TX := (TX + TD);
  If TX>TermWide then TX:= TermWide-1 else
    If TX<0 then TX:= 1;
  TY := (TY + TD);
  If TY>TermHeight then TY:= TermHeight-1 else
    If TY<0 then TY:= 1;
end;*)

procedure TTermitesForm.TermitesRule;
var TDX, TDY,
  TermiteCount3, TermiteCount2, TermiteCount, TempX, TempY, Planting,
    I, J: Integer;
  ChipSetter: Double;
  TermiteString3, TermiteString2, TermiteString, CheckString: string;
  TermX, TermY, TermDX, TermDY: array of Integer;
  TermiteWorld: array of array of Integer;
  NewRect: TRect;
  Bitmap: TBitmap;
begin
{144x144 (here)
157x113 (639x480)
197x143 (799x600)
TermWide,TermHeight, Termites,Chipset}
  if (TermHeight < 144) then
  begin
     {Enlarge form to display}
    TermitesForm.Left := 0;
    TermitesForm.Top := 0;
    TermitesForm.Width := (TermWide * 4) + 90 + 4 + 11;
    TermitesForm.Height := (TermHeight * 4) + 28 + 4;
    TermitePanel.Left := 90;
    TermitePanel.Top := 0;
    TermitePanel.Width := (TermWide * 4) + 2;
    TermitePanel.Height := (TermHeight * 4) + 2;
    TermitesForm.Image1.Left := 1;
    TermitesForm.Image1.Top := 1;
    TermitesForm.Image1.Width := (TermWide * 4) + 1; {391 401}
    TermitesForm.Image1.Height := (TermHeight * 4) + 1; {241 268}
    Bitmap := TBitmap.Create;
    Bitmap.Width := (TermWide * 4) + 1; {393}
    Bitmap.Height := (TermHeight * 4) + 1; {241}
    TermitesForm.Image1.Picture.Graphic := Bitmap;
  end
  else if (TermHeight > 444) then
  begin
    TermitesForm.Left := 0;
    TermitesForm.Top := 0;
    TermitesForm.Width := (TermWide) + 90 + 11 + 4;
    TermitesForm.Height := (TermHeight) + 28 + 4;
    TermitePanel.Left := 90;
    TermitePanel.Top := 0;
    TermitePanel.Width := (TermWide) + 4;
    TermitePanel.Height := (TermHeight) + 4;
    Image1.Left := 1;
    Image1.Top := 1;
    Image1.Width := (TermWide); {391 401}
    Image1.Height := (TermHeight);
    Bitmap := TBitmap.Create;
    Bitmap.Width := (TermWide); {393}
    Bitmap.Height := (TermHeight); {241}
    Image1.Picture.Graphic := Bitmap;
  end else
  begin
    TermitesForm.Left := TermiteFormX;
    TermitesForm.Top := TermiteFormY;
    TermitesForm.Width := 250;
    TermitesForm.Height := 258;
    TermitePanel.Left := 90;
    TermitePanel.Top := 0; {148 228}
    TermitePanel.Width := (TermWide) + 2;
    TermitePanel.Height := (TermHeight) + 2;
    Image1.Left := 1;
    Image1.Top := 1;
    Image1.Width := (TermWide); {391 401}
    Image1.Height := (TermHeight);
    Bitmap := TBitmap.Create;
    Bitmap.Width := (TermWide); {393}
    Bitmap.Height := (TermHeight); {241}
    Image1.Picture.Graphic := Bitmap;
  end;
  Application.ProcessMessages;
  SetLength(TermiteWorld, TermWide, TermHeight);
  SetLength(TermX, Termites);
  SetLength(TermY, Termites);
  SetLength(TermDX, Termites);
  SetLength(TermDY, Termites);
  Randomize;
  OrthoClor := False;
  ChipSetter := ChipSet;
  ChipSetter := (ChipSetter / 100);
  for I := 0 to TermWide - 1 do
    for J := 0 to TermHeight - 1 do begin
      if (Random < ChipSetter) then TermiteWorld[I, J] := 1
      else TermiteWorld[I, J] := 0;
      if (TermHeight < 144) then begin
         {USE other form to display}
        if (TermiteWorld[I, J] = 0) then
          Image1.Canvas.Brush.Color := ClWhite
        else if (TermiteWorld[I, J] = 1) then
          Image1.Canvas.Brush.Color := Full;
        NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
          4));
        Image1.Canvas.FillRect(NewRect);
      end else begin
        if (TermiteWorld[I, J] = 0) then
          Image1.Canvas.Pixels[I, J] := ClWhite
        else Image1.Canvas.Pixels[I, J] := Full;
      end;
    end;
  Application.ProcessMessages;
  for I := 0 to Termites - 1 do
  begin
    TermX[I] := Random(TermWide - 1);
    TermY[I] := Random(TermHeight - 1);
    TermDX[I] := Random(3) - 1;
    TermDY[I] := Random(3) - 1;
  end;
  Application.ProcessMessages;
  repeat
{TermitesForm.Caption:='Termites turns: '+TermiteString3;  }
    Application.ProcessMessages;
    for I := 0 to Termites - 1 do begin
// Save the current position and take a new step.
      TempX := TermX[I];
      TempY := TermY[I];
{newstep(TempX, TempY, TermD[I]);}
      TermDX[I] := (TermDX[I] + (Random(3) - 1));
      TermDY[I] := (TermDY[I] + (Random(3) - 1));
      if (TermDY[I] < -1) then
        TermDY[I] := -1 else if (TermDY[I] > 1) then TermDY[I] := 1;
      if (TermDY[I] < -1) then
        TermDY[I] := -1 else if (TermDY[I] > 1) then TermDY[I] := 1;
      TempX := TempX + TermDX[I];
      TempY := TempY + TermDY[I];
      if TempX > TermWide - 1 then TempX := TermWide - 1 else
        if TempX < 0 then TempX := 0;
      if TempY > TermHeight - 1 then TempY := TermHeight - 1 else
        if TempY < 0 then TempY := 0;
// Carrying a chip and moving to an empty spot:
      if ((TermiteWorld[TermX[I], TermY[I]] = 1) and
        (TermiteWorld[TempX, TempY] = 0)) then
      begin
        // Erase the old spot.
        TermiteWorld[TermX[I], TermY[I]] := 0;
        if (TermHeight < 144) then begin
          Image1.Canvas.Brush.Color := clWhite;
          NewRect := Rect((TermX[I] * 4), (TermY[I] * 4),
            ((TermX[I] * 4) + 4), ((TermY[I] * 4) + 4));
          Image1.Canvas.FillRect(NewRect);
        end else
          Image1.Canvas.Pixels[TermX[I], TermY[I]] := clWhite;
            {Desolate;}
        // Update the new position and draw the new spot. */
        TermX[I] := TempX;
        TermY[I] := TempY;
        TermiteWorld[TermX[I], TermY[I]] := 1;
        if (TermHeight < 144) then begin
          Image1.Canvas.Brush.Color := Full;
          NewRect := Rect((TermX[I] * 4), (TermY[I] * 4),
            ((TermX[I] * 4) + 4), ((TermY[I] * 4) + 4));
          Image1.Canvas.FillRect(NewRect);
        end else
          Image1.Canvas.Pixels[TermX[I], TermY[I]] := Full;
      end
     // Carrying a chip and moving to a chip:
      else if ((TermiteWorld[TermX[I], TermY[I]] = 1) and
        (TermiteWorld[TempX, TempY] = 1)) then
      begin
        // Reverse direction and step, thereby dropping the
        // current chip (and perhaps getting a new one?).
        if (TermDX[I] = 1) then TermDX[I] := -1 else
          if (TermDX[I] = -1) then TermDX[I] := 1;
        if (TermDY[I] = 1) then TermDY[I] := -1 else
          if (TermDY[I] = -1) then TermDY[I] := 1;
        TermX[I] := (TermX[I] + TermDX[I]);
        TermY[I] := (TermY[I] + TermDY[I]);
        if TermX[I] > TermWide - 1 then TermX[I] := TermWide - 1 else
          if TermX[I] < 0 then TermX[I] := 0;
        if TermY[I] > TermHeight - 1 then
          TermY[I] := TermHeight - 1 else
          if TermY[I] < 0 then TermY[I] := 0;
      end
      else begin // Not carrying anything, so just move to empty spot
        TermX[I] := TempX;
        TermY[I] := TempY;
      end;
      Application.ProcessMessages;
      if (OrthoClor = False) then begin
     // Not carrying anything, moving to a chip:
        if (MajiciansCB.Checked) then begin
          if ((TermiteWorld[TermX[I], TermY[I]] = 0) and
            (TermiteWorld[TempX, TempY] = 1)) then
          begin
        // Reverse direction and step,
        // Update the new position and draw the new spot. */
            if (TermDX[I] = 1) then TermDX[I] := -1 else
              if (TermDX[I] = -1) then TermDX[I] := 1;
            if (TermDY[I] = 1) then TermDY[I] := -1 else
              if (TermDY[I] = -1) then TermDY[I] := 1;
            TermX[I] := (TermX[I] + TermDX[I]);
            TermY[I] := (TermY[I] + TermDY[I]);
            if TermX[I] > TermWide - 1 then
              TermX[I] := TermWide - 1 else
              if TermX[I] < 0 then TermX[I] := 0;
            if TermY[I] > TermHeight - 1 then
              TermY[I] := TermHeight - 1 else
              if TermY[I] < 0 then TermY[I] := 0;
            TermiteWorld[TermX[I], TermY[I]] := 1;
            if (TermHeight < 144) then begin
              Image1.Canvas.Brush.Color := Full;
              NewRect := Rect((TermX[I] * 4), (TermY[I] * 4),
                ((TermX[I] * 4) + 4), ((TermY[I] * 4) + 4));
              Image1.Canvas.FillRect(NewRect);
            end else
              Image1.Canvas.Pixels[TermX[I], TermY[I]] := Full;
        // Erase the old spot.
            TermiteWorld[TempX, TempY] := 0;
            if (TermHeight < 144) then begin
              Image1.Canvas.Brush.Color := clWhite;
              NewRect := Rect((TempX * 4), (TempY * 4),
                ((TempX * 4) + 4), ((TempY * 4) + 4));
              Image1.Canvas.FillRect(NewRect);
            end else
              Image1.Canvas.Pixels[TempX, TempY] := clWhite;
          end
        end;
      end; end;
  until (OrthoClor = True);
  SetLength(TermiteWorld, 0, 0);
  SetLength(TermX, 0);
  SetLength(TermY, 0);
  SetLength(TermDX, 0);
  SetLength(TermDY, 0);
end;


procedure TTermitesForm.Image1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    OrthoClor := True;
    TermitesOKBtn.Enabled := True;
  end
  else if (Button = mbRight) then
  begin
    SaveDialog1.DefaultExt := GraphicExtension(TBitmap);
    SaveDialog1.Filter := GraphicFilter(TBitmap);
    if SaveDialog1.Execute then
      Image1.Picture.Bitmap.SaveToFile(SaveDialog1.Filename);
  end;
end;

end.

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
unit nAntsFarm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TAntsForm = class(TForm)
    AntsPanel: TPanel;
    Image1: TImage;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    HelpBtn: TSpeedButton;
    AntsCB: TCheckBox;
    AntsizeRG: TRadioGroup;
    AntChipPerEdit: TEdit;
    AntsPerEdit: TEdit;
    AntsEdit: TEdit;
    AntChipEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    AntsOkBtn: TButton;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ReallyClose;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure CancelBitBtnClick(Sender: TObject);
    procedure AntsOKBtnClick(Sender: TObject);
    procedure AntyMatilda;
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
     
  public
     
  end;

var
  AntsForm: TAntsForm;
  AntiOrtho, DXOrthoClor: Boolean;
  AntX, AntY, AntDX, AntDY: array of Integer;
  AntWorld: array of array of Integer;
  AntWide, AntHeight, Ants, Antset, Chipset: Integer;

implementation
uses nUGlobal {, UAntMoundForm};
{$R *.DFM}



procedure TAntsForm.FormCreate(Sender: TObject);
begin
  top := AntsFarmY;
  left := AntsFarmX;
  AntiOrtho := True;
end;

procedure TAntsForm.ReallyClose;
begin
  AntiOrtho := True;
  SetLength(AntWorld, 0, 0);
  SetLength(AntX, 0);
  SetLength(AntY, 0);
  SetLength(AntDX, 0);
  SetLength(AntDY, 0);
  Close;
end;

procedure TAntsForm.FormCloseQuery(Sender: TObject; var CanClose:
  Boolean);
begin
  AntiOrtho := True;
  AntsFarmY := AntsForm.top;
  AntsFarmX := AntsForm.left;
{If ReallyGone then } CanClose := True;
end;

procedure TAntsForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TAntsForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(7000);
end;

procedure TAntsForm.CancelBitBtnClick(Sender: TObject);
begin
  AntiOrtho := True;
  AntsOkBtn.Enabled := True;
end;

procedure TAntsForm.AntsOKBtnClick(Sender: TObject);
var
  OKtogo: Boolean;
  ChipPerset, AntsPer, CodeVx: Integer;
  CheckString: string;
begin
  OKtogo := True;
{Translate the Edits into Variables}
  val(AntsEdit.Text, Ants, CodeVx);
  if (CodeVx > 0) then
  begin OKtogo := False; Codefx(AntsEdit.Text, CodeVx); end;
  if Ants = 0 then Ants := 1;
  val(AntsPerEdit.Text, AntsPer, CodeVx);
  if (CodeVx > 0) then
  begin OKtogo := False; Codefx(AntsPerEdit.Text, CodeVx); end;
  if AntsPer = 0 then AntsPer := 1
  else if AntsPer > 99 then AntsPer := 99;
  val(AntChipEdit.Text, Chipset, CodeVx);
  if (CodeVx > 0) then
  begin OKtogo := False; Codefx(AntChipEdit.Text, CodeVx); end;
  if Chipset = 0 then Chipset := 1;
  val(AntChipPerEdit.Text, ChipPerset, CodeVx);
  if (CodeVx > 0) then
  begin OKtogo := False; Codefx(AntChipPerEdit.Text, CodeVx); end;
  if ChipPerset = 0 then ChipPerset := 1
  else if ChipPerset > 99 then ChipPerset := 99;
  if OKtogo then
  begin
    case AntsizeRG.ItemIndex of
      0: begin
          AntWide := 148; {pixels}
          AntHeight := 228; {pixels}
        end;
      1: begin
          AntWide := 540; {540 pixels}
          AntHeight := 452; {452}
        end;
      2: begin
          AntWide := 135; {639}
          AntHeight := 113; {480}
        end;
      3: begin
          AntWide := 176; {799}
          AntHeight := 141; {600}
        end;

    end; {case}
    if (Ants > ((AntWide * AntHeight) div Round(100 / AntsPer)))
      then Ants := ((AntWide * AntHeight) div Round(100 / AntsPer));
    str(Ants, CheckString);
    AntsEdit.Text := CheckString;
    if (Chipset > ((AntWide * AntHeight) div Round(100 / ChipPerset)))
      then
        Chipset := ((AntWide * AntHeight) div Round(100 / ChipPerset));
    str(Chipset, CheckString);
    AntChipEdit.Text := CheckString;
    AntsOkBtn.Enabled := False;
    AntyMatilda;
  end else ShowMessage('Input parameters dry rotted');
{OKtogo}
end;

procedure TAntsForm.AntyMatilda;
var TDX, TDY,
  TempX, TempY, Planting, I, J: Integer;
  ChipSetter: Double;
  Antstring3, Antstring2, Antstring, CheckString: string;
  NewRect: TRect;
  Bitmap: TBitmap;
begin
{AntWide,AntHeight, Ants,Chipset}
  if (AntHeight < 144) then
  begin
     {Enlarge form to display}
    AntsForm.Left := 0;
    AntsForm.Top := 0;
    AntsForm.Width := (AntWide * 4) + 90 + 11 + 4;
    AntsForm.Height := (AntHeight * 4) + 28 + 4;
    AntsPanel.Left := 90;
    AntsPanel.Top := 0;
    AntsPanel.Width := (AntWide * 4) + 4;
    AntsPanel.Height := (AntHeight * 4) + 4;
    Image1.Left := 1;
    Image1.Top := 1;
    Image1.Width := (AntWide * 4) + 1; {391 401}
    Image1.Height := (AntHeight * 4) + 1; {241 268}
    Bitmap := TBitmap.Create;
    Bitmap.Width := (AntWide * 4) + 1; {393}
    Bitmap.Height := (AntHeight * 4) + 1; {241}
    Image1.Picture.Graphic := Bitmap;
  end
  else if (AntHeight > 444) then
  begin
    AntsForm.Left := 0;
    AntsForm.Top := 0;
    AntsForm.Width := (AntWide) + 90 + 11 + 4;
    AntsForm.Height := (AntHeight) + 28 + 4;
    AntsPanel.Left := 90;
    AntsPanel.Top := 0;
    AntsPanel.Width := (AntWide) + 4;
    AntsPanel.Height := (AntHeight) + 4;
    Image1.Left := 1;
    Image1.Top := 1;
    Image1.Width := (AntWide); {391 401}
    Image1.Height := (AntHeight);
    Bitmap := TBitmap.Create;
    Bitmap.Width := (AntWide); {393}
    Bitmap.Height := (AntHeight); {241}
    Image1.Picture.Graphic := Bitmap;
  end else
  begin
    AntsForm.Left := AntsFarmX;
    AntsForm.Top := AntsFarmY;
    AntsForm.Width := 250;
    AntsForm.Height := 258;
    AntsPanel.Left := 90;
    AntsPanel.Top := 0; {148 228}
    AntsPanel.Width := (AntWide) + 2;
    AntsPanel.Height := (AntHeight) + 2;
    Image1.Left := 1;
    Image1.Top := 1;
    Image1.Width := (AntWide); {391 401}
    Image1.Height := (AntHeight);
    Bitmap := TBitmap.Create;
    Bitmap.Width := (AntWide); {393}
    Bitmap.Height := (AntHeight); {241}
    Image1.Picture.Graphic := Bitmap;
  end;
  Application.ProcessMessages;
  SetLength(AntWorld, AntWide, AntHeight);
  SetLength(AntX, Ants);
  SetLength(AntY, Ants);
  SetLength(AntDX, Ants);
  SetLength(AntDY, Ants);
  Randomize;
  AntiOrtho := False;
  ChipSetter := ChipSet;
  ChipSetter := (ChipSetter / 100);
  for I := 0 to AntWide - 1 do
    for J := 0 to AntHeight - 1 do begin
      if (Random < (ChipSetter / 2)) then AntWorld[I, J] := 2
      else if (Random < ChipSetter) then AntWorld[I, J] := 1
      else AntWorld[I, J] := 0;
      if (AntHeight < 144) then begin
         {USE other form to display}
        if (AntWorld[I, J] = 0) then
{         AntMoundForm.} Image1.Canvas.Brush.Color := ClWhite
        else
{         AntMoundForm.} Image1.Canvas.Brush.Color := Full;
        NewRect := Rect((I * 4), (J * 4), ((I * 4) + 4), ((J * 4) +
          4));
{         AntMoundForm.} Image1.Canvas.FillRect(NewRect);
      end else begin
        if (AntWorld[I, J] = 0) then
          Image1.Canvas.Pixels[I, J] := ClWhite
        else Image1.Canvas.Pixels[I, J] := Full;
      end;
    end;
  for I := 0 to Ants - 1 do
  begin
    AntX[I] := Random(AntWide - 1);
    AntY[I] := Random(AntHeight - 1);
    AntDX[I] := Random(3) - 1;
    AntDY[I] := Random(3) - 1;
  end;
  Application.ProcessMessages;

{Langton also created creatures called Vants, or virtual ants.
These were V-shaped constructs that moved in the direction of
their points. If the lead cell moved into a blank square on an
imaginary grid, the vant continued moving in that direction.
If the square was blue,
the vant turned right and changed the colour of the cell to yellow.
If the square was yellow,
the vant turned left and changed the colour of the square to blue.
Thus the vant left a trail behind it.}
  repeat
    Application.ProcessMessages;
    for I := 0 to Ants - 1 do begin
{ Save the current position and take a new step.}
      TempX := AntX[I] + AntDX[I];
      TempY := AntY[I] + AntDY[I];
      if TempX > AntWide - 1 then TempX := 0 else
        if TempX < 0 then TempX := AntWide - 1;
      if TempY > AntHeight - 1 then TempY := 0 else
        if TempY < 0 then TempY := AntHeight - 1;
{Change old spot}
      if (AntWorld[AntX[I], AntY[I]] = 1) then
      begin
        AntWorld[AntX[I], AntY[I]] := 2;
        if (AntHeight < 144) then
        begin
          Image1.Canvas.Brush.Color := Growing;
          NewRect := Rect((AntX[I] * 4), (AntY[I] * 4),
            ((AntX[I] * 4) + 4), ((AntY[I] * 4) + 4));
          Image1.Canvas.FillRect(NewRect);
        end else
          Image1.Canvas.Pixels[AntX[I], AntY[I]] := Growing;
            {Desolate;}
      end else
        if (AntWorld[AntX[I], AntY[I]] = 2) then
        begin
          AntWorld[AntX[I], AntY[I]] := 1;
          if (AntHeight < 144) then
          begin
            Image1.Canvas.Brush.Color := clBlue;
            NewRect := Rect((AntX[I] * 4), (AntY[I] * 4),
              ((AntX[I] * 4) + 4), ((AntY[I] * 4) + 4));
            Image1.Canvas.FillRect(NewRect);
          end else
            Image1.Canvas.Pixels[AntX[I], AntY[I]] := clBlue;
              {Desolate;}
        end else
        begin
          AntWorld[AntX[I], AntY[I]] := 0;
          if (AntHeight < 144) then
          begin
            Image1.Canvas.Brush.Color := clWhite;
            NewRect := Rect((AntX[I] * 4), (AntY[I] * 4),
              ((AntX[I] * 4) + 4), ((AntY[I] * 4) + 4));
            Image1.Canvas.FillRect(NewRect);
          end else
            Image1.Canvas.Pixels[AntX[I], AntY[I]] := clWhite;
        end;
{Application.ProcessMessages;}
{Turn depending on new loco location}
      if (AntWorld[TempX, TempY] = 1) then
      begin {turn right}
        if ((AntDX[I] = -1) and (AntDY[I] = 1)) then
        begin AntDX[I] := 0; AntDY[I] := 1; end else
          if ((AntDX[I] = 0) and (AntDY[I] = 1)) then
          begin AntDX[I] := 1; AntDY[I] := 1; end else
            if ((AntDX[I] = 1) and (AntDY[I] = 1)) then
            begin AntDX[I] := 1; AntDY[I] := 0; end else
              if ((AntDX[I] = 1) and (AntDY[I] = 0)) then
              begin AntDX[I] := 1; AntDY[I] := -1; end else
                if ((AntDX[I] = 1) and (AntDY[I] = -1)) then
                begin AntDX[I] := 0; AntDY[I] := -1; end else
                  if ((AntDX[I] = 0) and (AntDY[I] = -1)) then
                  begin AntDX[I] := -1; AntDY[I] := -1; end else
                    if ((AntDX[I] = -1) and (AntDY[I] = -1)) then
                    begin AntDX[I] := -1; AntDY[I] := 0; end else
                      if ((AntDX[I] = -1) and (AntDY[I] = 0)) then
                      begin AntDX[I] := -1; AntDY[I] := 1; end;
      end else if (AntWorld[TempX, TempY] = 2) then
      begin {turn left}
        if ((AntDX[I] = -1) and (AntDY[I] = 1)) then
        begin AntDX[I] := -1; AntDY[I] := 0; end else
          if ((AntDX[I] = 0) and (AntDY[I] = 1)) then
          begin AntDX[I] := -1; AntDY[I] := 1; end else
            if ((AntDX[I] = 1) and (AntDY[I] = 1)) then
            begin AntDX[I] := 0; AntDY[I] := 1; end else
              if ((AntDX[I] = 1) and (AntDY[I] = 0)) then
              begin AntDX[I] := 1; AntDY[I] := 1; end else
                if ((AntDX[I] = 1) and (AntDY[I] = -1)) then
                begin AntDX[I] := 1; AntDY[I] := 0; end else
                  if ((AntDX[I] = 0) and (AntDY[I] = -1)) then
                  begin AntDX[I] := 1; AntDY[I] := -1; end else
                    if ((AntDX[I] = -1) and (AntDY[I] = -1)) then
                    begin AntDX[I] := 0; AntDY[I] := -1; end else
                      if ((AntDX[I] = -1) and (AntDY[I] = 0)) then
                      begin AntDX[I] := -1; AntDY[I] := -1; end;
      end;
{move ant to new location}
      AntX[I] := TempX;
      AntY[I] := TempY;
      if AntsCB.Checked then
      begin
        if (AntHeight < 144) then
        begin
          Image1.Canvas.Brush.Color := OverPopulated;
          NewRect := Rect((AntX[I] * 4), (AntY[I] * 4),
            ((AntX[I] * 4) + 4), ((AntY[I] * 4) + 4));
          Image1.Canvas.FillRect(NewRect);
        end else
          Image1.Canvas.Pixels[AntX[I], AntY[I]] := OverPopulated;
      end;
{Application.ProcessMessages;}
    end;
  until (AntiOrtho = True);
  SetLength(AntWorld, 0, 0);
  SetLength(AntX, 0);
  SetLength(AntY, 0);
  SetLength(AntDX, 0);
  SetLength(AntDY, 0);
end;


procedure TAntsForm.Image1MouseUp(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then begin
    AntiOrtho := True;
    AntsOkBtn.Enabled := True;
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

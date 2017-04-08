{Mandel / Julia Explorer
Copyright 2000 Hugh Allen
Hugh.Allen@oz.quest.com
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
Phoenix added by Ivan Lee Herring Copyright 2001}

unit fjul;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,Buttons, ComCtrls,
   fastDIB, Fractal,  fPhoenix;

type
  TFMJForm = class(TForm)
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    edtPalette: TEdit;
    edtCx: TEdit;
    edtCy: TEdit;
    edtMaxIters: TEdit;
    Label5: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    frcMandel: TFractal;
    frcJulia: TFractal;
    Label7: TLabel;
    Label6: TLabel;
    Label4: TLabel;
    FPManDemBtn: TSpeedButton;
    FPJuliaDemBtn: TSpeedButton;
    MJSave: TSpeedButton;
    FPHelpBtn: TSpeedButton;
    JuliaClipEdit: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    PaletteEdit: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    EggCy: TEdit;
    EggCx: TEdit;
    EggDisplay: TSpeedButton;
    EggSaver: TSpeedButton;
    PHelpBtn: TSpeedButton;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Egg: TPhoenix;
    EggItersEdit: TEdit;
    Phimagery: TImage;
    ProgressBar1: TProgressBar;

    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FPHelpBtnClick(Sender: TObject);
    procedure PHelpBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure frcMandelMouseDown(Sender: TObject;
    Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure frcMandelMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
    procedure PhimageryMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PhimageryMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
procedure PhimageryPaint;

    procedure frcMandelPaint(Sender: TObject);
    procedure edtPaletteKeyPress(Sender: TObject; var Key: Char);
    procedure PaletteEditKeyPress(Sender: TObject; var Key: Char);
    procedure edtCyKeyPress(Sender: TObject; var Key: Char);
    procedure EggCxKeyPress(Sender: TObject; var Key: Char);
    procedure edtMaxItersKeyPress(Sender: TObject; var Key: Char);
    procedure EggItersEditKeyPress(Sender: TObject; var Key: Char);

    procedure FPManDemBtnClick(Sender: TObject);
    procedure FPJuliaDemBtnClick(Sender: TObject);
    procedure MJSaveClick(Sender: TObject);
procedure CplxSqrtFJUL(a, b: Real; out x, y: Real);
procedure Render(JIsJulia:Boolean);
    procedure EggDisplayClick(Sender: TObject);
    procedure EggSaverClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMJForm: TFMJForm;

implementation

{$R *.DFM}
uses
  fUGlobal,
  {fGlForm,}
  fXYZ3D,
  FMath,
  fmain,
  fGlfrm;

var {ManJuliaSet,}
JuliaClip:Integer;
isEggLaidDem, BitmapDisplayed, isJuliaLaidDem:Boolean;
PhBitmap:TBitmap;

procedure TFMJForm.FormCreate(Sender: TObject);
begin
  top := FractalPreviewFormY;{0;}
  left := FractalPreviewFormX;{488;}
  frcJulia.Cx := 0.359171;
  frcJulia.Cy := 0.109141;
  edtCx.Text := Format('%1.8f', [0.359171]);
  edtCy.Text := Format('%1.8f', [0.109141]);
  Egg.Cx := 0.1953;
  Egg.Cy := 0.2004;
  EggCx.Text := Format('%1.8f', [0.1953]);
  EggCy.Text := Format('%1.8f', [0.2001]);
isJuliaLaidDem:=False;
BitmapDisplayed:=False;
JuliaClip:=77;
Max_Iterations:= 200;
end;
procedure TFMJForm.FormResize(Sender: TObject);
begin
width:=540;
height:=371;
end;

procedure TFMJForm.FPHelpBtnClick(Sender: TObject);
begin  {1001}
  Application.HelpContext(1001);
end;
procedure TFMJForm.PHelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(1002);
end;
procedure TFMJForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
{If BitmapDisplayed then PhBitmap.Free;}
  FractalPreviewFormX := FMJForm.left;
  FractalPreviewFormY := FMJForm.top;
  DoSaver;
end;


procedure TFMJForm.frcMandelMouseDown(Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  f: TFractal;
  rx, ry: real;
  z: real;
begin
  if Shift = [ssLeft, ssShift] then
    z := 0.5
  else
  if Shift = [ssRight, ssShift] then
    z := -1.0
  else
  begin
    if Sender = frcMandel then
      frcMandelMouseMove(Sender, Shift, X, Y);
    exit;
  end;
  {If shifted then Zoom else pass to the egg}
  f := (Sender as TFractal);
  rx := f.ViewLeft + (x * f.ViewWidth  / f.Width);
  ry := f.ViewTop  + (y * f.ViewHeight / f.Height);
  f.ViewLeft  := f.ViewLeft + z * (rx - f.ViewLeft);
  f.ViewWidth := f.ViewWidth * (1 - z);
  f.ViewTop   := f.ViewTop  + z * (ry - f.ViewTop);
  f.ViewHeight := f.ViewHeight * (1 - z);
  f.SetChanged;
end;
procedure TFMJForm.frcMandelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  rx, ry: real;
begin
  if Shift = [ssLeft] then
  begin
    rx :=
        frcMandel.ViewLeft +
        (x * frcMandel.ViewWidth
        / frcMandel.Width);
    ry :=
        frcMandel.ViewTop  +
        ((frcMandel.Height-y) * frcMandel.ViewHeight
        / frcMandel.Height);
    frcJulia.Cx := rx;
    frcJulia.Cy := ry;
    edtCx.Text := Format('%1.8f', [rx]);
    edtCy.Text := Format('%1.8f', [ry]);
    frcJulia.Repaint;
    frcMandel.Invalidate;
  end;
end;


procedure TFMJForm.PhimageryMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rx, ry: real;
  z: real;
begin
  if Shift = [ssLeft, ssShift] then
    z := 0.5
  else
  if Shift = [ssRight, ssShift] then
    z := -1.0
  else
  begin
    if Sender = Phimagery then
     PhimageryMouseMove(Sender, Shift, X, Y);
    exit;
  end;
  {If shifted then Zoom else pass to the egg}
{  f := (Sender as TFractal);}
{  rx := f.ViewLeft + x * f.ViewWidth  / f.Width;}
    rx := FXMin + (x * (FXMax) / Filesizex);
{  ry := f.ViewTop  + y * f.ViewHeight / f.Height;}
                  {Flip to correct  Image upside down}
    ry := FYMin + ((Filesizey-y) * (FYMax) / Filesizey);
  FXMin  := FXMin + z * (rx - FXMin);
  FXMax := FXMax * (1 - z);
  FYMin   := FYMin  + z * (ry - FYMin);
  FYMax := FYMax * (1 - z);
{  f.SetChanged;}
  If BitmapDisplayed then
{      PhimageryMouseMove(Sender, Shift, X, Y) else}
      PhimageryPaint;
end;

{Shove the coordinates to the Egg}
procedure TFMJForm.PhimageryMouseMove(Sender: TObject;
  Shift: TShiftState;
  X, Y: Integer);
var
  rx, ry: real;
begin
  if Shift = [ssLeft] then
  begin
{    rx := -2.7+ (x * 4.0 / 256);}
    rx := FXMin + (x * (FXMax) / Filesizex);
{Phoenix1.ViewLeft + x * Phoenix1.ViewWidth  / Phoenix1.Width;}
{  FXMax: Extended;  FXMin: Extended;
    FYMax: Extended;  FYMin: Extended;}
{    ry := -2.0+ (y * 4.0 / 256);}
    ry := FYMin + ((Filesizey-y) * (FYMax) / Filesizey);
{Phoenix1.ViewTop  + y * Phoenix1.ViewHeight / Phoenix1.Height;}
  FHQ:= rx;
  FVP:= ry;
    Egg.Cx := rx;
    Egg.Cy := ry;
    EggCx.Text := Format('%1.8f', [rx]);
    EggCy.Text := Format('%1.8f', [ry]);
    Egg.Repaint;
{Egg
  ViewLeft   := -2.30;
  ViewWidth  :=  2.5;
  ViewTop    := -2.30;
  ViewHeight :=  2.5;}
(*
var
  f: TFractal;
  c: TCanvas;
  x,y: integer;
begin
  f := (Sender as TFractal);
  c := f.Canvas;
  c.Pen.Color := clWhite;
  x := f.ViewToPixelX(frcJulia.Cx);
  y := f.ViewToPixelY(frcJulia.Cy);
  c.MoveTo(x - 3, y - 3);
  c.LineTo(x + 4, y + 4);{Lineto does NOT draw Last point of line}
  c.MoveTo(x - 3, y + 3);
  c.LineTo(x + 4, y - 4);
*)
  end;
  If (not BitmapDisplayed) then
  begin                 {one time initialize Phoenix}
          FXMin := -2.7;
          FXMax := 4.0; {1.3;}
          FYMin := -2.0;
          FYMax := 4.0; {2.0;}
  PhimageryPaint;
  end;
end;

procedure TFMJForm.PhimageryPaint;
var
Pixelx, maxcolx, maxrowy, FMcolor, rowy, colx: integer;
Yi, Xi, Xisquare, Xitemp, Xtemp,  X, Y, Xsquare,
 deltaP, deltaQ, P: Extended;
  Q: array[0..302] of Extended;
  PixelLine: PByteArray;
  Begin {Display the Phoenix}
BitmapDisplayed:=True;
PhBitmap:= TBitmap.create;
    PhBitmap.PixelFormat := MyPixelFormat;
    FileSizeX:=256;
    FileSizeY:=256;
    PhBitmap.Width := FileSizeX; { assign the initial width... }
    PhBitmap.Height := FileSizeY; { ...and the initial height }
    maxcolx :=(FileSizeX - 1);
    maxrowy := (FileSizeY - 1);

    deltaP := (FXMax ){4} / maxcolx;
    deltaQ := (FYMax ) {4}/ maxrowy;
    Q[0] := (deltaQ*FileSizeY) +FYMin;
    for rowy := 1 to maxrowy do Q[rowy] := Q[rowy - 1] - deltaQ;
    P := (FXMin + (0 * deltaP));
    for colx := 0 to maxcolx do begin
      for rowy := 0 to maxrowy do begin
        PixelLine := PhBitmap.ScanLine[rowy];
        Y := 0;
        Yi := 0;
        X := 0;
        Xi := 0;
        FMcolor := 0;
        Xsquare := 0;
        Xisquare := 0;
        while (FMcolor < Max_Iterations) and
          (Xsquare + Xisquare < 4.0) do
        begin
          Xsquare := X * X;
          Xisquare := Xi * Xi;
          Xtemp := Xsquare - Xisquare + P
            + Q[rowy] * Y;
          Xitemp := 2 * X * Xi + Q[rowy] * Yi;
          Y := X;
          Yi := Xi;
          X := Xtemp;
          Xi := Xitemp;
          inc(FMcolor);
        end;
       {place the pixel   Color_Option}
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
            PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
            PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
      end; { of row }
      P := P + deltaP;
    end; { of col }
    Phimagery.Picture.Bitmap.Assign(PhBitmap);
    Phimagery.canvas.draw(0, 0,
      Phimagery.Picture.Bitmap);
    PhBitmap.free;
end;

procedure TFMJForm.frcMandelPaint(Sender: TObject);
var
  f: TFractal;
  c: TCanvas;
  x,y: integer;
begin
  f := (Sender as TFractal);
  c := f.Canvas;
  c.Pen.Color := clWhite;
  x := f.ViewToPixelX(frcJulia.Cx);
  y := frcMandel.Height-f.ViewToPixelY(frcJulia.Cy);
  c.MoveTo(x - 3, y - 3);
  c.LineTo(x + 4, y + 4);{Lineto does NOT draw Last point of line}
  c.MoveTo(x - 3, y + 3);
  c.LineTo(x + 4, y - 4);
end;

procedure TFMJForm.edtPaletteKeyPress(Sender: TObject; var Key: Char);
var
  i: Longint;
begin
  if Key = #13 then
  begin
    i := StrToInt(edtPalette.Text);
    frcMandel.MakePalette(i);
    frcMandel.Invalidate;
    frcJulia.MakePalette(i);
    frcJulia.Invalidate;
    Key := #0;
  end;
end;
procedure TFMJForm.PaletteEditKeyPress(Sender: TObject; var Key: Char);
var
  i: Longint;
begin
  if Key = #13 then
  begin
    i := StrToInt(PaletteEdit.Text);
{    Phoenix1.MakePalette(i);
    Phoenix1.Invalidate;PhimageryPaint;}
    Egg.MakePalette(i);
    Egg.Invalidate;
    Key := #0;
  end;
end;

procedure TFMJForm.edtCyKeyPress(Sender: TObject; var Key: Char);
var
  i: Integer;
  r: Real;
begin
  if Key = #13 then
  begin
    Val((Sender as TEdit).Text, r, i);
    if Sender = edtCx then
      frcJulia.Cx := r
    else
      frcJulia.Cy := r;
    frcMandel.Invalidate;
    Key := #0;
  end;
end;
procedure TFMJForm.EggCxKeyPress(Sender: TObject; var Key: Char);
var
  i: Integer;
  r: Real;
begin
  if Key = #13 then
  begin
    Val((Sender as TEdit).Text, r, i);
    if Sender = EggCx then
    begin
      Egg.Cx := r;
      FHQ:= r;
    end
    else
    begin
      Egg.Cy := r;
      FVP:= r;
    end;
{    FHQ: Extended;  FVP: Extended;  Phoenix1.Invalidate;}
{    PhimageryPaint;}
    Key := #0;
  end;
end;


procedure TFMJForm.edtMaxItersKeyPress(Sender: TObject; var Key: Char);
var
  i: Longint;
begin
  if Key = #13 then
  begin
    i := StrToInt(edtMaxIters.Text);
    if (i<10) or (i>255) then
    begin
    DoMessages(21);
{      showmessage('Must be between 10 and 255');}
      exit;
    end;
    frcMandel.MaxIterations := i;
    frcJulia.MaxIterations := i;
    Key := #0;
  end;
end;
procedure TFMJForm.EggItersEditKeyPress(Sender: TObject; var Key: Char);
var
  i: Longint;
begin
  if Key = #13 then
  begin
    i := StrToInt(EggItersEdit.Text);
    if (i<10) or (i>255) then
    begin
    DoMessages(21);
      exit;
    end;
    Egg.MaxIterations := i;
    Max_Iterations:= i;
    PhimageryPaint;
    Key := #0;
  end;
end;

procedure TFMJForm.FPManDemBtnClick(Sender: TObject);
begin
{Use selected Coordinates to compute DEM data
  ViewLeft   := -2.0;
  ViewTop    := -2.0;
  ViewWidth  :=  4.0;
  ViewHeight :=  4.0;
SendToJulia(StrToFloat(edtCx.Text),StrToFloat(edtCy.Text),
StrToInt(edtMaxIters.Text));
Manmatrix Dem Rcd Flm Deh
{SET ALL required data }
FileSizeX:=256;
FileSizeY:=256;
NullDemValue:=0;
CellSizeX:=30;
CellSizeY:=30;
 DemiLeftX1e:= frcMandel.ViewLeft;      {frcJulia.}
 DemiTopY1e:= frcMandel.ViewTop;
 DemiRightX2e:= frcMandel.ViewLeft  + frcMandel.ViewWidth;
 DemiBottomY2e:= frcMandel.ViewTop  + frcMandel.ViewHeight ;
Render(False);{Is Julia.. or Mandel.. sets the matrix}
isJuliaLaidDem:=True; {ok to save}
{Send Dem to Fractal GL form}
{         XYZGL.FormShowDown;
         XYZGL.show;}

{    Phimagery.Picture.Bitmap.Assign(PhBitmap);
    Phimagery.canvas.draw(0, 0,
      Phimagery.Picture.Bitmap); }
dtmGlForm.Image1.Picture.Assign(Phimagery.Picture.Bitmap);
dtmGlForm.FormShowDown;
dtmGlForm.show;
end;



procedure TFMJForm.FPJuliaDemBtnClick(Sender: TObject);
begin
FileSizeX:=256;
FileSizeY:=256;
NullDemValue:=0;
CellSizeX:=frcJulia.ViewLeft * frcJulia.ViewWidth / FileSizeX;
CellSizeY:=frcJulia.ViewTop  * frcJulia.ViewHeight / FileSizeY;
 DemiLeftX1e:= frcJulia.ViewLeft;      {frcJulia.}
 DemiTopY1e:=  frcJulia.ViewTop;
 DemiRightX2e:= frcJulia.ViewLeft + frcJulia.ViewWidth{ / FileSizeX};
 DemiBottomY2e:= frcJulia.ViewTop  + frcJulia.ViewHeight{ / FileSizeY};
JuliaClip:=StrtoInt(JuliaClipEdit.Text);
{NullDemValue:=-32767;
CellSizeX:=30;
CellSizeY:=30;
DemiLeftX1e:=0; DemiTopY1e:=0;
DemiRightX2e:=FileSizeX*CellSizeX;
DemiBottomY2e:=FileSizeY*CellSizeY;}
Render(True);{Is Julia.. or Mandel.. sets the matrix}
isJuliaLaidDem:=True; {ok to save}
{Send Dem to Fractal GL form}
{         XYZGL.FormShowDown;
         XYZGL.show;}
{dtmGlForm.Image1.Picture.Assign(Phimagery.Picture.Bitmap);}
dtmGlForm.FormShowDown;
dtmGlForm.show;
end;


procedure TFMJForm.MJSaveClick(Sender: TObject);
var MyFilesS: string;
 maxcolx, maxrowy,  XCount, YCount : Integer;
  F_File: file of Smallint;
begin
{Save Mandelbrot data as a DEM   Save Julia data as a DEM
isJuliaLaidDem}
If isJuliaLaidDem then
Begin
{Save the Manmatrix DEM ..whatever-both-either
is already SET : Dem Rcd Flm Deh }
  SaveDialog1.Filter := 'Fractal Mountain files|*.bin;*.flm';
  SaveDialog1.Filename := '*.flm';
  SaveDialog1.InitialDir:=DemsDir;
  if (SaveDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(SaveDialog1.FileName);
{save DEM ?}
    MyFilesS := Uppercase(ExtractFileExt(SaveDialog1.FileName));
    if (MyFilesS = '.FLM') then
    begin
      FractalFileName := SaveDialog1.FileName;
      iMadeMountains:= 999;
  VDEM:= 0;
  Vzx:=FileSizeX;
  Vzy:=FileSizeY;
  Vx:= 0; Vy:= 0;
  Vi:= 0; Vh:= 0; Vg:= 0;
  Viki:= StrToInt(edtMaxIters.Text);
  Vre:=StrToFloat(edtCx.Text);
  Vde:=StrToFloat(edtCy.Text);
      FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
ContourInterval := 0;
Fractalgorhythym := 0;
GridOriginString:=    'NW';
CellOriginString:=  'Center';
ILorUGridString:= 'Fractal';
ProjectionString:= 'None';
UtmZoneString:= 'None';
DatumString:= 'None';
SpheroidString:= 'None';
ZunitsString:=   'Meters';
GridUnitString:=  'Grid Unit';
{FractalFileMatrix got from inside procedures}
      XYZ3DForm.WriteDehFile(ChangeFileExt(SaveDialog1.FileName,'.deh'));
      XYZ3DForm.WriteFlmFile(SaveDialog1.FileName);
{        MainForm.HintPanel.Caption := 'Saving Mountains to disk';}
        Application.ProcessMessages;
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            ProgressBar1.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
    end;
  end;
end;


procedure TFMJForm.Render(JIsJulia:Boolean);
var
  XCount2, YCount2,I, Switch,Radius2G,RadiusG4,
  MaxZDo, maxcolx, maxrowy,  XCount, YCount, TempMan : Integer;
  FJULMaxIters:Integer;
var
    RFCx, RFCy: Real;
  x, y,  j: integer;
  vx, vy: real;
  vx2: real;
  tx1, ty1: real;
  //tx2, ty2: real;
  lake: boolean;
  dx, dy: real;
  d, ld: real;
  label done;
begin
    RFCx:=StrToFloat(edtCx.Text);
    RFCy:=StrToFloat(edtCy.Text);
    FJULMaxIters:=StrToInt(edtMaxIters.Text);
{  dib.SetSize(Width, Height, 8, 0);}
    SetLength(ManMatrix, FileSizeX, FileSizeY);
  if JIsJulia then                    {IsJulia}
  begin
    // see if it has a lake
    vx := 0;
    vy := 0;
    for i := 1 to 500 do
    begin
      if (vx * vx + vy * vy) > 4.0 then
        break;
      vx2 := vx * vx - vy * vy + RFCx;
      vy :=  vx * vy * 2.0 + RFCy;
      vx := vx2;
    end;
    lake := i > 500;
    tx1 := vx; ty1 := vy;   // trap point
  end
  else
    lake := false;

  for y := 0 to FileSizeY{Height} - 1 do
    for x := 0 to FileSizeX{Width} - 1 do
    begin
      j := 255;
      if JIsJulia then
      begin
        vx := frcJulia.PixelToViewX(x);
        vy := frcJulia.PixelToViewY(y);
        ld := 0.0;
        for i := 0 to FJULMaxIters do            {FMaxIters}
        begin
          if (vx * vx + vy * vy) > 4.0 then
          begin
            j := i;
            break;
          end;
          vx2 := vx * vx - vy * vy + RFCx;
          vy :=  vx * vy * 2.0 + RFCy;
          vx := vx2;
          if lake and (i > 9) then
          begin
            dx := vx - tx1;
            dy := vy - ty1;
            d := (dx * dx + dy * dy);
            if d < 0.001 then
            begin
              if d < ld then
              begin
                j := 255;//4 - 4*i;
                break;
              end;
              ld := d * 0.999;
            end
            else
              ld := ld * 0.99;
          end;
        end;
      end
      else // Mandelbrot
      begin
        vx := 0;
        vy := 0;
        RFCx := frcMandel.PixelToViewX(x);
        RFCy := frcMandel.PixelToViewY(y);

        // is point in main cardioid?
        // calculate stable fixed point
        tx1 := 1 - 4 * RFCx;
        ty1 := 0 - 4 * RFCy;
        CplxSqrtFJUL(tx1, ty1, tx1, ty1);
        //tx2 := 0.5 + 0.5 * tx1;  this one not used because
        //ty2 := 0.0 + 0.5 * ty1;  it's not a stable fixed-point
        //tx1 := 0.5 - 0.5 * tx1;
        //ty1 := 0.0 - 0.5 * ty1;
        tx1 := 1.0 - tx1;  // doubling it
        //ty1 :=     - ty1; // not needed; it'll be squared
        d := (tx1 * tx1 + ty1 * ty1);
        // square of magnitude of derivative at fixed-point
        if d < 1.0 then
        begin
          j := 255;
          goto done;
        end;

        // is point in circle to left of main cardioid?
        dx := RFCx + 1.0;
        d := dx * dx + RFCy * RFCy;
        if d < 0.0625 then
        begin
          j := 255;
          goto done;
        end;

        for i := 0 to FJULMaxIters do
        begin
          if (vx * vx + vy * vy) > 4.0 then
          begin
            j := i;
            break;
          end;
          vx2 := vx * vx - vy * vy + RFCx;
          vy :=  vx * vy * 2.0 + RFCy;
          vx := vx2;
        end;
      end;
      done: {ManMatrix[x, y] := j;}
      begin
        if JIsJulia then
        begin
{        If j < JuliaClip then j:=JuliaClip; }
{        If j < 0 then j:=0;}
        j:= j div JuliaClip;
        ManMatrix[x, y] :=  j;
        {Invert so Mass is lake}
{        If j > JuliaClip then j:=j-JuliaClip;
        If j < 0 then j:=0;
        ManMatrix[x, y] :=  j;}
        end
        else ManMatrix[x, y] := j;
      end;

end;
{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            ProgressBar1.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
{Create the bitmap here?}                
            end;
          end;

{  if JIsJulia then
  begin
          j:=  (MinimumElevation);
          for Ycount := 0 to maxrowy do
          begin
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              ManMatrix[Xcount, Ycount] := TempMan- j;
            end;
          end;
          MinimumElevation:=0;
          MaximumElevation := MaximumElevation-j ;

   end;       }

ProgressBar1.Position :=0;
Application.ProcessMessages;
end;

procedure TFMJForm.CplxSqrtFJUL(a, b: Real; out x, y: Real);
var
  r: Real;
begin
  r := sqrt(a*a + b*b);
  x := sqrt(0.5 * (r + a));
  if b < 0.0 then
    y := -sqrt(0.5 * (r - a))
  else
    y := sqrt(0.5 * (r - a));
end;



procedure TFMJForm.EggDisplayClick(Sender: TObject);
var
Pixelx,    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
    Xi, Yi, Xisquare, Xitemp, deltaXi,deltaX,{deltaP, deltaQ,}
KeepFXMax,KeepFYMax:Extended;
    Xtemp,X, Y,Xsquare, Q, P: Extended;
{  Qarray: array[0..302] of Extended;}
  Bitmap: TBitmap;
  PixelLine: PByteArray;
begin
isEggLaidDem:=True;
SetLength(ManMatrix, FYImageX,FYImageY);
FileSizeX:=FYImageX;
FileSizeY:=FYImageY;
KeepFXMax:=FXMax;
KeepFYMax:=FYMax;
FXMax := FXMin + (Filesizex * (FXMax / Filesizex));
FYMax := FYMin + (FilesizeY * (FYMax / FilesizeY));
      maxcolx := (FYImageX - 1);
      maxrowy := (FYImageY - 1);
    Bitmap := TBitmap.create;
    try
      Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
      Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
        {   FYImageX:= 640;FYImageY:= 480;}
      Bitmap.Width := FYImageX; { assign the initial width... }
      Bitmap.Height := FYImageY;
      P := FVP;{FVP;}
      Q := FHQ;{FHQ;}
      deltaX := (FYMax - FYMin) / (maxrowy);
      deltaXi := (FXMax - FXMin) / (maxcolx);
      for colx := 0 to maxcolx do begin
        ProgressBar1.Position :=
        Round((colx / (FYImageX - 1)) * 100);
        Application.ProcessMessages;
        for rowy := 0 to maxrowy do begin
          PixelLine := Bitmap.ScanLine[rowy];
          Y := 0;
          Yi := 0;
          X := FYMax - rowy * deltaX;
          Xi := FXMin + colx * deltaXi;
          Xsquare := 0;
          Xisquare := 0;
          FMcolor := 0;
          while (FMcolor < Max_Iterations) and
            ((Xsquare + Xisquare)< 4.0)
              do
          begin
            Xsquare := X * X;
            Xisquare := Xi * Xi;
            Xtemp := Xsquare - Xisquare + P + Q * Y;
            Xitemp := 2 * X * Xi + Q * Yi;
            Y := X;
            Yi := Xi;
            X := Xtemp;
            Xi := Xitemp;
            inc(FMcolor);
          end;
          ManMatrix[colx,rowy]:=FMcolor;
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
              PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
              PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
        end;
        end;{col}
      MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
      MainForm.Image2.canvas.draw(0, 0,
        MainForm.Image2.Picture.Bitmap);
dtmGlForm.Image1.Picture.Assign(Bitmap);
dtmGlForm.Image2.Picture.Assign(Bitmap);
dtmGlForm.FormShowDown;
dtmGlForm.show;
    finally
      Bitmap.free;
    end;
Application.ProcessMessages;
FXMax:=KeepFXMax;
FYMax:=KeepFYMax;
ProgressBar1.Position :=0;
end;
{Save the data to matrix}


procedure TFMJForm.EggSaverClick(Sender: TObject);
var MyFilesS: string;
 maxcolx, maxrowy,  XCount, YCount : Integer;
  F_File: file of Smallint;
begin
{Save Egg data as a DEM  }
If isEggLaidDem then
Begin
{Save the Manmatrix DEM ..whatever-both-either
is already SET : Dem Rcd Flm Deh }
  SaveDialog1.Filter := 'Egg files|*.bin;*.flm';
  SaveDialog1.Filename := '*.flm';
  SaveDialog1.InitialDir:=DemsDir;
  if (SaveDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(SaveDialog1.FileName);
{save DEM ?}
    MyFilesS := Uppercase(ExtractFileExt(SaveDialog1.FileName));
    if (MyFilesS = '.FLM') then
    begin
      FractalFileName := SaveDialog1.FileName;
      iMadeMountains:= 99;
  VDEM:= 0;
  Vzx:=FileSizeX;
  Vzy:=FileSizeY;
  Vx:= 0; Vy:= 0;
  Vi:= 0; Vh:= 0; Vg:= 0;
  Viki:= StrToInt(EggItersEdit.Text);
  Vre:=StrToFloat(EggCx.Text);
  Vde:=StrToFloat(EggCy.Text);
      FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
ContourInterval := 0;
Fractalgorhythym := 0;
GridOriginString:=    'NW';
CellOriginString:=  'Center';
ILorUGridString:= 'Phoenix Egg';
ProjectionString:= 'None';
UtmZoneString:= 'None';
DatumString:= 'None';
SpheroidString:= 'None';
ZunitsString:=   'Meters';
GridUnitString:=  'Grid Unit';
{FractalFileMatrix got from inside procedures}
      XYZ3DForm.WriteDehFile(ChangeFileExt(SaveDialog1.FileName,'.deh'));
      XYZ3DForm.WriteFlmFile(SaveDialog1.FileName);
{        MainForm.HintPanel.Caption := 'Saving Mountains to disk';}
        Application.ProcessMessages;
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            ProgressBar1.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
    end;
  end;
end;


end.

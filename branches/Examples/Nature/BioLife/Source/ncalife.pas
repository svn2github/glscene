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
unit ncalife;
{28Mar00           9apr          14 Nov00  DX and Delphi Libraries
4180 lines         6386           55561    56086
455868 code        497088         278208   275572
26021 Data         26037          30349    10717 ... changed Array to Dynamics
16384 Stack
535552 File size   579072        429056    430080
}
interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ColorGrd, Mask, UFmxUtils,
  Buttons, Menus, ComCtrls, ExtCtrls;

type
  TLifeForm = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PopupMenu1: TPopupMenu;
    LargeLife: TMenuItem;
    MediumLife: TMenuItem;
    FullSmall: TMenuItem;
    Torus1: TMenuItem;
    Slow1: TMenuItem;
    N2: TMenuItem;
    FullPixel: TMenuItem;
    ColorMyWorld: TMenuItem;
    N4: TMenuItem;
    Walled1: TMenuItem;
    Open1: TMenuItem;
    QUIT1: TMenuItem;
    N3: TMenuItem;
    FullScreenPixels: TMenuItem;
    Alien1: TMenuItem;
    Fredkin: TMenuItem;
    Directed: TMenuItem;
    Invasion: TMenuItem;
    kin: TMenuItem;
    StepChild: TMenuItem;
    BriansBrain: TMenuItem;
    Conway1: TMenuItem;
    N5: TMenuItem;
    Image1: TImage;
    Panel1: TPanel;
    POExBtn: TBitBtn;
    POGeniBtn: TBitBtn;
    POLeviBtn: TBitBtn;
    Panel2: TPanel;
    NumberEdit: TEdit;
    Sporadic: TMenuItem;
    HodgePodge: TMenuItem;
    N1: TMenuItem;
    FullScreen: TMenuItem;
    N6: TMenuItem;
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReallyClose;
    procedure POGeniBtnClick(Sender: TObject);
    procedure POExBtnClick(Sender: TObject);
    procedure POLeviBtnClick(Sender: TObject);
    procedure Torus1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Walled1Click(Sender: TObject);
    procedure Slow1Click(Sender: TObject);
    procedure ColorMyWorldClick(Sender: TObject);
    procedure QUIT1Click(Sender: TObject);
    procedure StopLife;
    procedure MENUChanges;
    procedure LargeLifeClick(Sender: TObject);
    procedure MediumLifeClick(Sender: TObject);
    procedure FullPixelClick(Sender: TObject);
    procedure FullSmallClick(Sender: TObject);
    procedure FullScreenPixelsClick(Sender: TObject);

    procedure BriansBrainClick(Sender: TObject);
    procedure StepChildClick(Sender: TObject);
    procedure SporadicClick(Sender: TObject);
    procedure HodgePodgeClick(Sender: TObject);
{    procedure SchellingClick(Sender: TObject);  }
    procedure FredkinClick(Sender: TObject);
    procedure kinClick(Sender: TObject);
    procedure DirectedClick(Sender: TObject);
    procedure InvasionClick(Sender: TObject);
    procedure FullScreenClick(Sender: TObject);


    procedure Gaia(FileTodo: string);
    procedure GaiaM(FileTodo: string);
    procedure GaiaS(FileTodo: string);
    procedure GaiaP(FileTodo: string);
{procedure GaiaFullP(FileTodo:String);}
    procedure ColorGaiaFullP(FileTodo: string);
    procedure ColorGaia(FileTodo: string);
    procedure ColorGaiaM(FileTodo: string);
    procedure ColorGaiaS(FileTodo: string);
    procedure ColorGaiaP(FileTodo: string);

  private
     
  public
     

  end;

var
  LifeForm: TLifeForm;
  ChangeRow, ChangeCol: Longint;
  PatternWide, PatternHigh, XPlacer, YPlacer: Integer;
{type}
{Array3924 = Array[0..38,0..23]  of Byte;}{Large LLF}
{Array7848 = Array[0..77,0..47]  of Byte;}{Medium LMF}
{Array15696 = Array[0..155,0..95]  of Byte;}{Small LSF}
var
{TVFile :Array3924;
TV7848File:Array7848;
TV15696File:Array15696;}
  Aa, Image, {: Array3924;}
    Image78,
    TV, {:Array3924;}
    TV7848, {:Array7848;}
    TV15696, {:Array15696; }
    TempArray: array of array of Byte;
  bColorLife, bTorusLife, bOpenLife, bIamDone, bSlowDown,
    bCell1, bEditor39, bEditor78: Boolean;
const
  TDigitsSet = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',',
    '-'];
{Bitmap = Pixels LPF}
implementation

uses nEP, nUGlobal, nColors, nEditor, nLifeFiles, nuLifeAlien;

{$R *.DFM}

var Bitmap: TBitmap;
{------------------------------------------------------------------------}
{ bGlider1, bLtShip1,  bMdShip1,  bHvyShip1,
bBar51,  bBar71,  bBar81,  bBar91,  bLatin1,  bN5by51,  bLargeT1,
bAcorn1,   bPi1,   bRPent1,
bBeacon1,  bClock1,  bToad1, bCat1,  bPinwheel1, bH1,}
  bkin, bStepChild, bBriansBrain, {bSchelling,} bHodgePodge,
    bSporadic,
    bDirected, bFredkin, bInvasion, bFullScreen,
    bLargeLife, bMediumLife, b78Life, bPixelife, bFullPixelife,
    bWalledLife: Boolean;

{------------------------------------------------------------------------}

procedure TLifeForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  bIamDone := True;
  LifeFormY := LifeForm.top;
  LifeFormX := LifeForm.left;
  SetLength(Aa, 0, 0);
  SetLength(Image, 0, 0);
  SetLength(Image78, 0, 0);
  SetLength(TV15696, 0, 0);
  SetLength(TV7848, 0, 0);
  SetLength(TV, 0, 0);
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TLifeForm.ReallyClose;
begin
  bIamDone := True;

  Close;
end;

procedure TLifeForm.FormCreate(Sender: TObject);
begin
  top := LifeFormY;
  left := LifeFormX;
  bIamDone := False;
  bEditor39 := True; bEditor78 := False;
  bLargeLife := True;
  bMediumLife := False;
  b78Life := False;
  bPixelife := False;
  bFullPixelife := False;
  bFredkin := False;
  bDirected := False;
  bInvasion := False;
  bBriansBrain := False;
  bStepChild := False;
  bFullScreen := False;
  bWalledLife := False; bTorusLife := True;
  bOpenLife := False; bColorLife := False;
  bSlowDown := False;
  LargeLife.Checked := True;
  MediumLife.Checked := False;
  FullSmall.Checked := False;
  FullPixel.Checked := False;
  FullScreenPixels.Checked := False;
  Fredkin.Checked := False;
  Directed.Checked := False;
  Invasion.Checked := False;
  kin.Checked := False; bkin := False;
  BriansBrain.Checked := False;
  StepChild.Checked := False;
  Torus1.Checked := True;
  Walled1.Checked := False;
  Open1.Checked := False;
  ColorMyWorld.Checked := False;
  Slow1.Checked := False;
  SaveDialog1.InitialDir := LifeDir;
  OpenDialog1.InitialDir := LifeDir;
end;

procedure TLifeForm.QUIT1Click(Sender: TObject);
begin
  bIamDone := True;
  Close;
end;

procedure TLifeForm.FormHide(Sender: TObject);
begin
  StopLife;
  POLeviBtn.Enabled := True;
  POGeniBtn.Enabled := True;
  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  Image1.Width := 391; {391 781}
  Image1.Height := 241; {241 481}
end;

procedure TLifeForm.POExBtnClick(Sender: TObject);
begin {Stop Life}
  StopLife;
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end;

procedure TLifeForm.StopLife;
begin
{Stop Life} bIamDone := True;
end;
(********************************************************************)
{LIFE BUTTON COMMANDERS Genesis, Exedus, Leviticus}

procedure TLifeForm.POGeniBtnClick(Sender: TObject);
var
  Temptress, Tempted: string;
begin {Start Life} {disable other generators}
  POLeviBtn.Enabled := False; POGeniBtn.Enabled := False;
 {set OpenDialog1 filter to display Life's only}
  if bLargeLife then
    OpenDialog1.filter := 'Life files|*.llf'
  else if ((bMediumLife) or (b78Life)) then
    OpenDialog1.filter := 'Life files|*.lmf'
  else if (bFullPixelife) then
    OpenDialog1.filter := 'Life files|*.bmp'
  else if ((bFredkin) or (bkin) or (bDirected) or (bInvasion)
    or (bStepChild) or (bBriansBrain)
          {or(bSchelling)}or (bHodgePodge) or (bSporadic)
    or (bPixelife)) then
    OpenDialog1.filter := 'Life files|*.lmf;*.lrf';
  Tempted := ParamStr(0);
  Temptress := ExtractFilePath(Tempted);
  OpenDialog1.InitialDir := Temptress;
  if OpenDialog1.execute then begin
    Tempted := UpperCase(ExtractFileExt(OpenDialog1.filename));
    if ((bBriansBrain) and ((Tempted = '.LMF'))) then
      BriansBrainDo(OpenDialog1.filename) else
      if ((bBriansBrain) and ((Tempted = '.LRF'))) then
        BriansBrainDoColor(OpenDialog1.filename) else
        if ((bStepChild) and ((Tempted = '.LMF'))) then
          StepChildDo(OpenDialog1.filename) else
          if ((bStepChild) and ((Tempted = '.LRF'))) then
            StepChildDoColor(OpenDialog1.filename) else
            if ((bFredkin) and ({(Tempted = '.LMF')or}(Tempted =
              '.LRF'))) then FredkinDoColor(OpenDialog1.filename) else
              if ((bFredkin) and (Tempted = '.LMF')) then
                FredkinDo(OpenDialog1.filename) else
                if ((bkin) and ({(Tempted = '.LMF')or}(Tempted =
                  '.LRF'))) then kinDoColor(OpenDialog1.filename) else
                  if ((bkin) and ((Tempted = '.LMF')
                    {or(Tempted = '.LRF')})) then
                    kinDo(OpenDialog1.filename) else
                    if ((bDirected) and
                      ({(Tempted = '.LMF')or}(Tempted = '.LRF'))) then
                      DirectedDoColor(OpenDialog1.filename) else
                      if ((bDirected) and (Tempted = '.LMF')) then
                        DirectedDo(OpenDialog1.filename) else
                        if ((bInvasion) and (Tempted = '.LMF')) then
                          InvasionDo(OpenDialog1.filename) else
                          if ((bInvasion) and (Tempted = '.LRF')) then
                            InvasionDoColor(OpenDialog1.filename) else
                            if ((bSporadic) and ((Tempted = '.LMF')
                              {or(Tempted = '.LRF')})) then
                              SporadicDo {Color}(OpenDialog1.filename)
                              else
                              if ((bSporadic) and
                                ({(Tempted = '.LMF')or}(Tempted =
                                '.LRF'))) then
                                SporadicDo(OpenDialog1.filename) else
                                if ((bHodgePodge) and ((Tempted =
                                  '.LMF') or (Tempted = '.LRF'))) then
                                  HodgePodgeDo
                                  {Color}(OpenDialog1.filename) else
                                  if ((bHodgePodge) and ((Tempted =
                                    '.LMF') or (Tempted = '.LRF'))) then
                                    HodgePodgeDo(OpenDialog1.filename)
                                    else
{   If ((bSchelling) And((Tempted = '.LMF')or(Tempted = '.LRF'))) then  SchellingDoColor(OpenDialog1.filename)ELSE}
{   If ((bSchelling) And((Tempted = '.LMF')or(Tempted = '.LRF'))) then  SchellingDo(OpenDialog1.filename)ELSE}

                                    if ((bColorLife) and (Tempted =
                                      '.LLF')) then
                                      ColorGaia(OpenDialog1.filename)
                                      else
                                      if (Tempted = '.LLF') then
                                        Gaia(OpenDialog1.filename) else
                                        if ((bColorLife) and
                                          (bMediumLife) and (Tempted =
                                          '.LMF')) then
                                          ColorGaiaM(OpenDialog1.filename) else
                                          if ((bMediumLife) and
                                            (Tempted = '.LMF')) then
                                            GaiaM(OpenDialog1.filename)
                                            else
                                            if ((bColorLife) and
                                              (b78Life) and (Tempted =
                                              '.LMF')) then
                                              ColorGaiaS(OpenDialog1.filename) else
                                              if ((b78Life) and
                                                (Tempted = '.LMF')) then
                                                GaiaS(OpenDialog1.filename) else
                                                if ((bColorLife) and
                                                  (bPixelife) and
                                                  ((Tempted = '.LMF')
                                                  or (Tempted =
                                                  '.LRF'))) then
                                                  ColorGaiaP(OpenDialog1.filename) else
                                                  if ((bPixelife) and
                                                    ((Tempted = '.LMF')
                                                    or (Tempted =
                                                    '.LRF'))) then
                                                    GaiaP(OpenDialog1.filename) else
{   If ((bFullPixelife) And((Tempted = '.LRF'))) then  GaiaFullPIntro(OpenDialog1.filename) ELSE}
                                                    if
                                                      ((bFullPixelife)
                                                      and ((Tempted =
                                                      '.BMP'))) then
                                                      ColorGaiaFullP(OpenDialog1.filename);
  end else begin {return to normal}
    POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
  end;
end;
(********************************************************************)

procedure TLifeForm.POLeviBtnClick(Sender: TObject);
begin {Start Life at Random} {disable other generators}
  POLeviBtn.Enabled := False; POGeniBtn.Enabled := False;
  if (bColorLife = True) then begin
    if bMediumLife then ColorGaiaM('Random') else
      if b78Life then ColorGaiaS('Random') else
        if bPixelife then ColorGaiaP('Random') else
          if bFullPixelife then ColorGaiaFullP('Random') else
          begin
            LargeLife.Checked := True; bLargeLife := True;
            MediumLife.Checked := False; bMediumLife := False;
            FullSmall.Checked := False; b78Life := False;
            FullPixel.Checked := False; bPixelife := False;
            FullScreenPixels.Checked := False; bFullPixelife :=
              False;
            Fredkin.Checked := False; bFredkin := False;
            Directed.Checked := False; bDirected := False;
            Invasion.Checked := False; bInvasion := False;
            StepChild.Checked := False; bStepChild := False;
            kin.Checked := False; bkin := False;
            BriansBrain.Checked := False; bBriansBrain := False;
            Sporadic.Checked := False; bSporadic := False;
            HodgePodge.Checked := False; bHodgePodge := False;
{  Schelling.Checked:=False;bSchelling:=False;}
            ColorGaia('Random');
          end;
  end else begin
    if bFredkin and bFullScreen then FredkinDoColor('Random') else
      if bDirected and bFullScreen then
        DirectedDoColor('Random') else
        if bInvasion and bFullScreen then
          InvasionDoColor('Random') else
          if bkin and bFullScreen then kinDoColor('Random') else
            if bBriansBrain and bFullScreen then
              BriansBrainDoColor('Random') else
              if bStepChild and bFullScreen then
                StepChildDoColor('Random') else
{   If bSchelling and bFullScreen  then  SchellingDoColor('Random')Else}
                if bHodgePodge and bFullScreen then
                  HodgePodgeDoColor('Random') else
                  if bSporadic and bFullScreen then
                    SporadicDoColor('Random') else

                    if bFredkin then FredkinDo('Random') else
                      if bDirected then DirectedDo('Random') else
                        if bInvasion then InvasionDo('Random') else
                          if bkin then kinDo('Random') else
                            if bBriansBrain then
                              BriansBrainDo('Random') else
                              if bStepChild then
                                StepChildDo('Random') else
{   If bSchelling  then  SchellingDo('Random')Else}
                                if bHodgePodge then
                                  HodgePodgeDo('Random') else
                                  if bSporadic then
                                    SporadicDo('Random') else

                                    if bMediumLife then
                                      GaiaM('Random') else
                                      if b78Life then
                                        GaiaS('Random') else
                                        if bPixelife then
                                          GaiaP('Random') else
                                          if bFullPixelife then
                                            ColorGaiaFullP('Random')
                                            else
                                          begin
                                            LargeLife.Checked :=
                                              True; bLargeLife := True;
                                            MediumLife.Checked :=
                                              False; bMediumLife :=
                                              False;
                                            FullSmall.Checked :=
                                              False; b78Life := False;
                                            FullPixel.Checked :=
                                              False; bPixelife :=
                                              False;
                                            FullScreenPixels.Checked
                                              := False; bFullPixelife
                                              := False;
                                            Fredkin.Checked := False;
                                              bFredkin := False;
                                            Directed.Checked :=
                                              False; bDirected :=
                                              False;
                                            Invasion.Checked :=
                                              False; bInvasion :=
                                              False;
                                            StepChild.Checked :=
                                              False; bStepChild :=
                                              False;
                                            kin.Checked := False; bkin
                                              := False;
                                            BriansBrain.Checked :=
                                              False; bBriansBrain :=
                                              False;
                                            Sporadic.Checked :=
                                              False; bSporadic :=
                                              False;
                                            HodgePodge.Checked :=
                                              False; bHodgePodge :=
                                              False;
{  Schelling.Checked:=False;bSchelling:=False;}
                                            Gaia('Random');
                                          end;
  end;
end;
(********************************************************************)
(********************************************************************)
(********************************************************************)
{MENU Changes}

procedure TLifeForm.MENUChanges;
begin {}
  bLargeLife := False; LargeLife.Checked := False;
  MediumLife.Checked := False; bMediumLife := False;
  FullSmall.Checked := False; b78Life := False;
  FullPixel.Checked := False; bPixelife := False;
  FullScreenPixels.Checked := False; bFullPixelife := False;
  Fredkin.Checked := False; bFredkin := False;
  kin.Checked := False; bkin := False;
  Directed.Checked := False; bDirected := False;
  Invasion.Checked := False; bInvasion := False;
  StepChild.Checked := False; bStepChild := False;
  BriansBrain.Checked := False; bBriansBrain := False;
  Sporadic.Checked := False; bSporadic := False;
  HodgePodge.Checked := False; bHodgePodge := False;
{  Schelling.Checked:=False;bSchelling:=False;}
end;
(********************************************************************)

procedure TLifeForm.LargeLifeClick(Sender: TObject);
begin {}
  MENUChanges;
  bLargeLife := True; LargeLife.Checked := True;
end;

procedure TLifeForm.MediumLifeClick(Sender: TObject);
begin {bLargeLife,bMediumLife,bPixelife}
  MENUChanges;
  bMediumLife := True; MediumLife.Checked := True;
end;

procedure TLifeForm.FullSmallClick(Sender: TObject);
begin {}
  MENUChanges;
  b78Life := True; FullSmall.Checked := True;
end;

procedure TLifeForm.FullPixelClick(Sender: TObject);
begin
  MENUChanges;
  bPixelife := True; FullPixel.Checked := True;
end;
{Start ?  the Full screen option of life}

procedure TLifeForm.FullScreenPixelsClick(Sender: TObject);
begin
  MENUChanges;
  bFullPixelife := True; FullScreenPixels.Checked := True;
end;

procedure TLifeForm.FredkinClick(Sender: TObject);
begin
  MENUChanges;
  bFredkin := True; Fredkin.Checked := True;
end;

procedure TLifeForm.DirectedClick(Sender: TObject);
begin
  MENUChanges;
  bDirected := True; Directed.Checked := True;
end;

procedure TLifeForm.InvasionClick(Sender: TObject);
begin
  MENUChanges;
  bInvasion := True; Invasion.Checked := True;
end;

procedure TLifeForm.kinClick(Sender: TObject);
begin
  MENUChanges;
  bkin := True; kin.Checked := True;
end;

procedure TLifeForm.StepChildClick(Sender: TObject);
begin
  MENUChanges;
  bStepChild := True; StepChild.Checked := True;
end;

procedure TLifeForm.BriansBrainClick(Sender: TObject);
begin
  MENUChanges;
  bBriansBrain := True; BriansBrain.Checked := True;
end;

procedure TLifeForm.SporadicClick(Sender: TObject);
begin
  MENUChanges;
  bSporadic := True; Sporadic.Checked := True;
end;

procedure TLifeForm.HodgePodgeClick(Sender: TObject);
begin
  MENUChanges;
  bHodgePodge := True; HodgePodge.Checked := True;
end;

(********************************************************************)
{procedure TLifeForm.SchellingClick(Sender: TObject);
begin
MENUChanges;
bSchelling:=True; Schelling.Checked:=True;
end;}
(********************************************************************)

procedure TLifeForm.Open1Click(Sender: TObject);
begin {}
  bOpenLife := (not bOpenLife);
  if (bOpenLife = True) then begin
    Open1.Checked := True;
    bTorusLife := False; Torus1.Checked := False;
    bWalledLife := False; Walled1.Checked := False;
  end else Open1.Checked := False;
end;

procedure TLifeForm.Walled1Click(Sender: TObject);
begin {}
  bWalledLife := (not bWalledLife);
  if (bWalledLife = True) then begin
    Walled1.Checked := True;
    bTorusLife := False; Torus1.Checked := False;
    bOpenLife := False; Open1.Checked := False;
  end else Walled1.Checked := False;
end;

procedure TLifeForm.Torus1Click(Sender: TObject);
begin {}
  bTorusLife := (not bTorusLife);
  if bTorusLife then begin
    Torus1.Checked := True;
    bWalledLife := False; Walled1.Checked := False;
    bOpenLife := False; Open1.Checked := False;
  end else Torus1.Checked := False;
end;

procedure TLifeForm.Slow1Click(Sender: TObject);
begin
  bSlowDown := (not bSlowDown);
  if bSlowDown then begin
    Slow1.Checked := True;
  end else Slow1.Checked := False;
end;

procedure TLifeForm.ColorMyWorldClick(Sender: TObject);
begin {}
  bColorLife := (not bColorLife);
  if bColorLife then begin
    ColorMyWorld.Checked := True;
  end else ColorMyWorld.Checked := False;
end;

(********************************************************************)

procedure TLifeForm.FullScreenClick(Sender: TObject);
begin
  bFullScreen := (not bFullScreen);
  if bFullScreen then begin
    FullScreen.Checked := True;
  end else FullScreen.Checked := False;
end;
(********************************************************************)
(********************************************************************)


(********************************************************************)
(********************************************************************)

procedure TLifeForm.Gaia(FileTodo: string);
var
  ImageByte: Byte;
  Name, Cs, Hs, s: string;

  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte; {Array3924;}
  NewRect: TRect;
begin
  SetLength(Aa, 39, 24);
  SetLength(Image, 39, 24);
  SetLength(TV, 39, 24);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  Image1.Width := 391; {391 781}
  Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 38 do
      for J := 0 to 23 do begin
        Image[I, J] := Trunc(Random(2));
      end;
  end else begin
    for I := 0 to 38 do for J := 0 to 23 do Image[I, J] := 0;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
      for I := 0 to 38 do for J := 0 to 23 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
{FOR I := 0 TO 42 DO   FOR J := 0 TO 25 DO InImage[I,J] := Image[I,J];}
{Read the Hamming,Cells,Cycles}
    Name := ExtractFileName(FileTodo);
  end;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 38 do for J := 0 to 23 do TV[I, J] := 0;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 37 do for J := 1 to 22 do if Image[I, J] <> 0 then
      TV[I, J] := 1;
    for I := 0 to 38 do for J := 0 to 23 do Image[I, J] := TV[I, J];
  end else begin
    for I := 0 to 38 do for J := 0 to 23 do if Image[I, J] <> 0 then
      TV[I, J] := 1;
  end;

  for I := 0 to 38 do
  begin
    for J := 0 to 23 do
      if (TV[I, J] = 1) then
      begin
        Image1.Canvas.Brush.Color := Desolate; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        Image1.Canvas.FillRect(NewRect);
      end;
  end;
  POGeniBtn.Caption := Name;
  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV;}
      for I := 0 to 38 do for J := 0 to 23 do AA[I, J] := TV[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 37 do begin
        for J := 1 to 22 do begin { I is across 38 J is DOWN 23 }
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV[I, J] := 0; end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 38 J is DOWN 23 }
        I := 0; for J := 1 to 22 do begin
   {J-1} C := Aa[I + 38, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 38, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 38, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV[I, J] := 0; end
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        I := 38; for J := 1 to 22 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 38, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 38, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 38, J +
     1];
          if C <= 1 then begin TV[I, J] := 0; end
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 0; for I := 1 to 37 do begin
   {J-1} C := Aa[I - 1, J + 23] + Aa[I, J + 23] + Aa[I + 1, J + 23];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV[I, J] := 0; end
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 23; for I := 1 to 37 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 23] + Aa[I, J - 23] + Aa[I + 1, J -
     23];
          if C <= 1 then begin TV[I, J] := 0; end
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRight}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 23; I := 38; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 38, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 38, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 23] + Aa[I, J - 23] + Aa[I - 38, J -
     23];
          if C <= 1 then begin TV[I, J] := 0; end
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMLeft}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 23; I := 0; begin
   {J-1} C := Aa[I + 38, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 38, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 38, J - 23] + Aa[I, J - 23] + Aa[I + 1, J -
     23];
          if C <= 1 then begin TV[I, J] := 0; end
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TopLeft}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 0; I := 0; begin
   {J-1} C := Aa[I + 38, J + 23] + Aa[I, J + 23] + Aa[I + 1, J + 23];
   {J} C := C + Aa[I + 38, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 38, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV[I, J] := 0; end
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TopRight}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 0; I := 38; begin
   {J-1} C := Aa[I - 1, J + 23] + Aa[I, J + 23] + Aa[I - 38, J + 23];
   {J} C := C + Aa[I - 1, J] + Aa[I - 38, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 38, J +
     1];
          if C <= 1 then begin TV[I, J] := 0; end
          else if C = 3 then begin TV[I, J] := 1; end
          else if C >= 4 then begin TV[I, J] := 0; end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs + ' Cells:'
        + Cs;
      NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;

      for I := 0 to 38 do
      begin
        for J := 0 to 23 do
          if (Aa[I, J] <> TV[I, J]) then
          begin
            if TV[I, J] = 1 then
              Image1.Canvas.Brush.Color := Full
            else Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if bSlowDown then begin
              Application.ProcessMessages;
              Sleep(NapTime);
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

    { if Check_P_R_Save was on other side the procedure would never end
     due to never getting a keypressed signal past the Check_P_R_Save }
{Check_P_R_Save;}
 {set OpenDialog1 filter to display Life's only}

{Save the InImage?}
  if MessageDlg('Save the Beggining of Life values?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.llf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'llf';
    SaveDialog1.filename := 'RandomLife.llf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
         {write( fil, Image ); }
          for I := 0 to 38 do for J := 0 to 23 do
            begin
              ImageByte := Image[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end; end; end;
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.llf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'llf';
    SaveDialog1.filename := 'NewLife.llf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TV );}
          for I := 0 to 38 do for J := 0 to 23 do
            begin
              ImageByte := TV[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
   {Save the Hamming,Cells,Cycles}
      end; end; end;
  POGeniBtn.Caption := 'Genesis';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure GAIA L}
(********************************************************************)
(********************************************************************)

procedure TLifeForm.GaiaM(FileTodo: string); {80}
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte; {Array7848;}
  NewRect: TRect;
{Ds:DirStr;
N : NameStr;
E:ExtStr;}

begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  Image1.Width := 391; {391 781}
  Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 77 do
      for J := 0 to 47 do begin
        Image[I, J] := Trunc(Random(2));
      end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );  }
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do
  begin
    for J := 1 to 46 do
      if (TV7848[I, J] = 1) then
      begin
        Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        Image1.Canvas.FillRect(NewRect);
      end else begin
        Image1.Canvas.Brush.Color := Desolate; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        Image1.Canvas.FillRect(NewRect);
      end;

  end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0; end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J +
     1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I - 77, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I + 77, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
 {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I - 77, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs + ' Cells:'
        + Cs;
      NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              Image1.Canvas.Brush.Color := Full {CurrentColor}
            else Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

    { if Check_P_R_Save was on other side the procedure would never end
     due to never getting a keypressed signal past the Check_P_R_Save }
{Check_P_R_Save;}
 {set OpenDialog1 filter to display Life's only}
{Save the InImage?}
  if MessageDlg('Save the Beginning of Life values?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lmf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'RandomLife.lmf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, Image ); }
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := Image[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end; end; end;
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lmf';
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'NewLife.lmf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TV7848); }
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := TV7848[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
   {Save the Hamming,Cells,Cycles}
      end; end; end;
  POGeniBtn.Caption := 'Genesis';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure GAIA M}
(********************************************************************)
(********************************************************************)
(********************************************************************)

procedure TLifeForm.GaiaS(FileTodo: string);
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte; {Array7848;}
  NewRect: TRect;
{Ds:DirStr;
N : NameStr;
E:ExtStr;}

begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsMaximized; {wsNormal}
  Image1.Width := 781; {391}
  Image1.Height := 481; {241}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 77 do
      for J := 0 to 47 do begin
        Image[I, J] := Trunc(Random(2));
      end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image ); }
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do begin
    for J := 1 to 46 do begin
      if (TV7848[I, J] = 1) then
      begin
        Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        Image1.Canvas.FillRect(NewRect);
      end else begin
        Image1.Canvas.Brush.Color := Desolate; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        Image1.Canvas.FillRect(NewRect);
      end;
    end; end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0; end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J +
     1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I - 77, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I + 77, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
 {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I - 77, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0; end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0; end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs + ' Cells:'
        + Cs;
      NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 77 do
      begin
        for J := 0 to 47 do
          if (Aa[I, J] <> TV7848[I, J]) then
          begin
            if TV7848[I, J] = 1 then
              Image1.Canvas.Brush.Color := Full {CurrentColor}
            else Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

    { if Check_P_R_Save was on other side the procedure would never end
     due to never getting a keypressed signal past the Check_P_R_Save }
{Check_P_R_Save;}
 {set OpenDialog1 filter to display Life's only}
{Save the InImage?}
  if MessageDlg('Save the Beginning of Life values?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lmf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'RandomLife.lmf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, Image );}
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := Image[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end; end; end;
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lmf';
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'NewLife.lmf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TV7848);  }
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := TV7848[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
   {Save the Hamming,Cells,Cycles}
      end; end; end;
  POGeniBtn.Caption := 'Genesis';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure GAIA S}
(********************************************************************)
(********************************************************************)
{
Array7848 = Array[0..78,0..48]  of Byte;
Array15696 = Array[0..156,0..96]  of Byte;
}
(********************************************************************)
(********************************************************************)
(********************************************************************)

procedure TLifeForm.GaiaP(FileTodo: string); { }
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte {Array15696};
  fil78: file of Byte {Array7848};
  NewRect: TRect;
begin
{Aa ,Image : Array15696;Image78 : Array7848;}
  SetLength(Aa, 156, 96);
  SetLength(Image, 156, 96);
  SetLength(TV15696, 156, 96);
  SetLength(Image78, 78, 48);

  LifeForm.WindowState := wsMaximized; {wsNormal}
  Image1.Width := 781; {391}
  Image1.Height := 481; {241}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {391}
  Bitmap.Height := 481; {241}
  Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 155 do begin
      for J := 0 to 95 do begin
        Image[I, J] := Trunc(Random(2));
      end; end;
  end else begin
    Name := ExtractFileExt(FileTodo); Name := UpperCase(Name);
    if (Name = '.LMF') then begin
      for I := 0 to 77 do begin for J := 0 to 47 do
        begin Image[I, J] := 0; end; end;
      AssignFile(fil78, FileTodo);
{$I-}Reset(fil78); {$I+}
      i := IOresult;
      if i <> 0 then EXIT {ReportError( i )} else
      begin
{         read( fil78, Image78 ); }
        for I := 0 to 77 do for J := 0 to 47 do
          begin
            read(fil, ImageByte);
            Image78[I, J] := ImageByte;
          end;
        CloseFile(fil78);
      end;
      for I := 0 to 77 do begin for J := 0 to 47 do begin
          TV15696[I + 39, J + 24] := Image78[I, J]; end; end;
      for I := 0 to 155 do begin for J := 0 to 95 do begin
          Image[I, J] := TV15696[I, J]; end; end;
    end else if (Name = '.LRF') then begin
      for I := 0 to 155 do begin
        for J := 0 to 95 do begin
          Image[I, J] := 0; end; end;
      AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
      i := IOresult;
      if i <> 0 then EXIT {ReportError( i )} else
      begin
{         read( fil, Image );}
        for I := 0 to 155 do for J := 0 to 95 do
          begin
            read(fil, ImageByte);
            Image[I, J] := ImageByte;
          end;
        CloseFile(fil);
      end;
    end else begin
      Randomize;
      for I := 0 to 155 do begin
        for J := 0 to 95 do begin
          Image[I, J] := Trunc(Random(2));
        end; end;
      MessageDlg('Mistake getting Pixel life', mtInformation, [mbOk],
        0);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if (TV15696[I, J] = 1) then
      begin
        Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        Image1.Canvas.FillRect(NewRect);
      end else begin
        Image1.Canvas.Brush.Color := Desolate; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        Image1.Canvas.FillRect(NewRect);
      end;
    end; end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0; end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 94 do begin
   {J-1} C := Aa[I + 155, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 95, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 155, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV15696[I, J] := 0; end
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 155; for J := 1 to 94 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 155, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 155, J +
     1];
          if C <= 1 then begin TV15696[I, J] := 0; end
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 154 do begin
   {J-1} C := Aa[I - 1, J + 95] + Aa[I, J + 95] + Aa[I + 1, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0; end
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; for I := 1 to 154 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 95] + Aa[I, J - 95] + Aa[I + 1, J -
     95];
          if C <= 1 then begin TV15696[I, J] := 0; end
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; I := 155; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 155, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 95] + Aa[I, J - 47] + Aa[I - 155, J -
     95];
          if C <= 1 then begin TV15696[I, J] := 0; end
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; I := 0; begin
   {J-1} C := Aa[I + 155, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 155, J - 95] + Aa[I, J - 95] + Aa[I + 1, J -
     95];
          if C <= 1 then begin TV15696[I, J] := 0; end
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I + 155, J + 95] + Aa[I, J + 95] + Aa[I + 1, J + 95];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 155, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0; end
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 155; begin
 {J-1} C := Aa[I - 1, J + 95] + Aa[I, J + 95] + Aa[I - 155, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 155, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0; end
          else if C = 3 then begin TV15696[I, J] := 1; end
          else if C >= 4 then begin TV15696[I, J] := 0; end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs + ' Cells:'
        + Cs;
      NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
      for I := 0 to 155 do
      begin
        for J := 0 to 95 do
          if (Aa[I, J] <> TV15696[I, J]) then
          begin
            if TV15696[I, J] = 1 then
              Image1.Canvas.Brush.Color := Full {CurrentColor}
            else Image1.Canvas.Brush.Color := Desolate;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
      end;
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

    { if Check_P_R_Save was on other side the procedure would never end
     due to never getting a keypressed signal past the Check_P_R_Save }
{Check_P_R_Save;}
 {set OpenDialog1 filter to display Life's only}
{Save the InImage?}
  if MessageDlg('Save the Beginning of Life values?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lrf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lrf';
    SaveDialog1.filename := 'RandomLife.lrf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, Image );  }
          for I := 0 to 155 do for J := 0 to 95 do
            begin
              ImageByte := Image[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end; end; end;
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lrf';
    SaveDialog1.DefaultExt := 'lrf';
    SaveDialog1.filename := 'NewLife.lrf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TV15696); }
          for I := 0 to 155 do for J := 0 to 95 do
            begin
              ImageByte := TV15696[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
   {Save the Hamming,Cells,Cycles}
      end; end; end;
  POGeniBtn.Caption := 'Genesis';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure GAIA P }
(********************************************************************)
(********************************************************************)
(********************************************************************)

(********************************************************************)
(********************************************************************)

procedure TLifeForm.ColorGaia(FileTodo: string);
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;

  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte {Array3924};
  NewRect: TRect;
begin
{Aa  ,Image : Array3924;}
  SetLength(Aa, 39, 24);
  SetLength(Image, 39, 24);
  SetLength(TV, 39, 24);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  Image1.Width := 391; {391 781}
  Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 38 do begin for J := 0 to 23 do begin
        Image[I, J] := Trunc(Random(2));
      end; end;
  end else begin
    for I := 0 to 38 do begin for J := 0 to 23 do
      begin Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 39 do for J := 0 to 24 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
{FOR I := 0 TO 42 DO   FOR J := 0 TO 25 DO InImage[I,J] := Image[I,J];}
{Read the Hamming,Cells,Cycles}
    Name := ExtractFileName(FileTodo);
  end;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 38 do begin for J := 0 to 23 do
    begin TV[I, J] := 0; end; end;
{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 37 do begin for J := 1 to 22 do
      begin if Image[I, J] <> 0 then TV[I, J] := 1; end; end;
    for I := 0 to 38 do begin for J := 0 to 23 do
      begin Image[I, J] := TV[I, J]; end; end;
  end else begin
    for I := 0 to 38 do begin for J := 0 to 23 do
      begin if Image[I, J] <> 0 then TV[I, J] := 1; end; end;
  end;


  for I := 0 to 38 do begin
    for J := 0 to 23 do begin
      if (TV[I, J] = 1) then
      begin
        Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        Image1.Canvas.FillRect(NewRect);
      end else begin
        Image1.Canvas.Brush.Color := Desolate; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        Image1.Canvas.FillRect(NewRect);
      end;
    end; end;
  POGeniBtn.Caption := Name;
  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV;}
      for I := 0 to 38 do for J := 0 to 23 do AA[I, J] := TV[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 37 do begin
        for J := 1 to 22 do begin { I is across 38 J is DOWN 23 }
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 38 J is DOWN 23 }
        I := 0; for J := 1 to 22 do begin
   {J-1} C := Aa[I + 38, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 38, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 38, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        I := 38; for J := 1 to 22 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 38, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 38, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 38, J +
     1];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 0; for I := 1 to 37 do begin
   {J-1} C := Aa[I - 1, J + 23] + Aa[I, J + 23] + Aa[I + 1, J + 23];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 23; for I := 1 to 37 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 23] + Aa[I, J - 23] + Aa[I + 1, J -
     23];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRight}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 23; I := 38; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 38, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 38, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 23] + Aa[I, J - 23] + Aa[I - 38, J -
     23];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMLeft}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 23; I := 0; begin
   {J-1} C := Aa[I + 38, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 38, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 38, J - 23] + Aa[I, J - 23] + Aa[I + 1, J -
     23];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TopLeft}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 0; I := 0; begin
   {J-1} C := Aa[I + 38, J + 23] + Aa[I, J + 23] + Aa[I + 1, J + 23];
   {J} C := C + Aa[I + 38, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 38, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TopRight}{ I is across J is DOWN }{ I is across 38 J is DOWN 23 }
        J := 0; I := 38; begin
   {J-1} C := Aa[I - 1, J + 23] + Aa[I, J + 23] + Aa[I - 38, J + 23];
   {J} C := C + Aa[I - 1, J] + Aa[I - 38, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 38, J +
     1];
          if C <= 1 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV[I, J]) then HAMMING := (HAMMING + 1);
          if TV[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
      Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs + ' Cells:'
        + Cs;
      NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
(*
FOR I := 0 to 38 DO
begin
  FOR J := 0 to 23 DO
    IF (Aa[I, J] <> TV[I, J]) THEN
    Begin
    If TV[I, J] = 1 then
    Image1.Canvas.Brush.Color:= Full
    else Image1.Canvas.Brush.Color:= Desolate;
    NewRect:=     Rect((I*10+1),(J*10+1),((I*10)+10),((J*10)+10));
    Image1.Canvas.FillRect(NewRect);
    If bSlowDown then begin
    Application.ProcessMessages;
    Sleep(NapTime);
    end;
    end;
end;
*)
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

    { if Check_P_R_Save was on other side the procedure would never end
     due to never getting a keypressed signal past the Check_P_R_Save }
{Check_P_R_Save;}
 {set OpenDialog1 filter to display Life's only}

{Save the InImage?}
  if MessageDlg('Save the Beggining of Life values?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.llf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'llf';
    SaveDialog1.filename := 'RandomLife.llf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, Image ); }
          for I := 0 to 39 do for J := 0 to 24 do
            begin
              ImageByte := Image[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end; end; end;
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.llf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'llf';
    SaveDialog1.filename := 'NewLife.llf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TV );  }
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := TV[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
   {Save the Hamming,Cells,Cycles}
      end; end; end;
  POGeniBtn.Caption := 'Genesis';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure GAIA L}
(********************************************************************)
(********************************************************************)


(********************************************************************)
(********************************************************************)

procedure TLifeForm.ColorGaiaM(FileTodo: string); {80}
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte; {Array7848;}
  NewRect: TRect;
{Ds:DirStr;
N : NameStr;
E:ExtStr;}

begin
{Aa ,Image : Array7848;}
  SetLength(Aa, 79, 49);
  SetLength(Image, 79, 49);
  SetLength(TV, 79, 49);

  LifeForm.WindowState := wsNormal; {wsNormal wsMaximized}
  Image1.Width := 391; {391 781}
  Image1.Height := 241; {241 481}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 391; {640}
  Bitmap.Height := 241; {480}
  Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 77 do begin for J := 0 to 47 do begin
        Image[I, J] := Trunc(Random(2));
      end; end;
  end else begin
    for I := 0 to 77 do begin
      for J := 0 to 47 do begin
        Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );}
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do begin
    for J := 1 to 46 do begin
      if (TV7848[I, J] = 1) then
      begin
        Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        Image1.Canvas.FillRect(NewRect);
      end else begin
        Image1.Canvas.Brush.Color := Desolate; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        Image1.Canvas.FillRect(NewRect);
      end;
    end; end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J +
     1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I - 77, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I + 77, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1; end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
 {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I - 77, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs + ' Cells:'
        + Cs;
      NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
(*
FOR I := 0 to 77 DO
begin
  FOR J := 0 to 47 DO
    IF (Aa[I, J] <> TV7848[I, J]) THEN
    Begin
    If TV7848[I, J] = 1 then
    Image1.Canvas.Brush.Color:= Full{CurrentColor}
    else Image1.Canvas.Brush.Color:= Desolate;
    NewRect:=     Rect((I*5+1),(J*5+1),((I*5)+5),((J*5)+5));
    Image1.Canvas.FillRect(NewRect);
    If (bSlowDown = true) then begin
    Application.ProcessMessages;
    Sleep(NapTime); {MsgWaitForMultipleObjects}
    end;
    end;
end;
*)
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

    { if Check_P_R_Save was on other side the procedure would never end
     due to never getting a keypressed signal past the Check_P_R_Save }
{Check_P_R_Save;}
 {set OpenDialog1 filter to display Life's only}
{Save the InImage?}
  if MessageDlg('Save the Beginning of Life values?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lmf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'RandomLife.lmf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, Image );}
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := Image[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end; end; end;
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lmf';
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'NewLife.lmf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TV7848);}
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := TV7848[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
   {Save the Hamming,Cells,Cycles}
      end; end; end;
  POGeniBtn.Caption := 'Genesis';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure GAIA M}
(********************************************************************)


(********************************************************************)
(********************************************************************)

procedure TLifeForm.ColorGaiaS(FileTodo: string);
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte {Array7848};
  NewRect: TRect;
begin
{Aa ,Image : Array7848;  }
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsMaximized; {wsNormal}
  Image1.Width := 781; {391}
  Image1.Height := 481; {241}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {640}
  Bitmap.Height := 481; {480}
  Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 77 do begin
      for J := 0 to 47 do begin
        Image[I, J] := Trunc(Random(2));
      end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do begin
        Image[I, J] := 0; end; end;
    AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
    i := IOresult;
    if i <> 0 then EXIT {ReportError( i )} else
    begin
{         read( fil, Image );  }
      for I := 0 to 77 do for J := 0 to 47 do
        begin
          read(fil, ImageByte);
          Image[I, J] := ImageByte;
        end;
      CloseFile(fil);
    end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 77 do begin for J := 0 to 47 do
    begin TV7848[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 76 do begin for J := 1 to 46 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin Image[I, J] := TV7848[I, J]; end; end;
  end else begin
    for I := 0 to 77 do begin for J := 0 to 47 do
      begin if Image[I, J] <> 0 then TV7848[I, J] := 1; end; end;
  end;

  for I := 1 to 76 do begin
    for J := 1 to 46 do begin
      if (TV7848[I, J] = 1) then
      begin
        Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        Image1.Canvas.FillRect(NewRect);
      end else begin
        Image1.Canvas.Brush.Color := Desolate; { CurrentColor;}
        NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) + 10),
          ((J * 10) + 10));
        Image1.Canvas.FillRect(NewRect);
      end;
    end; end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV7848;}
      for I := 0 to 77 do for J := 0 to 47 do
        AA[I, J] := TV7848[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 76 do begin
        for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 46 do begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 47, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 77; for J := 1 to 46 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J +
     1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; for I := 1 to 76 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 77; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 77, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 47] + Aa[I, J - 47] + Aa[I - 77, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 47; I := 0; begin
   {J-1} C := Aa[I + 77, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 77, J - 47] + Aa[I, J - 47] + Aa[I + 1, J -
     47];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I + 77, J + 47] + Aa[I, J + 47] + Aa[I + 1, J + 47];
   {J} C := C + Aa[I + 77, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 77, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 77; begin
 {J-1} C := Aa[I - 1, J + 47] + Aa[I, J + 47] + Aa[I - 77, J + 47];
   {J} C := C + Aa[I - 1, J] + Aa[I - 77, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 77, J + 1];
          if C <= 1 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV7848[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV7848[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 10 + 1), (J * 10 + 1), ((I * 10) +
              10), ((J * 10) + 10));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV7848[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV7848[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs + ' Cells:'
        + Cs;
      NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;
(*
FOR I := 0 to 77 DO
begin
  FOR J := 0 to 47 DO
    IF (Aa[I, J] <> TV7848[I, J]) THEN
    Begin
    If TV7848[I, J] = 1 then
    Image1.Canvas.Brush.Color:= Full{CurrentColor}
    else Image1.Canvas.Brush.Color:= Desolate;
    NewRect:=     Rect((I*10+1),(J*10+1),((I*10)+10),((J*10)+10));
    Image1.Canvas.FillRect(NewRect);
    If (bSlowDown = true) then begin
    Application.ProcessMessages;
    Sleep(NapTime); {MsgWaitForMultipleObjects}
    end;
    end;
end;*)
      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

    { if Check_P_R_Save was on other side the procedure would never end
     due to never getting a keypressed signal past the Check_P_R_Save }
{Check_P_R_Save;}
 {set OpenDialog1 filter to display Life's only}
{Save the InImage?}
  if MessageDlg('Save the Beginning of Life values?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lmf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'RandomLife.lmf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, Image );}
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := Image[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end; end; end;
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lmf';
    SaveDialog1.DefaultExt := 'lmf';
    SaveDialog1.filename := 'NewLife.lmf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TV7848);}
          for I := 0 to 77 do for J := 0 to 47 do
            begin
              ImageByte := TV7848[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
   {Save the Hamming,Cells,Cycles}
      end; end; end;
  POGeniBtn.Caption := 'Genesis';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure GAIA S}
(********************************************************************)
(********************************************************************)
{
Array7848 = Array[0..78,0..48]  of Byte;
Array15696 = Array[0..156,0..96]  of Byte;
}
(********************************************************************)

procedure TLifeForm.ColorGaiaP(FileTodo: string); { }
var
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  fil: file of Byte {Array15696};
  fil78: file of Byte {Array7848};
  NewRect: TRect;
begin
{Aa ,Image : Array15696;Image78 : Array7848;}
  SetLength(Aa, 78, 48);
  SetLength(Image, 78, 48);
  SetLength(TV7848, 78, 48);

  LifeForm.WindowState := wsMaximized; {wsNormal}
  Image1.Width := 781; {391}
  Image1.Height := 481; {241}
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {391}
  Bitmap.Height := 481; {241}
  Image1.Picture.Graphic := Bitmap;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
  NapTime := 0;
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;

  if FileTodo = 'Random' then
  begin
    Randomize;
    for I := 0 to 155 do begin
      for J := 0 to 95 do begin
        Image[I, J] := Trunc(Random(2));
      end; end;
  end else begin
    Name := ExtractFileExt(FileTodo); Name := UpperCase(Name);
    if (Name = '.LMF') then begin
      for I := 0 to 77 do begin
        for J := 0 to 47 do begin
          Image[I, J] := 0; end; end;
      AssignFile(fil78, FileTodo);
{$I-}Reset(fil78); {$I+}
      i := IOresult;
      if i <> 0 then EXIT {ReportError( i )} else
      begin
{         read( fil78, Image78 );}
        for I := 0 to 77 do for J := 0 to 47 do
          begin
            read(fil, ImageByte);
            Image78[I, J] := ImageByte;
          end;
        CloseFile(fil78);
      end;
      for I := 0 to 77 do begin for J := 0 to 47 do begin
          TV15696[I + 39, J + 24] := Image78[I, J]; end; end;
      for I := 0 to 155 do begin for J := 0 to 95 do begin
          Image[I, J] := TV15696[I, J]; end; end;
    end else
      if (Name = 'LRF') then begin
        for I := 0 to 155 do begin
          for J := 0 to 95 do begin
            Image[I, J] := 0; end; end;
        AssignFile(fil, FileTodo);
{$I-}Reset(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         read( fil, Image );   }
          for I := 0 to 155 do for J := 0 to 95 do
            begin
              read(fil, ImageByte);
              Image[I, J] := ImageByte;
            end;
          CloseFile(fil);
        end;
      end else
      begin
        Randomize;
        for I := 0 to 155 do begin
          for J := 0 to 95 do begin
            Image[I, J] := Trunc(Random(2));
          end; end;
      end;
    Name := ExtractFileName(FileTodo);
{Read the Hamming,Cells,Cycles from the not yet made file...}
  end;
  POGeniBtn.Caption := Name;
  Hamming := 0;
  CellCount := 0;
  for I := 0 to 155 do begin for J := 0 to 95 do
    begin TV15696[I, J] := 0; end; end;

{ read in the  SCREEN }
  if (bOpenLife = True) then begin
    for I := 1 to 154 do begin for J := 1 to 94 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin Image[I, J] := TV15696[I, J]; end; end;
  end else begin
    for I := 0 to 155 do begin for J := 0 to 95 do
      begin if Image[I, J] <> 0 then TV15696[I, J] := 1; end; end;
  end;

  for I := 1 to 154 do begin
    for J := 1 to 94 do begin
      if (TV15696[I, J] = 1) then
      begin
        Image1.Canvas.Brush.Color := Full; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        Image1.Canvas.FillRect(NewRect);
      end else begin
        Image1.Canvas.Brush.Color := Desolate; { CurrentColor;}
        NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5), ((J
          * 5) + 5));
        Image1.Canvas.FillRect(NewRect);
      end;
    end; end;


  Cycle := 0;
  repeat
    {2} begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
{AA := TV15696;}
      for I := 0 to 155 do for J := 0 to 95 do
        AA[I, J] := TV15696[I, J];
{'INSERT code here for Cellular Automata rules}
      for I := 1 to 154 do begin
        for J := 1 to 94 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off}
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
      end;
      if (bTorusLife = true) then begin
{*************************************************}
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
        I := 0; for J := 1 to 94 do begin
   {J-1} C := Aa[I + 155, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 95, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 155, J + 1] + Aa[I, J + 1] + Aa[I + 1, J +
     1];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := 155; for J := 1 to 94 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 155, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 155, J +
     1];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; for I := 1 to 154 do begin
   {J-1} C := Aa[I - 1, J + 95] + Aa[I, J + 95] + Aa[I + 1, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; for I := 1 to 154 do begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 95] + Aa[I, J - 95] + Aa[I + 1, J -
     95];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; I := 155; begin
   {J-1} C := Aa[I - 1, J - 1] + Aa[I, J - 1] + Aa[I - 155, J - 1];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I - 1, J - 95] + Aa[I, J - 47] + Aa[I - 155, J -
     95];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 95; I := 0; begin
   {J-1} C := Aa[I + 155, J - 1] + Aa[I, J - 1] + Aa[I + 1, J - 1];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
   {J+1} C := C + Aa[I + 155, J - 95] + Aa[I, J - 95] + Aa[I + 1, J -
     95];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 0; begin
 {J-1} C := Aa[I + 155, J + 95] + Aa[I, J + 95] + Aa[I + 1, J + 95];
   {J} C := C + Aa[I + 155, J] + Aa[I + 1, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I + 155, J + 1] + Aa[I, J + 1] + Aa[I + 1, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0; I := 155; begin
 {J-1} C := Aa[I - 1, J + 95] + Aa[I, J + 95] + Aa[I - 155, J + 95];
   {J} C := C + Aa[I - 1, J] + Aa[I - 155, J];
     { I,J (itself) is NOT counted}
 {J+1} C := C + Aa[I - 1, J + 1] + Aa[I, J + 1] + Aa[I - 155, J + 1];
          if C <= 1 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Desolate; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 2 then begin
            Image1.Canvas.Brush.Color := Growing; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C = 3 then begin TV15696[I, J] := 1;
            Image1.Canvas.Brush.Color := Full; {CurrentColor}
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end
          else if C >= 4 then begin TV15696[I, J] := 0;
            Image1.Canvas.Brush.Color := Overpopulated;
            NewRect := Rect((I * 5 + 1), (J * 5 + 1), ((I * 5) + 5),
              ((J * 5) + 5));
            Image1.Canvas.FillRect(NewRect);
            if (bSlowDown = true) then begin
              Application.ProcessMessages;
              Sleep(NapTime); {MsgWaitForMultipleObjects}
            end;
          end;
          if not (Aa[I, J] = TV15696[I, J]) then
            HAMMING := (HAMMING + 1);
          if TV15696[I, J] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{*************************************************}
      end; {Of if Torus}
{ Display is after the text so Text can be seen while image is altered }
      Str(Hamming, Hs);
      Str(Cycle, s);
      Str(Cellcount, Cs);
{Hs := concat('Hamming # ',Hs);
Cs := concat('Cells   : ',Cs);
 s := concat('Cycle # ',s);   }

      Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs + ' Cells:'
        + Cs;
      NumberEdit.Text := {'Cycle } '# ' + s + ' ';
      Application.ProcessMessages;

      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
      CellCount := 0;
    {2-}end; { of Repeat loop  }
  until (bIamDone = True);

    { if Check_P_R_Save was on other side the procedure would never end
     due to never getting a keypressed signal past the Check_P_R_Save }
{Check_P_R_Save;}
 {set OpenDialog1 filter to display Life's only}
{Save the InImage?}
  if MessageDlg('Save the Beginning of Life values?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lrf';
  {set OpenDialog1 Default extension}
    SaveDialog1.DefaultExt := 'lrf';
    SaveDialog1.filename := 'RandomLife.lrf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, Image );}
          for I := 0 to 155 do for J := 0 to 95 do
            begin
              ImageByte := Image[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
      end; end; end;
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
{    OpenDialog1.InitialDir:=RPM_Storage;}
    SaveDialog1.filter := 'Life files|*.lrf';
    SaveDialog1.DefaultExt := 'lrf';
    SaveDialog1.filename := 'NewLife.lrf';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
{$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
{         write( fil, TV15696);}
          for I := 0 to 155 do for J := 0 to 95 do
            begin
              ImageByte := TV15696[I, J];
              write(fil, ImageByte);
            end;
          CloseFile(fil);
        end;
   {Save the Hamming,Cells,Cycles}
      end; end; end;
  POGeniBtn.Caption := 'Genesis';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure Color GAIA P }
(********************************************************************)
(********************************************************************)
(********************************************************************)
(********************************************************************)

procedure TLifeForm.ColorGaiaFullP(FileTodo: string);
var
  Name, Cs, Hs, s: string;
  NapTime, ErrorCode, Color, W, V,
    OldHamming, OldCellCount, LastHamming, LastCellCount,
    CellCount, Z, B, C, I, J, Hamming, X, Y, Cycle, TVC: integer;
  AnyThingThere: TColor;
  AliveW: array[0..780] of Boolean;
  AliveV: array[0..480] of Boolean;
{	Blank, }
  Putting, Next, Now: array[0..480] of Integer; {TColor;}
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := 781; {391}
  Bitmap.Height := 481; {241}
  LifeForm.WindowState := wsMaximized; {wsNormal}
  Image1.Width := 781; {391}
  Image1.Height := 481; {241}
  Image1.Picture.Graphic := Bitmap;
  Image1.Canvas.Pen.Color := Overpopulated;
  Image1.Canvas.Brush.Color := RGB(255, 255, 255); {Desolate;}
  Image1.Canvas.Brush.Style := bsSolid;
  Image1.Canvas.FillRect(Rect(0, 0, 780, 480));
  Image1.Canvas.Moveto(0, 0);
  Image1.Canvas.Lineto(0, 480);
  Image1.Canvas.Lineto(780, 480);
  Image1.Canvas.Lineto(780, 0);
  Image1.Canvas.Lineto(0, 0);
  for W := 0 to 780 do AliveW[W] := False;
  for V := 0 to 480 do AliveV[V] := False;
  bIamDone := False;
  LastHamming := 1; LastCellCount := 1; OldHamming := 1; OldCellCount
    := 1;
  Name := 'Random';
{NapTime:=0;}
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
    for I := 0 to NapTime do Application.ProcessMessages;
  end;

{If (FileTodo = 'Random') then Begin
   Randomize;
     FOR I := 0 TO 155 DO begin
       FOR J := 0 TO 95 DO begin
  If (Trunc(Random(2))=1) then
       Image1.Canvas.Pixels[I+351,J+216]:= Full;
     end;end;
end else begin}

  Name := ExtractFileExt(FileTodo);
  Name := UpperCase(Name);
  if (Name = '.BMP') then begin
    Image1.Picture.LoadFromFile(FileTodo)
  end else begin
    Randomize;
    for I := 0 to 155 do begin
      for J := 0 to 95 do begin
        if (Trunc(Random(2)) = 1) then
          Image1.Canvas.Pixels[I + 351, J + 216] := Full;
      end; end;
  end;
{  ShowMessage('FullColor'+Name+#13#10+FileTodo);}
  Image1.Canvas.Moveto(0, 0);
  Image1.Canvas.Lineto(0, 480);
  Image1.Canvas.Lineto(780, 480);
  Image1.Canvas.Lineto(780, 0);
  Image1.Canvas.Lineto(0, 0);
  Name := ExtractFileName(FileTodo);
{End;}
  LifeForm.Caption := 'Cellular Life: ' + Name;
  Application.ProcessMessages;
{Read the Hamming,Cells,Cycles from the not yet made file...}
  Hamming := 0;
{  CellCount := 0;}

  for W := 3 to 777 do begin
    Str(W, S);
    Panel2.Caption := 'Pixel Life:  ' + S;
    Application.ProcessMessages;
    for V := 3 to 477 do begin
      AnythingThere := Image1.Canvas.Pixels[W, V];
      if ((AnyThingThere = Full) or (AnyThingThere = clWhite)) then
        begin
        AliveW[W] := true;
        AliveV[V] := true;
        Image1.Canvas.Pixels[W, V] := Full;
        Image1.Canvas.Pixels[W, 479] := Full;
      end
      else begin
        Image1.Canvas.Pixels[W, V] := Desolate;
        Image1.Canvas.Pixels[W, 479] := Desolate;
      end;
    end; end;

                            { Blank}
  for V := 0 to 480 do Putting[V] := 0;
 {Putting := Blank; }
  Next := Putting;
  Now := Putting;
  Cycle := 0;

  repeat {2}  begin
      Application.ProcessMessages;
      CYCLE := CYCLE + 1;
      CellCount := 0;
{'INSERT code here for Cellular Automata rules}
      for W := 3 to 777 do begin
        if (AliveW[W + 1] = true) or (AliveW[W] = true)
          or (AliveW[W - 1] = True) or (AliveW[W - 2] = True) then
            begin
          for V := 3 to 477 do begin
            if (AliveV[V + 1] = true) or (AliveV[V] = true)
              or (AliveV[V - 1] = True) or (AliveV[V - 2] = True) then
                begin
              C := 0;
              if (Image1.Canvas.Pixels[W - 1, V - 1] = Full) then
                inc(C);
              if (Image1.Canvas.Pixels[W, V - 1] = Full) then inc(C);
              if (Image1.Canvas.Pixels[W + 1, V - 1] = Full) then
                inc(C);
              if (Image1.Canvas.Pixels[W - 1, V] = Full) then inc(C);
              if (Image1.Canvas.Pixels[W + 1, V] = Full) then inc(C);
              if (Image1.Canvas.Pixels[W - 1, V + 1] = Full) then
                inc(C);
              if (Image1.Canvas.Pixels[W, V + 1] = Full) then inc(C);
              if (Image1.Canvas.Pixels[W + 1, V + 1] = Full) then
                inc(C);
              if C <= 1 {1} then begin NOW[V] := 0; end
        {the trick is if C = 2 then if [W,V] is on it will stay on
        and if off it will stay off}
              else if C = 2 {2} then begin
                if (Image1.Canvas.Pixels[W, V] = Full) then
                  Now[V] := 1 else Now[V] := 2;
              end
              else if C = 3 {3} then begin
                Now[V] := 1;
              end
              else if C >= 4 {4} then begin
                Now[V] := 4; {0;}
              end;
              if Now[V] > 0 then begin
                Inc(CellCount);
                AliveW[W] := true;
                AliveV[V] := true;
              end;
              Image1.Canvas.Pixels[1, V] := Full;
              if (Putting[V] = 1) then
                Image1.Canvas.Pixels[W - 2, V] := Full
              else if (Putting[V] = 2) then
                Image1.Canvas.Pixels[W - 2, V] := Growing
              else if (Putting[V] = 4) then
                Image1.Canvas.Pixels[W - 2, V] := Overpopulated
              else
                {If (Putting[V]=2)then }  Image1.Canvas.Pixels[W - 2, V]
                := Desolate;
            {6-}end; { of Vertical column 479 }
            Image1.Canvas.Pixels[W, 479] := Full;
          {5-}end;
{ Display is after the text so Text can be seen while image is altered }
          Str(Hamming, Hs);
          Str(Cycle, s);
          Str(Cellcount, Cs);
          Panel2.Caption := 'Pixel Life:  ' + 'Hamming:' + Hs +
            ' Cells:' + Cs;
          NumberEdit.Text := {'Cycle } '# ' + s + ' ';
          Application.ProcessMessages;
          Putting := Next;
          Next := Now;
        {4-}end;
      {3-}end; { of Horizontal 639 }


      if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
      begin
        if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
          bIamDone := True;
        LastHamming := OldHamming; LastCellCount := OldCellCount;
      end;
      OldHamming := Hamming;
      OldCellCount := CellCount;
      Hamming := 0;
{CellCount := 0;}

    {2-}end; { of Repeat loop  }

  until (bIamDone = True);
{SAVE ?}
  if MessageDlg('Save the End of Life values into a file ?',
    mtInformation, [mbYes, mbNo], 0) = mrYes then begin
    SaveDialog1.filter := 'Life files|*.bmp';
    SaveDialog1.DefaultExt := 'bmp';
    SaveDialog1.filename := 'NewLife.bmp';
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
  {SAVe the bitmap}
        Image1.Picture.SaveToFile(SaveDialog1.filename)
      end; end; end;
  LifeForm.Caption := 'Cellular Life';
  POLeviBtn.Enabled := True; POGeniBtn.Enabled := True;
end; { of procedure Color Full Pixels }
(********************************************************************)
(********************************************************************)

(********************************************************************)
(********************************************************************)













end.

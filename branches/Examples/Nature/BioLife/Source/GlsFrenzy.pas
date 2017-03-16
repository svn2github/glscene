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

unit GlsFrenzy;
{a bunch of old stuff is hidden at end of file
Hummingbird
Flicker
Yellow Finch
Red Finch
Cardinals
Mockingbird
Woodpecker
Blue Jay
Crow
Duck
Owl
Goose
Hawk
Eagle
}
interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  FileCtrl, StdCtrls, Buttons, ComCtrls, ExtCtrls;

type
  TFrenzyForm = class(TForm)
    FrenzyNameEdit: TEdit;
    FrenzyAddBtn: TBitBtn;
    FrenzyLB: TListBox;
    FrenzyLoadBtn: TBitBtn;
    FrenzySaveBtn: TBitBtn;
    HelpBtn: TBitBtn;
    FrenzyDeletebirdBtn: TBitBtn;
    FileListBox1: TFileListBox;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ClearBtn: TSpeedButton;
    GroupBox1: TGroupBox;
    BirdsColorPanel: TPanel;
    WingColorPanel: TPanel;
    BirdsSizeTB: TTrackBar;
    SizeLabel: TLabel;
    BirdsSpeedTB: TTrackBar;
    SpeedLabel: TLabel;
    HunterBtn: TSpeedButton;
    HunterTextureBtn: TSpeedButton;
    HunterTexEdit: TEdit;
    ColorDialog1: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure ReallyClose;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FrenzyAddBtnClick(Sender: TObject);
    procedure FrenzyDeletebirdBtnClick(Sender: TObject);
    procedure FrenzyLoadBtnClick(Sender: TObject);
    procedure FrenzySaveBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure BirdsSpeedTBChange(Sender: TObject);
    procedure BirdsSizeTBChange(Sender: TObject);
    procedure BirdsColorPanelClick(Sender: TObject);
    procedure HunterTextureBtnClick(Sender: TObject);
    procedure HunterBtnClick(Sender: TObject);
  private
     
  public
     
  end;

var
  FrenzyForm: TFrenzyForm;

implementation
uses nUGlobal;
{$R *.DFM}

procedure TFrenzyForm.FormCreate(Sender: TObject);
begin
  top := FrenzyFormY;
  left := FrenzyFormX;
  FrenzyFilesLoaded := False;
  FrenzyLB.Clear;
end;

procedure TFrenzyForm.FormShow(Sender: TObject);
var FStr: string;
begin
  FStr := ExtractFileDrive(BirdLifeDir);
  if length(FStr) = 1 then DriveComboBox1.Drive := FStr[1];
end;

procedure TFrenzyForm.ReallyClose;
begin
  Close;
end;

procedure TFrenzyForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  FrenzyFormY := FrenzyForm.top;
  FrenzyFormX := FrenzyForm.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TFrenzyForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(8100);
end;
             
procedure TFrenzyForm.ClearBtnClick(Sender: TObject);
begin
  FrenzyLB.Clear;
  FrenzyFilesLoaded := False;
end;

procedure TFrenzyForm.FrenzyAddBtnClick(Sender: TObject);
begin
  if FileListBox1.Itemindex > -1 then
  begin
    FrenzyLB.Items.Add(ExtractFileName(FileListBox1.Items.Strings[FileListBox1.Itemindex]));
    FrenzyFilesLoaded := True;
  end else showmessage('Select a file first');
end;

procedure TFrenzyForm.FrenzyDeletebirdBtnClick(Sender: TObject);
begin
  if FrenzyLB.Itemindex > -1 then begin
    FrenzyLB.Items.Delete(FrenzyLB.Itemindex);
  end else showmessage('Select a file first');
  if (FrenzyForm.FrenzyLB.Items.Count = 0) then
    FrenzyFilesLoaded := False;
end;

procedure TFrenzyForm.FrenzyLoadBtnClick(Sender: TObject);
var
  F: Textfile;
  FStr: string;
begin
  OpenDialog1.Title := 'Frenzy Files';
  OpenDialog1.Filter := 'Frenzy (*.fnz)|*.fnz';
  OpenDialog1.InitialDir := BirdLifeDir;
  OpenDialog1.Filename := FrenzyNameEdit.Text;
  if OpenDialog1.Execute then begin
    if ((FileExists(OpenDialog1.Filename))
      and (lowercase(ExtractFileExt(OpenDialog1.Filename)) = '.fnz'))
      then
    begin
      FrenzyNameEdit.Text := ExtractFileName(OpenDialog1.Filename);
{      FrenzyFilesName := FrenzyNameEdit.Text;}
      AssignFile(F, OpenDialog1.Filename);
      Reset(F);
      while (not eof(F)) do
      begin
        Readln(F, FStr);
        FrenzyLB.Items.Add(FStr);
      end;
      CloseFile(F);
      FrenzyFilesLoaded := True;
    end;
  end;
end;

procedure TFrenzyForm.FrenzySaveBtnClick(Sender: TObject);
var
  F: Textfile;
  i: Integer;
  FStr: string;
begin
  if (FrenzyLB.Items.Count > 0) then
  begin
    SaveDialog1.Title := 'Frenzy Files';
    SaveDialog1.Filter := 'Frenzy (*.fnz)|*.fnz';
    SaveDialog1.InitialDir := BirdLifeDir;
    SaveDialog1.Filename := FrenzyNameEdit.Text;
    if SaveDialog1.Execute then begin
      if ((lowercase(ExtractFileExt(SaveDialog1.Filename)) = '.fnz'))
        then
      begin {Load}
        AssignFile(F, SaveDialog1.Filename);
        Rewrite(F);
        for i := 0 to (FrenzyLB.Items.Count - 1) do begin
          FStr := FrenzyLB.Items.Strings[i];
          writeln(F, FStr);
        end;
        CloseFile(F);
        FrenzyFilesLoaded := True;
{        FrenzyFilesName := FrenzyNameEdit.Text;}
      end;
    end;
  end else showmessage('Select a file first');
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TFrenzyForm.BirdsSpeedTBChange(Sender: TObject);
begin
  SpeedLabel.Caption := Format('Speed %d ', [BirdsSpeedTB.Position]);
end;

procedure TFrenzyForm.BirdsSizeTBChange(Sender: TObject);
begin
  SizeLabel.Caption := Format('Size %d ', [BirdsSizeTB.Position]);
end;

procedure TFrenzyForm.BirdsColorPanelClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
    BirdsColorPanel.Color := ColorDialog1.Color;
end;

procedure TFrenzyForm.HunterTextureBtnClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Hunter Files';
  OpenDialog1.Filter := 'Hunter Texture (*.bmp)|*.bmp';
  OpenDialog1.InitialDir := BirdLifeDir;
  OpenDialog1.Filename := FrenzyNameEdit.Text;
  if OpenDialog1.Execute then
   HunterTexEdit.Text := ExtractFileName(OpenDialog1.Filename);
end;

procedure TFrenzyForm.HunterBtnClick(Sender: TObject);
var F: Textfile;
begin
  AssignFile(F, ExtractFilePath(ParamStr(0))+'hunter.hk');
  Rewrite(F);
  writeln(F, Inttostr(WingColorPanel.Color));
  writeln(F, Inttostr(BirdsColorPanel.Color));
  writeln(F, Inttostr(BirdsSizeTB.Position));
  writeln(F, Inttostr(BirdsSpeedTB.Position));
  writeln(F, HunterTexEdit.Text);
  CloseFile(F);
end;
{
  inc(CurrentFrenzy);
  inc(FrenzyCount);
  FlockEdit.Text:= inttostr(CurrentFrenzy);
  SetLength(BirdBrainArray, FrenzyCount);

    SetLength(BirdProxies[CurrentFrenzy], HowMany);
    setlength(boidArray[CurrentFrenzy],HowMany);  
}
{Calculate other trigonometric functions using
  Sin, Cos, and ArcTan in the following expressions:
Tan(x) = Sin(x) / Cos(x)
ArcSin(x) = ArcTan (x/sqrt (1-sqr (x)))
ArcCos(x) = ArcTan (sqrt (1-sqr (x)) /x)  }
{  Direction.x  := arccos(t); }
{  Direction.y  := cos(t);
  Direction.z  := sin(t);}
{  BirdYardWide := GLSceneViewer1.Width div FlyingAreaSize;
  BirdYardHeight := GLSceneViewer1.Height div FlyingAreaSize;}
(*
  Movecenter,CenterBias,MoveAvgvelocity,AvgvelBias,chilling,{MoveBoid}
  chill, bigchill, {BoidChillOut}
  PercAvgvel, {BoidAvVel}
  PercCenter: Vec;  {BoidPerceiveCenter}
*)
{  RealCenter, RealAvgvel: Vec; }{CentersinnStartup}
(*
    procedure MoveFFBoid(WhichBoid: Integer);
    procedure BoidFPerceiveCenter(WhichBoid: Integer);
    procedure BoidFAvVel(WhichBoid: Integer);
    procedure BoidFChillOut(WhichBoid: Integer);

{ Move this boid, wrt allboids }
procedure TAAABirdForm.MoveFFBoid(WhichBoid: Integer);
var
  m: Integer;
  f: double;
begin
  if ((boidArray[CurrentFrenzy, WhichBoid].perching) and
    (boidArray[CurrentFrenzy, WhichBoid].perchtimer > 0)) then
    dec(boidArray[CurrentFrenzy, WhichBoid].perchtimer) else
    if ((boidArray[CurrentFrenzy, WhichBoid].Hungry) and
      (boidArray[CurrentFrenzy, WhichBoid].Hungrytimer > 0)) then
      dec(boidArray[CurrentFrenzy, WhichBoid].Hungrytimer) else
    begin
      boidArray[CurrentFrenzy, WhichBoid].perching := False;
      boidArray[CurrentFrenzy, WhichBoid].Hungry := False;
      BoidFeed(WhichBoid);
      BoidFPerceiveCenter(WhichBoid);
      BoidFAvVel(WhichBoid);
      BoidFChillOut(WhichBoid);

{   vecadd(boid[WhichBoid].vel, CenterBias);}
      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + CenterBias.x;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + CenterBias.y;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + CenterBias.z;
{   vecadd(boid[WhichBoid].vel, AvgvelBias);}
      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + AvgvelBias.x;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + AvgvelBias.y;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + AvgvelBias.z;
{   vecadd(boid[WhichBoid].vel, chilling);}
      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + chilling.x;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + chilling.y;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + chilling.z;

      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + Feeding.x;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + Feeding.y;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + Feeding.z;

{   veclimit(boid[WhichBoid].vel, 100);}
      m := max(round(abs(boidArray[CurrentFrenzy, WhichBoid].vel.x)),
               round(abs(boidArray[CurrentFrenzy, WhichBoid].vel.y)));
      m := max(m, round(abs(boidArray[CurrentFrenzy,
                   WhichBoid].vel.z)));
      if (m > BirdBrainArray[CurrentFrenzy].BirdsSpeed) then begin
        f := BirdBrainArray[CurrentFrenzy].BirdsSpeed / m;
        boidArray[CurrentFrenzy, WhichBoid].vel.x :=
          boidArray[CurrentFrenzy, WhichBoid].vel.x * f;
        boidArray[CurrentFrenzy, WhichBoid].vel.y :=
          boidArray[CurrentFrenzy, WhichBoid].vel.y * f;
        boidArray[CurrentFrenzy, WhichBoid].vel.z :=
          boidArray[CurrentFrenzy, WhichBoid].vel.z * f;
      end;
{   vecadd(boid[WhichBoid].pos, boid[WhichBoid].vel);}
      boidArray[CurrentFrenzy, WhichBoid].pos.x :=
        boidArray[CurrentFrenzy, WhichBoid].pos.x +
        boidArray[CurrentFrenzy, WhichBoid].vel.x;
      boidArray[CurrentFrenzy, WhichBoid].pos.y :=
        boidArray[CurrentFrenzy, WhichBoid].pos.y +
        boidArray[CurrentFrenzy, WhichBoid].vel.y;
      boidArray[CurrentFrenzy, WhichBoid].pos.z :=
        boidArray[CurrentFrenzy, WhichBoid].pos.z +
        boidArray[CurrentFrenzy, WhichBoid].vel.z;

{   /* bound world */}{BirdsWideDX    BirdsHeightDX}
      if (boidArray[CurrentFrenzy, WhichBoid].pos.x < -BirdYardWide)
        then
          boidArray[CurrentFrenzy, WhichBoid].vel.x :=
          boidArray[CurrentFrenzy, WhichBoid].vel.x + 1
      else
        if (boidArray[CurrentFrenzy, WhichBoid].pos.x > BirdYardWide)
          then
            boidArray[CurrentFrenzy, WhichBoid].vel.x :=
            boidArray[CurrentFrenzy, WhichBoid].vel.x - 1;

      if (boidArray[CurrentFrenzy, WhichBoid].pos.y < -BirdYardHeight)
        then
          boidArray[CurrentFrenzy, WhichBoid].vel.y :=
          boidArray[CurrentFrenzy, WhichBoid].vel.y + 1
      else
        if (boidArray[CurrentFrenzy, WhichBoid].pos.y > BirdYardHeight)
          then
            boidArray[CurrentFrenzy, WhichBoid].vel.y :=
            boidArray[CurrentFrenzy, WhichBoid].vel.y - 1;
{/* Hit ground!! */
BirdsHeightGround must be < -BirdYardHeight}
{      if (boidArray[CurrentFrenzy, WhichBoid].pos.y < BirdsHeightGround)
        then
      begin
        boidArray[CurrentFrenzy, WhichBoid].pos.y := BirdYardHeight;

        boidArray[CurrentFrenzy, WhichBoid].perching := True;
        boidArray[CurrentFrenzy, WhichBoid].perchtimer :=
          (Random(20) +
            BirdBrainArray[CurrentFrenzy].BirdsActivity);
        boidArray[CurrentFrenzy, WhichBoid].vel.y := 0;
      end;}

      if (boidArray[CurrentFrenzy, WhichBoid].pos.z < BirdYardWide)
        then
        boidArray[CurrentFrenzy, WhichBoid].vel.z :=
          boidArray[CurrentFrenzy, WhichBoid].vel.z + 1
      else
        if (boidArray[CurrentFrenzy, WhichBoid].pos.z > (BirdYardWide))
          then
          boidArray[CurrentFrenzy, WhichBoid].vel.z :=
            boidArray[CurrentFrenzy, WhichBoid].vel.z - 1;
      {boidFperspective(WhichBoid);}
    end; {Perching}
end; {Procedure}

procedure TAAABirdForm.BoidFPerceiveCenter(WhichBoid: Integer);
{(Boid boid, Vec real_cent, int numboids)}
begin
  CenterBias.x := 0; CenterBias.y := 0; CenterBias.z := 0;
  PercCenter.x := 0; PercCenter.y := 0; PercCenter.z := 0;
{   vecdiff(RealCenter, boid[WhichBoid].pos, PercCenter); }
  PercCenter.x := BirdBrainArray[CurrentFrenzy].RealCenter.x -
    boidArray[CurrentFrenzy, WhichBoid].pos.x;
  PercCenter.y := BirdBrainArray[CurrentFrenzy].RealCenter.y -
    boidArray[CurrentFrenzy, WhichBoid].pos.y;
  PercCenter.z := BirdBrainArray[CurrentFrenzy].RealCenter.z -
    boidArray[CurrentFrenzy, WhichBoid].pos.z;
{   vecsdiv(PercCenter, (HowManyBirds-1));}
  PercCenter.x := PercCenter.x /
    (BirdBrainArray[CurrentFrenzy].BirdsCount - 1); {HowManyBirds-1;}
  PercCenter.y := PercCenter.y /
    (BirdBrainArray[CurrentFrenzy].BirdsCount - 1);
  PercCenter.z := PercCenter.z /
    (BirdBrainArray[CurrentFrenzy].BirdsCount - 1);
{   BoidPerceiveCenter:= PercCenter;}
  Movecenter.x := PercCenter.x;
  Movecenter.y := PercCenter.y;
  Movecenter.z := PercCenter.z;
{   vecdiff(Movecenter, boid[WhichBoid].pos, CenterBias);}
  CenterBias.x := Movecenter.x - boidArray[CurrentFrenzy,
    WhichBoid].pos.x;
  CenterBias.y := Movecenter.y - boidArray[CurrentFrenzy,
    WhichBoid].pos.y;
  CenterBias.z := Movecenter.z - boidArray[CurrentFrenzy,
    WhichBoid].pos.z;
{   vecrshift(CenterBias, DefaultCenterBias  BirdsGlideDX);}
  CenterBias.x := ((CenterBias.x) *
    BirdBrainArray[CurrentFrenzy].dWCentroid);
  CenterBias.y := ((CenterBias.y) *
    BirdBrainArray[CurrentFrenzy].dWCentroid);
  CenterBias.z := ((CenterBias.z) *
    BirdBrainArray[CurrentFrenzy].dWCentroid);
end;


procedure TAAABirdForm.BoidFAvVel(WhichBoid: Integer);
{Vec boid_av_vel(Boid boid, Vec real_avgvel, int numboids)}
begin
  PercAvgvel.x := 0; PercAvgvel.y := 0; PercAvgvel.z := 0;
  AvgvelBias.x := 0; AvgvelBias.y := 0; AvgvelBias.z := 0;
{   vecdiff(RealAvgvel, boid[WhichBoid].vel, PercAvgvel);}
  PercAvgvel.x := BirdBrainArray[CurrentFrenzy].RealAvgvel.x
    - boidArray[CurrentFrenzy, WhichBoid].vel.x;
  PercAvgvel.y := BirdBrainArray[CurrentFrenzy].RealAvgvel.y
    - boidArray[CurrentFrenzy, WhichBoid].vel.y;
  PercAvgvel.z := BirdBrainArray[CurrentFrenzy].RealAvgvel.z
    - boidArray[CurrentFrenzy, WhichBoid].vel.z;
{   vecsdiv(PercAvgvel, (HowManyBirds-1));}
  PercAvgvel.x := PercAvgvel.x /
    (BirdBrainArray[CurrentFrenzy].BirdsCount - 1);
  PercAvgvel.y := PercAvgvel.y /
    (BirdBrainArray[CurrentFrenzy].BirdsCount - 1);
  PercAvgvel.z := PercAvgvel.z /
    (BirdBrainArray[CurrentFrenzy].BirdsCount - 1);
{   BoidAvVel:= PercAvgvel;
   MoveAvgvelocity := BoidAvVel(WhichBoid);}
  MoveAvgvelocity.x := PercAvgvel.x;
  MoveAvgvelocity.y := PercAvgvel.y;
  MoveAvgvelocity.z := PercAvgvel.z;
{   vecdiff(MoveAvgvelocity, boid[WhichBoid].vel, AvgvelBias);}
  AvgvelBias.x := MoveAvgvelocity.x - boidArray[CurrentFrenzy,
    WhichBoid].vel.x;
  AvgvelBias.y := MoveAvgvelocity.y - boidArray[CurrentFrenzy,
    WhichBoid].vel.y;
  AvgvelBias.z := MoveAvgvelocity.z - boidArray[CurrentFrenzy,
    WhichBoid].vel.z;
{   vecrshift(AvgvelBias, DefaultAvgVelocity  BirdsEnergyDX);}
  AvgvelBias.x := ((AvgvelBias.x) *
    BirdBrainArray[CurrentFrenzy].dWCopy);
  AvgvelBias.y := ((AvgvelBias.y) *
    BirdBrainArray[CurrentFrenzy].dWCopy);
  AvgvelBias.z := ((AvgvelBias.z) *
    BirdBrainArray[CurrentFrenzy].dWCopy);
end;

procedure TAAABirdForm.BoidFChillOut(WhichBoid: Integer);
{Vec boid_chill_out(Boid boid, Boid boids[], int numboids)}
var
  i: Integer;
  dx, dy, dz, dm: double;
begin
  chill.x := 0; chill.y := 0; chill.z := 0;
  bigchill.x := 0; bigchill.y := 0; bigchill.z := 0;
  for i := 0 to (BirdBrainArray[CurrentFrenzy].BirdsCount - 1) do begin
    if (i <> WhichBoid) then begin
      dx := boidArray[CurrentFrenzy, WhichBoid].pos.x -
        boidArray[CurrentFrenzy, i].pos.x;
      dy := boidArray[CurrentFrenzy, WhichBoid].pos.y -
        boidArray[CurrentFrenzy, i].pos.y;
      dz := boidArray[CurrentFrenzy, WhichBoid].pos.z -
        boidArray[CurrentFrenzy, i].pos.z;
      if (abs(dx)) > (abs(dy)) then dm := abs(dx) else dm := abs(dy);
      if (abs(dz)) > (dm) then dm := abs(dz);
      if (dm <= BirdBrainArray[CurrentFrenzy].RAvoid)
{	 if(vecdist(boid[WhichBoid].pos, boid[i].pos) <= ChillFactor)}
      then begin
     {vecdiff(boid[WhichBoid].pos, boid[i].pos, chill);}
        chill.x := boidArray[CurrentFrenzy, WhichBoid].pos.x -
          boidArray[CurrentFrenzy, i].pos.x;
        chill.y := boidArray[CurrentFrenzy, WhichBoid].pos.y -
          boidArray[CurrentFrenzy, i].pos.y;
        chill.z := boidArray[CurrentFrenzy, WhichBoid].pos.z -
          boidArray[CurrentFrenzy, i].pos.z;
{	    vecadd(bigchill, chill); }
        bigchill.x := bigchill.x + chill.x;
        bigchill.y := bigchill.y + chill.y;
        bigchill.z := bigchill.z + chill.z;
      end;
    end;
  end;
  chilling.x := bigchill.x;
  chilling.y := bigchill.y;
  chilling.z := bigchill.z;
 {   vecrshift(chilling, DefaultChilling);}{CHANGE THIS????}
  chilling.x := ((chilling.x) *
    BirdBrainArray[CurrentFrenzy].dWAvoid);
  chilling.y := ((chilling.y) *
    BirdBrainArray[CurrentFrenzy].dWAvoid);
  chilling.z := ((chilling.z) *
    BirdBrainArray[CurrentFrenzy].dWAvoid);
end;
*)

(*
procedure TAAABirdForm.SetBirdsDatatoArray;
begin
{BirdBrainArray[BirdNameCB.ItemIndex].BirdName:=BirdNameCB.Items[BirdNameCB.ItemIndex];}
  BirdBrainArray[CurrentFlock].BirdsColor := BirdsColor;
  BirdBrainArray[CurrentFlock].BirdsWingColor := BirdWingColor;
  BirdBrainArray[CurrentFlock].BirdName := CurrentBirdName;
  BirdBrainArray[CurrentFlock].CurrentBirdSong := CurrentBirdSong;
  BirdBrainArray[CurrentFlock].BirdsCount := BirdsCount;
  BirdBrainArray[CurrentFlock].dBirdsFeed := dBirdsFeed;
  BirdBrainArray[CurrentFlock].BirdsCount := BirdsCount;
  BirdBrainArray[CurrentFlock].dBirdsMinV := dBirdsMinV;
  BirdBrainArray[CurrentFlock].dBirdsMoment := dBirdsMoment;
  BirdBrainArray[CurrentFlock].dBirdsView := dBirdsView;
  BirdBrainArray[CurrentFlock].dBirdsAvoid := dBirdsAvoid;
  BirdBrainArray[CurrentFlock].RCopy := RCopy;
  BirdBrainArray[CurrentFlock].dWCopy := dWCopy;
  BirdBrainArray[CurrentFlock].RCentroid := RCentroid;
  BirdBrainArray[CurrentFlock].dWCentroid := dWCentroid;
  BirdBrainArray[CurrentFlock].RVisual := RVisual;
  BirdBrainArray[CurrentFlock].dWVisual := dWVisual;
  BirdBrainArray[CurrentFlock].RAvoid := RAvoid;
  BirdBrainArray[CurrentFlock].dWAvoid:= dWAvoid;
  BirdBrainArray[CurrentFlock].BirdsTenacity := BirdsTenacity;
  BirdBrainArray[CurrentFlock].BirdsFerocity := BirdsFerocity;
  BirdBrainArray[CurrentFlock].BirdsActivity := BirdsActivity;
  BirdBrainArray[CurrentFlock].dBirdsGlide := dBirdsGlide;
  BirdBrainArray[CurrentFlock].dBirdsEnergy := dBirdsEnergy;
  BirdBrainArray[CurrentFlock].BirdsSpeed := BirdsSpeed;
{Randomize initial positions}
{BirdsXP,BirdsYP,BirdsZP,
BirdsXV,BirdsYV,BirdsZV,
BirdsXnV,BirdsYnV,BirdsZnV}
end;*)


end.

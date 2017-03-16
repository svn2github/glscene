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
unit nGRF;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TGRFForm = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    GrassNEdit: TEdit;
    Label3: TLabel;
    GrassEEdit: TEdit;
    Label4: TLabel;
    GrassGMinEdit: TEdit;
    GrassGMaxEdit: TEdit;
    Label6: TLabel;
    Label5: TLabel;
    GrassGEEdit: TEdit;
    Bevel2: TBevel;
    Label7: TLabel;
    Label8: TLabel;
    RabbitNEdit: TEdit;
    RabbitEEdit: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    RabbitEMEdit: TEdit;
    Bevel3: TBevel;
    FoxEMEdit: TEdit;
    Label11: TLabel;
    FoxEEdit: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    FoxNEdit: TEdit;
    Label14: TLabel;
    GRFCheckBox: TCheckBox;
    GRFImageSizeRG: TRadioGroup;
    GRFHelpBtn: TBitBtn;
    GRFOKBtn: TBitBtn;
    RabbitMPEdit: TEdit;
    FoxMPEdit: TEdit;
    GrassMPEdit: TEdit;
    Label2: TLabel;
    CancelBitBtn: TBitBtn;
    procedure ReallyClose;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
{procedure ShowForm(IsFormShowing:Boolean);}
    procedure GRFHelpBtnClick(Sender: TObject);
    procedure GRFOKBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CancelBitBtnClick(Sender: TObject);
  private
     
  public
     
  end;

var
  GRFForm: TGRFForm;
type
  TLifeCell = record
    What: Short; {Byte?}
    Mark: Short;
    Energy: Integer;
  end;

const
  Empty = 0;
  Grass = 1;
  Rabbit = 2;
  Fox = 3;
  NewGrass = 4;
{Dir:Array[0..7,0..1] of Integer=((0,1),(1,1),(1,0),(1,-1),
                           (0,-1),(-1,-1),(-1,0),(-1,1));}

var
  RabbitsRunning: Boolean;
  WorldArray: array of array of TLifeCell;
  GRFWide, GRFHeight: Integer;
  Grasses, GrassEnergy, GrassGE, GrassGMin, GrassGMax,
    Rabbits, RabbitEnergy, RabbitEMove,
    Foxes, FoxEnergy, FoxEMove: Integer;

  GrassMP, RabbitMP, FoxMP: Double;

implementation
uses nUGlobal, nGSRShow;
{$R *.DFM}



procedure TGRFForm.ReallyClose;
begin
  RabbitsRunning := False;
  SetLength(WorldArray, 0, 0);
  Close;
end;
procedure TGRFForm.CancelBitBtnClick(Sender: TObject);
begin
  RabbitsRunning := False;
  GRFOKBtn.Enabled := True;
  Close;
end;
procedure TGRFForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  GRFFormY := GRFForm.top;
  GRFFormX := GRFForm.left;
  RabbitsRunning := False;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TGRFForm.FormActivate(Sender: TObject);
begin
{ShowForm(True);} RabbitsRunning := False;
end; (*
procedure TGRFForm.ShowForm(IsFormShowing:Boolean);
Begin
  RabbitsRunning:=(not IsFormShowing);
{  Image1.Visible:=(not IsFormShowing);
   GrassNEdit.Visible:=IsFormShowing;
   GrassEEdit.Visible:=IsFormShowing;
   GrassGEEdit.Visible:=IsFormShowing;
   GrassGMinEdit.Visible:=IsFormShowing;
   GrassGMaxEdit.Visible:=IsFormShowing;
   RabbitNEdit.Visible:=IsFormShowing;
   RabbitEEdit.Visible:=IsFormShowing;
   RabbitEMEdit.Visible:=IsFormShowing;
   GRFCheckBox.Visible:=IsFormShowing;
   GRFHelpBtn.Visible:=IsFormShowing;
   GRFOKBtn.Visible:=IsFormShowing;
   FoxNEdit.Visible:=IsFormShowing;
   FoxEEdit.Visible:=IsFormShowing;
   FoxEMEdit.Visible:=IsFormShowing;
   GRFImageSizeRG.Visible:=IsFormShowing;}
End;                  *)

procedure TGRFForm.FormCreate(Sender: TObject);
begin
  top := GRFFormY;
  left := GRFFormX;
  RabbitsRunning := False;
end;

procedure TGRFForm.FormResize(Sender: TObject);
begin
  GRFForm.Width := 401;
  GRFForm.Height := 268;
end;

procedure TGRFForm.GRFHelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(5000);
end;

procedure TGRFForm.GRFOKBtnClick(Sender: TObject);
var
  OKtogo: Boolean;
  CodeVx: Integer;
  CheckString: string;
begin
  OKtogo := True;
{Translate the Edits into Variables}
  val(GrassNEdit.Text, Grasses, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(GrassNEdit.Text, CodeVx); end;
  val(GrassEEdit.Text, GrassEnergy, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(GrassEEdit.Text, CodeVx); end;
  val(GrassGEEdit.Text, GrassGE, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(GrassGEEdit.Text, CodeVx); end;
  val(GrassGMinEdit.Text, GrassGMin, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(GrassGMinEdit.Text, CodeVx); end;
  val(GrassGMaxEdit.Text, GrassGMax, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(GrassGMaxEdit.Text, CodeVx); end;
  val(RabbitNEdit.Text, Rabbits, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(RabbitNEdit.Text, CodeVx); end;
  val(RabbitEEdit.Text, RabbitEnergy, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(RabbitEEdit.Text, CodeVx); end;
  val(RabbitEMEdit.Text, RabbitEMove, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(RabbitEMEdit.Text, CodeVx); end;
  val(FoxNEdit.Text, Foxes, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(FoxNEdit.Text, CodeVx); end;
  val(FoxEEdit.Text, FoxEnergy, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(FoxEEdit.Text, CodeVx); end;
  val(FoxEMEdit.Text, FoxEMove, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(FoxEMEdit.Text, CodeVx); end;
  val(GrassMPEdit.Text, GrassMP, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(GrassMPEdit.Text, CodeVx); end;
  val(RabbitMPEdit.Text, RabbitMP, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(RabbitMPEdit.Text, CodeVx); end;
  val(FoxMPEdit.Text, FoxMP, CodeVx);
  if (CodeVx > 0) then
    begin OKtogo := False; Codefx(FoxMPEdit.Text, CodeVx); end;
  if OKtogo then
  begin
    case GRFImageSizeRG.ItemIndex of
{393x241 (98x60)
785x481 (196x120)
640x480 [120x90]
800x600 [150x120]}
      0: begin
          GRFWide := 98; {403}
          GRFHeight := 60; {268}
        end;
{  GSRShowForm.Width := (GRFWide*4)+11;
  GSRShowForm.Height := (GRFHeight*4)+28;}
      1: begin
          GRFWide := 157; {639}
          GRFHeight := 113; {480}
        end;
      2: begin
          GRFWide := 197; {799}
          GRFHeight := 143; {600}
        end;
    end; {case}

    if (Grasses > ((GRFWide * GRFHeight) div Round(100 / GrassMP)))
      then
        Grasses := ((GRFWide * GRFHeight) div Round(100 / GrassMP));
    str(Grasses, CheckString);
    GrassNEdit.Text := CheckString;
    if (Rabbits > ((GRFWide * GRFHeight) div Round(100 / RabbitMP)))
      then
        Rabbits := ((GRFWide * GRFHeight) div Round(100 / RabbitMP));
    str(Rabbits, CheckString);
    RabbitNEdit.Text := CheckString;
    if (Foxes > ((GRFWide * GRFHeight) div Round(100 / FoxMP)))
      then Foxes := ((GRFWide * GRFHeight) div Round(100 / FoxMP));
    str(Foxes, CheckString);
    FoxNEdit.Text := CheckString;
    GRFOKBtn.Enabled := False;
    Application.ProcessMessages;
    GSRShowForm.Show;
    GSRShowForm.MakeSallyDance;
  end; {OKtogo}
end;




end.

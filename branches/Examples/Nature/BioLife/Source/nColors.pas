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
unit nColors;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TColorsForm = class(TForm)
    DesolatePanel: TPanel;
    GrowingPanel: TPanel;
    FullPanel: TPanel;
    OverpopulatedPanel: TPanel;
    OverpopulatedBtn: TSpeedButton;
    FullBtn: TSpeedButton;
    GrowingBtn: TSpeedButton;
    DesolateBtn: TSpeedButton;
    ReloadBtn: TBitBtn;
    DefaultsBtn: TBitBtn;
    SaveBtn: TBitBtn;
    ColorDialog: TColorDialog;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    HelpBitBtn: TBitBtn;
    procedure DesolateBtnClick(Sender: TObject);
    procedure GrowingBtnClick(Sender: TObject);
    procedure FullBtnClick(Sender: TObject);
    procedure OverpopulatedBtnClick(Sender: TObject);
    procedure ReloadBtnClick(Sender: TObject);
    procedure DefaultsBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReallyClose;
    procedure HelpBitBtnClick(Sender: TObject);
  private
     
  public
     
    procedure LoadColors;
  end;

var
  ColorsForm: TColorsForm;

implementation

{$R *.DFM}

uses nUGlobal;

procedure TColorsForm.FormCreate(Sender: TObject);
begin
{Set the X,Y}
  top := ColorsFormY;
  left := ColorsFormX;
end;

procedure TColorsForm.ReallyClose;
begin
  Close;
end;

procedure TColorsForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
{Get the X,Y}
  ColorsFormY := ColorsForm.top;
  ColorsFormX := ColorsForm.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TColorsForm.FormActivate(Sender: TObject);
begin
  DesolatePanel.Color := Desolate;
  GrowingPanel.Color := Growing;
  FullPanel.Color := Full;
  OverpopulatedPanel.Color := Overpopulated;
end;


procedure TColorsForm.DesolateBtnClick(Sender: TObject);
begin
{DesolatePanel    ColorDialog}
  if (HasTrueColors = False) then
    ColorDialog.Options := [cdPreventFullOpen];
  if ColorDialog.Execute then begin
    Desolate := ColorDialog.Color;
    DesolatePanel.Color := Desolate;
  end;
end;

procedure TColorsForm.GrowingBtnClick(Sender: TObject);
begin
{GrowingPanel}
  if (HasTrueColors = False) then
    ColorDialog.Options := [cdPreventFullOpen];
  if ColorDialog.Execute then begin
    Growing := ColorDialog.Color;
    GrowingPanel.Color := Growing;
  end;
end;

procedure TColorsForm.FullBtnClick(Sender: TObject);
begin
   {FullPanel}
  if (HasTrueColors = False) then
    ColorDialog.Options := [cdPreventFullOpen];
  if ColorDialog.Execute then begin
    Full := ColorDialog.Color;
    FullPanel.Color := Full;
  end;
end;

procedure TColorsForm.OverpopulatedBtnClick(Sender: TObject);
begin
   {OverpopulatedPanel.Color:= }
  if (HasTrueColors = False) then
    ColorDialog.Options := [cdPreventFullOpen];
  if ColorDialog.Execute then begin
    Overpopulated := ColorDialog.Color;
    OverpopulatedPanel.Color := Overpopulated;
  end;
end;

procedure TColorsForm.ReloadBtnClick(Sender: TObject);
begin
  LoadColors;
end;

procedure TColorsForm.DefaultsBtnClick(Sender: TObject);
begin
  {Desolate Growing Full Overpopulated}
  Desolate := clSilver; DesolatePanel.Color := Desolate;
  Growing := clYellow; GrowingPanel.Color := Growing;
  Full := clGreen; FullPanel.Color := Full;
  Overpopulated := clRed; OverpopulatedPanel.Color := Overpopulated;
end;

procedure TColorsForm.SaveBtnClick(Sender: TObject);
var fil: file of Tcolor; i: Integer;
begin {}
  AssignFile(fil, 'L_colors.clr');
{$I-}rewrite(fil); {$I+}
  i := IOresult;
  if i <> 0 then MessageDlg('L_colors.clr File not found',
      mtInformation, [mbOk], 0) else
  begin
    Write(fil, Desolate);
    Write(fil, Growing);
    Write(fil, Full);
    Write(fil, Overpopulated);
    CloseFile(fil);
  end;
end;


procedure TColorsForm.LoadColors;
var fil: file of TColor; i: Integer;
begin
  AssignFile(fil, 'L_colors.clr');
{$I-}reset(fil); {$I+}
  i := IOresult;
  if i <> 0 then begin
    MessageDlg('L_colors.clr File not found,loading default colors',
      mtInformation, [mbOk], 0);
    Desolate := clSilver;
    Growing := clYellow;
    Full := clGreen;
    Overpopulated := clRed;
  end else
  begin
    read(fil, Desolate);
    read(fil, Growing);
    read(fil, Full);
    read(fil, Overpopulated);
    CloseFile(fil);
  end;
end;






procedure TColorsForm.HelpBitBtnClick(Sender: TObject);
begin
  Application.HelpContext(3000);
end;

end.

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
unit nTorso;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TDXTorsoForm = class(TForm)
    PageControl1: TPageControl;
    TorsoTS: TTabSheet;
    TRHand: TShape;
    TRElbow: TShape;
    TTorso: TShape;
    TRArm: TShape;
    THead: TShape;
    TLArm: TShape;
    TLElbow: TShape;
    TLHand: TShape;
    TLLeg: TShape;
    TRLeg: TShape;
    TRCalf: TShape;
    TLCalf: TShape;
    TLFoot: TShape;
    TRFoot: TShape;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label97: TLabel;
    Label94: TLabel;
    Label96: TLabel;
    Label98: TLabel;
    Label95: TLabel;
    TorsoClear: TSpeedButton;
    TorsoFileOpen: TSpeedButton;
    TorsoFileSave: TSpeedButton;
    TorsoMouse: TSpeedButton;
    TorsoOK: TSpeedButton;
    TorsoXEdit: TEdit;
    TorsoYEdit: TEdit;
    TorsoZxEdit: TEdit;
    TorsoZyEdit: TEdit;
    TorsoiEdit: TEdit;
    TorsohEdit: TEdit;
    TorsodEdit: TEdit;
    TorsorEdit: TEdit;
    TorsoFileName: TEdit;
    TorsoHelp: TBitBtn;
    ControlTS: TTabSheet;
    SpeedButton9: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton7: TSpeedButton;
    GRFOKBtn: TBitBtn;
    HelpBtn: TSpeedButton;
    CancelBitBtn: TBitBtn;
    Image1: TImage;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    procedure FormCreate(Sender: TObject);
    procedure ReallyClose;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBitBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure GRFOKBtnClick(Sender: TObject);
    procedure TorsoHelpClick(Sender: TObject);
  private
     
  public
     
  end;

var
  DXTorsoForm: TDXTorsoForm;
  DXTorsoClor: Boolean;
implementation
uses nUGlobal;
{$R *.DFM}

procedure TDXTorsoForm.FormCreate(Sender: TObject);
begin
  top := DXTorsoFormY;
  left := DXTorsoFormX;
end;

procedure TDXTorsoForm.ReallyClose;
begin
  Close;
end;

procedure TDXTorsoForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  DXTorsoFormY := DXTorsoForm.top;
  DXTorsoFormX := DXTorsoForm.left;
  DXTorsoClor := True;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TDXTorsoForm.CancelBitBtnClick(Sender: TObject);
begin
  DXTorsoClor := True;
end;

procedure TDXTorsoForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(9000);
end;

procedure TDXTorsoForm.GRFOKBtnClick(Sender: TObject);
var
  iTermWide, iTermHeight, {Planting,} I, J: Integer;
{NewRect:TRect; }
{CheckString:String; }
  TermiteWorld: array of array of Integer;
  Bitmap: TBitmap;
begin
{144x144 (here)
157x113 (639x480)
197x143 (799x600)
TermWide,TermHeight, Termites,Chipset}
  iTermWide := 144;
  iTermHeight := 144;
  Bitmap := TBitmap.Create;
  Bitmap.Width := (iTermWide); {393}
  Bitmap.Height := (iTermHeight); {241}
  Image1.Picture.Graphic := Bitmap;
  Application.ProcessMessages;
  SetLength(TermiteWorld, iTermWide, iTermHeight);
  Randomize;
  DXTorsoClor := False;
  repeat
    for I := 0 to iTermWide - 1 do
      for J := 0 to iTermHeight - 1 do begin
        TermiteWorld[I, J] := Random(2);
        begin
          if (TermiteWorld[I, J] = 0) then
            Image1.Canvas.Pixels[I, J] := Desolate
          else Image1.Canvas.Pixels[I, J] := Full;
        end;
      end;
    Application.ProcessMessages;
  until (DXTorsoClor = True);
  SetLength(TermiteWorld, 0, 0);
end;


procedure TDXTorsoForm.TorsoHelpClick(Sender: TObject);
begin
  Application.HelpContext(9000);
end;

end.

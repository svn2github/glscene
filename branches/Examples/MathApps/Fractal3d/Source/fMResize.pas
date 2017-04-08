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
unit fMResize;

interface

uses Windows, Classes, Graphics, Forms,
     Controls, Buttons, StdCtrls, ExtCtrls;

type
  TResizeForm = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    WidthEdit: TEdit;
    Label2: TLabel;
    HeightEdit: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ResizeForm: TResizeForm;

implementation

uses fUGlobal,fMain;

{$R *.DFM}

procedure TResizeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ResizeFormX := ResizeForm.left;
  ResizeFormY := ResizeForm.top;
  DoSaver;
end;

procedure TResizeForm.FormCreate(Sender: TObject);
begin
  left := ResizeFormX;{  left := 498;}
  top := ResizeFormY;
end;

procedure TResizeForm.BitBtn1Click(Sender: TObject);
begin
  Application.HelpContext(0);
end;

procedure TResizeForm.BitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TResizeForm.BitBtn3Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TResizeForm.FormShow(Sender: TObject);
var InString: string;
begin
{Get the Image X Y and place into form}
  str(FYImageX, InString);
  WidthEdit.Text := InString;
  str(FYImageY, InString);
  HeightEdit.Text := InString;
end;



end.

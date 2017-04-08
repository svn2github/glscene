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
unit fMJPEG;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  jpeg, StdCtrls, Buttons;

type
  TJPEGForm = class(TForm)
    Scale: TComboBox;
    PixelFormat: TComboBox;
    ColorSpace: TComboBox;
    Performance: TComboBox;
    ProgressiveDisplay: TCheckBox;
    IncrementalDisplay: TCheckBox;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SetJPEGOptions(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JPEGForm: TJPEGForm;

implementation

uses fUGlobal,fMain;

{$R *.DFM}

procedure TJPEGForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  JPEGFormX := JPEGForm.left;
  JPEGFormY := JPEGForm.top;
  DoSaver;
end;

procedure TJPEGForm.FormCreate(Sender: TObject);
begin
  top := JPEGFormY;{0;}
  left := JPEGFormX;{488;}
  Scale.ItemIndex := 0;
  PixelFormat.ItemIndex := 0;
  Colorspace.ItemIndex := 0;
  Performance.ItemIndex := 0;
end;

procedure TJPEGForm.SetJPEGOptions(Sender: TObject);
var
  Temp: Boolean;
begin
  Temp := MainForm.Image2.Picture.Graphic is TJPEGImage;
  if Temp then
    with TJPEGImage(MainForm.Image2.Picture.Graphic) do
    begin
      PixelFormat := TJPEGPixelFormat(Self.PixelFormat.ItemIndex);
      Scale := TJPEGScale(Self.Scale.ItemIndex);
      Grayscale := Boolean(Colorspace.ItemIndex);
      Performance := TJPEGPerformance(Self.Performance.ItemIndex);
      ProgressiveDisplay := Self.ProgressiveDisplay.Checked;
    end;
  Scale.Enabled := Temp;
  PixelFormat.Enabled := Temp;
  Colorspace.Enabled := Temp;
  Performance.Enabled := Temp;
  ProgressiveDisplay.Enabled := Temp
    and
      TJPEGImage(MainForm.Image2.Picture.Graphic).ProgressiveEncoding;
  MainForm.Image2.IncrementalDisplay := IncrementalDisplay.Checked;
end;

procedure TJPEGForm.OKBtnClick(Sender: TObject);
begin
  SetJPEGOptions(self);
  ModalResult := mrOK;
end;

procedure TJPEGForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



end.

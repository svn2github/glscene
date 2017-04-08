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
unit fanno;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TAnnotationForm = class(TForm)
    AnnoEdit: TEdit;
    Label1: TLabel;
    ATextCancelBtn: TBitBtn;
    Label2: TLabel;
    procedure ATextCancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AnnotationForm: TAnnotationForm;

implementation

{$R *.DFM}
uses fUGlobal,FMain;

procedure TAnnotationForm.ATextCancelBtnClick(Sender: TObject);
begin
  DIYAnnotate := False;
  MainForm.ImageAnnotate.Checked := False;
  AnnotationForm.Hide;
end;

procedure TAnnotationForm.FormCreate(Sender: TObject);
begin
  left := AnnotationFormX;
  top := AnnotationFormY;
end;

procedure TAnnotationForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  AnnotationFormX := AnnotationForm.left;
  AnnotationFormY := AnnotationForm.top;
  DoSaver;
end;

end.

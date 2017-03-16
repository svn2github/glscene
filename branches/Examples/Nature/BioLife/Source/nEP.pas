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
unit nEP;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TEditorPlacer = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    EditorPlacerEditX: TEdit;
    EditorPlacerEditY: TEdit;
    EditorPlacerCancel: TBitBtn;
    EditorPlacerOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure ReallyClose;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditorPlacerOKClick(Sender: TObject);
  private
     
  public
     
  end;

var
  EditorPlacer: TEditorPlacer;

implementation
uses nUGlobal;
{$R *.DFM}

procedure TEditorPlacer.FormCreate(Sender: TObject);
begin
  top := EPFormY;
  left := EPFormX;
end;

procedure TEditorPlacer.ReallyClose;
begin
  Close;
end;

procedure TEditorPlacer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  EPFormY := EditorPlacer.top;
  EPFormX := EditorPlacer.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure Codefx(WhatString: string; Codein: Integer);
var CodeS: string;
begin
  if Codein > 0 then begin
    str(Codein, CodeS);
    ShowMessage('Error in File data Number: ' + #13#10 +
      CodeS + #13#10 +
      WhatString);
  end;
end;

procedure TEditorPlacer.EditorPlacerOKClick(Sender: TObject);
var XP, YP, Code1, Code2: Integer;
begin
  val(EditorPlacerEditX.Text, XP, Code1);
  Codefx(EditorPlacerEditX.Text, Code1);
  val(EditorPlacerEditY.Text, YP, Code2);
  Codefx(EditorPlacerEditY.Text, Code2);
  if ((Code1 + Code2) = 0) then ModalResult := mrOK;
  if Xp = YP then ;
end;



end.

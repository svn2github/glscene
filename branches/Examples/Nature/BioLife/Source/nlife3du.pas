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
unit nlife3du;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons;

type
  TaUniversesForm = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    R0: TCheckBox;
    R1: TCheckBox;
    R2: TCheckBox;
    R3: TCheckBox;
    R4: TCheckBox;
    R5: TCheckBox;
    R6: TCheckBox;
    R7: TCheckBox;
    R8: TCheckBox;
    R9: TCheckBox;
    R19: TCheckBox;
    R18: TCheckBox;
    R17: TCheckBox;
    R16: TCheckBox;
    R15: TCheckBox;
    R14: TCheckBox;
    R13: TCheckBox;
    R12: TCheckBox;
    R11: TCheckBox;
    R10: TCheckBox;
    R26: TCheckBox;
    R25: TCheckBox;
    R24: TCheckBox;
    R23: TCheckBox;
    R22: TCheckBox;
    R21: TCheckBox;
    R20: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox2: TGroupBox;
    C0: TCheckBox;
    C1: TCheckBox;
    C2: TCheckBox;
    C3: TCheckBox;
    C4: TCheckBox;
    C5: TCheckBox;
    C6: TCheckBox;
    C7: TCheckBox;
    C8: TCheckBox;
    C9: TCheckBox;
    C19: TCheckBox;
    C18: TCheckBox;
    C17: TCheckBox;
    C16: TCheckBox;
    C15: TCheckBox;
    C14: TCheckBox;
    C13: TCheckBox;
    C12: TCheckBox;
    C11: TCheckBox;
    C10: TCheckBox;
    C26: TCheckBox;
    C25: TCheckBox;
    C24: TCheckBox;
    C23: TCheckBox;
    C22: TCheckBox;
    C21: TCheckBox;
    C20: TCheckBox;
    BitBtn3: TBitBtn;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
     
  public
     
  end;

var
  aUniversesForm: TaUniversesForm;

implementation

{$R *.DFM}
uses nUGlobal;
procedure TaUniversesForm.FormCreate(Sender: TObject);
begin
  top := aUniversesFormY;
  left := aUniversesFormX;
end;

procedure TaUniversesForm.BitBtn1Click(Sender: TObject);
begin
  aUniversesFormY := aUniversesForm.top;
  aUniversesFormX := aUniversesForm.left;
    Universal[0,0]:=R0.Checked;
    Universal[0,1]:=R1.Checked;
    Universal[0,2]:=R2.Checked;
    Universal[0,3]:=R3.Checked;
    Universal[0,4]:=R4.Checked;
    Universal[0,5]:=R5.Checked;
    Universal[0,6]:=R6.Checked;
    Universal[0,7]:=R7.Checked;
    Universal[0,8]:=R8.Checked;
    Universal[0,9]:=R9.Checked;
    Universal[0,19]:=R19.Checked;
    Universal[0,18]:=R18.Checked;
    Universal[0,17]:=R17.Checked;
    Universal[0,16]:=R16.Checked;
    Universal[0,15]:=R15.Checked;
    Universal[0,14]:=R14.Checked;
    Universal[0,13]:=R13.Checked;
    Universal[0,12]:=R12.Checked;
    Universal[0,11]:=R11.Checked;
    Universal[0,10]:=R10.Checked;
    Universal[0,26]:=R26.Checked;
    Universal[0,25]:=R25.Checked;
    Universal[0,24]:=R24.Checked;
    Universal[0,23]:=R23.Checked;
    Universal[0,22]:=R22.Checked;
    Universal[0,21]:=R21.Checked;
    Universal[0,20]:=R20.Checked;

    Universal[1,0]:=C0.Checked;
    Universal[1,1]:=C1.Checked;
    Universal[1,2]:=C2.Checked;
    Universal[1,3]:=C3.Checked;
    Universal[1,4]:=C4.Checked;
    Universal[1,5]:=C5.Checked;
    Universal[1,6]:=C6.Checked;
    Universal[1,7]:=C7.Checked;
    Universal[1,8]:=C8.Checked;
    Universal[1,9]:=C9.Checked;
    Universal[1,19]:=C19.Checked;
    Universal[1,18]:=C18.Checked;
    Universal[1,17]:=C17.Checked;
    Universal[1,16]:=C16.Checked;
    Universal[1,15]:=C15.Checked;
    Universal[1,14]:=C14.Checked;
    Universal[1,13]:=C13.Checked;
    Universal[1,12]:=C12.Checked;
    Universal[1,11]:=C11.Checked;
    Universal[1,10]:=C10.Checked;
    Universal[1,26]:=C26.Checked;
    Universal[1,25]:=C25.Checked;
    Universal[1,24]:=C24.Checked;
    Universal[1,23]:=C23.Checked;
    Universal[1,22]:=C22.Checked;
    Universal[1,21]:=C21.Checked;
    Universal[1,20]:=C20.Checked;
end;


procedure TaUniversesForm.BitBtn3Click(Sender: TObject);
begin
  Application.HelpContext(1856);
end;

procedure TaUniversesForm.ComboBox1Change(Sender: TObject);
begin
R0.Checked:=False;
R1.Checked:=False;
R2.Checked:=False;
R3.Checked:=False;
R4.Checked:=False;
R5.Checked:=False;
R6.Checked:=False;
R7.Checked:=False;
R8.Checked:=False;
R9.Checked:=False;
R10.Checked:=False;
R11.Checked:=False;
R12.Checked:=False;
R13.Checked:=False;
R14.Checked:=False;
R15.Checked:=False;
R16.Checked:=False;
R17.Checked:=False;
R18.Checked:=False;
R19.Checked:=False;
R20.Checked:=False;
R21.Checked:=False;
R22.Checked:=False;
R23.Checked:=False;
R24.Checked:=False;
R25.Checked:=False;
R26.Checked:=False;
C0.Checked:=False;
C1.Checked:=False;
C2.Checked:=False;
C3.Checked:=False;
C4.Checked:=False;
C5.Checked:=False;
C6.Checked:=False;
C7.Checked:=False;
C8.Checked:=False;
C9.Checked:=False;
C10.Checked:=False;
C11.Checked:=False;
C12.Checked:=False;
C13.Checked:=False;
C14.Checked:=False;
C15.Checked:=False;
C16.Checked:=False;
C17.Checked:=False;
C18.Checked:=False;
C19.Checked:=False;
C20.Checked:=False;
C21.Checked:=False;
C22.Checked:=False;
C23.Checked:=False;
C24.Checked:=False;
C25.Checked:=False;
C26.Checked:=False;

Case ComboBox1.ItemIndex of
0:begin C2.Checked:=True; end;{/2:  Seeds (2)}
1:begin C2.Checked:=True; C3.Checked:=True;
        C4.Checked:=True;end;{/234:  Serviettes}
2:begin R0.Checked:=True;R1.Checked:=True;R2.Checked:=True;
        R4.Checked:=True;R5.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C3.Checked:=True; C4.Checked:=True;
        end;{01245678/34:  Hensel Artist}
3:begin R0.Checked:=True;R1.Checked:=True;R2.Checked:=True;
        R3.Checked:=True;R4.Checked:=True;R5.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C3.Checked:=True; end;{012345678/3:  Flakes}
4:begin R1.Checked:=True;
        C1.Checked:=True;end;{1/1:  Gnarl}
5:begin R1.Checked:=True;R2.Checked:=True;
        R3.Checked:=True;R4.Checked:=True;
        C3.Checked:=True; end;{1234/3:  Mazectric}
6:begin R1.Checked:=True;R2.Checked:=True;
        R3.Checked:=True;R4.Checked:=True;R5.Checked:=True;
        C3.Checked:=True;end;{12345/3:  Maze}
7:begin R1.Checked:=True;R2.Checked:=True;
        R3.Checked:=True;R4.Checked:=True;R5.Checked:=True;
        C3.Checked:=True; C7.Checked:=True;
        end;{12345/37:  Maze Rats!}
8:begin R1.Checked:=True;R2.Checked:=True;
        R3.Checked:=True;R4.Checked:=True;R5.Checked:=True;
        C5.Checked:=True; C4.Checked:=True;
        end;{12345/45:  Mazex}
9:begin R1.Checked:=True;R2.Checked:=True;
        R5.Checked:=True;
        C3.Checked:=True;
        C6.Checked:=True;end;{125/36  :2x2}
10:begin R1.Checked:=True;R2.Checked:=True;
        R5.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C3.Checked:=True; C6.Checked:=True;
        C7.Checked:=True;end;{125678/367:  White coagulations}
11:begin R1.Checked:=True;
        R3.Checked:=True;R5.Checked:=True;
        R7.Checked:=True;
        C1.Checked:=True; C3.Checked:=True;C5.Checked:=True;
        C7.Checked:=True;end;{1357/1357:  Replicator}
12:begin R1.Checked:=True;
        R3.Checked:=True;R5.Checked:=True;
        R8.Checked:=True;
        C3.Checked:=True; C5.Checked:=True;
        C7.Checked:=True;end;{1358/357:  Amoeba}
13:begin R2.Checked:=True;
        R3.Checked:=True;
        C3.Checked:=True; end;{23/3:  Conway's Life}
14:begin R2.Checked:=True;
        R3.Checked:=True;
        C3.Checked:=True; C6.Checked:=True;end;{23/36:  HighLife}
15:begin R2.Checked:=True;
        R3.Checked:=True;R4.Checked:=True;R5.Checked:=True;
        C8.Checked:=True; C4.Checked:=True;C5.Checked:=True;
        C6.Checked:=True;
        C7.Checked:=True;end;{2345/45678:  WalledCities}
16:begin R2.Checked:=True;
        R3.Checked:=True;R5.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C3.Checked:=True; C6.Checked:=True; C7.Checked:=True;
        C8.Checked:=True;end;{235678/3678:  Stains}
17:begin R2.Checked:=True;
        R3.Checked:=True;R5.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C3.Checked:=True; C7.Checked:=True;
        C8.Checked:=True;end;{235678/378:  Coagulations}
18:begin R2.Checked:=True;R3.Checked:=True;R8.Checked:=True;
        C3.Checked:=True; C5.Checked:=True;
        C7.Checked:=True;end;{238/357:  Pseudo life}
19:begin R2.Checked:=True;
        R4.Checked:=True;R5.Checked:=True;
        C3.Checked:=True; C6.Checked:=True;
        C8.Checked:=True;end;{245/368:  Move}
20:begin
        R3.Checked:=True;R4.Checked:=True;
        C3.Checked:=True; C4.Checked:=True;end;{34/34:  34 Life}
21:begin
        R3.Checked:=True;R4.Checked:=True;R5.Checked:=True;
        C5.Checked:=True; end;{345/5:  3D Life}
22:begin
        R3.Checked:=True;R4.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C0.Checked:=True; C1.Checked:=True;C2.Checked:=True;
        C3.Checked:=True;C4.Checked:=True;C7.Checked:=True;
        C8.Checked:=True;end;{34678/0123478: InverseLife}
23:begin
        R3.Checked:=True;R4.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C3.Checked:=True; C6.Checked:=True;C7.Checked:=True;
        C8.Checked:=True;end;{34678/3678:  Day & Night}
24:begin R4.Checked:=True;R5.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;
        C3.Checked:=True; C4.Checked:=True;
        C5.Checked:=True;end;{4567/345:  Assimilation}
25:begin R4.Checked:=True;R5.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C3.Checked:=True;end;{45678/3:  Coral}
26:begin R5.Checked:=True;
        C3.Checked:=True; C4.Checked:=True;
        C5.Checked:=True;end;{5/345:  Long life}
27:begin R5.Checked:=True;
        R6.Checked:=True;R7.Checked:=True;R8.Checked:=True;
        C3.Checked:=True; C5.Checked:=True; C6.Checked:=True;
        C7.Checked:=True; C8.Checked:=True;end;{5678/35678:  Diamoeba}
end;
end;

end.

unit GlsUvAboutFrm;

interface

uses Windows, SysUtils, Classes, Graphics,
  Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, jpeg;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    OKButton: TButton;
    CRSImage: TImage;
    DelphiButton: TSpeedButton;
    Image1: TImage;
    procedure ProgramIconClick(Sender: TObject);
    procedure CRSImageClick(Sender: TObject);
    procedure DelphiButtonClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
     
  public
     
  end;

var
  AboutBox: TAboutBox;

implementation
uses ShellAPI;
{$R *.DFM}

procedure TAboutBox.ProgramIconClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.glscene.org/', '', '', SW_SHOW);
end;

procedure TAboutBox.CRSImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.caperaven.co.za/glscene/glsceneindex.htm',
     '', '', SW_SHOW);
end;
           
procedure TAboutBox.DelphiButtonClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
   'http://www.borland.com/delphi/', '', '', SW_SHOW);
end;

procedure TAboutBox.Image1Click(Sender: TObject);
begin
  ShellExecute(0, 'open',
   'http://www.opengl.org/', '', '', SW_SHOW);
end;

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.
 

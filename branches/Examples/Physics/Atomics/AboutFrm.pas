unit AboutFrm;

interface

uses
  Windows, SysUtils, Classes, ShellAPI,{mail}
  Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, PngImage;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    OKButton: TButton;
    PrintBtn: TSpeedButton;
    RichEdit1: TRichEdit;
    CBLabel: TLabel;
    GlsceneImage: TImage;
    OpenglImage: TImage;
    procedure OKButtonClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure CBLabelClick(Sender: TObject);
    procedure GlsceneImageClick(Sender: TObject);
    procedure OpenglImageClick(Sender: TObject);
    procedure CopyrightClick(Sender: TObject);
  private
     
  public
     
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.DFM}

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.PrintBtnClick(Sender: TObject);
begin
  RichEdit1.Print('Atomics Help');
end;

procedure TAboutBox.CBLabelClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'mailto:cboyd@mailandnews.com', '', '', SW_SHOW);
end;

procedure TAboutBox.GlsceneImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.glscene.org/', '', '', SW_SHOW);
end;

procedure TAboutBox.OpenglImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.opengl.org/', '', '', SW_SHOW);
end;

procedure TAboutBox.CopyrightClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'mailto:ilh2o@ezl.com', '', '', SW_SHOW);
end;

end.


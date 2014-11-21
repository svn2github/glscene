unit fAboutCambrianLabs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg, ComCtrls, ToolWin, ShellAPI;

type
  TfrmAboutCambrianLabs = class(TForm)
    Panel3: TPanel;                        
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    Label_Visit: TLabel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton_Close: TToolButton;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Image3: TImage;
    Image2: TImage;
    procedure ToolButton_CloseClick(Sender: TObject);
    procedure Label_URLMouseEnter(Sender: TObject);
    procedure Label_URLMouseLeave(Sender: TObject);
    procedure Label_URLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAboutCambrianLabs: TfrmAboutCambrianLabs;

implementation

{$R *.dfm}

procedure TfrmAboutCambrianLabs.ToolButton_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAboutCambrianLabs.Label_URLMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderline];
end;

procedure TfrmAboutCambrianLabs.Label_URLMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
end;

procedure TfrmAboutCambrianLabs.Label_URLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open',
    pchar(TLabel(Sender).Caption),nil,nil, SW_SHOWNORMAL);
end;
end.

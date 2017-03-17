unit fSDialog;

interface

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils, 
  System.Variants,
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,

  fSForm;

type
  TSDialog = class(TSForm)
    PanelTop: TPanel;
    PanelMiddle: TPanel;
    PanelBottom: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonHelp: TButton;
    Memo: TMemo;
    procedure ButtonHelpClick(Sender: TObject);
  public
     
    function Execute: boolean; virtual;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  private
     
  end;

var
  SDialog: TSDialog;

implementation

{$R *.dfm}

procedure TSDialog.ButtonHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

function TSDialog.Execute: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TSDialog.ReadIniFile;
begin
  inherited;
  //
end;

procedure TSDialog.WriteIniFile;
begin
  //
  inherited;
end;

end.

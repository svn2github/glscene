unit FGLDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InitialForm, StdCtrls, ExtCtrls,

  FGLForm;

type
  TGLDialog = class(TGLForm)
    PanelTop: TPanel;
    PanelMiddle: TPanel;
    PanelBottom: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonHelp: TButton;
    procedure ButtonHelpClick(Sender: TObject);
  public
    { Public declarations }
    function Execute: boolean; virtual;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  private
    { Private declarations }
  end;

var
  GLDialog: TGLDialog;

implementation

{$R *.dfm}

procedure TGLDialog.ButtonHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

function TGLDialog.Execute: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TGLDialog.ReadIniFile;
begin
  inherited;
  //
end;

procedure TGLDialog.WriteIniFile;
begin
  //
  inherited;
end;

end.

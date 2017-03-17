unit dSMaster;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  Vcl.ExtDlgs;

type
  TdmSMaster = class(TDataModule)
    ColorDialog: TColorDialog;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    odTextures: TOpenDialog;
    sdTextures: TSaveDialog;
    opDialog: TOpenPictureDialog;
  private

  public
     
  end;

var
  dmSMaster: TdmSMaster;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.

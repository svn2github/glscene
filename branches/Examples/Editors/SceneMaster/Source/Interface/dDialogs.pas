unit dDialogs;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  Vcl.ExtDlgs;

type
  TDMDialogs = class(TDataModule)
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
  DMDialogs: TDMDialogs;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.

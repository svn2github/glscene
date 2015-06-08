// Datamodule for GLSViewer
unit DGLSViewer;

interface

uses
  System.SysUtils, System.Classes, Vcl.Dialogs, Vcl.ExtDlgs;

type
  TdmGLSViewer = class(TDataModule)
    ColorDialog: TColorDialog;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    odTextures: TOpenDialog;
    sdTextures: TSaveDialog;
    opDialog: TOpenPictureDialog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmGLSViewer: TdmGLSViewer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.

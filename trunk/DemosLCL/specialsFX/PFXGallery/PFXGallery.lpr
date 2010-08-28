program PFXGallery;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UPFXGallery in 'UPfxGallery.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.

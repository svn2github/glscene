unit fContent;

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

  fInitial;

type
  TFormContent = class(TFormInitial)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormContent: TFormContent;

implementation

{$R *.dfm}

end.

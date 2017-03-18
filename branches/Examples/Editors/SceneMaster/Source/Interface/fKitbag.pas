(*  Scene Master 3D *)
unit fKitbag;

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
  Vcl.ExtCtrls,
  Vcl.ComCtrls,

  dImages,
  fInitial;

type
  TFormKitbag = class(TFormInitial)
    twBasicGeometry: TTreeView;
    Panel1: TPanel;
    TreeView2: TTreeView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormKitbag: TFormKitbag;

implementation

{$R *.dfm}

end.

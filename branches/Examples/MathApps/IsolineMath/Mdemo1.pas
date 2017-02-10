unit Mdemo1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Tabs;

type
  TDemoForm = class(TForm)
    TabSet1: TTabSet;

    procedure FormCreate(Sender: TObject);
//    procedure TabSet1Change(Sender: TObject; NewTab: Integer;   var AllowChange: Boolean);
  private
    guest: TForm;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DemoForm: TDemoForm;

implementation

uses
  Contour;

{$R *.DFM}

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  guest := nil;
  ControlStyle := ControlStyle + [csOpaque];
  FreeLibrary(GetModuleHandle('OleAut32'));
    if guest = nil then guest := TContourForm.Create(Application);
  guest.Show;
end;

end.


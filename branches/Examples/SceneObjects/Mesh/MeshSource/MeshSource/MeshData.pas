unit MeshData;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  TfrmMeshData = class(TForm)
    rdMeshData: TRichEdit;
  private
     
  public
     
  end;

procedure ShowMeshData(const aList : TStringList);

implementation

{$R *.dfm}

procedure ShowMeshData(const aList : TStringList);
var
  lfrm : TfrmMeshData;
begin
  if aList = nil then
    Exit;
    
  lfrm := TfrmMeshData.Create(nil);
  try
    lfrm.rdMeshData.Lines.Assign(aList);
    lfrm.ShowModal;
  finally
    FreeAndNil(lfrm);
  end;
end;

end.

unit FLibMaterialPicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Media, FMX.Viewport3D,

  GLS.Material, FRMaterialPreview;

type
  TLibMaterialPicker = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    LBMaterials: TListBox;
    BBOK: TButton;
    ImageOK: TImage;
    BBCancel: TButton;
    ImageCancel: TImage;
    MPPreview: TRMaterialPreview;
    procedure LBMaterialsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Execute(var materialName: TGLLibMaterialName;
      materialLibrary: TGLAbstractMaterialLibrary): Boolean;
  end;

var
  LibMaterialPicker: TLibMaterialPicker;

implementation

{$R *.fmx}

{ TLibMaterialPicker }

function TLibMaterialPicker.Execute(var materialName: TGLLibMaterialName;
  materialLibrary: TGLAbstractMaterialLibrary): Boolean;
begin
  with LBMaterials do
  begin
    materialLibrary.SetNamesToTStrings(LBMaterials.Items);
    ItemIndex := Items.IndexOf(materialName);
    if (ItemIndex < 0) and (Items.Count > 0) then
      ItemIndex := 0;
    BBOk.Enabled := (Items.Count > 0);
  end;
  LBMaterialsClick(Self);
  Result := (ShowModal = mrOk);
  if Result then
  begin
    with LBMaterials do
      if ItemIndex >= 0 then
        materialName := Items[ItemIndex]
      else
        materialName := '';
  end;
end;

procedure TLibMaterialPicker.LBMaterialsClick(Sender: TObject);
begin
  with LBMaterials do
    if ItemIndex >= 0 then
      MPPreview.LibMaterial := TGLAbstractLibMaterial(Items.Objects[ItemIndex]);
end;

end.

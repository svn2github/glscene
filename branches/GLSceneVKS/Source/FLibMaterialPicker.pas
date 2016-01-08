//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
 Allows choosing a material in a material library 
    
}
unit FLibMaterialPicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math.Vectors,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Media, FMX.Viewport3D,
  FMX.Controls3D, FMX.Objects3D, FMX.Types3D,

  GLS.Material, FMX.MaterialSources, FRMaterialPreview;

type
  TLibMaterialPicker = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    LBMaterials: TListBox;
    BBOK: TButton;
    ImageOK: TImage;
    BBCancel: TButton;
    MPPreview: TRMaterialPreview;
    procedure LBMaterialsClick(Sender: TObject);
    procedure LBMaterialsDblClick(Sender: TObject);
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Execute(var materialName: TVKLibMaterialName;
      materialLibrary: TVKAbstractMaterialLibrary): Boolean;
  end;

function LibMaterialPicker: TLibMaterialPicker;
procedure ReleaseLibMaterialPicker;

implementation

{$R *.fmx}

var
  vLibMaterialPicker: TLibMaterialPicker;

function LibMaterialPicker: TLibMaterialPicker;
begin
  if not Assigned(vLibMaterialPicker) then
    vLibMaterialPicker := TLibMaterialPicker.Create(nil);
  Result := vLibMaterialPicker;
end;

procedure ReleaseLibMaterialPicker;
begin
  if Assigned(vLibMaterialPicker) then
  begin
    vLibMaterialPicker.Free;
    vLibMaterialPicker := nil;
  end;
end;

{ TLibMaterialPicker }

procedure TLibMaterialPicker.CBBackgroundChange(Sender: TObject);
begin
  //
end;

procedure TLibMaterialPicker.CBObjectChange(Sender: TObject);
begin
  //
end;

function TLibMaterialPicker.Execute(var materialName: TVKLibMaterialName;
  materialLibrary: TVKAbstractMaterialLibrary): Boolean;
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
      MPPreview.LibMaterial := TVKAbstractLibMaterial(Items.Objects[ItemIndex]);
end;

procedure TLibMaterialPicker.LBMaterialsDblClick(Sender: TObject);
begin
 /// BBOk.Click;
end;

end.

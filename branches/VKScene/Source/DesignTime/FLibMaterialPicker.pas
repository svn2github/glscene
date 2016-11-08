//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
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

  VKS.Material, FMX.MaterialSources, FRMaterialPreview,
  FMX.Controls.Presentation;

type
  TVKLibMaterialPicker = class(TForm)
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

function GLLibMaterialPicker: TVKLibMaterialPicker;
procedure ReleaseLibMaterialPicker;

implementation

{$R *.fmx}

var
  vGLLibMaterialPicker: TVKLibMaterialPicker;

function GLLibMaterialPicker: TVKLibMaterialPicker;
begin
  if not Assigned(vGLLibMaterialPicker) then
    vGLLibMaterialPicker := TVKLibMaterialPicker.Create(nil);
  Result := vGLLibMaterialPicker;
end;

procedure ReleaseLibMaterialPicker;
begin
  if Assigned(vGLLibMaterialPicker) then
  begin
    vGLLibMaterialPicker.Free;
    vGLLibMaterialPicker := nil;
  end;
end;

{ TLibMaterialPicker }

procedure TVKLibMaterialPicker.CBBackgroundChange(Sender: TObject);
begin
  //
end;

procedure TVKLibMaterialPicker.CBObjectChange(Sender: TObject);
begin
  //
end;

function TVKLibMaterialPicker.Execute(var materialName: TVKLibMaterialName;
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

procedure TVKLibMaterialPicker.LBMaterialsClick(Sender: TObject);
begin
  with LBMaterials do
    if ItemIndex >= 0 then
      MPPreview.LibMaterial := TVKAbstractLibMaterial(Items.Objects[ItemIndex]);
end;

procedure TVKLibMaterialPicker.LBMaterialsDblClick(Sender: TObject);
begin
 /// BBOk.Click;
end;

end.

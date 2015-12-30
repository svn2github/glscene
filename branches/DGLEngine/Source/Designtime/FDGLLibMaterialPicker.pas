//
// This unit is part of the DGLEngine Project, http://glscene.org
//
// FDGLLibMaterialPicker
{: <p>

 Allows choosing a material in a material library<p>

    <b>Historique : </b><font size=-1><ul>
      <li>29/12/15 - JD - Imported and Updated from GLScene
    </ul></font>
}
unit FDGLLibMaterialPicker;

interface

{$I DGLEngine.inc}

uses
  System.Classes, VCL.Forms, VCL.StdCtrls, VCL.Buttons, VCL.Controls,

  DGLViewer,
  DGLMaterial, Vcl.ExtCtrls;

type
  TDGLLibMaterialPicker = class(TForm)
    LBMaterials: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    Panel1: TPanel;
    CBBackground: TComboBox;
    CBObject: TComboBox;
    procedure LBMaterialsClick(Sender: TObject);
    procedure LBMaterialsKeyPress(Sender: TObject; var Key: Char);
    procedure LBMaterialsDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Déclarations publiques }
    function Execute(var materialName: TDGLLibMaterialName; materialLibrary: TDGLAbstractMaterialLibrary): Boolean;
  end;

function DGLLibMaterialPicker: TDGLLibMaterialPicker;
procedure ReleaseLibMaterialPicker;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.dfm}

var
  vLibMaterialPicker: TDGLLibMaterialPicker;

function DGLLibMaterialPicker: TDGLLibMaterialPicker;
begin
  if not Assigned(vLibMaterialPicker) then
    vLibMaterialPicker := TDGLLibMaterialPicker.Create(nil);
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

// Execute
//

function TDGLLibMaterialPicker.Execute(var materialName: TDGLLibMaterialName; materialLibrary: TDGLAbstractMaterialLibrary): Boolean;
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

procedure TDGLLibMaterialPicker.LBMaterialsClick(Sender: TObject);
begin
//  with LBMaterials do
//    if ItemIndex >= 0 then
//      MPPreview.LibMaterial := TGLAbstractLibMaterial(Items.Objects[ItemIndex]);
end;

procedure TDGLLibMaterialPicker.LBMaterialsKeyPress(Sender: TObject; var Key: Char);
begin
  LBMaterialsClick(Sender);
end;

procedure TDGLLibMaterialPicker.LBMaterialsDblClick(Sender: TObject);
begin
  BBOk.Click;
end;

initialization

finalization
  ReleaseLibMaterialPicker;

end.


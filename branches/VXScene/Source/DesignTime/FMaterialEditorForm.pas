//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Editor window for a material (with preview). 
}
unit FMaterialEditorForm;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Types,
  System.UITypes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.TabControl,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ListBox,
  FMX.Controls.Presentation,

  FRMaterialPreview,
  FRColorEditor,
  FRFaceEditor,
  VXS.Texture,
  FRTextureEdit,
  VXS.Material,
  VXS.State;

type
  TMaterialEditorForm = class(TForm)
    TabControl1: TTabControl;
    TIFront: TTabItem;
    TIBack: TTabItem;
    TITexture: TTabItem;
    FEFront: TRFaceEditor;
    FEBack: TRFaceEditor;
    GroupBox1: TGroupBox;
    MPPreview: TRMaterialPreview;
    BBOK: TButton;
    ImageOK: TImage;
    BBCancel: TButton;
    ImageCancel: TImage;
    RTextureEdit: TRTextureEdit;
    CBBlending: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    CBPolygonMode: TComboBox;
    procedure OnMaterialChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(AMaterial: TVXMaterial): Boolean;
  end;

function MaterialEditorForm: TMaterialEditorForm;
procedure ReleaseMaterialEditorForm;

//======================================================================
implementation
//======================================================================

{$R *.fmx}

var
  vMaterialEditorForm: TMaterialEditorForm;

function MaterialEditorForm: TMaterialEditorForm;
begin
  if not Assigned(vMaterialEditorForm) then
    vMaterialEditorForm := TMaterialEditorForm.Create(nil);
  Result := vMaterialEditorForm;
end;

procedure ReleaseMaterialEditorForm;
begin
  if Assigned(vMaterialEditorForm) then
  begin
    vMaterialEditorForm.Free;
    vMaterialEditorForm := nil;
  end;
end;

//------------------------------------------------------------------

constructor TMaterialEditorForm.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  for i := 0 to Integer(High(TBlendingMode)) do
    CBBlending.Items.Add(GetEnumName(TypeInfo(TBlendingMode), i));
  for i := 0 to Integer(High(TVXPolygonMode)) do
    CBPolygonMode.Items.Add(GetEnumName(TypeInfo(TVXPolygonMode), i));

  FEFront.OnChange := OnMaterialChanged;
  FEBack.OnChange := OnMaterialChanged;
  RTextureEdit.OnChange := OnMaterialChanged;
end;

function TMaterialEditorForm.Execute(AMaterial: TVXMaterial): Boolean;
begin
  with AMaterial.GetActualPrimaryMaterial do
  begin
    FEFront.FaceProperties := FrontProperties;
    FEBack.FaceProperties := BackProperties;
    RTextureEdit.Texture := Texture;
    CBPolygonMode.ItemIndex:=Integer(PolygonMode);
    CBBlending.ItemIndex := Integer(BlendingMode);
  end;
  MPPreview.Material := AMaterial;
  Result := (ShowModal = mrOk);
  if Result then
    with AMaterial.GetActualPrimaryMaterial do
    begin
      FrontProperties := FEFront.FaceProperties;
      BackProperties := FEBack.FaceProperties;
      Texture := RTextureEdit.Texture;
      BlendingMode := TBlendingMode(CBBlending.ItemIndex);
      PolygonMode := TVXPolygonMode(CBPolygonMode.ItemIndex);
    end;
end;

procedure TMaterialEditorForm.OnMaterialChanged(Sender: TObject);
begin
  MPPreview.GLSViewer.BeginUpdate;
  with MPPreview.Material do
  begin
    FrontProperties := FEFront.FaceProperties;
    BackProperties := FEBack.FaceProperties;
    Texture := RTextureEdit.Texture;
    BlendingMode := TBlendingMode(CBBlending.ItemIndex);
    PolygonMode := TVXPolygonMode(CBPolygonMode.ItemIndex);
  end;
  MPPreview.GLSViewer.EndUpdate;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

finalization

  ReleaseMaterialEditorForm;

end.

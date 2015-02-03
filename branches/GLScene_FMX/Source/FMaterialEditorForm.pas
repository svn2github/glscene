//
// This unit is part of the GLScene Project, http://glscene.org
//
{: FMaterialEditorForm<p>

   Editor window for a material (with preview).<p>

   <b>Historique : </b><font size=-1><ul>
      <li>05/01/14 - PW - Converted to FMX
      <li>07/05/10 - Yar - Fixed PolygonMode and texture image class lookup
      <li>05/10/08 - DanB - Removed Kylix support
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>19/12/06 - DaStr - All comboboxes get their Items using RTTI
                             (thanks to dikoe Kenguru for the reminder and Roman Ganz for the code)
      <li>03/07/04 - LR  - Make change for Linux
      <li>24/03/00 - Egg - Added Blending
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FMaterialEditorForm;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Types,
  System.UITypes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, 
  FMX.TabControl, FMX.StdCtrls, FMX.Objects, FMX.ListBox,
  
  
  FRMaterialPreview, FRColorEditor, FRFaceEditor,  
  GLS.Texture, FRTextureEdit,
  GLS.Material, GLS.State;

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
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function Execute(AMaterial: TGLMaterial): Boolean;
  end;

function MaterialEditorForm: TMaterialEditorForm;
procedure ReleaseMaterialEditorForm;


implementation

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

// Create
//

constructor TMaterialEditorForm.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  for i := 0 to Integer(High(TBlendingMode)) do
    CBBlending.Items.Add(GetEnumName(TypeInfo(TBlendingMode), i));
  for i := 0 to Integer(High(TPolygonMode)) do
    CBPolygonMode.Items.Add(GetEnumName(TypeInfo(TPolygonMode), i));

  FEFront.OnChange := OnMaterialChanged;
  FEBack.OnChange := OnMaterialChanged;
  RTextureEdit.OnChange := OnMaterialChanged;
end;

// Execute
//

function TMaterialEditorForm.Execute(AMaterial: TGLMaterial): Boolean;
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
      PolygonMode := TPolygonMode(CBPolygonMode.ItemIndex);
    end;
end;

// OnMaterialChanged
//

procedure TMaterialEditorForm.OnMaterialChanged(Sender: TObject);
begin
  MPPreview.GLSViewer.BeginUpdate;
  with MPPreview.Material do
  begin
    FrontProperties := FEFront.FaceProperties;
    BackProperties := FEBack.FaceProperties;
    Texture := RTextureEdit.Texture;
    BlendingMode := TBlendingMode(CBBlending.ItemIndex);
    PolygonMode := TPolygonMode(CBPolygonMode.ItemIndex);
  end;
  MPPreview.GLSViewer.EndUpdate;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

finalization

  ReleaseMaterialEditorForm;

end.

//
// This unit is part of the GLScene Project, http://glscene.org
//
{FMaterialEditorForm 

   Editor window for a material (with preview). 

    Historique :  
       07/05/10 - Yar - Fixed PolygonMode and texture image class lookup
       05/10/08 - DanB - Removed Kylix support
       29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
       19/12/06 - DaStr - All comboboxes get their Items using RTTI
                             (thanks to dikoe Kenguru for the reminder and Roman Ganz for the code)
       03/07/04 - LR  - Make change for Linux
       24/03/00 - Egg - Added Blending
       06/02/00 - Egg - Creation
    
}
unit FMaterialEditorForm;

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  System.Classes, System.TypInfo, VCL.Forms, VCL.ComCtrls, VCL.StdCtrls,
  VCL.Controls, VCL.Buttons,

  FRMaterialPreview, FRColorEditor, FRFaceEditor,
  GLTexture,  FRTextureEdit,  GLViewer,
  GLMaterial,  GLState;

type
  TMaterialEditorForm = class(TForm)
    PageControl1: TPageControl;
    TSFront: TTabSheet;
    TSBack: TTabSheet;
    TSTexture: TTabSheet;
    FEFront: TRFaceEditor;
    FEBack: TRFaceEditor;
    GroupBox1: TGroupBox;
    MPPreview: TRMaterialPreview;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
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

{$R *.dfm}

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
  with MPPreview.Material do
  begin
    FrontProperties := FEFront.FaceProperties;
    BackProperties := FEBack.FaceProperties;
    Texture := RTextureEdit.Texture;
    BlendingMode := TBlendingMode(CBBlending.ItemIndex);
    PolygonMode := TPolygonMode(CBPolygonMode.ItemIndex);
  end;
  MPPreview.GLSceneViewer.Invalidate;
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


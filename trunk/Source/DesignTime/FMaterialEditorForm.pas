//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Editor window for a material (with preview). 

    History:  
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
  TGLMaterialEditorForm = class(TForm)
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

function GLMaterialEditorForm: TGLMaterialEditorForm;
procedure ReleaseMaterialEditorForm;

implementation

{$R *.dfm}

var
  vGLMaterialEditorForm: TGLMaterialEditorForm;

function GLMaterialEditorForm: TGLMaterialEditorForm;
begin
  if not Assigned(vGLMaterialEditorForm) then
    vGLMaterialEditorForm := TGLMaterialEditorForm.Create(nil);
  Result := vGLMaterialEditorForm;
end;

procedure ReleaseMaterialEditorForm;
begin
  if Assigned(vGLMaterialEditorForm) then
  begin
    vGLMaterialEditorForm.Free;
    vGLMaterialEditorForm := nil;
  end;
end;

// Create
//

constructor TGLMaterialEditorForm.Create(AOwner: TComponent);
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

function TGLMaterialEditorForm.Execute(AMaterial: TGLMaterial): Boolean;
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

procedure TGLMaterialEditorForm.OnMaterialChanged(Sender: TObject);
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


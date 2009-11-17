{: FMaterialEditorForm<p>

   Editor window for a material (with preview).<p>

   <b>Historique : </b><font size=-1><ul>

   </ul></font>
}
unit FMaterialEditorFormLCL;

interface

{$i GLScene.inc}

uses

  {$IFDEF MSWINDOWS} Windows,{$ENDIF} FRTrackBarEditLCL, Forms,
  FRMaterialPreviewLCL, FRColorEditorLCL, ComCtrls, FRFaceEditorLCL, StdCtrls, Controls,
  Classes, GLTexture, Buttons, TypInfo, FRTextureEditLCL, GLViewer,
  GLMaterial,lresources;

type
  TMaterialEditorForm = class(TForm)
	 PageControl1: TPageControl;
    TSFront: TTabSheet;
    TSBack: TTabSheet;
    TSTexture: TTabSheet;
    GroupBox1: TGroupBox;
    MPPreview: TRMaterialPreview;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    RTextureEdit: TRTextureEdit;
    CBBlending: TComboBox;
    Label1: TLabel;
    procedure OnMaterialChanged(Sender : TObject);
  private
    { Diclarations privies }
  public
    { Diclarations publiques }
    constructor Create(AOwner : TComponent); override;

	 function Execute(var material : TGLMaterial) : Boolean;
  end;

function MaterialEditorForm : TMaterialEditorForm;
procedure ReleaseMaterialEditorForm;

implementation

var
	vMaterialEditorForm : TMaterialEditorForm;
        FEFront: TRFaceEditor;
        FEBack: TRFaceEditor;

function MaterialEditorForm : TMaterialEditorForm;
begin
	if not Assigned(vMaterialEditorForm) then
	   vMaterialEditorForm:=TMaterialEditorForm.Create(nil);
	Result:=vMaterialEditorForm;
end;

procedure ReleaseMaterialEditorForm;
begin
	if Assigned(vMaterialEditorForm) then begin
	   vMaterialEditorForm.Free; vMaterialEditorForm:=nil;
	end;
end;

// Create
//
constructor TMaterialEditorForm.Create(AOwner : TComponent);
var
  I: Integer;
begin
	inherited;
  for i := 0 to Integer(High(TBlendingMode)) do
    CBBlending.Items.Add(GetEnumName(TypeInfo(TBlendingMode), i));
    FEFront:= TRFaceEditor.Create(self);
    FEFront.Parent := TSFront;
    FEFront.Name:='FEFront';
    FEFront.Align:=alClient;

    FEBack:= TRFaceEditor.Create(self);
    FEBack.Parent := TSBack;
    FEBack.Name:='FEBack';
    FEBack.Align:=alClient;

    FEFront.OnChange:=OnMaterialChanged;
    FEBack.OnChange:=OnMaterialChanged;
    RTextureEdit.OnChange:=OnMaterialChanged;
end;

// Execute
//
function TMaterialEditorForm.Execute(var material : TGLMaterial) : Boolean;
begin
   with material do begin
      FEFront.FaceProperties:=FrontProperties;
		FEBack.FaceProperties:=BackProperties;
		RTextureEdit.Texture:=Texture;
      CBBlending.ItemIndex:=Integer(BlendingMode);
	end;
	MPPreview.Material:=material;
	Result:=(ShowModal=mrOk);
	if Result then with material do begin
		FrontProperties:=FEFront.FaceProperties;
		BackProperties:=FEBack.FaceProperties;
		Texture:=RTextureEdit.Texture;
      BlendingMode:=TBlendingMode(CBBlending.ItemIndex);
	end;
end;

// OnMaterialChanged
//
procedure TMaterialEditorForm.OnMaterialChanged(Sender : TObject);
begin
   with MPPreview.Material do begin
      FrontProperties:=FEFront.FaceProperties;
		BackProperties:=FEBack.FaceProperties;
		Texture:=RTextureEdit.Texture;
      BlendingMode:=TBlendingMode(CBBlending.ItemIndex);
	end;
 // MPPreview.Render;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  {$i FMaterialEditorFormLCL.lrs}

finalization

   ReleaseMaterialEditorForm;

end.




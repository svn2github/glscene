//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Editor for Gui skin. 
 
}
unit FGuiSkinEditor;

interface

uses
  System.Messaging,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Controls.Presentation, FMX.Edit,

  GLX.Gui, GLX.Texture, GLX.BaseClasses, GLX.Material, System.Math.Vectors,
  FMX.Controls3D;

type
  TGUISkinEditor = class(TForm)
    PanElements: TPanel;
    StatusBar: TStatusBar;
    PanBottom: TPanel;
    Label1: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    LBElements: TListBox;
    Label2: TLabel;
    ComboBox1: TComboBox;
    PanImageProperties: TPanel;
    PanZoomImage: TPanel;
    SBarHorizontal: TScrollBar;
    SBarVertical: TScrollBar;
    ImgFull: TImage;
    ButtonOK: TButton;
    ImageOK: TImage;
    ButtonCancel: TButton;
    ImageCancel: TImage;
    Panel1: TPanel;
    ImgPreview: TImage;
    Panel2: TPanel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    EditWidth: TEdit;
    EditHeight: TEdit;
    GLCamera1: TCamera;
    GLPanel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    
    FOriginalWndProc: TWndMethod;

    FFocusRect: TRect;
    VisibleRect: TRect;
    PreviewMousePoint: TPoint;
    PreviewWidth,
      PreviewHeight: Integer;
    FullMousePoint: TPoint;

    MouseDown: Boolean;

    procedure ImageWndProc(var Message: TMessage);

    procedure DrawImageFocusRect(ARect: TRect);
    procedure AlignZoomPanel;
    procedure UpdateRegionEdits;

    procedure SetEditState(Parent: TControl; Enabled: Boolean);
    procedure AddElement(Index: Integer);
    procedure DrawCrossair(Point: TPoint);
  public
    
    TheGuiComponent: TGLGuiElementList;
    SelectedElement: TGLGUIElement;

    Tex: TGLTexture;
    Zoom: Single;
    Width: Integer;
    Height: Integer;
    function Edit(GuiComponent: TGLGuiElementList): Boolean;
    procedure Render;
    procedure SetMax(Scrollbar: TScrollbar; Val: Integer);
  end;

var
  GUISkinEditor: TGUISkinEditor;

function GUIComponentDialog(GuiComponent: TGLGuiElementList): Boolean;

//--------------------------------------------------------------------
//--------------------------------------------------------------------
//--------------------------------------------------------------------
implementation
//--------------------------------------------------------------------
//--------------------------------------------------------------------
//--------------------------------------------------------------------

{$R *.fmx}

function GUIComponentDialog(GuiComponent: TGLGuiElementList): Boolean;
var
  Editor: TGUISkinEditor;
begin
  Editor := TGUISkinEditor.Create(nil);
  Result := Editor.Edit(GuiComponent);
  Editor.Free;
end;

procedure TGUISkinEditor.FormCreate(Sender: TObject);
begin
  //override original WndProc to capture image mouse leave message
  //with FMX tools
  (*
  FOriginalWndProc := ImgFull.WindowProc;
  imgFull.WindowProc := ImageWndProc;

  Tex := TGLTexture.Create(Self);
  Tex.SetImageClassName('TGLPersistentImage');
  GLPanel1.RedrawAtOnce := True;

  StatusBar.Panels[0].Text := 'X : 0';
  StatusBar.Panels[1].Text := 'Y : 0';
  AlignZoomPanel;
  UpdateRegionEdits;
  DoubleBuffered := True;
  FullMousePoint := Point(-1, -1);

  //this Delphi bug shows all panels transparent
  //the code below is to avoid this bug in XP
  panElements.ParentBackground := False;
  panElements.ParentBackground := True;
  panElements.ParentBackground := False;

  panImageProperties.ParentBackground := False;
  panImageProperties.ParentBackground := True;
  panImageProperties.ParentBackground := False;

  panBottom.ParentBackground := False;
  panBottom.ParentBackground := True;
  panBottom.ParentBackground := False;

  panZoomImage.ParentBackground := False;
  panZoomImage.ParentBackground := True;
  panZoomImage.ParentBackground := False;
 *)
end;

procedure TGUISkinEditor.FormDestroy(Sender: TObject);
begin
  Tex.Free;
end;

function TGUISkinEditor.Edit(GuiComponent: TGLGuiElementList): Boolean;
var
  Mat: TGLMaterial;
  GuiLayout: TGLGuiLayout;
  XC: Integer;

begin
  TheGuiComponent := GuiComponent;
  GuiLayout := (GuiComponent.GetOwner as TGLGuiComponent).Owner.GetOwner as
    TGLGuiLayout;
  Mat := GuiLayout.Material;
  GLPanel1.Visible := True;
   { TODO : E2003 Undeclared identifier: 'GuiLayout' }
   (*
  GLPanel1.GuiLayout := GuiLayout;
  GLPanel1.GuiLayoutName := (GuiComponent.GetOwner as TGLGuiComponent).Name;
  *)
  Zoom := 1.0;

  if (Assigned(mat.MaterialLibrary)
    and (mat.MaterialLibrary is TGLMaterialLibrary)
    and (Mat.LibMaterialName <> '')) then
  begin
    mat :=
      TGLMaterialLibrary(mat.MaterialLibrary).Materials.GetLibMaterialByName(Mat.LibMaterialName).Material;
  end;
  Width := Mat.Texture.Image.Width;
  Height := Mat.Texture.Image.Height;
   { TODO : E2003 Undeclared identifier }
   (*
  WidthEdit.Text := IntToStr(Mat.Texture.Image.Width);
  HeightEdit.Text := IntToStr(Mat.Texture.Image.Height);
  GLPanel1.GuiLayout.Material.Assign(Mat);
  *)
  Tex.Assign(mat.Texture);
   { TODO : E2003 Undeclared identifier }
   (*
  imgPreview.Bitmap.Canvas.StretchDraw(imgPreview.ClientRect, (Tex.Image
    as TGLPersistentImage).Picture.Bitmap);
  PreviewWidth := (Tex.Image as TGLPersistentImage).Picture.Width;
  Previewheight := (Tex.Image as TGLPersistentImage).Picture.Height;
  *)
  lbElements.Clear;
  for XC := 0 to TheGuiComponent.Count - 1 do
  begin
    lbElements.Items.Add(TheGuiComponent.Items[XC].Name);
  end;

  if TheGuiComponent.Count > 0 then
  begin
    SelectedElement := TheGuiComponent.Items[0];
    lbElements.ItemIndex := 0;
  end
  else
    SelectedElement := nil;
  Render;
  Result := ShowModal = mrOk;
end;

procedure TGUISkinEditor.AddElement(Index: Integer);
begin

end;

procedure TGUISkinEditor.AlignZoomPanel;
begin

end;

procedure TGUISkinEditor.DrawCrossair(Point: TPoint);
begin

end;

procedure TGUISkinEditor.DrawImageFocusRect(ARect: TRect);
begin

end;


procedure TGUISkinEditor.ImageWndProc(var Message: TMessage);
begin

end;

procedure TGUISkinEditor.Render;
begin

end;

procedure TGUISkinEditor.SetEditState(Parent: TControl; Enabled: Boolean);
begin

end;

procedure TGUISkinEditor.SetMax(Scrollbar: TScrollbar; Val: Integer);
begin

end;

procedure TGUISkinEditor.UpdateRegionEdits;
begin

end;

end.

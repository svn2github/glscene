{: GuiSkinEditorFormUnit<p>

   Editor for Gui skin.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>03/07/04 - LR - Make change for Linux
      <li>?/?/? -  - Creation
   </ul></font>
}
unit GuiSkinEditorFormUnit;

interface

{$i GLScene.inc}

{$IFDEF MSWINDOWS}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  StdCtrls, ComCtrls, ExtCtrls, GLTexture, GLScene, GLObjects, GLWindows, GLHUDObjects, 
  GLMisc, GLWin32Viewer, GLGui, GLGraphics, GLUtils;
{$ENDIF}
{$IFDEF LINUX}
uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs, 
  QStdCtrls, QComCtrls, QExtCtrls, GLTexture, GLScene, GLObjects, GLWindows, GLHUDObjects, 
  GLMisc, GLLinuxViewer, GLGui, GLGraphics, GLUtils; 
{$ENDIF}


type
  TGUISkinEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image2: TImage;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Panel3: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLPanel1: TGLPanel;
    HUDSprite1: TGLHUDSprite;
    GLMemoryViewer1: TGLMemoryViewer;
    Image1: TImage;
    Button3: TButton;
    Button4: TButton;
    WidthEdit: TEdit;
    HeightEdit: TEdit;
    Button5: TButton;
    Button6: TButton;
    Label7: TLabel;
    Label8: TLabel;
    GLLightSource1: TGLLightSource;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    ComboBox1: TComboBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    LeftEdit: TEdit;
    TopEdit: TEdit;
    RightEdit: TEdit;
    BottomEdit: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    ScaleXEdit: TEdit;
    ScaleYEdit: TEdit;
    Label14: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollbarChange(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
    procedure HeightEditChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
    procedure ScaleXEditChange(Sender: TObject);
    procedure ScaleYEditChange(Sender: TObject);
    procedure LeftEditChange(Sender: TObject);
    procedure TopEditChange(Sender: TObject);
    procedure RightEditChange(Sender: TObject);
    procedure BottomEditChange(Sender: TObject);
  private
  public
    TheGuiComponent : TGLGuiElementList;
    SelectedElement : TGLGUIElement;

    Tex : TGLTexture;
    Zoom : Single;
    Width : Integer;
    Height : Integer;
    Function Edit(GuiComponent : TGLGuiElementList) : Boolean;
    Procedure Render;
    Procedure SetMax(Scrollbar : TScrollbar; Val : Integer);
  end;

var
  GUISkinEditor: TGUISkinEditor;

implementation

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

uses GLCrossPlatform;


procedure TGUISkinEditor.FormCreate(Sender: TObject);
begin
  Tex := TGLTexture.Create(Self);
  Tex.SetImageClassName('TGLPersistentImage');
  GLPanel1.RedrawAtOnce := True;
end;

procedure TGUISkinEditor.FormDestroy(Sender: TObject);
begin
  Tex.Free;
end;

Function TGUISkinEditor.Edit(GuiComponent : TGLGuiElementList) : Boolean;

Var
  Mat : TGLMaterial;
  GuiLayout : TGLGuiLayout;
  XC : Integer;


begin
  TheGuiComponent := GuiComponent;
  GuiLayout := (GuiComponent.GetOwner as TGLGuiComponent).Owner.GetOwner as TGLGuiLayout;
  Mat := GuiLayout.Material;
  GLPanel1.Visible := True;
  GLPanel1.GuiLayout := GuiLayout;
  GLPanel1.GuiLayoutName := (GuiComponent.GetOwner as TGLGuiComponent).Name;
  Zoom := 1.0;

  If (Assigned(mat.MaterialLibrary) and (Mat.LibMaterialName <> '')) then
  Begin
    mat := mat.MaterialLibrary.Materials.GetLibMaterialByName(Mat.LibMaterialName).Material;
  End;
  Width := Mat.Texture.Image.Width;
  Height := Mat.Texture.Image.Height;
  WidthEdit.Text := IntToStr(Mat.Texture.Image.Width);
  HeightEdit.Text := IntToStr(Mat.Texture.Image.Height);

  GLPanel1.GuiLayout.Material.Assign(Mat);

  Tex.Assign(mat.Texture);
  Image2.Picture.Bitmap.Canvas.StretchDraw(Image2.ClientRect,(Tex.Image as TGLPersistentImage).Picture.Graphic);

  ListBox1.Clear;
  For XC := 0 to TheGuiComponent.Count-1 do
  Begin
    ListBox1.Items.Add(TheGuiComponent.Items[XC].Name);
  End;

  If TheGuiComponent.Count > 0 then
  Begin
    SelectedElement := TheGuiComponent.Items[0];
    ListBox1.ItemIndex := 0;
  End else SelectedElement := Nil;

  Render;

  Result := ShowModal = mrOk;
end;


procedure TGUISkinEditor.Button3Click(Sender: TObject);
begin
  Zoom := Zoom +  0.5;
  Label2.Caption := FormatFloat('####0.0',Zoom);
//  panel3.Invalidate;

  {$IFDEF MSWINDOWS}
  ScrollBar1.PageSize := Round(256/Zoom);
  ScrollBar2.PageSize := Round(256/Zoom);
  {$ENDIF}
  Render;
end;

procedure TGUISkinEditor.Button4Click(Sender: TObject);
begin
  Zoom := Zoom - 0.5;
  Label2.Caption := FormatFloat('####0.0',Zoom);
//  panel3.Invalidate;

  {$IFDEF MSWINDOWS}
  ScrollBar1.PageSize := Round(256/Zoom);
  ScrollBar2.PageSize := Round(256/Zoom);
  {$ENDIF}
  Render;
end;

procedure TGUISkinEditor.Render;
Var
  BitMap : TBitmap;
  Image  : TGLBitmap32;
begin
  if CheckBox1.Checked then
  Begin
    GLPanel1.Width := Width;
    GLPanel1.Height := Height;
    GLPanel1.Left := 1-ScrollBar2.position;
    GLPanel1.Top := 1-ScrollBar1.position;

    GLMemoryViewer1.Render;
    Image := GLMemoryViewer1.Buffer.CreateSnapShot;;
    Bitmap := Image.Create32BitsBitmap;
    try
      Image1.Canvas.Brush.Color := clBlack;
      Image1.Canvas.FillRect(Image1.Canvas.ClipRect);
      Image1.Canvas.StretchDraw(Rect(0,0,Round(((Tex.Image as TGLPersistentImage).Width)*Zoom),Round(((Tex.Image as TGLPersistentImage).Height)*Zoom)),Bitmap);{}
    finally
      Bitmap.Free;
    end;

//    Image1.Canvas.StretchDraw(Rect(Round((1-ScrollBar2.position)*Zoom),Round((1-ScrollBar1.position)*Zoom),Round((1-ScrollBar2.position+(Tex.Image as TGLPersistentImage).Width)*Zoom),Round((1-ScrollBar1.position+(Tex.Image as TGLPersistentImage).Height)*Zoom)),Bitmap);{}
  End else
  Begin
    Image1.Canvas.Brush.Color := clBlack;
    Image1.Canvas.FillRect(Image1.Canvas.ClipRect);
    Image1.Canvas.StretchDraw(Rect(Round((1-ScrollBar2.position)*Zoom),Round((1-ScrollBar1.position)*Zoom),Round((1-ScrollBar2.position+(Tex.Image as TGLPersistentImage).Width)*Zoom),Round((1-ScrollBar1.position+(Tex.Image as TGLPersistentImage).Height)*Zoom)),(Tex.Image as TGLPersistentImage).Picture.Graphic);
    If Assigned(SelectedElement) then
    Begin
      Image1.Canvas.Brush.Color := clWhite;
      {$IFDEF MSWINDOWS}
      Image1.Canvas.FrameRect(Rect(Round((1-ScrollBar2.position+SelectedElement.TopLeft.X)*Zoom),Round((1-ScrollBar1.position+SelectedElement.TopLeft.Y)*Zoom),Round((1-ScrollBar2.position+SelectedElement.BottomRight.X)*Zoom),Round((1-ScrollBar1.position+SelectedElement.BottomRight.Y)*Zoom)));
      {$ENDIF}
      {$IFDEF LINUX}
      Image1.Canvas.FillRect(Rect(Round((1-ScrollBar2.position+SelectedElement.TopLeft.X)*Zoom),Round((1-ScrollBar1.position+SelectedElement.TopLeft.Y)*Zoom),Round((1-ScrollBar2.position+SelectedElement.BottomRight.X)*Zoom),Round((1-ScrollBar1.position+SelectedElement.BottomRight.Y)*Zoom)));
      {$ENDIF}
    End;
  End;
end;

Procedure TGUISkinEditor.SetMax(Scrollbar : TScrollbar; Val : Integer);
Begin
  {$IFDEF MSWINDOWS}
  if Scrollbar.Position+Scrollbar.PageSize >= val then
  Begin
    Scrollbar.Position := val-Scrollbar.PageSize+1;
  End;
  {$ENDIF}
  Scrollbar.Max := val;
End;


procedure TGUISkinEditor.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  {$IFDEF MSWINDOWS}
  If ScrollPos+(Sender as TScrollBar).PageSize > (Sender as TScrollBar).Max then
  ScrollPos := (Sender as TScrollBar).Max-(Sender as TScrollBar).PageSize+1;
  {$ENDIF}
end;

procedure TGUISkinEditor.ScrollbarChange(Sender: TObject);
begin
  Render;
end;

procedure TGUISkinEditor.WidthEditChange(Sender: TObject);
Var
  Val : Integer;
begin
  val:=StrToIntDef(WidthEdit.Text, 0);
  If Val > 0 then
  Begin
    Width := Val;
    GLPanel1.Width := Val;
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    If Val > 256 then
    Begin
      SetMax(ScrollBar2,Val);
    End else
    Begin
      SetMax(ScrollBar2,256);
    End;
    Render;
  End;
end;

procedure TGUISkinEditor.HeightEditChange(Sender: TObject);
Var
  Val : Integer;
begin
  val:=StrToIntDef(HeightEdit.Text, 0);
  If Val > 0 then
  Begin
    Height := Val;
    GLPanel1.Height := Val;
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    If Val > 256 then
    Begin
      SetMax(ScrollBar1,Val);
    End else
    Begin
      SetMax(ScrollBar1,256);
    End;
    Render;
  End;
end;

procedure TGUISkinEditor.Button1Click(Sender: TObject);

Var
  S : String;
  Count : Integer;
  NewElement : TGLGuiElement;

begin
  Count := 1;
  Repeat
    S := ComboBox1.Text+IntToStr(Count);
    inc(Count);
  Until ListBox1.Items.IndexOf(S) = -1;
  NewElement := TheGuiComponent.Add as TGLGuiElement;
  NewElement.Name := S;
  NewElement.Align := TGUIAlignments(ComboBox1.ItemIndex);
  NewElement.BottomRight.SetPoint(0,0,0);
  NewElement.TopLeft.SetPoint(0,0,0);
  ListBox1.ItemIndex := ListBox1.Items.Add(S);
end;

procedure TGUISkinEditor.ListBox1Click(Sender: TObject);
begin
  If (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then
  Begin
    SelectedElement := TheGuiComponent.Items[ListBox1.ItemIndex];
    ComboBox1.ItemIndex := Integer(SelectedElement.Align);
    ScaleXEdit.Text := FloatToStr(SelectedElement.Scale.X);
    ScaleYEdit.Text := FloatToStr(SelectedElement.Scale.Y);
    LeftEdit.Text := FloatToStr(SelectedElement.TopLeft.X);
    TopEdit.Text := FloatToStr(SelectedElement.TopLeft.Y);
    RightEdit.Text := FloatToStr(SelectedElement.BottomRight.X);
    BottomEdit.Text := FloatToStr(SelectedElement.BottomRight.Y);
    Render;
  End else SelectedElement := Nil;
end;

procedure TGUISkinEditor.ComboBox1Change(Sender: TObject);
begin
  If Assigned(SelectedElement) then
  Begin
    SelectedElement.Align := TGUIAlignments(ComboBox1.ItemIndex);
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    Render;
  End;
end;

procedure TGUISkinEditor.Button2Click(Sender: TObject);
Var
  Index : Integer;

begin
  If (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then
  Begin
    Index := ListBox1.ItemIndex;
    TheGuiComponent.Delete(Index);
    ListBox1.Items.Delete(Index);
    If (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then
    Begin
      SelectedElement := TheGuiComponent.Items[ListBox1.ItemIndex];
      ComboBox1.ItemIndex := Integer(SelectedElement.Align);
    End else SelectedElement := Nil;
    Render;
  End;
end;

procedure TGUISkinEditor.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Button = TMouseButton(mbLeft) then
  if not CheckBox1.Checked then
  If Assigned(SelectedElement) then
  Begin
    SelectedElement.TopLeft.X := (ScrollBar2.Position-1)+Int(x/Zoom);
    SelectedElement.TopLeft.Y := (ScrollBar1.Position-1)+Int(y/Zoom);
  End;
end;

procedure TGUISkinEditor.Image1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Button = TMouseButton(mbLeft) then
  if not CheckBox1.Checked then
  If Assigned(SelectedElement) then
  Begin
    SelectedElement.BottomRight.X := (ScrollBar2.Position)+Int(x/Zoom);
    SelectedElement.BottomRight.Y := (ScrollBar1.Position)+Int(y/Zoom);
    Render;
  End;
end;

procedure TGUISkinEditor.Image1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Label7.Caption := 'X : '+FormatFloat('###0',Round((ScrollBar2.Position+x-1)/Zoom));
  Label8.Caption := 'Y : '+FormatFloat('###0',Round((ScrollBar1.Position+y-1)/Zoom));
end;

procedure TGUISkinEditor.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If key = glKey_LEFT then
  Begin
    If ListBox1.ItemIndex > 0 then ListBox1.ItemIndex := ListBox1.ItemIndex -1;
    key := glKey_CANCEL;
  End;
  If key = glKey_RIGHT then
  Begin
    If ListBox1.ItemIndex+1 < ListBox1.Items.Count then
      ListBox1.ItemIndex := ListBox1.ItemIndex +1;
    key := glKey_CANCEL;
  End;
    
end;

procedure TGUISkinEditor.CheckBox1Click(Sender: TObject);
begin
  GLPanel1.ReBuildGui := True;
  GLPanel1.GUIRedraw := True;
  Render;
end;

procedure TGUISkinEditor.ScaleXEditChange(Sender: TObject);
var
   res : Single;
begin
   if Assigned(SelectedElement) then begin
      res:=StrToFloatDef(ScaleXEdit.Text, 0);
      if res>0 then begin
         SelectedElement.Scale.X:=Res;
         GLPanel1.ReBuildGui:=True;
         GLPanel1.GUIRedraw:=True;
         Render;
      end;
   end;
end;

procedure TGUISkinEditor.ScaleYEditChange(Sender: TObject);
var
   res : Single;
begin
   if Assigned(SelectedElement) then begin
      res:=StrToFloatDef(ScaleYEdit.Text, 0);
      if res>0 then begin
         SelectedElement.Scale.Y:=Res;
         GLPanel1.ReBuildGui:=True;
         GLPanel1.GUIRedraw:=True;
         Render;
      end;
   end;
end;

procedure TGUISkinEditor.LeftEditChange(Sender: TObject);
var
   res : Single;
begin
   if Assigned(SelectedElement) then begin
      GLPanel1.BlockRender;
      try
        res:=StrToFloatDef(LeftEdit.Text, -1);
        if res>=0 then begin
           SelectedElement.TopLeft.X:=Res;
        end;
      finally
        GLPanel1.UnBlockRender;
      end;
      GLPanel1.ReBuildGui:=True;
      GLPanel1.GUIRedraw:=True;
      Render;
   end;
end;

procedure TGUISkinEditor.TopEditChange(Sender: TObject);
var
   res : Single;
begin
   if Assigned(SelectedElement) then begin
      GLPanel1.BlockRender;
      try
        res:=StrToFloatDef(TopEdit.Text, -1);
        if res>=0 then begin
           SelectedElement.TopLeft.Y:=Res;
        end;
      finally
        GLPanel1.UnBlockRender;
      end;
      GLPanel1.ReBuildGui:=True;
      GLPanel1.GUIRedraw:=True;
      Render;
   end;
end;

procedure TGUISkinEditor.RightEditChange(Sender: TObject);
var
   res : Single;
begin
   if Assigned(SelectedElement) then begin
      GLPanel1.BlockRender;
      try
        res:=StrToFloatDef(RightEdit.Text, -1);
        if res>=0 then begin
           SelectedElement.BottomRight.X:=Res;
        end;
      finally
        GLPanel1.UnBlockRender;
      end;
      GLPanel1.ReBuildGui:=True;
      GLPanel1.GUIRedraw:=True;
      Render;
   end;
end;

procedure TGUISkinEditor.BottomEditChange(Sender: TObject);
var
   res : Single;
begin
   if Assigned(SelectedElement) then begin
      GLPanel1.BlockRender;
      try
        res:=StrToFloatDef(BottomEdit.Text, -1);
        if res>=0 then begin
           SelectedElement.BottomRight.Y:=Res;
        end;
      finally
        GLPanel1.UnBlockRender;
      end;
      GLPanel1.ReBuildGui:=True;
      GLPanel1.GUIRedraw:=True;
      Render;
   end;
end;

end.




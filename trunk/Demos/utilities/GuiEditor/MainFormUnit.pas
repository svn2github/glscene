{: This is more than a sample which demonstrates the use of the Gui system,
   its an editor for combining several GUI-Component into one layout, using
   the same texture for them all.<p>

   The default layout image, is a modification based on Jan Horn's image in his
   windows (opengl) release...  <p>

   Be aware that for HUD purposes mip mapping should allways be disabled as the
   result might become blurred by the mipmap... Reason unknown.<br>

	<b>History : </b><font size=-1><ul>
      <li>19/09/02 - JAJ - Submitted to GLScene. Open/Save/Import + Edit/Preview.
	</ul></font>
}
unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, GLMisc, GLGui, StdCtrls, GLScene, GLWin32Viewer,
  GLObjects, GLHUDObjects, GLWindows, GLBitmapFont, GLWindowsFont, ExtDlgs,
  GLTexture;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Import1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    GLGuiLayout1: TGLGuiLayout;
    ImportDialog: TOpenDialog;
    Edit1: TMenuItem;
    EditLayout1: TMenuItem;
    ListBox: TListBox;
    ListPopup: TPopupMenu;
    Add1: TMenuItem;
    Remove1: TMenuItem;
    Edit2: TMenuItem;
    N3: TMenuItem;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLPanel1: TGLPanel;
    WindowsBitmapFont1: TGLWindowsBitmapFont;
    Image1: TMenuItem;
    Load1: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    GLMaterialLibrary1: TGLMaterialLibrary;
    HUDSprite1: TGLHUDSprite;
    Edit3: TEdit;
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Import1Click(Sender: TObject);
    procedure EditLayout1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    Procedure UpdateLayoutList;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   with GLMaterialLibrary1.Materials[0].Material.Texture.Image do
      LoadFromFile(ExtractFilePath(Application.ExeName)+'..\..\Media\DefaultSkin.bmp');
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  If OpenDialog.Execute then
  Begin
    GLScene1.BeginUpdate;
    try
      GLGuiLayout1.Clear;
      GLGuiLayout1.LoadFromFile(OpenDialog.FileName);
      UpdateLayoutList;
    finally
      GLScene1.EndUpdate;
    end;
  End;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if SaveDialog.Execute then
  Begin
    GLGuiLayout1.SaveToFile(SaveDialog.FileName);
  End;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  GLScene1.BeginUpdate;
  try
    GLGuiLayout1.Clear;
    UpdateLayoutList;
  finally
    GLScene1.EndUpdate;
  end;
End;

procedure TForm1.Import1Click(Sender: TObject);
Var
  XC : Integer;

begin
  if ImportDialog.Execute then
  Begin
    GLScene1.BeginUpdate;
    try
      For XC := 0 to ImportDialog.Files.Count-1 do
      Begin
        try
          GLGuiLayout1.LoadFromFile(ImportDialog.Files[XC]);
        except
        end;
      End;
      UpdateLayoutList;
    finally
      GLScene1.EndUpdate;
    end;
  End;
end;

procedure TForm1.EditLayout1Click(Sender: TObject);
begin
  GLScene1.BeginUpdate;
  try
    If ListBox.ItemIndex >= 0 then
    GLGui.GUIComponentDialog((ListBox.Items.Objects[ListBox.ItemIndex] as TGLGuiComponent).Elements)
  finally
    GLScene1.EndUpdate;
  end;
end;

Procedure TForm1.UpdateLayoutList;
var
  i : Integer;
begin
  ListBox.Clear;
  With GLGuiLayout1.GuiComponents do for i:=0 to Count-1 do
    ListBox.Items.AddObject(Items[i].Name, Items[i]);
End;

procedure TForm1.Add1Click(Sender: TObject);
Var
  GuiComp : TGLGuiComponent;

begin
  GuiComp := GLGuiLayout1.GuiComponents.Add as TGLGuiComponent;

  If ListBox.ItemIndex >= 0 then
  begin
    GuiComp.Name := 'Newly Added';
  end else GuiComp.Name := Edit3.Text;

  ListBox.Items.AddObject(GuiComp.Name, GuiComp);
end;

procedure TForm1.Remove1Click(Sender: TObject);
begin
  If ListBox.ItemIndex >= 0 then
  Begin
    GLScene1.BeginUpdate;
    try
      GLGuiLayout1.GUIComponents.Delete(ListBox.ItemIndex);
      ListBox.Items.Delete(ListBox.ItemIndex);
    finally
      GLScene1.EndUpdate;
    end;
  End;
end;

procedure TForm1.Edit2Click(Sender: TObject);
begin
  If ListBox.ItemIndex >= 0 then
  GLGui.GUIComponentDialog((ListBox.Items.Objects[ListBox.ItemIndex] as TGLGuiComponent).Elements)
end;

procedure TForm1.ListBoxClick(Sender: TObject);
begin
  GLScene1.BeginUpdate;
  try
    If ListBox.ItemIndex >= 0 then
    Begin
      GLPanel1.GuiLayoutName := GLGuiLayout1.GuiComponents.Items[ListBox.ItemIndex].Name;
      Edit3.text := GLPanel1.GuiLayoutName;
    End else
    Begin
      GLPanel1.GuiLayoutName := '';
      Edit3.text := 'Newly Added';
    End;
    GLPanel1.DoChanges;
  finally
    GLScene1.EndUpdate;
  end;
end;

procedure TForm1.Load1Click(Sender: TObject);
Var
  Mat : TGLLibMaterial;
  MatName : String;
begin
  If OpenPictureDialog.Execute then
  Begin
    GLScene1.BeginUpdate;
    try
      MatName := ExtractFileName(OpenPictureDialog.FileName);
      Mat := GLMaterialLibrary1.Materials.GetLibMaterialByName(MatName);
      If not Assigned(Mat) then
      Begin
        GLMaterialLibrary1.AddTextureMaterial(MatName,OpenPictureDialog.FileName).Material.Texture.TextureMode := tmReplace;
      End;
      GLGuiLayout1.Material.LibMaterialName := MatName;
      GLPanel1.Material.LibMaterialName := MatName;
      HUDSprite1.Material.LibMaterialName := MatName;
    finally
      GLScene1.EndUpdate;
    end;
  End;
end;

procedure TForm1.Edit3Change(Sender: TObject);
begin
  If (ListBox.ItemIndex >= 0) then
  Begin
    ListBox.Items[ListBox.ItemIndex] := Edit3.Text;
    GLGuiLayout1.GuiComponents.Items[ListBox.ItemIndex].Name := Edit3.Text;
  End;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
   Application.Terminate;
end;

end.

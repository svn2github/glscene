unit FDGLLibShaderPicker;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  DGLShader;

type
  TDGLLibShaderPicker = class(TForm)
    Label2: TLabel;
    LBShaders: TListBox;
    BBCancel: TBitBtn;
    BBOk: TBitBtn;
    procedure LBShadersDblClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function Execute(var shaderName: TDGLLibShaderName; shaderLibrary: TDGLAbstractShaderLibrary): Boolean;
  end;

function DGLLibShaderPicker: TDGLLibShaderPicker;
procedure ReleaseLibShaderPicker;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.dfm}

var
  vLibShaderPicker: TDGLLibShaderPicker;

function DGLLibShaderPicker: TDGLLibShaderPicker;
begin
  if not Assigned(vLibShaderPicker) then
    vLibShaderPicker := TDGLLibShaderPicker.Create(nil);
  Result := vLibShaderPicker;
end;

procedure ReleaseLibShaderPicker;
begin
  if Assigned(vLibShaderPicker) then
  begin
    vLibShaderPicker.Free;
    vLibShaderPicker := nil;
  end;
end;

function TDGLLibShaderPicker.Execute(var shaderName: TDGLLibShaderName; shaderLibrary: TDGLAbstractShaderLibrary): Boolean;
begin
  with LBShaders do
  begin
    shaderLibrary.SetNamesToTStrings(LBShaders.Items);
    ItemIndex := Items.IndexOf(shaderName);
    if (ItemIndex < 0) and (Items.Count > 0) then
      ItemIndex := 0;
    BBOk.Enabled := (Items.Count > 0);
  end;
  //LBMaterialsClick(Self);
  Result := (ShowModal = mrOk);
  if Result then
  begin
    with LBShaders do
      if ItemIndex >= 0 then
        shaderName := Items[ItemIndex]
      else
        shaderName := '';
  end;
end;

procedure TDGLLibShaderPicker.LBShadersDblClick(Sender: TObject);
begin
 BBOk.Click;
end;

initialization

finalization
  ReleaseLibShaderPicker;

end.

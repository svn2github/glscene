unit FRMaterialPreview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Media, FMX.Viewport3D, FMX.ListBox,

  GLS.Scene, GLS.SceneViewer, GLS.Material, GLS.Teapot, FMX.MaterialSources;

type
  TRMaterialPreview = class(TFrame)
    CBObject: TComboBox;
    CBBackground: TComboBox;
    GLSceneViewer: TViewport3D;
    Camera: TCameraComponent;
    LightMaterialSource1: TLightMaterialSource;
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
  private
    FLibMaterial: TGLAbstractLibMaterial;
    function GetLibMaterial: TGLAbstractLibMaterial;
    function GetMaterial: TGLMaterial;
    procedure SetLibMaterial(const Value: TGLAbstractLibMaterial);
    procedure SetMaterial(const Value: TGLMaterial);
    { Private declarations }
  public
    { Public declarations }
    GLMaterialLibrary: TGLMaterialLibrary;
    constructor Create(AOwner : TComponent); override;
    property Material : TGLMaterial read GetMaterial
      write SetMaterial;
    property LibMaterial : TGLAbstractLibMaterial read GetLibMaterial
      write SetLibMaterial;
  end;

implementation

{$R *.fmx}

var
  MX, MY: Integer;

{ TRMaterialPreview }

procedure TRMaterialPreview.CBBackgroundChange(Sender: TObject);
var
   bgColor : TColor;
begin
   case CBBackground.ItemIndex of
      1 : bgColor:=clWhite;
      2 : bgColor:=clBlack;
      3 : bgColor:=clBlue;
      4 : bgColor:=clRed;
      5 : bgColor:=clGreen;
   else
      bgColor:=clNone;
   end;
   with BackGroundSprite.Material do begin
      Texture.Disabled:=(bgColor<>clNone);
      FrontProperties.Diffuse.Color:=ConvertWinColor(bgColor);
   end;
end;

procedure TRMaterialPreview.CBObjectChange(Sender: TObject);
var
   i : Integer;
begin
   i:=CBObject.ItemIndex;
   Cube.Visible   := I = 0;
   Sphere.Visible := I = 1;
   Cone.Visible   := I = 2;
   Teapot.Visible := I = 3;
end;

constructor TRMaterialPreview.Create(AOwner: TComponent);
begin
  inherited;
   BackGroundSprite.Position.X := GLSceneViewer.Width div 2;
   BackGroundSprite.Position.Y := GLSceneViewer.Height div 2;
   BackGroundSprite.Width := GLSceneViewer.Width;
   BackGroundSprite.Height := GLSceneViewer.Height;

   CBObject.ItemIndex:=0;     CBObjectChange(Self);
   CBBackground.ItemIndex:=0; CBBackgroundChange(Self);
end;

function TRMaterialPreview.GetLibMaterial: TGLAbstractLibMaterial;
begin
  Result := FLibMaterial;
end;

function TRMaterialPreview.GetMaterial: TGLMaterial;
begin
  Result := GLMaterialLibrary.Materials[0].Material;
end;

procedure TRMaterialPreview.SetLibMaterial(const Value: TGLAbstractLibMaterial);
begin
  FLibMaterial := Value;
  if Assigned(FLibMaterial) then
  begin
    with GLMaterialLibrary.Materials[0] do
    begin
      Material.MaterialLibrary := FLibMaterial.MaterialLibrary;
      Material.LibMaterialName := FLibMaterial.Name
    end;
  end
  else
  with GLMaterialLibrary.Materials[0] do
  begin
    Material.MaterialLibrary := nil;
    Material.LibMaterialName := '';
  end;
end;

procedure TRMaterialPreview.SetMaterial(const Value: TGLMaterial);
begin
  GLMaterialLibrary.Materials[0].Material.Assign(Value.GetActualPrimaryMaterial);
end;

end.

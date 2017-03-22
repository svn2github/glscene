unit uHint;

interface

uses
  GLScene, GLHUDObjects, GLWindowsFont, GLMaterial, GLTexture, GLCrossPlatform,

  Windows, Classes;



const
  C_HINTS_TEXTUREPATH = 'data\hints\';

type

  TdfHintAlign = (haTop, haBottom, haLeft, haRight, haCenter);

  TdfHint = class (TGLHUDSprite)
  private
    FHintText: TGLHUDText;
  public
    property HintText: TGLHUDText read FHintText write FHintText;

    procedure Init(aText: String; aFont: TGLWindowsBitmapFont;
      aHintTexturePath: String; aAlign: TdfHintAlign;
      aOffsetX, aOffsetY: Single; aSpriteScale, aTextScale: Single);

    procedure SetPosition(X, Y: Single);
  end;

implementation

uses
  uGLSceneObjects;

{ TdfHint }

procedure TdfHint.Init(aText: String; aFont: TGLWindowsBitmapFont;
  aHintTexturePath: String; aAlign: TdfHintAlign; aOffsetX, aOffsetY: Single;
  aSpriteScale, aTextScale: Single);

var
  w, h: Single;
begin
  if not Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(aHintTexturePath)) then
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := aHintTexturePath;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_HINTS_TEXTUREPATH + aHintTexturePath);
        w := Texture.Image.Width  * aSpriteScale;
        h := Texture.Image.Height * aSpriteScale;
        Texture.Enabled := True;
        Texture.TextureMode := tmModulate;
        Texture.TextureWrap := twNone;
        MaterialOptions := [moIgnoreFog, moNoLighting];
        BlendingMode := bmTransparency;
        FrontProperties.Diffuse.SetColor(1,1,1);
      end;
    end
  else
    with dfGLSceneObjects.MatLibrary.LibMaterialByName(aHintTexturePath).Material.Texture.Image do
    begin
      w := Width  * aSpriteScale;
      h := Height * aSpriteScale;
    end;

  Material.MaterialLibrary := dfGLSceneObjects.MatLibrary;
  Material.LibMaterialName := aHintTexturePath;
  Width  := w;
  Height := h;

  FHintText := TGLHUDText.CreateAsChild(Self);
  FHintText.Text := aText;
  FHintText.BitmapFont := aFont;
  aFont.EnsureString(aText);
  FHintText.Scale.SetVector(aTextScale, aTextScale);

  case aAlign of
    haTop:
    begin
      FHintText.Position.SetPoint(Position.X + aOffsetX,
                                  Position.Y - Height / 2 + aOffsetY, 0);
      FHintText.Alignment := taCenter;
      FHintText.Layout := tlBottom;
    end;
    haBottom:
    begin
      FHintText.Position.SetPoint(Position.X + aOffsetX,
                                  Position.Y + Height / 2 + aOffsetY, 0);
      FHintText.Alignment := taCenter;
      FHintText.Layout := tlTop;
    end;
    haLeft:
    begin
      FHintText.Position.SetPoint(Position.X - Width / 2 + aOffsetX,
                                  Position.Y + aOffsetY, 0);
      FHintText.Alignment := taRightJustify;
      FHintText.Layout := tlCenter;
    end;
    haRight:
    begin
      FHintText.Position.SetPoint(Position.X + Width / 2 + aOffsetX,
                                  Position.Y + aOffsetY, 0);
      FHintText.Alignment := taLeftJustify;
      FHintText.Layout := tlCenter;
    end;
    haCenter:
    begin
      FHintText.Position.SetPoint(Position.X + aOffsetX,
                                  Position.Y + aOffsetY, 0);
      FHintText.Alignment := taCenter;
      FHintText.Layout := tlCenter;
    end;
  end;
end;

procedure TdfHint.SetPosition(X, Y: Single);
var
  dx, dy: Single;
begin
  dx := X - Position.X;
  dy := Y - Position.Y;
  Position.SetPoint(X, Y, 0);
  with FHintText.Position do
  begin
    X := X + dx;
    Y := Y + dy;
  end;
end;

end.

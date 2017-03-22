unit uFakeSpacefighter;

interface

uses
  GLMaterial, GLTexture,

  uSpacefighter, uSimplePhysics;

type
  TdfFakeSpaceFighter = class(TdfSpaceFighter)
  protected
    procedure SetFighterMaterial(texturePath: String); override;
  end;

implementation

{ TdfFakeSpaceFighter }

procedure TdfFakeSpaceFighter.SetFighterMaterial(texturePath: String);
begin
  inherited;
  with FFighter.Material.GetActualPrimaryMaterial do
  begin
    BlendingMode := bmTransparency;
    FrontProperties.Diffuse.SetColor(0.5, 0.5, 1, 0.5);
  end;
end;

end.

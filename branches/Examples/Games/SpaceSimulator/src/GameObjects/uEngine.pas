unit uEngine;

interface

uses
  GLScene, GLObjects, GLParticleFX;

type
  TdfEngine = class(TGLDummyCube)
  private
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;

    procedure SetEffect(aPFXManager: TGLParticleFXManager);
  end;

implementation

{ TdfEngine }

constructor TdfEngine.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
//  VisibleAtRunTime := True;
end;

procedure TdfEngine.SetEffect(aPFXManager: TGLParticleFXManager);
begin
  if aPFXManager <> nil then
    with TGLSourcePFXEffect(Self.Effects.GetOrCreate(TGLSourcePFXEffect)) do
    begin
      Manager := aPFXManager;
      Enabled := True;
      PositionDispersion := 0.2;
      ParticleInterval := 0.02;
      VelocityDispersion := 0.3;
      DisabledIfOwnerInvisible := True;
    end;
end;

end.

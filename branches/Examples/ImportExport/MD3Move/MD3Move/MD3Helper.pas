unit MD3Helper;

{$DEFINE MD3Helper_ZeroDivide1}
interface 

uses 
  SysUtils, Classes, Jpeg,
   cene
  Q3MD3, GLFileMD3, GLObjects, GLVectorFileObjects, GLScene, GLTexture,
  GLVectorGeometry,GLMaterial;

type 
  TGLMD3Actor = class 
    private 
      LegsTags, TorsoTags, WeaponTags: TMD3TagList; 
      FCharacterModel, FWeaponModel, FName, FAnimation: string; 
      FAnimationMode: TGLActorAnimationMode;
      FAlphaLevel: Single;
      FTag: integer;
      procedure LoadSkin(SkinFilePath, SkinShortName: string);
      procedure SetCharacterModel(AValue: string);
      procedure SetWeaponModel(AValue: string);
      procedure SetAnimationMode(AValue: TGLActorAnimationMode);
      procedure SetAlphaLevel(AValue: Single);
      procedure SetTag(AValue: integer);
      procedure SetName(AValue: string);
      procedure SetAnimation(AValue: string);
    public
      Legs, Head, Torso, Weapon: TGLActor;
      MaterialLibrary: TGLMaterialLibrary;
      HeadRot:double;
      constructor Create(AOwner: TGLBaseSceneObject);
      function StartFrame: integer;
      function CurrentFrame: integer; 
      function EndFrame: integer; 
      procedure Progress(deltaTime: Single);
      property CharacterModel: string read FCharacterModel write SetCharacterModel; 
      property WeaponModel: string read FWeaponModel write SetWeaponModel; 
      property AnimationMode: TGLActorAnimationMode read FAnimationMode write SetAnimationMode;
      property AlphaLevel: Single read FAlphaLevel write SetAlphaLevel; 
      property Animation: string read FAnimation write SetAnimation; 
      property Tag: integer read FTag write SetTag; 
      property Name: string read FName write SetName;
      destructor Destroy; override;
  end; 

implementation 

uses
  Main;

function MD3Random(ARange: integer): integer; 
begin
  Result := Random(ARange);
  if Result = 0 then Result := 1;
end;
function GetRnd (rn:Integer):String;
var t:integer;
begin
  t:=Random(rn);
  if t=0 then Result:='' else Result:=InTToStr(t);
end;
{$IFNDEF MD3Helper_ZeroDivide}
procedure SwitchToAnimation(AObject: TGLActor; AAnimationName: string; ASmooth: boolean = false);
begin 
  if AObject.CurrentAnimation <> AAnimationName then 
    AObject.SwitchToAnimation(AAnimationName, ASmooth);
end; 

function LoadModel(AActor: TGLActor; AFileName: string): boolean;
begin 
  Result := false; 
  if not FileExists(AFileName) then Exit;
  AActor.LoadFromFile(AFileName); 
  Result := true; 
end; 

function LoadTexture(AMaterial: TGLMaterial; AFileName: string): boolean; 
begin 
  Result := false; 
  if not FileExists(AFileName) then Exit; 
  AMaterial.Texture.Image.LoadFromFile(AFileName); 
  Result := true; 
end; 
{$ENDIF} 

function InterpolateMatrix(m1, m2: TMatrix; delta: single): TMatrix; 
var 
  i, j: integer; 
begin 
  for j := 0 to 3 do for i := 0 to 3 do Result.V[i].V[j] :=
     m1.V[i].V[j] + (m2.V[i].V[j] - m1.V[i].V[j]) * delta;
end; 

constructor TGLMD3Actor.Create(AOwner: TGLBaseSceneObject);
begin
  inherited Create;
  HeadRot:=0;
  MaterialLibrary := TGLMaterialLibrary.Create(nil);
  Legs := TGLActor.CreateAsChild(AOwner);
  Torso := TGLActor.CreateAsChild(Legs);
  Head := TGLActor.CreateAsChild(Torso);
  Weapon := TGLActor.CreateAsChild(Torso);
  Legs.Direction.Assign(AOwner.Direction);
  Legs.Up.Assign(AOwner.Up);
  Torso.Direction.Assign(AOwner.Direction);
  Torso.Up.Assign(AOwner.Up);
  Head.Direction.Assign(AOwner.Direction);
  Head.Up.Assign(AOwner.Up);
  Weapon.Direction.Assign(AOwner.Direction);
  Weapon.Up.Assign(AOwner.Up);
  Weapon.Material.Texture.Disabled := false;
  LegsTags := TMD3TagList.Create;
  TorsoTags := TMD3TagList.Create;
  WeaponTags := TMD3TagList.Create;
  Legs.MaterialLibrary := MaterialLibrary;
  Torso.MaterialLibrary := MaterialLibrary;
  Head.MaterialLibrary := MaterialLibrary;
  Legs.AnimationMode := aamLoop;
  Torso.AnimationMode := aamLoop;
  Legs.TagObject := AOwner;
  Torso.TagObject := AOwner;
  Head.TagObject := AOwner;
  Weapon.TagObject := AOwner;
  FAlphaLevel := 1;
end;

function TGLMD3Actor.StartFrame: integer;
begin 
  Result := 0; 
  if Animation = 'stand' then Result := Legs.StartFrame; 
  if Animation = 'crstnd' then Result := Legs.StartFrame; 
  if Animation = 'attack' then Result := Torso.StartFrame; 
  if Animation = 'attack2' then Result := Torso.StartFrame; 
  if Animation = 'walk' then Result := Legs.StartFrame; 
  if Animation = 'run' then Result := Legs.StartFrame; 
  if Animation = 'pain' then Result := Torso.StartFrame; 
  if Animation = 'death' then Result := Legs.StartFrame; 
end; 

function TGLMD3Actor.CurrentFrame: integer; 
begin 
  Result := 0; 
  if Animation = 'stand' then Result := Legs.CurrentFrame; 
  if Animation = 'crstnd' then Result := Legs.CurrentFrame; 
  if Animation = 'attack' then Result := Torso.CurrentFrame; 
  if Animation = 'attack2' then Result := Torso.CurrentFrame; 
  if Animation = 'walk' then Result := Legs.CurrentFrame; 
  if Animation = 'run' then Result := Legs.CurrentFrame; 
  if Animation = 'pain' then Result := Torso.CurrentFrame; 
  if Animation = 'death' then Result := Legs.CurrentFrame; 
end; 

function TGLMD3Actor.EndFrame: integer; 
begin 
  Result := 0; 
  if Animation = 'stand' then Result := Legs.EndFrame; 
  if Animation = 'crstnd' then Result := Legs.EndFrame;
  if Animation = 'attack' then Result := Torso.EndFrame;
  if Animation = 'attack2' then Result := Torso.EndFrame;
  if Animation = 'walk' then Result := Legs.EndFrame;
  if Animation = 'run' then Result := Legs.EndFrame;
  if Animation = 'pain' then Result := Torso.EndFrame;
  if Animation = 'death' then Result := Legs.EndFrame;
end;

procedure TGLMD3Actor.Progress(deltaTime: Single);
begin
  Torso.Matrix := InterpolateMatrix(LegsTags.GetTransform('tag_torso', Legs.CurrentFrame), LegsTags.GetTransform('tag_torso', Legs.NextFrameIndex), Legs.CurrentFrameDelta);
  Head.Matrix := InterpolateMatrix(TorsoTags.GetTransform('tag_head', Torso.CurrentFrame), TorsoTags.GetTransform('tag_head', Torso.NextFrameIndex), Torso.CurrentFrameDelta);
  Head.Roll(HeadRot);

  Weapon.Matrix := InterpolateMatrix(TorsoTags.GetTransform('tag_weapon', Torso.CurrentFrame), TorsoTags.GetTransform('tag_weapon', Torso.NextFrameIndex), Torso.CurrentFrameDelta);
end; 

procedure TGLMD3Actor.LoadSkin(SkinFilePath, SkinShortName: string); 
var 
  Index: integer; 
  MatName, PicFileName: string; 
  stl, stlBuf, stlPics: TStringList; 
  MaterialLibrary: TGLMaterialLibrary; 

  procedure FetchStlBuf(Prefix: string); 
  var 
    FileName: string; 
  begin 
    FileName := SkinFilePath + Prefix + SkinShortName; 
    if FileExists(FileName) then stl.LoadFromFile(FileName); 
    stlBuf.AddStrings(stl); 
  end; 

  function GetMaterialPicFilename(Material: string): string; 
  var 
    i: integer; 
  begin 
    Material := UpperCase(Material); 
    for i := 0 to stlBuf.Count - 1 do 
      if Pos(Material, UpperCase(stlBuf[i])) = 1 then begin 
        Result := ExtractFileName(StringReplace(stlBuf[i], '/', '\', [rfReplaceAll])); 
        Break; 
      end; 
  end; 

  procedure DoActorMaterials(Actor: TGLActor); 
  var 
    i, n: integer; 
  begin 
    for i := 0 to Actor.MeshObjects.Count - 1 do 
      for n := 0 to Actor.MeshObjects[i].FaceGroups.Count - 1 do begin 
        MatName := Actor.MeshObjects[i].FaceGroups[n].MaterialName; 
        PicFileName := GetMaterialPicFilename(MatName); 
        Index := stlPics.IndexOf(PicFileName); 
        if Index = - 1 then begin 
          stlPics.AddObject(PicFileName, Actor.MeshObjects[i].FaceGroups[n]); 
          PicFileName := SkinFilePath + ChangeFileExt(PicFileName, '.jpg');
          if FileExists(PicFileName) then MaterialLibrary.Materials.GetLibMaterialByName(MatName).Material.Texture.Image.LoadFromFile(PicFileName); 
        end else Actor.MeshObjects[i].FaceGroups[n].MaterialName := TGLFaceGroup(stlPics.Objects[Index]).MaterialName;
      end; 
  end; 
begin 
  MaterialLibrary := Head.MaterialLibrary; 
  if not Assigned(MaterialLibrary) then Exit; 
  stl := TStringList.Create; 
  stlBuf := TStringList.Create; 
  stlPics := TStringList.Create; 
  SkinFilePath := IncludeTrailingBackslash(SkinFilePath); 
  SkinShortName := ChangeFileExt(SkinShortName, '.skin'); 
  FetchStlBuf('Head_'); 
  FetchStlBuf('Upper_'); 
  FetchStlBuf('Lower_'); 
  DoActorMaterials(Head); 
  DoActorMaterials(Torso); 
  DoActorMaterials(Legs); 
  stl.Free; 
  stlBuf.Free; 
  stlPics.Free; 
end; 

procedure TGLMD3Actor.SetCharacterModel(AValue: string); 
begin 
  if (AValue = FCharacterModel) or (AValue = '') then Exit;
  MaterialLibrary.Materials.Clear; 
  LoadModel(Legs, Format('.\Models\%s\%s.md3', [AValue, 'lower']));
  LoadModel(Torso, Format('.\Models\%s\%s.md3', [AValue, 'upper'])); 
  LoadModel(Head, Format('.\Models\%s\%s.md3', [AValue, 'head'])); 
  LegsTags.LoadFromFile(Format('.\Models\%s\%s.md3', [AValue, 'lower'])); 
  TorsoTags.LoadFromFile(Format('.\Models\%s\%s.md3', [AValue, 'upper'])); 
  LoadSkin(Format('.\Models\%s\', [AValue]), 'default'); 
  LoadQ3Anims(Legs.Animations, Format('.\Models\%s\animation.cfg', [AValue]), 'BOTH'); 
  LoadQ3Anims(Legs.Animations, Format('.\Models\%s\animation.cfg', [AValue]), 'LEGS');
  LoadQ3Anims(Torso.Animations, Format('.\Models\%s\animation.cfg', [AValue]), 'BOTH');
  LoadQ3Anims(Torso.Animations, Format('.\Models\%s\animation.cfg', [AValue]), 'TORSO');

  Legs.SwitchToAnimation('legs_idle');
  Torso.SwitchToAnimation('torso_stand'); 
  FCharacterModel := AValue; 
end; 

procedure TGLMD3Actor.SetWeaponModel(AValue: string); 
begin 
  if (AValue = FWeaponModel) or (AValue = '') then Exit; 
  LoadModel(Weapon, Format('.\Models\Weapons\%s\%s.md3', [AValue,AValue])); 
  LoadTexture(Weapon.Material, Format('.\Models\Weapons\%s\%s.jpg', [AValue,AValue]));
  FWeaponModel := AValue; 
end; 

procedure TGLMD3Actor.SetAnimationMode(AValue: TGLActorAnimationMode);
begin 
  if AValue = FAnimationMode then Exit; 
  Legs.AnimationMode := AValue; 
  Torso.AnimationMode := AValue; 
  FAnimationMode := AValue; 
end; 

procedure TGLMD3Actor.SetAlphaLevel(AValue: Single); 
var 
  i: integer; 
begin 
  if AValue = FAlphaLevel then Exit; 
  if AValue = 1 then begin 
    for i := 0 to MaterialLibrary.Materials.Count - 1 do 
      MaterialLibrary.Materials[i].Material.BlendingMode := bmOpaque; 
    Weapon.Material.BlendingMode := bmOpaque; 
    Exit; 
  end else begin 
    for i := 0 to MaterialLibrary.Materials.Count - 1 do 
      MaterialLibrary.Materials[i].Material.BlendingMode := bmTransparency; 
    Weapon.Material.BlendingMode := bmTransparency; 
  end; 
  for i := 0 to MaterialLibrary.Materials.Count - 1 do 
    MaterialLibrary.Materials[i].Material.FrontProperties.Diffuse.Alpha := AValue; 
  Weapon.Material.FrontProperties.Diffuse.Alpha := AValue; 
  FAlphaLevel := AValue; 
end; 

procedure TGLMD3Actor.SetTag(AValue: integer); 
begin 
  if AValue = FTag then Exit; 
  Head.Tag := AValue; 
  Torso.Tag := AValue; 
  Legs.Tag := AValue; 
  Weapon.Tag := AValue; 
end; 

procedure TGLMD3Actor.SetName(AValue: string); 
begin 
  if AValue = FName then Exit; 
  Head.Name := AValue + '_Head'; 
  Torso.Name := AValue + '_Torso'; 
  Legs.Name := AValue + '_Legs'; 
  Weapon.Name := AValue + '_Weapon'; 
end; 

procedure TGLMD3Actor.SetAnimation(AValue: string); 
begin 
  AValue := LowerCase(AValue);
  if AValue = LowerCase(FAnimation) then Exit; 
  if AValue = 'stand' then begin 
    SwitchToAnimation(Legs, 'legs_idle');
    SwitchToAnimation(Torso, 'torso_stand' + GetRnd (3));
  end; 
  if AValue = 'attack' then begin 
    SwitchToAnimation(Legs, 'legs_idle');
    SwitchToAnimation(Torso, 'torso_attack'); 
  end; 
  if AValue = 'attack2' then begin 
    SwitchToAnimation(Legs, 'legs_idle'); 
    SwitchToAnimation(Torso, 'torso_attack2'); 
  end; 
  if AValue = 'walk' then begin 
    SwitchToAnimation(Legs, 'legs_walk'); 
    SwitchToAnimation(Torso, 'torso_stand' + GetRnd (3));
  end; 
  if AValue = 'run' then begin 
    SwitchToAnimation(Legs, 'legs_run'); 
    SwitchToAnimation(Torso, 'torso_stand' + GetRnd (3)); 
  end; 
  if AValue = 'pain' then begin 
    SwitchToAnimation(Legs, 'legs_idle'); 
    SwitchToAnimation(Torso, 'torso_raise'); 
  end; 
  if AValue = 'death' then begin 
    SwitchToAnimation(Legs, 'both_death' + GetRnd (4));
    SwitchToAnimation(Torso, 'torso_dead' + GetRnd (4));
  end;
  if AValue = 'crstnd' then begin
    SwitchToAnimation(Legs, 'legs_idlecr');
    SwitchToAnimation(Torso, 'torso_stand' + GetRnd (3));
  end; 
  FAnimation := LowerCase(AValue); 
end; 

destructor TGLMD3Actor.Destroy; 
begin 
  MaterialLibrary.Free; 
  Weapon.Free; 
  Head.Free; 
  Torso.Free; 
  Legs.Free; 
  LegsTags.Free; 
  TorsoTags.Free; 
  WeaponTags.Free; 
  inherited Destroy; 
end; 

end. 


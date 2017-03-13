{
  Демка, демонстрирующая вращение костей и смешивание кадров скелетной
  анимации без использования TGLAnimationControler, ну и на закуску - кривое
  прикрепление оружия к руке... =)

  Интерфейс и код такой убогий потому что демка писАлась где-то в середине 2008 года
  и предназначалась для личного пользования, а потом дописывалась впопыхах... =)

  В общем, скроллбары подписаны, для переключения анимаций используется комбобокс,
  а Большая Кнопка отвечает за стрельбу...

  Модельки и код можете юзать по усмотрению.

  И не взумайте удалять файл hero.ini!!!!!

  Евгений Шишкин a.k.a. E-Cone & FA~SHISH
  MATRA Games (c) 2005-2009
}
unit herotester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls,
   
  GLScene, GLVectorFileObjects, GLWin32Viewer, GLCrossPlatform, GLFileSMD,
  GLCadencer, GLFile3DS, GLObjects, GLVectorGeometry, GLVectorTypes,
  GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    hero: TGLActor;
    TrackBar1: TTrackBar;
    ComboBox1: TComboBox;
    GLCadencer1: TGLCadencer;
    Label1: TLabel;
    TrackBar2: TTrackBar;
    inhander: TGLFreeForm;
    fpser: TTimer;
    dummy: TGLCube;
    TrackBar3: TTrackBar;
    GLLines1: TGLLines;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    bigbtn: TButton;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure fpserTimer(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure bigbtnClick(Sender: TObject);
    procedure heroFrameChanged(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;
  savedanim: TGLSkeletonFrameList;
  INDEX, oldframe, sf: integer;
  animate, saved: boolean;

implementation

{$R *.dfm}

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  hero.Interval := TrackBar1.Position;
  Label1.Caption := inttostr(TrackBar1.Position);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ComboBox1.Items.LoadFromFile('hero.ini');
  hero.LoadFromFile('hero_reference.smd');
  for i := 0 to ComboBox1.Items.Count - 1 do
  begin
    ComboBox1.Items.Strings[i] := 'hero_a_' + ComboBox1.Items.Strings[i];
    hero.AddDataFromFile(ComboBox1.Items.Strings[i] + '.smd');
  end;
  hero.SwitchToAnimation(1);

  inhander.LoadFromFile('weapon.3ds');
  savedanim := TGLSkeletonframelist.Create;

  animate := false;
  saved := false;
end;

procedure TForm1.bigbtnClick(Sender: TObject);
begin
  INDEX := 0;
  animate := true;
  oldframe := hero.CurrentFrame;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  hero.SwitchToAnimation(ComboBox1.Text);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  hero.RollAngle := TrackBar2.Position;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);

begin
  if not saved then
  begin
    savedanim.Assign(hero.Skeleton.Frames);
    saved := true;
  end;
  inhander.Matrix := hero.Skeleton.bonebyname('Bip01 R Hand').globalmatrix;
end;

procedure TForm1.heroFrameChanged(Sender: TObject);
var
  frame, i: integer;
begin
  if (animate) then
  begin
    sf := hero.Animations.Items[12].StartFrame;

    frame := hero.NextFrameIndex;
    hero.Skeleton.Frames[frame].Rotation[14] := savedanim[sf + index]
      .Rotation[14];
    hero.Skeleton.Frames[frame].Rotation[15] := savedanim[sf + index]
      .Rotation[15];
    hero.Skeleton.Frames[frame].Rotation[16] := savedanim[sf + index]
      .Rotation[16];
    hero.Skeleton.Frames[frame].Rotation[17] := savedanim[sf + index]
      .Rotation[17];
    hero.Skeleton.Frames[frame].Rotation[18] := savedanim[sf + index]
      .Rotation[18];

    INDEX := INDEX + 1;
    if index > 7 then
    begin
      for i := 0 to savedanim.Count - 1 do
        hero.Skeleton.Frames[i].Assign(savedanim[i]);

      animate := false;
      index := 0;
    end;

  end;
  hero.StructureChanged;

end;

procedure TForm1.fpserTimer(Sender: TObject);
begin
  Form1.Caption := floattostr(GLSceneViewer1.FramesPerSecond);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
var
  i: integer;
begin
  dummy.RollAngle := TrackBar3.Position - 180;
  // hero.Skeleton.CurrentFrame.Rotation.Items[hero.Skeleton.BoneByName('Bip01 Spine2').BoneID]:=affinevectormake(dummy.RollAngle,dummy.RollAngle,dummy.RollAngle);
  if (hero.CurrentAnimation = 'hero_a_idle_pistol') or
    (hero.CurrentAnimation = 'hero_a_atk_pistol') then
    for i := hero.StartFrame to hero.EndFrame do
      hero.Skeleton.Frames.Items[i].Rotation.Items
        [hero.Skeleton.bonebyname('Bip01 Spine2').BoneID] :=
        affinevectormake(0, 0, (TrackBar3.Position - 180) / 100);
  // hero.StructureChanged;
end;

end.

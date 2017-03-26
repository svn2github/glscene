unit fMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Menus,
   
  GLScene,
  GLMaterial,
  GLVectorFileObjects,
  GLObjects,
  GLWin32Viewer,
  GLVectorGeometry,
  GLTexture,
  GLCadencer,
  GLFileSMD,
  GLVectorLists,
  GLGeomObjects,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,

  uSkeletonColliders, GLGraph;

type
  TMainForm = class(TForm)
    GLScene1: TGLScene;
    Panel1: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    GLCadencer1: TGLCadencer;
    BonesCombo: TComboBox;
    Label1: TLabel;
    ColliderTypeCombo: TComboBox;
    Label2: TLabel;
    RadiusEdit: TEdit;
    Label3: TLabel;
    ObjectName: TLabel;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    GroupBox1: TGroupBox;
    Button3: TButton;
    Label4: TLabel;
    Label5: TLabel;
    HeightEdit: TEdit;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CameraDummy: TGLDummyCube;
    FrameLabel: TLabel;
    TrackBar1: TTrackBar;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    LoadModel1: TMenuItem;
    AddAnimation1: TMenuItem;
    Colliders1: TMenuItem;
    Help1: TMenuItem;
    Generate1: TMenuItem;
    DeleteAll1: TMenuItem;
    N2: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    ReadMe1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    Button8: TButton;
    Button9: TButton;
    DepthEdit: TEdit;
    Label9: TLabel;
    GLHeightField1: TGLHeightField;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BonesComboChange(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GLActor1FrameChanged(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure LoadModel1Click(Sender: TObject);
    procedure AddAnimation1Click(Sender: TObject);
    procedure Generate1Click(Sender: TObject);
    procedure DeleteAll1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     
  public
     
    mx, my: Integer;
    procedure Reset;
  end;

var
  MainForm: TMainForm;
  ColliderNames: TStrings;
  ColliderRotations: TAffineVectorList;
  SelectedCollider: TGLSceneObject;

procedure AlignSkeletonBoundingObjects(Actor: TGLActor;
  CreateBoundingObjects: Boolean);
procedure CollidersToStrings(Actor: TGLActor; DescNames, ObjNames: TStrings);
procedure ExtractBoneIDsFromString(str: string; var BoneID1, BoneID2: Integer);

implementation

{$R *.dfm}

const
  BaseColliderColor = clSkyBlue;
  ActiveColliderColor = clRed;
  AlphaLevel = 0.5;

procedure AlignSkeletonBoundingObjects(Actor: TGLActor;
  CreateBoundingObjects: Boolean);

  procedure RecursBones(bone: TGLSkeletonBone);
  var
    i, j, k: Integer;
    BoneVertices: TAffineVectorList;
    bounding_radius, temp: single;
    a, a_hat: TAffineVector;
    bounding_object: TGLBaseSceneObject;
  begin
    // Check if the bone has any children
    if bone.count > 0 then
    begin
      // Loop through each of the bones children
      for i := 0 to bone.count - 1 do
      begin
        bounding_object := nil;

        // Are we generating the bounding objects?
        if CreateBoundingObjects then
        begin
          // Get all vertices weighted to this bone
          BoneVertices := TAffineVectorList.Create;
          for j := 0 to Actor.MeshObjects.count - 1 do
            with TGLSkeletonMeshObject(Actor.MeshObjects[j]) do
              for k := 0 to Vertices.count - 1 do
                if bone.BoneID = VerticesBonesWeights[k][0].BoneID then
                  BoneVertices.FindOrAdd(Vertices[k]);

          // Now get the maximum vertex distance away from the line between
          // the bone and it's child
          bounding_radius := 0;
          if BoneVertices.count > 0 then
          begin
            // Get the vector between the bone and it's child
            a := AffineVectorMake(VectorSubtract(bone[i].GlobalMatrix.W,
              bone.GlobalMatrix.W));
            a_hat := VectorNormalize(a);
            // Check each vertices distance from the line, set it to the
            // bounding radius if it is the current maxiumum
            for j := 0 to BoneVertices.count - 1 do
            begin
              temp := abs(PointLineDistance(BoneVertices[j],
                AffineVectorMake(bone.GlobalMatrix.W), a_hat));
              if temp > bounding_radius then
                bounding_radius := temp;
            end;

            // Add GLScene object to the Actor and set up it's properties
            // The collider is a child of a dummy to allow for custom
            // transformations after generating the colliders
            with Actor.AddNewChild(TGLDummyCube) do
              bounding_object := AddNewChild(TGLCylinder);
            with TGLCylinder(bounding_object) do
            begin
              Name := Format('Bone_%d_%d', [bone.BoneID, bone[i].BoneID]);
              // Alignment:=caBottom;
              TopRadius := bounding_radius;
              BottomRadius := bounding_radius;
              Height := VectorLength(a);
              Parts := [cySides];
              Material.FrontProperties.Diffuse.AsWinColor := BaseColliderColor;
              Material.FrontProperties.Diffuse.Alpha := AlphaLevel;
              Material.BlendingMode := bmTransparency;
              Position.SetPoint(Height / 2, 0, 0);
              Roll(90);
              // Add the spheres to the ends of the cylinder (making
              // a capsule)
              with TGLSphere(AddNewChild(TGLSphere)) do
              begin
                Position.SetPoint(0, Height / 2, 0);
                Radius := bounding_radius;
                Bottom := 0;
                Material.FrontProperties.Diffuse.AsWinColor :=
                  BaseColliderColor;
                Material.FrontProperties.Diffuse.Alpha := AlphaLevel;
                Material.BlendingMode := bmTransparency;
              end;
              with TGLSphere(AddNewChild(TGLSphere)) do
              begin
                Position.SetPoint(0, -Height / 2, 0);
                Radius := bounding_radius;
                Top := 0;
                Material.FrontProperties.Diffuse.AsWinColor :=
                  BaseColliderColor;
                Material.FrontProperties.Diffuse.Alpha := AlphaLevel;
                Material.BlendingMode := bmTransparency;
              end;
            end;
          end;
          BoneVertices.Free;
        end
        else
        begin
          // OK, we're only aligning the objects to the skeleton so find
          // the bounding_object if it exists
          bounding_object :=
            Actor.FindChild(Format('Bone_%d_%d',
            [bone.BoneID, bone[i].BoneID]), False);
        end;
        // If there is a bounding_object assigned, align it to the skeleton
        if Assigned(bounding_object) then
        begin
          bounding_object.Parent.Matrix := bone.GlobalMatrix;
        end;
        // Continue recursion into the child bones
        RecursBones(bone[i]);
      end;
    end
    else
    begin
      bounding_object := nil;
      // Are we generating the bounding objects?
      if CreateBoundingObjects then
      begin
        // Get all vertices weighted to this bone
        BoneVertices := TAffineVectorList.Create;
        for j := 0 to Actor.MeshObjects.count - 1 do
          with TGLSkeletonMeshObject(Actor.MeshObjects[j]) do
            for k := 0 to Vertices.count - 1 do
              if bone.BoneID = VerticesBonesWeights[k][0].BoneID then
                BoneVertices.FindOrAdd(Vertices[k]);
        if BoneVertices.count > 0 then
        begin
          // Get the maximum vertex distance from the bone
          bounding_radius := 0;
          for j := 0 to BoneVertices.count - 1 do
          begin
            temp := abs(VectorLength(VectorSubtract(BoneVertices[j],
              AffineVectorMake(bone.GlobalMatrix.W))));
            if temp > bounding_radius then
              bounding_radius := temp;
          end;
          // Add the sphere to the Actor and set up it's properties
          // The collider is a child of a dummy to allow for custom
          // transformations after generating the colliders
          with Actor.AddNewChild(TGLDummyCube) do
            bounding_object := AddNewChild(TGLSphere);
          with TGLSphere(bounding_object) do
          begin
            Name := Format('Bone_%d', [bone.BoneID]);
            Radius := bounding_radius;
            Material.FrontProperties.Diffuse.AsWinColor := BaseColliderColor;
            Material.FrontProperties.Diffuse.Alpha := AlphaLevel;
            Material.BlendingMode := bmTransparency;
            Roll(90);
          end;
        end;
        BoneVertices.Free;
      end
      else
      begin
        // OK, we're only aligning the objects to the skeleton so find
        // the bounding_object if it exists
        bounding_object := Actor.FindChild
          (Format('Bone_%d', [bone.BoneID]), False);
      end;
      // If there is a bounding_object assigned, align it to the skeleton
      if Assigned(bounding_object) then
      begin
        bounding_object.Parent.Matrix := bone.GlobalMatrix;
      end;
    end;
  end;

var
  i: Integer;
begin
  // Start the recursive traversal of the skeleton heirachy
  for i := 0 to Actor.Skeleton.RootBones.count - 1 do
  begin
    RecursBones(Actor.Skeleton.RootBones[i]);
  end;
end;

procedure CollidersToStrings(Actor: TGLActor; DescNames, ObjNames: TStrings);

  procedure RecursBones(bone: TGLSkeletonBone);
  var
    i: Integer;
    obj: TGLBaseSceneObject;
  begin
    // The bone is an end-node if it has no children
    if bone.count > 0 then
    begin
      // Check each bone in relation to it's children
      for i := 0 to bone.count - 1 do
      begin
        // Does this object exist?
        obj := Actor.FindChild(Format('Bone_%d_%d',
          [bone.BoneID, bone[i].BoneID]), False);
        if Assigned(obj) then
        begin
          // It does, add the description and object name to the
          // string lists
          DescNames.Add(bone.Name + ' -> ' + bone[i].Name);
          ObjNames.Add(obj.Name);
        end;
        // Continue the recursion
        RecursBones(bone[i]);
      end;
    end
    else
    begin
      // No children, check if this end-node exists
      obj := Actor.FindChild(Format('Bone_%d', [bone.BoneID]), False);
      if Assigned(obj) then
      begin
        // It does, add the description and object name to the
        // string lists
        DescNames.Add(bone.Name);
        ObjNames.Add(obj.Name);
      end;
    end;
  end;

var
  i: Integer;
begin
  // Start the recursive traversal of the skeleton heirachy
  for i := 0 to Actor.Skeleton.RootBones.count - 1 do
  begin
    RecursBones(Actor.Skeleton.RootBones[i]);
  end;
end;

procedure ExtractBoneIDsFromString(str: string; var BoneID1, BoneID2: Integer);
var
  s: string;
begin
  s := Copy(str, Pos('_', str) + 1, Length(str));
  if Pos('_', s) > 0 then
  begin
    BoneID1 := StrToInt(Copy(s, 1, Pos('_', s) - 1));
    BoneID2 := StrToInt(Copy(s, Pos('_', s) + 1, Length(s)));
  end
  else
  begin
    BoneID1 := StrToInt(s);
    BoneID2 := -1;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Put the path to the smd file here
  SetCurrentDir('.\model\');

  // Load model and animations
  GLActor1.Reference := aarSkeleton;
  GLActor1.LoadFromFile('Bdroid_a.smd');
  GLActor1.AddDataFromFile('idle.smd');
  GLActor1.AddDataFromFile('look_idle.smd');
  GLActor1.AddDataFromFile('walk.smd');
  GLActor1.AddDataFromFile('run.smd');
  GLActor1.AddDataFromFile('jump.smd');

  // Copy Frame 1 over Frame 0 because Frame 0 seems to
  // have a different origin to the loaded animaitons.
  GLActor1.Skeleton.Frames[0].Assign(GLActor1.Skeleton.Frames[1]);

  // Stop the actor from moving around while animating
  for i := 1 to GLActor1.Animations.count - 1 do
    GLActor1.Animations[i].MakeSkeletalTranslationStatic;

  GLActor1.AnimationMode := aamLoop;

  // Assign a translucent green to the actor
  with GLActor1.Material do
  begin
    BlendingMode := bmTransparency;
    FrontProperties.Diffuse.AsWinColor := clGreen;
    FrontProperties.Diffuse.Alpha := AlphaLevel;
  end;

  ColliderNames := TStringList.Create;
end;

procedure TMainForm.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  mat: TMatrix;
  axis, axis2: TAffineVector;
begin
  // Camera movement
  if ssLeft in Shift then
  begin
    if ssShift in Shift then
      GLCamera1.AdjustDistanceToTarget(1 - (my - Y) / 500)
    else
      GLCamera1.MoveAroundTarget(my - Y, mx - X);
  end;

  // Collider manipulation
  if (ssRight in Shift) and (Assigned(SelectedCollider)) then
  begin
    if ssShift in Shift then
      axis := VectorCrossProduct(SelectedCollider.Direction.AsAffineVector,
        SelectedCollider.Up.AsAffineVector)
    else if ssCtrl in Shift then
      axis := SelectedCollider.Up.AsAffineVector
    else
      axis := SelectedCollider.Direction.AsAffineVector;
    mat := SelectedCollider.Matrix;
    mat.W := NullHMGPoint;
    mat := MatrixMultiply(mat, CreateRotationMatrix(axis, (mx - X) / 40));
    mat.W := SelectedCollider.Position.AsVector;
    SelectedCollider.Matrix := mat;
  end;

  if (ssMiddle in Shift) and (Assigned(SelectedCollider)) then
  begin
    if ssCtrl in Shift then
    begin
      axis := SelectedCollider.Direction.AsAffineVector;
      axis2 := VectorCrossProduct(SelectedCollider.Up.AsAffineVector,
        SelectedCollider.Direction.AsAffineVector);
    end
    else
    begin
      axis := SelectedCollider.Up.AsAffineVector;
      axis2 := VectorCrossProduct(SelectedCollider.Up.AsAffineVector,
        SelectedCollider.Direction.AsAffineVector);
    end;

    SelectedCollider.Position.Translate(VectorScale(axis, -(my - Y) / 10));
    SelectedCollider.Position.Translate(VectorScale(axis2, -(mx - X) / 10));
  end;

  mx := X;
  my := Y;
end;

procedure TMainForm.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TMainForm.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if ColliderNames.count > 0 then
    AlignSkeletonBoundingObjects(GLActor1, False);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  GLCadencer1.Enabled := not GLCadencer1.Enabled;
  if GLCadencer1.Enabled then
    Button3.Caption := 'Pause animation'
  else
    Button3.Caption := 'Play animation';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ColliderNames.Free;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TMainForm.BonesComboChange(Sender: TObject);
var
  TempCollider: TGLSceneObject;
  i: Integer;
begin
  if BonesCombo.ItemIndex > -1 then
  begin
    ObjectName.Caption := BonesCombo.Text;
    TempCollider := TGLSceneObject
      (GLActor1.FindChild(ColliderNames[BonesCombo.ItemIndex], False));
    if Assigned(TempCollider) then
    begin
      if Assigned(SelectedCollider) then
      begin
        with SelectedCollider.Material.FrontProperties do
        begin
          Diffuse.AsWinColor := BaseColliderColor;
          Diffuse.Alpha := AlphaLevel;
        end;
        if SelectedCollider is TGLCylinder then
          for i := 0 to SelectedCollider.count - 1 do
            with TGLSphere(SelectedCollider.Children[i])
              .Material.FrontProperties do
            begin
              Diffuse.AsWinColor := BaseColliderColor;
              Diffuse.Alpha := AlphaLevel;
            end;
        SelectedCollider.ShowAxes := False;
      end;
      SelectedCollider := TempCollider;
      with SelectedCollider.Material.FrontProperties do
      begin
        Diffuse.AsWinColor := ActiveColliderColor;
        Diffuse.Alpha := AlphaLevel;
      end;
      if SelectedCollider is TGLCylinder then
        for i := 0 to SelectedCollider.count - 1 do
          with TGLSphere(SelectedCollider.Children[i])
            .Material.FrontProperties do
          begin
            Diffuse.AsWinColor := ActiveColliderColor;
            Diffuse.Alpha := AlphaLevel;
          end;

      if SelectedCollider is TGLSphere then
        with TGLSphere(SelectedCollider) do
        begin
          ColliderTypeCombo.ItemIndex := 0;
          RadiusEdit.Text := FloatToStr(Radius);
          HeightEdit.Text := '';
        end;
      if SelectedCollider is TGLCylinder then
        with TGLCylinder(SelectedCollider) do
        begin
          ColliderTypeCombo.ItemIndex := 1;
          RadiusEdit.Text := FloatToStr(TopRadius);
          HeightEdit.Text := FloatToStr(Height);
        end;

      SelectedCollider.ShowAxes := True;
    end;
  end;
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  AlignSkeletonBoundingObjects(GLActor1, True);
  BonesCombo.Clear;
  ColliderNames.Clear;
  CollidersToStrings(GLActor1, BonesCombo.Items, ColliderNames);
  Button6.Enabled := False;
  Button7.Enabled := True;
  Generate1.Enabled := False;
  DeleteAll1.Enabled := True;
end;

procedure TMainForm.Button7Click(Sender: TObject);
begin
  if Application.MessageBox('Are you sure you want to delete the colliders?',
    'Confirm', MB_YESNO) = ID_YES then
    Reset;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  i: Integer;
  SkeletonColliders: TSkeletonColliders;
begin
  OpenDialog1.Filter := 'SMC files|*.smc|All files|*.*';
  if not OpenDialog1.Execute then
    exit;

  Reset;
  SkeletonColliders := TSkeletonColliders.Create;
  SkeletonColliders.LoadFromFile(OpenDialog1.FileName);
  for i := 0 to SkeletonColliders.count - 1 do
  begin
    if SkeletonColliders[i] is TSkeletonColliderSphere then
    begin
      with GLActor1.AddNewChild(TGLDummyCube) do
      begin
        Matrix := GLActor1.Skeleton.BoneByID(SkeletonColliders[i].Bone1)
          .GlobalMatrix;
        with TGLSphere(AddNewChild(TGLSphere)) do
        begin
          if SkeletonColliders[i].Bone2 <> -1 then
            Name := Format('Bone_%d_%d', [SkeletonColliders[i].Bone1,
              SkeletonColliders[i].Bone2])
          else
            Name := Format('Bone_%d', [SkeletonColliders[i].Bone1]);
          Radius := TSkeletonColliderSphere(SkeletonColliders[i]).Radius;
          Material.FrontProperties.Diffuse.AsWinColor := BaseColliderColor;
          Material.FrontProperties.Diffuse.Alpha := AlphaLevel;
          Material.BlendingMode := bmTransparency;
          Matrix := TSkeletonColliderSphere(SkeletonColliders[i]).Matrix;
        end;
      end;
    end
    else if SkeletonColliders[i] is TSkeletonColliderCapsule then
    begin
      with GLActor1.AddNewChild(TGLDummyCube) do
      begin
        Matrix := GLActor1.Skeleton.BoneByID(SkeletonColliders[i].Bone1)
          .GlobalMatrix;
        with TGLCylinder(AddNewChild(TGLCylinder)) do
        begin
          if SkeletonColliders[i].Bone2 <> -1 then
            Name := Format('Bone_%d_%d', [SkeletonColliders[i].Bone1,
              SkeletonColliders[i].Bone2])
          else
            Name := Format('Bone_%d', [SkeletonColliders[i].Bone1]);
          TopRadius := TSkeletonColliderCapsule(SkeletonColliders[i]).Radius;
          BottomRadius := TopRadius;
          Height := TSkeletonColliderCapsule(SkeletonColliders[i]).Height;
          Parts := [cySides];
          Material.FrontProperties.Diffuse.AsWinColor := BaseColliderColor;
          Material.FrontProperties.Diffuse.Alpha := AlphaLevel;
          Material.BlendingMode := bmTransparency;
          Matrix := TSkeletonColliderCapsule(SkeletonColliders[i]).Matrix;
          // Add the spheres to the ends of the cylinder (making
          // a capsule)
          with TGLSphere(AddNewChild(TGLSphere)) do
          begin
            Position.SetPoint(0, Height / 2, 0);
            Radius := TopRadius;
            Bottom := 0;
            Material.FrontProperties.Diffuse.AsWinColor := BaseColliderColor;
            Material.FrontProperties.Diffuse.Alpha := AlphaLevel;
            Material.BlendingMode := bmTransparency;
          end;
          with TGLSphere(AddNewChild(TGLSphere)) do
          begin
            Position.SetPoint(0, -Height / 2, 0);
            Radius := TopRadius;
            Top := 0;
            Material.FrontProperties.Diffuse.AsWinColor := BaseColliderColor;
            Material.FrontProperties.Diffuse.Alpha := AlphaLevel;
            Material.BlendingMode := bmTransparency;
          end;
        end;
      end;
    end;
  end;
  SkeletonColliders.Free;

  BonesCombo.Clear;
  ColliderNames.Clear;
  CollidersToStrings(GLActor1, BonesCombo.Items, ColliderNames);
  Button6.Enabled := False;
  Button7.Enabled := True;
  Generate1.Enabled := False;
  DeleteAll1.Enabled := True;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  i, b1, b2: Integer;
  SkeletonColliders: TSkeletonColliders;
  obj: TGLBaseSceneObject;
begin
  SaveDialog1.Filter := 'SMC files|*.smc|All files|*.*';
  if not SaveDialog1.Execute then
    exit;

  SkeletonColliders := TSkeletonColliders.Create;
  with SkeletonColliders do
    for i := 0 to ColliderNames.count - 1 do
    begin
      obj := GLActor1.FindChild(ColliderNames[i], False);
      if Assigned(obj) then
      begin
        ExtractBoneIDsFromString(ColliderNames[i], b1, b2);
        if obj is TGLSphere then
          with TSkeletonColliderSphere.CreateOwned(SkeletonColliders) do
          begin
            Bone1 := b1;
            Bone2 := b2;
            Radius := TGLSphere(obj).Radius;
            Matrix := obj.Matrix;
          end
        else if obj is TGLCylinder then
        begin
          with TSkeletonColliderCapsule.CreateOwned(SkeletonColliders) do
          begin
            Bone1 := b1;
            Bone2 := b2;
            Radius := TGLCylinder(obj).TopRadius;
            Height := TGLCylinder(obj).Height;
            Matrix := obj.Matrix;
          end
        end;
      end;
    end;
  SkeletonColliders.SaveToFile(SaveDialog1.FileName);
  SkeletonColliders.Free;
end;

procedure TMainForm.GLActor1FrameChanged(Sender: TObject);
begin
  FrameLabel.Caption := Format('Frame : %d (%s)',
    [GLActor1.CurrentFrame, GLActor1.CurrentAnimation]);
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  if SelectedCollider is TGLSphere then
    with TGLSphere(SelectedCollider) do
    begin
      ColliderTypeCombo.ItemIndex := 0;
      RadiusEdit.Text := FloatToStr(Radius);
      HeightEdit.Text := '';
    end;
  if SelectedCollider is TGLCylinder then
    with TGLCylinder(SelectedCollider) do
    begin
      ColliderTypeCombo.ItemIndex := 1;
      RadiusEdit.Text := FloatToStr(TopRadius);
      HeightEdit.Text := FloatToStr(Height);
    end;
end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  GLActor1.Interval := TrackBar1.Position;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.LoadModel1Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'SMD files|*.smd|All files|*.*';
  if not OpenDialog1.Execute then
    exit;

  Reset;
  SetCurrentDir(ExtractFileDir(OpenDialog1.FileName));
  GLActor1.LoadFromFile(ExtractFileName(OpenDialog1.FileName));
end;

procedure TMainForm.Reset;
begin
  GLActor1.DeleteChildren;
  GLSceneViewer1.Invalidate;
  BonesCombo.Clear;
  ColliderNames.Clear;
  ColliderTypeCombo.ItemIndex := -1;
  RadiusEdit.Text := '';
  HeightEdit.Text := '';
  SelectedCollider := nil;
  Button6.Enabled := True;
  Button7.Enabled := False;
  Generate1.Enabled := True;
  DeleteAll1.Enabled := False;
end;

procedure TMainForm.AddAnimation1Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'SMD files|*.smd|All files|*.*';
  if not OpenDialog1.Execute then
    exit;

  SetCurrentDir(ExtractFileDir(OpenDialog1.FileName));
  GLActor1.AddDataFromFile(ExtractFileName(OpenDialog1.FileName));
  GLActor1.Skeleton.Frames[0].Assign(GLActor1.Skeleton.Frames[1]);
  GLActor1.Animations[GLActor1.Animations.count - 1]
    .MakeSkeletalTranslationStatic;
end;

procedure TMainForm.Generate1Click(Sender: TObject);
begin
  Button6Click(Generate1);
end;

procedure TMainForm.DeleteAll1Click(Sender: TObject);
begin
  Button7Click(DeleteAll1);
end;

procedure TMainForm.Button8Click(Sender: TObject);
begin
  if not Assigned(SelectedCollider) then
    exit;

  ColliderNames.Delete(BonesCombo.ItemIndex);
  BonesCombo.DeleteSelected;
  GLActor1.Remove(SelectedCollider.Parent, False);
  SelectedCollider := nil;
  ObjectName.Caption := '';
end;

procedure TMainForm.Load1Click(Sender: TObject);
begin
  Button1Click(Load1);
end;

procedure TMainForm.Save1Click(Sender: TObject);
begin
  Button2Click(Save1);
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  Application.MessageBox('GLScene Skeleton Collider Editor' + #13 + #10 +
    'By Stuart Gooding (2003)', 'About', MB_OK);
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  p1, p2: single;
begin
  if not Assigned(SelectedCollider) then
    exit;

  try
    if Trim(RadiusEdit.Text) = '' then
      p1 := 0
    else
      p1 := StrToFloat(RadiusEdit.Text);
    if Trim(HeightEdit.Text) = '' then
      p2 := 0
    else
      p2 := StrToFloat(HeightEdit.Text);
  except
    Application.MessageBox
      ('Invalid floating point number. Reseting collider data.',
      'Error', MB_OK);
    Button5Click(Button4);
    exit;
  end;

  if SelectedCollider is TGLSphere then
    with TGLSphere(SelectedCollider) do
    begin
      Radius := p1;
      exit;
    end;

  if SelectedCollider is TGLCylinder then
    with TGLCylinder(SelectedCollider) do
    begin
      TopRadius := p1;
      BottomRadius := p1;
      Height := p2;
      TGLSphere(Children[0]).Radius := TopRadius;
      TGLSphere(Children[0]).Position.SetPoint(0, p2 / 2, 0);
      TGLSphere(Children[1]).Radius := TopRadius;
      TGLSphere(Children[1]).Position.SetPoint(0, -p2 / 2, 0);
    end;
end;

end.

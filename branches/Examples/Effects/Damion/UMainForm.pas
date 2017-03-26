unit UMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.jpeg, Vcl.StdCtrls, Vcl.ExtCtrls,

  GLSkydome,
  GLMirror,
  GLScene,
  GLObjects,
  GLShadowPlane,
  GLWin32Viewer,
  GLCadencer,
  GLTexture,   GLParticleFX, GLVectorGeometry,
  GLbehaviours, GLKeyboard, GLSpaceText,
  GLColor, GLGui, GLMaterial, GLBitmapFont, GLWindows, GLHUDObjects,
  GLRenderContextInfo, OpenGLTokens, GLCoordinates, GLCrossPlatform,
  GLBaseClasses;

TYPE
  TGameStatus = (gsLevelPreview, gsWarmup, gsPlaying, gsLevelWon, gsLevelLost);

TYPE
  TForm1 = CLASS(TForm)
    GLScene1: TGLScene;
    Cadencer: TGLCadencer;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    CubeMapCamera: TGLCamera;
    DCBoard: TGLDummyCube;
    GLMaterialLibrary: TGLMaterialLibrary;
    DCTable: TGLDummyCube;
    Mirror: TGLMirror;
    SPTable: TGLShadowplane;
    GLDirectOpenGL1: TGLDirectOpenGL;
    SelCube: TGLCube;
    Timer1: TTimer;
    DCInterface: TGLDummyCube;
    GLSpaceText1: TGLSpaceText;
    GLMemoryViewer1: TGLMemoryViewer;
    GLPlane1: TGLPlane;
    Panel1: TPanel;
    Button2: TButton;
    Button1: TButton;

    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    PROCEDURE FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
    procedure GLDirectOpenGL1Render(VAR rci: TGLRenderContextInfo);
    procedure CadencerProgress(Sender: TObject; CONST deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure GenerateCubeMap;
  public
    pickstartx, pickstarty, speedrot: integer;
    gameStatus: TGameStatus;
    MirrorObj, cubeMapWarnDone: Boolean;
    procedure glDraw(VAR rci: TGLRenderContextInfo);
    procedure SetObjectMaterial(SceneObject: TGLBaseSceneObject; matindex: integer);
    procedure SetBehavioursDamping(rc, rl, rq, sr, sp, st, tc, tl, tq, sx, sy, sz, mass: single);
    procedure InitDamion;
    procedure InitDamion3D;
    function ModifieDamion(aff: boolean): boolean;
    procedure selectdamion;
  end;

const
  Prof = 150;
  pas_prof = 30;
TYPE
  StructDamion = RECORD
    etat: integer;
    col: integer;
    zeta: integer;
  end;

TYPE
  glcoord = RECORD
    x, y, z: Single;
  end;
CONST
  TEXTURE_SPEED = 1 / 75;

VAR
  Form1: TForm1;
  Damion: ARRAY[0..4, 0..4] OF StructDamion;
  cursx, cursy: integer;
  Tunnels: ARRAY[0..32, 0..32] OF glcoord;
  Angle: Single;
  Speed: Single;

implementation

{$R *.DFM}

TYPE
  TOutLineShader = CLASS(TGLShader)
  private
    BackgroundColor, LineColor: TColorVector;
    OutlineSmooth, lighting: boolean;
    OutlineWidth, oldlinewidth: single;
    PassCount: Integer;
  public
    procedure DoApply(VAR rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(VAR rci: TGLRenderContextInfo): Boolean; override;
  end;

VAR
  shader1: TOutlineShader;

procedure TOutLineShader.DoApply(VAR rci: TGLRenderContextInfo; Sender: TObject);
begin
  PassCount := 1;
  glPushAttrib(GL_ENABLE_BIT);
  glDisable(GL_LIGHTING);

  IF outlineSmooth then
    begin
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glEnable(GL_LINE_SMOOTH);
    END
  ELSE
    glDisable(GL_LINE_SMOOTH);

  glGetFloatv(GL_LINE_WIDTH, @oldlinewidth);
  glLineWidth(OutLineWidth);
  glPolygonMode(GL_BACK, GL_LINE);
  //SetGLPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glCullFace(GL_FRONT);
  glDepthFunc(GL_LEQUAL);
  glColor3fv(@lineColor);
end;

function TOutLineShader.DoUnApply(VAR rci: TGLRenderContextInfo): Boolean;
begin
  CASE PassCount OF
    1:
      begin
        PassCount := 2;
        IF lighting then
          glEnable(GL_LIGHTING)
        ELSE
          glColor3fv(@backGroundColor);
        glDepthFunc(GL_LESS);
        glCullFace(GL_BACK);
        //glEnable(GL_BLEND);
//         glBlendFunc(GL_SRC_ALPHA,GL_SRC_ALPHA);
        //glPolygonMode(GL_Back, GL_Fill);
       // SetGLPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

        Result := True;
      end;
    2:
      begin
        glPopAttrib;
        glLineWidth(oldLineWidth);
        Result := False;
      end;
  ELSE
    Assert(False);
    Result := False;
  end;
end;

procedure Tform1.glDraw(VAR rci: TGLRenderContextInfo);
VAR
  I, J: Integer;
  C, J1, J2: Single;
begin
  // GLTunnel FX From Jans Horn www.sulaco.co.za

  glClear(GL_COLOR_BUFFER_BIT OR GL_DEPTH_BUFFER_BIT); // Clear The Screen And The Depth Buffer
  glLoadIdentity(); // Reset The View

  glTranslatef(0.0, 0.0, -30.0);

  Angle := Angle + speed;

  // setup tunnel coordinates
  FOR I := 0 TO 24 DO
    begin
      FOR J := 0 TO 30 DO
        begin
          Tunnels[I, J].x := (24 - J / 12) * cos(2 * pi / 12 * I) + 2 * sin((Angle + 2 * j) / 24) + cos((Angle + 2 * j)
            / 10) - 2 * sin(Angle / 24) - cos(Angle / 10);
          Tunnels[I, J].y := (24 - J / 12) * sin(2 * pi / 12 * I) + 2 * cos((Angle + 2 * j) / 29) + sin((Angle + 2 * j)
            / 14) - 2 * cos(Angle / 29) - sin(Angle / 14);
          Tunnels[I, J].z := -(J - 10);
        end;
    end;

  // draw tunnel
  FOR J := 0 TO 30 DO
    begin
      J1 := J / 32 + Angle * TEXTURE_SPEED; // precalculate texture v coords for speed
      J2 := (J + 1) / 32 + Angle * TEXTURE_SPEED;

      // near the end of the tunnel, fade the effect away
      IF J > 24 then
        C := 1.0 - (J - 24) / 10
      ELSE
        C := 1.0;
      glColor3f(C, C, C);
      GLMaterialLibrary.ApplyMaterial('mat001', rci);
      glbegin(GL_QUADS);
      FOR I := 0 TO 11 DO
        begin
          glTexCoord2f((I - 3) / 12, J1); glVertex3f(Tunnels[I, J].X, Tunnels[I, J].Y, Tunnels[I, J].Z);
          glTexCoord2f((I - 2) / 12, J1); glVertex3f(Tunnels[I + 1, J].X, Tunnels[I + 1, J].Y, Tunnels[I + 1, J].Z);
          glTexCoord2f((I - 2) / 12, J2); glVertex3f(Tunnels[I + 1, J + 1].X, Tunnels[I + 1, J + 1].Y, Tunnels[I + 1, J
            + 1].Z);
          glTexCoord2f((I - 3) / 12, J2); glVertex3f(Tunnels[I, J + 1].X, Tunnels[I, J + 1].Y, Tunnels[I, J + 1].Z);
        end;
      glEnd();
      GLMaterialLibrary.UnApplyMaterial(rci);
    end;
end;

procedure TForm1.SetObjectMaterial(SceneObject: TGLBaseSceneObject; matindex: integer);
VAR
  obj : TGLCustomSceneObject;
begin
  IF assigned(sceneobject) AND (matindex >= 0) then
    begin
      obj := TGLCustomSceneObject(sceneobject);
      Obj.Material.MaterialLibrary := GLMaterialLibrary;
      obj.Material.Assign(GLMaterialLibrary.Materials[matindex].Material);
    end;
end;

//-- GAME CODE -----------------------------------------------------------------

procedure CaseBleu;
begin
  IF (cursy > 0) then
    damion[cursx, cursy - 1].etat := 1 - Damion[cursx, cursy - 1].etat
  ELSE
    damion[cursx, 4].etat := 1 - Damion[cursx, 4].etat;
end;

procedure CaseVerte;
begin
  IF (cursx < 4) then
    damion[cursx + 1, cursy].etat := 1 - Damion[cursx + 1, cursy].etat
  ELSE
    damion[0, cursy].etat := 1 - Damion[0, cursy].etat;
end;

procedure CaseRouge;
begin
  IF (cursy < 4) then
    damion[cursx, cursy + 1].etat := 1 - Damion[cursx, cursy + 1].etat
  ELSE
    damion[cursx, 0].etat := 1 - Damion[cursx, 0].etat;
end;

procedure CaseJaune;
begin
  IF (cursx > 0) then
    damion[cursx - 1, cursy].etat := 1 - Damion[cursx - 1, cursy].etat
  ELSE
    damion[4, cursy].etat := 1 - Damion[4, cursy].etat;
end;

function TForm1.ModifieDamion(aff: boolean): boolean;
VAR
  i, j, z, c: integer;
begin
  damion[cursx, cursy].etat := 1 - damion[cursx, cursy].etat;
  CASE damion[cursx, cursy].col OF
    0: caseBleu;
    1: caseVerte;
    2: caseRouge;
    3: caseJaune;
    4:
      begin
        casebleu;
        caseverte;
        caserouge;
        casejaune;
      end;
  end;
  z := 0;
  WHILE (z < prof) DO
    begin
      FOR j := 0 TO 4 DO
        begin
          FOR i := 0 TO 4 DO
            begin
              IF ((damion[i, j].etat = 1) AND (damion[i, j].zeta <> prof)) then
                damion[i, j].zeta := damion[i, j].zeta + pas_prof;
              IF ((damion[i, j].etat = 0) AND (damion[i, j].zeta = prof)) then
                damion[i, j].zeta := damion[i, j].zeta - pas_prof;
            end;
        end;
      z := z + pas_prof;
    end;

  IF aff then
    begin
      c := -1;
      FOR j := 0 TO 4 DO
        begin
          FOR i := 0 TO 4 DO
            begin
              inc(c);
              dcboard.Children[1 + c].Position.y := ((damion[i, j].zeta) / 50);
            end;
        end;
    end;
  result := true;
  FOR j := 0 TO 4 DO
    begin
      FOR i := 0 TO 4 DO
        begin
          IF (damion[i, j].etat = 0) then
            begin
              result := false;
              break;
            end;
          continue;
        end;
      IF result = false then break;
    end;

end;

procedure TForm1.InitDamion;
VAR
  k, cx, cy, i, j: integer;
begin

  FOR j := 0 TO 4 DO
    begin
      FOR i := 0 TO 4 DO
        begin
          Damion[i, j].etat := 0;
          Damion[i, j].zeta := 0;
          k := random(5);
          damion[i, j].col := k;
        end;
    end;
  cx := random(5);
  cy := random(5);
  FOR j := 0 TO 4 DO
    begin
      cursy := j;
      FOR i := 0 TO 4 DO
        begin
          cursx := i;
          IF ((cursx <> cx) OR (cursy <> cy)) then modifieDamion(true);
        end;
    end;
  cursx := cx;
  cursy := cy;
end;

procedure TForm1.selectdamion;
VAR
  tempmaterial: tglmaterial;
begin
  Selcube.Position.x := dcboard.Children[1 + (cursx) + (cursy * 5)].Position.x;
  Selcube.Position.y := dcboard.Children[1 + (cursx) + (cursy * 5)].Position.y;
  Selcube.Position.z := dcboard.Children[1 + (cursx) + (cursy * 5)].Position.z;
  tempmaterial := tglmaterial.create(self);
  CASE damion[cursx, cursy].col OF
    0: tempmaterial.FrontProperties.Diffuse.AsWinColor := clBlue;
    1: tempmaterial.FrontProperties.Diffuse.AsWinColor := clGreen;
    2: tempmaterial.FrontProperties.Diffuse.AsWinColor := clRed;
    3: tempmaterial.FrontProperties.Diffuse.AsWinColor := clyellow;
    4: tempmaterial.FrontProperties.Diffuse.AsWinColor := clFuchsia;
  end;
  tempmaterial.FrontProperties.Diffuse.Alpha := 0.4;
  tempmaterial.BackProperties.Diffuse := tempmaterial.FrontProperties.Diffuse;

  WITH shader1 DO
    begin

      CASE (dcboard.Children[1 + (cursx + 1) * (cursy + 1)] AS TGLCube).tag OF
        0: BackgroundColor := convertwincolor(clBlue, 0.2);
        1: BackgroundColor := convertwincolor(clGreen, 0.2);
        2: BackgroundColor := convertwincolor(clRed, 0.2);
        3: BackgroundColor := convertwincolor(clyellow, 0.2);
        4: BackgroundColor := convertwincolor(clFuchsia, 0.2);
      end;

      Outlinesmooth := true;
      OutLineWidth := 2;
      lighting := True;
      LineColor := clrWhite;
    end;

  glmateriallibrary.Materials[7].Shader := NIL;
  glmateriallibrary.Materials[7].Material.Assign(tempmaterial);
  glmateriallibrary.Materials[7].Shader := shader1;
end;

procedure TForm1.InitDamion3D;
VAR
  i, j, n: integer;
  AObject: TGLBaseSceneObject;
  x, y, z: single;

begin
  n := -1;
  y := 1.5;
  z := 3.0;
  speedrot := 6;
  FOR j := 0 TO 4 DO
    begin
      IF j > 0 then z := z - 1.5;
      x := -3.0;
      FOR i := 0 TO 4 DO
        begin
          inc(n);
          IF i > 0 then x := x + 1.5;
          AObject := TGLCube.Create(self);
          AObject.Direction.x := 0;
          AObject.Direction.y := 1;
          AObject.Direction.z := 0;
          AObject.up.x := 0;
          AObject.up.y := 0;
          AObject.up.z := -1;
          AObject.Position.x := x;
          AObject.Position.y := y;
          AObject.Position.z := z;
          AObject.Name := 'Cube' + inttostr(n);
          AObject.Tag := Damion[i, j].col;
          SetObjectMaterial(AObject, 0);
          DCBoard.AddChild(AOBject);
        end;
    end;
  GLSceneviewer1.Camera.TargetObject := DCBoard.Children[13];
  randomize;
  initDamion;
  shader1 := TOutLineShader.Create(Self);

  FOR j := 0 TO 4 DO
    begin
      FOR i := 0 TO 4 DO
        begin
          CASE Damion[i, j].col OF
            0: setobjectmaterial(dcboard.Children[1 + i + j * 5], 1);
            1: setobjectmaterial(dcboard.Children[1 + i + j * 5], 2);
            2: setobjectmaterial(dcboard.Children[1 + i + j * 5], 3);
            3: setobjectmaterial(dcboard.Children[1 + i + j * 5], 4);
            4: setobjectmaterial(dcboard.Children[1 + i + j * 5], 5);
          end;
        end;
    end;
  selectdamion;
  //  glscene1.SaveToFile('damion.gls');
end;

procedure TForm1.Button1Click(Sender: TObject);
VAR
  i, j, n: integer;
begin
  randomize;
  initDamion;

  n := -1;
  FOR j := 0 TO 4 DO
    begin
      FOR i := 0 TO 4 DO
        begin
          inc(n);
          CASE Damion[i, j].col OF
            0: setobjectmaterial(dcboard.Children[n + 1], 1);
            1: setobjectmaterial(dcboard.Children[n + 1], 2);
            2: setobjectmaterial(dcboard.Children[n + 1], 3);
            3: setobjectmaterial(dcboard.Children[n + 1], 4);
            4: setobjectmaterial(dcboard.Children[n + 1], 5);
          end;
        end;
    end;
  Form1.FocusControl(glsceneviewer1);
  selectdamion;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  IF (ssRight IN Shift) then
    begin
      IF GLScene1.CurrentGLCamera.TargetObject <> NIL then
        begin
          // Rotate around Object
          GLScene1.CurrentGLCamera.MoveAroundTarget((y - pickstarty), (x - pickstartx));
        end;
    end;
  PickStartX := x;
  PickStartY := y;
  glsceneviewer1.Invalidate;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
begin
  GLSceneViewer1.Camera.AdjustDistanceToTarget(Power(1.005, WheelDelta / 120));
end;

procedure TForm1.GLDirectOpenGL1Render(VAR rci: TGLRenderContextInfo);
begin
  //glDisable(GL_CULL_FACE);
  glDraw(rci);
  //glEnable(GL_CULL_FACE);
end;

procedure TForm1.GenerateCubeMap;
//var tmp : tglmaterial;
begin
  // Don't do anything if cube maps aren't supported

  IF GL_TEXTURE_CUBE_MAP_ARB <> 0 then
    begin
      IF NOT cubeMapWarnDone then
        ShowMessage('Your graphics board does not support cube maps...');
      cubeMapWarnDone := True;
      Exit;
    end;

  // Here we generate the new cube map, from CubeMapCamera (a child of the
  WITH GLPlane1 DO
    begin
      GLDirectOpenGL1.Visible := false;
      DCInterface.Visible := false;
      SPTable.Visible := false;
      Visible := False;
      glmemoryviewer1.Render;
      GLMemoryViewer1.CopyToTexture(material.Texture, 0, 0, 512, 512, 0, 0);
      Material.Texture.Disabled := False;
      GLDirectOpenGL1.Visible := True;
      Visible := True;
      SPTable.Visible := True;
      DCInterface.Visible := True;
    end;
end;

procedure TForm1.SetBehavioursDamping(rc, rl, rq, sr, sp, st, tc, tl, tq, sx, sy, sz, mass: single);
begin
  GetOrCreateInertia(DCBoard.Behaviours).RotationDamping.SetDamping(rc, rl, rq);
  GetOrCreateInertia(DCBoard).Mass := mass;
  GetOrCreateInertia(DCBoard).Turnspeed := st;
  GetOrCreateInertia(DCBoard).RollSpeed := sr;
  GetOrCreateInertia(DCBoard).PitchSpeed := sp;
  GetOrCreateInertia(DCBoard).TranslationDamping.SetDamping(tc, tl, tq);
  GetOrCreateInertia(DCBoard).TranslationSpeed.SetVector(sx, sy, sz);
  GetOrCreateInertia(DCBoard).DampingEnabled := true;
end;

procedure TForm1.CadencerProgress(Sender: TObject; CONST deltaTime,
  newTime: Double);
VAR
  mx, my: integer;
begin

  IF GameStatus = gsPlaying then
    begin
      button1.Enabled := false;
      IF isKeydown(VK_DOWN) then
        IF cursy < 4 then inc(cursy);
      IF isKeydown(VK_UP) then
        IF cursy > 0 then dec(cursy);
      IF isKeydown(VK_LEFT) then
        IF cursx > 0 then dec(cursx);
      IF isKeydown(VK_RIGHT) then
        IF cursx < 4 then inc(cursx);

      IF iskeydown(vk_space) then modifiedamion(true);

      selectdamion;
    END
  ELSE
    IF GameStatus = gsLevelPreview then
    begin
      // if behaviour is created, comment the next line (see setbehaviour... in FormCreate)
      dcboard.TurnAngle := dcboard.TurnAngle + (10 * deltatime);
      //dcboard.TransformationChanged;
      //Mirror.TransformationChanged;
      GLSpacetext1.TurnAngle := GLSpacetext1.TurnAngle + (-15 * deltatime);

      IF ISKeyDown(VK_Space) then Gamestatus := gsPlaying;
    end;
  IF NOT (mirrorobj) then GenerateCubeMap;
  glsceneviewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := 'Damion : ' + Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]) + '  - ' + inttostr(cursx) + ' - ' +
    inttostr(cursy);
  GLSceneViewer1.ResetPerformanceMonitor;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Speed := 4;
  Angle := 0;
  cursx := 0;
  cursy := 0;
  GameStatus := gsLevelPreview;
  InitDamion3d;
  Mirrorobj := true;
  //SetBehavioursDamping(0,0,0,0,0,10,0,0,0,0,0,0,1);
  cadencer.Enabled := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  IF mirrorobj then
    begin
      glplane1.visible := true;
      mirror.Visible := false;
      mirrorobj := false;
    END
  ELSE
    begin
      glplane1.visible := False;
      mirror.Visible := True;
      mirrorobj := True;
    end;
   GLSceneViewer1.Invalidate;
end;

END.

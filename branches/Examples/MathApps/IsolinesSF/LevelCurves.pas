unit LevelCurves;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Math,
  System.ImageList,
  System.Actions,
  System.Variants,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ImgList,
  Vcl.StdActns,
  Vcl.ActnList,
  Vcl.ToolWin,
  Vcl.DBCtrls,
  Generics.Collections,

  GLTypes,
  GLCadencer,
  GLMaterial,
  GLScene,
  GLObjects,
  GLGeomObjects,
  GLVectorGeometry,
  GLVectorFileObjects,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLWin32Viewer,
  GLHUDObjects,
  GLWindowsFont,
  GLTexture,
  GLUserShader,
  GLColor,
  GLgraphics,
  GLRenderContextInfo,
  GLMesh,
  GLVectorTypes,
  GLMultiPolygon,
  GLAVIRecorder,
  GLExtrusion,
  GLBitmapFont,
  GLcontext,
  GLState,
  GLTextureFormat,
  GLProxyObjects,
  GLSpaceText,
  GLAsyncTimer,
  GLIsolines,

  About,
  UNcube;

const
  Nx = 40; // dimension west - east
  Ny = 40; // dimenstion north west
  Nz = 40; // dimenstion height

  res3DmaxForm3D= 100;   res3DminForm3D= 10;

  Kfixed = 4; // Came from a 3D code where the plane XY can move as Z[I,J,K}
  gxmin = -3;
  gxmax = 6.5;
  gymin = -4;
  gymax = 4;

type
  TLevelCurvesForm = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton9: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    Edit1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    Help1: TMenuItem;
    HelpAboutItem: TMenuItem;
    pnlGLscene2: TPanel;
    StaticText1: TStaticText;
    View: TGLSceneViewer;
    GLScene: TGLScene;
    GLLightSource1: TGLLightSource;
    dc_cam: TGLDummyCube;
    PlaneXY: TGLFreeForm;
    cam: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    pnlDimensoesGL: TPanel;
    RGexamples: TRadioGroup;
    SeeIsolines: TCheckBox;
    TrackBarNC: TTrackBar;
    rgNodeAspect: TRadioGroup;
    Label2: TLabel;
    TrackBarPosition: TTrackBar;
    rgPlaneSelection: TRadioGroup;
    cad: TGLCadencer;
    AsyncTimer1: TGLAsyncTimer;
    RGpanning: TRadioGroup;
    DC_world: TGLDummyCube;
    DC_utils: TGLDummyCube;
    lbNL: TLabel;
    rgSplineModes: TRadioGroup;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure ViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure ViewMouseMove(Sender: TObject; Shift: TShiftState;
      x, y: Integer);
    procedure ViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject); // values of cut levels
    procedure DrawPlaneClick(Sender: TObject);
    procedure Use_CONREC_XY (Kfix:integer);
    procedure RGexamplesClick(Sender: TObject);
    procedure TrackBarNCChange(Sender: TObject);
    procedure rgNodeAspectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrackBarPositionChange(Sender: TObject);
    procedure RGpanningClick(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure rgPlaneSelectionClick(Sender: TObject);
    procedure rgSplineModesClick(Sender: TObject);
  private
    mdx, mdy: Integer;
    mx, my: Integer;
    UserAbort: Boolean;
  public
    dir: Integer;
    NC: Integer; // dimension for contour levels
    pt1AX, pt1AY, pt1AZ, pt1BX, pt1BY, pt1BZ, flap1A, flap1B: single;
    Isolines: TGLIsolines;
    procedure CreateInputData;
  end;

var
  LevelCurvesForm: TLevelCurvesForm;
  Mat: TMatrixArr; // 2D - Datafield    replaced by  res3DPlane
  Scx: TVectorArr; // scaling vector west - east
  Scy: TVectorArr; // scaling vector north - west
  Scz: TVectorArr; // scaling vector north - west
  Hgt: TVectorArr; // vector for the countur levels
  i, j, k: Integer; // adress indexes
  mi, ma: Double; // for minimum & maximum
  Xarr, Yarr,    // coord. values
  Zarr, res3DPlane: array of array of array of Double;
  Z_Kfixed: Integer;
  ncube: TGLNCube;

//======================================
implementation
//======================================

{$R *.dfm}

procedure TLevelCurvesForm.FormCreate(Sender: TObject);
begin
   SetCurrentDir(ExtractFilePath(ParamStr(0)));
   Z_Kfixed := TrackBarPosition.Position;
   Isolines := TGLIsolines.CreateAsChild(PlaneXY);
   Isolines.LineColor.Color := clrBlack;
   Isolines.LinePattern := $FFFF;
   Isolines.LineWidth := 2;
   Isolines.NodesAspect := TGLLineNodesAspect(0);
   Isolines.SplineMode := lsmSegments; //lsmBezierSpline;

  // using  my 3D type variables . Each point in the 3D space is defined by
  // X[I,J,K]  Y[I,J,K]and  Z[I,J,K] . The value of the variable to map is
  // res3Dplane[I,J,K] in that point (already normalized for =0 and <=1)
  SetLength(Xarr, Nx+1 , Ny+1 , Nz+1 );
  SetLength(Yarr, Nx+1 , Ny+1 , Nz +1);
  SetLength(Zarr, Nx +1, Ny+1 , Nz+1 );
  SetLength(res3DPlane, Nx+1 , Ny+1 , Nz+1 );
  RGexamplesClick(Self);
  RGpanningClick(self);
end;

procedure TLevelCurvesForm.RGpanningClick(Sender: TObject);
begin
  case RGpanning.ItemIndex of
    0:
      begin
        if ncube = nil then
        begin
          cad.Enabled := true;
          ncube := TGLNCube.CreateAsChild(GLScene.Objects);
          ncube.SceneViewer := View;
          ncube.FPS := 30;
          application.OnActivate := OnActivate;
          application.OnDeactivate := OnDeactivate;
        end;
      end;
    1:
      begin
        if ncube <> nil then
        begin
          if Assigned(ncube) then
          begin
            cad.Enabled := False;
            ncube.Free;
            ncube := nil;
          end;
        end;
      end;
  end;
end;

procedure TLevelCurvesForm.rgPlaneSelectionClick(Sender: TObject);
begin
  case rgPlaneSelection.ItemIndex of
  0: begin //XY
       PlaneXY.Roll(0);
       PlaneXY.Pitch(0);
       PlaneXY.Turn(90);
     end;
  1: begin //YZ;
       PlaneXY.Roll(0);
       PlaneXY.Pitch(90);
       PlaneXY.Turn(0);
     end;
  2: begin //ZX;
       PlaneXY.Roll(90);
       PlaneXY.Pitch(0);
       PlaneXY.Turn(0);
     end;
  end;
//  rgExamplesClick(self);
end;

procedure TLevelCurvesForm.rgSplineModesClick(Sender: TObject);
begin
  case rgSplineModes.ItemIndex of
    0: Isolines.SplineMode := lsmSegments;
    1: Isolines.SplineMode := lsmCubicSpline;
    2: Isolines.SplineMode := lsmBezierSpline;
    3: Isolines.SplineMode := lsmNURBSCurve;
    4: Isolines.SplineMode := lsmLines;
    5: Isolines.SplineMode := lsmLoop;
  end;
end;

// __________________________________________________________________________
procedure TLevelCurvesForm.AsyncTimer1Timer(Sender: TObject);
begin
  caption := 'naviCube: ' + View.FramesPerSecondText(2);
  View.ResetPerformanceMonitor;
end;

procedure TLevelCurvesForm.cadProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
 if ncube<> nil then
  begin
  if ncube.InactiveTime > 5 then begin
      if ncube.InactiveTime < 8 then
        dc_cam.TurnAngle := dc_cam.TurnAngle + (ncube.InactiveTime - 5) * deltatime * 2
      else
        dc_cam.TurnAngle := dc_cam.TurnAngle + deltatime * 6;
     end;
    View.Refresh;
  end;
end;

procedure TLevelCurvesForm.ViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  View.SetFocus;
end;

procedure TLevelCurvesForm.ViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;

begin
  // calculate delta since last move or last mousedown
  dx := mdx - X;
  dy := mdy - Y;
  mdx := X;
  mdy := Y;
  if ssLeft in Shift then
  begin
    if ssShift in Shift then
    begin // right button with shift rotates DC1 (rotation happens around camera's axis)
      cam.RotateObject(dc_world, dy, dx);
    end
    else
    begin // right button without shift changes camera angle *** moving around the parent and target dummycube)
      cam.MoveAroundTarget(dy, dx)
    end;
  end
  else if Shift = [ssRight] then
  begin // left button moves our target and parent dummycube
    // v:=cam1.ScreenDeltaToVectorXY(dx, -dy,0.12*cam1.DistanceToTarget/cam1.FocalLength);
    // DC1.Position.Translate(v);     	// notify camera that its position/target has been changed
    // cam1.TransformationChanged;
  end;

end;

procedure TLevelCurvesForm.ViewMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  cam.AdjustDistanceToTarget(Power(1.03, WheelDelta / 300));
end;

procedure TLevelCurvesForm.FormDeactivate(Sender: TObject);
begin
  cad.Enabled := false;
end;

procedure TLevelCurvesForm.FormActivate(Sender: TObject);
begin
   cad.Enabled := true;
end;
// __________________________________________________________________________
procedure TLevelCurvesForm.rgNodeAspectClick(Sender: TObject);
begin
  case rgNodeAspect.ItemIndex of
    0: Isolines.NodesAspect := TGLLineNodesAspect(0);
    1: Isolines.NodesAspect := TGLLineNodesAspect(2);
    2: Isolines.NodesAspect := TGLLineNodesAspect(1);
  end;
end;

// __________________________________________________________________________
procedure TLevelCurvesForm.RGexamplesClick(Sender: TObject);
var I:integer  ;
begin
  CreateInputData;
  DrawPlaneClick(Self);
end;


procedure TLevelCurvesForm.TrackBarNCChange(Sender: TObject);
begin
  lbNL.Caption := 'NL: '+ IntToStr(TrackBarNC.Position);
  RGexamplesClick(self);
end;

procedure TLevelCurvesForm.TrackBarPositionChange(Sender: TObject);
begin
  RGexamplesClick(self);
end;


procedure TLevelCurvesForm.CreateInputData;
var
  i, j: Integer;
  xAux, yAux: Double;
begin
  for i := 0 to Nx - 1 Do
  // ----------------------------------- set 2d data field
  begin
    for j := 0 to Ny - 1 Do
    Begin
      for k := 0 to Nz - 1 Do
      begin
        case RGexamples.ItemIndex of
          0:
            begin
              Xarr[i, j, k] := i - Nx / 2;
              Yarr[i, j, k] := j - Ny / 2;
              Zarr[i, j, k] := Z_Kfixed;
            end;
          1:
            begin
              Xarr[i, j, k] := gxmin + i * (gxmax - gxmin) / Nx;
              Yarr[i, j, k] := gymin + j * (gymax - gymin) / Ny;
              Zarr[i, j, k] := Z_Kfixed;
            end;
          2:
            begin
              Xarr[i, j, k] := i - Nx / 2;
              Yarr[i, j, k] := j - Ny / 2;
              Zarr[i, j, k] := Z_Kfixed;
            end;

        end;
      end;
    end;
  end;

  for i := 0 to Nx - 1 do // ------ set 2d data field
  begin
    for j := 0 to Ny - 1 do
    begin
      // example of mapping xAux and yAux are the coordinates in the X and Y axis
      // and res3Dplane are normalized values ( >0 and <1) at the points to mapp

      xAux := Xarr[i, j, Kfixed];
      yAux := Yarr[i, j, Kfixed];

      case RGexamples.ItemIndex of
        0:
          begin
            res3DPlane[i, j, Kfixed] :=
              (sin(xAux / Nx * 4 * pi) * cos(yAux / Ny * 4 * pi)) +
              (sin(xAux / Nx * 2 * pi) * cos(yAux / Ny * 2 * pi)) +
              (sin(xAux / Nx * 1 * pi) * cos(yAux / Ny * 1 * pi)) +
              (sin(xAux / Nx * 0.5 * pi) * cos(yAux / Ny * 0.5 * pi)) +
              (sin(xAux / Nx * 0.25 * pi) * cos(yAux / Ny * 0.25 * pi));
          end;
        1:
          begin
            if (xAux <> 4) or (yAux <> 0) then
              res3DPlane[i, j, Kfixed] := 0.5 *
                (sin(sqrt(sqr(xAux) + sqr(yAux))) + 1 /
                sqrt(sqr(xAux - 4) + sqr(yAux)));
          end;
        2:
          begin
            xAux := 0.08 * (i - Nx / 2);
            yAux := 0.08 * (j - Ny / 2);
            res3DPlane[i, j, Kfixed] :=
              (sqr(sqr(xAux) + (yAux - 0.842) * (yAux + 0.842)) +
              sqr(xAux * (yAux - 0.842) + xAux * (yAux - 0.842)));
          end;
      end;
    end;
  end;
end;

procedure TLevelCurvesForm.DrawPlaneClick(Sender: TObject);
var
  ResultsMesh: TMeshObject;
  Quads: TFGVertexIndexList;
  i, j, k, MMT: Integer;
begin
  // reconvert  to 3D variables before drawing a plane
  View.Update;
  View.Invalidate;
  ///////////////////////////////////////////////////////////
  PlaneXY.MeshObjects.Clear;
  ResultsMesh := TMeshObject.CreateOwned(PlaneXY.MeshObjects);
  ResultsMesh.Mode := momFaceGroups;

  case rgPlaneSelection.ItemIndex of
    0:
      begin
        k := Kfixed;
        with ResultsMesh do
        begin
          for i := 1 to Nx do // 1 to Nx
          begin // 1 to Ny
            for j := 1 to Ny do
            begin // coloquei o deltaXwall para ficar centrado
              Vertices.Add(Xarr[i, j, k], Yarr[i, j, k], Zarr[i, j, k]);
              TexCoords.Add(res3DPlane[i, j, k], 0);

            end;
          end;
          Quads := TFGVertexIndexList.CreateOwned(ResultsMesh.FaceGroups);
          Quads.Mode := fgmmQuads;
          for i := 1 to Nx - 2 do
          begin
            for j := 1 to Ny - 2 do
            begin
              MMT := (i - 1) * Ny + j - 1;
              Quads.VertexIndices.Add(MMT, MMT + 1);
              Quads.VertexIndices.Add(MMT + Ny + 1, MMT + Ny);
            end;
          end;
        end;

        if SeeIsolines.Checked then
          Use_CONREC_XY(k)
        else
          Isolines.Nodes.Clear;
      end;

    1:
      begin
        i := Kfixed;
        with ResultsMesh do
        begin
          for j := 1 to Ny do // 1 to Nx
          begin // 1 to Ny
            for k := 1 to Nz do
            begin // coloquei o deltaXwall para ficar centrado
              Vertices.Add(Xarr[i, j, k], Yarr[i, j, k], Zarr[i, j, k]);
              TexCoords.Add(res3DPlane[i, j, k], 0);

            end;
          end;
          Quads := TFGVertexIndexList.CreateOwned(ResultsMesh.FaceGroups);
          Quads.Mode := fgmmQuads;
          for j := 1 to Ny - 2 do
          begin
            for k := 1 to Nz - 2 do
            begin
              MMT := (j - 1) * Nz + k - 1;
              Quads.VertexIndices.Add(MMT, MMT + 1);
              Quads.VertexIndices.Add(MMT + Nz + 1, MMT + Nz);
            end;
          end;
        end;

        if SeeIsolines.Checked then
          Use_CONREC_XY(i)
        else
          Isolines.Nodes.Clear;
      end;

    2:
      begin
        j := Kfixed;
        with ResultsMesh do
        begin
          for k := 1 to Nz do // 1 to Nx
          begin // 1 to Ny
            for i := 1 to Nx do
            begin // coloquei o deltaXwall para ficar centrado
              Vertices.Add(Xarr[i, j, k], Yarr[i, j, k], Zarr[i, j, k]);
              TexCoords.Add(res3DPlane[i, j, k], 0);

            end;
          end;
          Quads := TFGVertexIndexList.CreateOwned(ResultsMesh.FaceGroups);
          Quads.Mode := fgmmQuads;
          for k := 1 to Nz - 2 do
          begin
            for i := 1 to Nx - 2 do
            begin
              MMT := (k - 1) * Nx + i - 1;
              Quads.VertexIndices.Add(MMT, MMT + 1);
              Quads.VertexIndices.Add(MMT + Nx + 1, MMT + Nx);
            end;
          end;
        end;

        if SeeIsolines.Checked then
          Use_CONREC_XY(j)
        else
          Isolines.Nodes.Clear;
      end;
  end;
  PlaneXY.StructureChanged;
end;

// __________________________________________________________________________
procedure TLevelCurvesForm.Use_CONREC_XY (Kfix:integer);
 var
  I,J,K:integer;   NCL:integer;    planeName:string;
  F38: TextFile;
  File38: string;
  FileIsolines: TFileName;
  IsoValuesList: TList<Real>;
  TextIso:TGlFlatText;
begin
  File38 := ExtractFilePath(application.ExeName) + 'Isolines.txt';
  If FileExists(File38) Then
    DeleteFile(File38);
  AssignFile(F38, File38);
  Rewrite(F38);
  writeln(F38, ' Hgt[i]  Nodes.Items[i].X   Nodes.Items[i].Y ]');

  // convert 3D variables to CONREC type variables
  SetLength(Scx, Nx);
  SetLength(Scy, Ny);
  SetLength(Scz, Nz);

  case rgPlaneSelection.ItemIndex of
    0:
      begin
        SetLength(Mat, Nx, Ny);

        for i := 0 to Nx - 1 Do
          Scx[i] := Xarr[i, 0, Kfixed];
        for j := 0 to Ny - 1 Do
          Scy[j] := Yarr[0, j, Kfixed];

        for i := 0 to Nx - 1 Do
        begin
          for j := 0 to Ny - 1 Do
          begin
            Mat[i, j] := res3DPlane[i, j, Kfixed];
          end; // ----------------------------------------------------------------
        end;

        mi := 1E16;
        // ------------    Set the minimunm and maximum f of the data field
        ma := -1E16;

        for i := 0 to Nx - 1 Do
        begin
          for j := 0 to Ny - 1 Do
          begin
            if Mat[i, j] < mi then
              mi := Mat[i, j];
            if Mat[i, j] > ma then
              ma := Mat[i, j];
          end; // ----------------------------------------------------------------
        end;

        NC := TrackBarNC.Position;
        Z_Kfixed := TrackBarPosition.Position;
        SetLength(Hgt, NC);
        for i := 0 to NC - 1 Do // ----- create cut levels
          Hgt[i] := mi + i * (ma - mi) / (NC - 1);

        Isolines.Nodes.Clear;
        Isolines.Conrec(rgPlaneSelection.ItemIndex, PlaneXY, Mat, 0, Nx - 1, 0,
          Ny - 1, Scx, Scy, NC, Hgt, Z_Kfixed, res3DmaxForm3D, res3DminForm3D);
      end;
    1:
      begin
        SetLength(Mat, Ny, Nz);

        for j := 0 to Ny - 1 do
          Scy[j] := Yarr[Kfixed, j, 0];
        for k := 0 to Nz - 1 do
          Scz[k] := Zarr[Kfixed, 0, k];

        for j := 0 to Ny - 1 do
        begin
          for k := 0 to Nz - 1 do
          begin
            Mat[j, k] := res3DPlane[Kfixed, j, k];
          end; // ----------------------------------------------------------------
        end;

        mi := 1E16;
        // ------------    Set the minimunm and maximum f of the data field
        ma := -1E16;

        for j := 0 to Ny - 1 do
        begin
          for k := 0 to Nz - 1 do
          begin
            if Mat[j, k] < mi then
              mi := Mat[j, k];
            if Mat[j, k] > ma then
              ma := Mat[j, k];
          end; // ----------------------------------------------------------------
        end;

        NC := TrackBarNC.Position;
        Z_Kfixed := TrackBarPosition.Position;
        SetLength(Hgt, NC);
        for i := 0 to NC - 1 Do // ----- create cut levels
          Hgt[i] := mi + i * (ma - mi) / (NC - 1);

        Isolines.Nodes.Clear;
        Isolines.Conrec(rgPlaneSelection.ItemIndex, PlaneXY, Mat, 0, Ny - 1, 0,
          Nz - 1, Scy, Scz, NC, Hgt, Z_Kfixed, res3DmaxForm3D, res3DminForm3D);
      end;
    2:
      begin
        SetLength(Mat, Nz, Nx);

        for k := 0 to Nz - 1 do
          Scy[k] := Zarr[0, Kfixed, k];
        for i := 0 to Nx - 1 do
          Scz[i] := Xarr[i, Kfixed, 0];

        for k := 0 to Nz - 1 Do
        begin
          for i := 0 to Nx - 1 Do
          begin
            Mat[k, i] := res3DPlane[i, Kfixed, k];
          end; // ----------------------------------------------------------------
        end;

        mi := 1E16;
        // ------------    Set the minimunm and maximum f of the data field
        ma := -1E16;

        for k := 0 to Nz - 1 Do
        begin
          for i := 0 to Nx - 1 Do
          begin
            if Mat[k, i] < mi then
              mi := Mat[k, i];
            if Mat[k, i] > ma then
              ma := Mat[k, i];
          end; // ----------------------------------------------------------------
        end;

        NC := TrackBarNC.Position;
        Z_Kfixed := TrackBarPosition.Position;
        SetLength(Hgt, NC);
        for i := 0 to NC - 1 Do // ----- create cut levels
          Hgt[i] := mi + i * (ma - mi) / (NC - 1);
        Isolines.Conrec(rgPlaneSelection.ItemIndex, PlaneXY, Mat, 0, Nz - 1, 0,
          Nx - 1, Scz, Scx, NC, Hgt, Z_Kfixed, res3DmaxForm3D, res3DminForm3D);
      end;

  end;

  /// start write
  IsoValuesList := TList<Real>.Create;
  for i := 0 to Isolines.Nodes.Count - 1 do
  begin
    writeln(F38, Format('%10.2f %5.2f %5.2f',
      [Hgt[i], Isolines.Nodes.Items[i].x, Isolines.Nodes.Items[i].y]));
  end;

  IsoValuesList.Free;
  CloseFile(F38);
end;

procedure TLevelCurvesForm.FileSave1Execute(Sender: TObject);
begin
  SaveDialog.Execute;
end;


procedure TLevelCurvesForm.FileNew1Execute(Sender: TObject);
begin
  { Do nothing }
end;

procedure TLevelCurvesForm.FileOpen1Execute(Sender: TObject);
begin
  OpenDialog.Execute;
end;


procedure TLevelCurvesForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  UserAbort := Key = #27;
  with dc_cam do
    case Key of
      '7': RotateAbsolute(-15, 0, 0);
      '9': RotateAbsolute(+15, 0, 0);
      '4': RotateAbsolute(0, -15, 0);
      '6': RotateAbsolute(0, +15, 0);
      '1': RotateAbsolute(0, 0, -15);
      '3': RotateAbsolute(0, 0, +15);
    end;
end;

procedure TLevelCurvesForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TLevelCurvesForm.HelpAbout1Execute(Sender: TObject);
begin
  with TAboutBox.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TLevelCurvesForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  temp: Tcomponent;
begin
  SetLength(Mat,0);
  SetLength(Scx,0);
  SetLength(Scy,0);
  SetLength(Scz,0);

  SetLength(Hgt,0);
  SetLength(Xarr,0);
  SetLength(Yarr,0);
  SetLength(Zarr,0);
  SetLength(res3DPlane,0);
  PlaneXY.MeshObjects.Clear;
  Isolines.Nodes.Clear;
  Action := caFree;

  for i := ComponentCount - 1 downto 0 do
  begin
    temp := Components[i]; // if (Temp is TObject) then
    RemoveComponent(temp);
  end;
end;

end.

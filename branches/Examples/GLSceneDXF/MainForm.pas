{: MainForm<p>

  Main form of the ManuCAD program<p>

  <b>History :</b><font size=-1><ul>
    <li>13/02/03 - SJ - Added Block drawing
    <li>10/02/03 - SJ - Added Arc & Circle drawing & color selection
    <li>08/02/03 - DA - A new DXf is opened with the form
                 - SJ - Added Polyline drawing
    <li>26/01/03 - DA - Implemented the SaveAs action
                 - SJ - Added line drawing
    <li>18/01/03 - SJ - Added status bar
    <li>11/01/03 - SJ - Added print comfirmation
    <li>06/12/02 - SJ - Added toolbar
    <li>03/10/02 - DA - Zoom is now a little more intelligent
    <li>01/10/02 - DA - Added zooming and unzooming
    <li>30/10/02 - DA - Added a ifdef for delphi7 uses
    <li>13/10/02 - DA - Changes on LoadDXF
    <li>2?/09/02 - DA - Unit creation
  </ul></font>
}
unit MainForm;

interface

uses
  Windows,
  Winapi.Messages,
  System.SysUtils,
  System.UITypes,
  System.Variants,
  System.Classes,
  System.Actions,
  ToolWin,
  ComCtrls,
  Buttons,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ActnList, Vcl.ActnMan, Vcl.Menus,
  StdCtrls, Vcl.XPStyleActnCtrls, Printers,
  //GLS
  GLWin32Viewer, GLScene,
  GLObjects,

  // DXF
  GLDXFVectorFile, GLBitmapFont, GLWindowsFont,
  GLCadencer,

  // Added for objects adding
  TypesDXF,
  GLVectorGeometry,
  GLVectorTypes,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene: TGLScene;
    MainMenu1: TMainMenu;
    ActionManager1: TActionManager;
    ActLoadDXF: TAction;
    ActQuit: TAction;
    Fichier1: TMenuItem;
    ChargerDXF1: TMenuItem;
    Quitter1: TMenuItem;
    N1: TMenuItem;
    OpenDXFDialog: TOpenDialog;
    GLLightSource1: TGLLightSource;
    GLCamera: TGLCamera;
    Affichage1: TMenuItem;
    Zoomer1: TMenuItem;
    Dzoomer1: TMenuItem;
    ActZoomer: TAction;
    ActDezoomer: TAction;
    ToolBar1: TToolBar;
    MoveButton: TSpeedButton;
    ZoomButton: TSpeedButton;
    ZoomPButton: TSpeedButton;
    ZoomMButton: TSpeedButton;
    ActInit: TAction;
    InitButton: TSpeedButton;
    ActPrint: TAction;
    Imprimer1: TMenuItem;
    PrintDXFDialog: TPrintDialog;
    SelectButton: TSpeedButton;
    GLViewer: TGLSceneViewer;
    Initialise1: TMenuItem;
    StatusBar: TStatusBar;
    SpeedButton4: TSpeedButton;
    LineButton: TSpeedButton;
    PolyLButton: TSpeedButton;
    ArcButton: TSpeedButton;
    ActSave: TAction;
    ActSaveas: TAction;
    Enregistrer1: TMenuItem;
    Enregistrersous1: TMenuItem;
    SaveDXFDialog: TSaveDialog;
    CircleButton: TSpeedButton;
    ColorDXFDialog: TColorDialog;
    ColorButton: TSpeedButton;
    BlockButton: TSpeedButton;
    BlockComboBox: TComboBox;
    RotateButton: TSpeedButton;
    GLDummyCube1: TGLDummyCube;
    procedure ActLoadDXFExecute(Sender: TObject);
    procedure ActQuitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActZoomerExecute(Sender: TObject);
    procedure ActDezoomerExecute(Sender: TObject);
    procedure GLViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ActInitExecute(Sender: TObject);
    procedure InitButtonClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ActPrintExecute(Sender: TObject);
    procedure Enregistrersous1Click(Sender: TObject);
    procedure ToolBarButtonSelect(Sender: TObject);
    procedure GLViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ActSaveExecute(Sender: TObject);
    procedure ActSaveasExecute(Sender: TObject);
    procedure ColorButtonClick(Sender: TObject);
  private
    { Déclarations privées }
    DXFFile : TGLDXFFile;
    OldMousePosition : TPoint;
    DownPosition : TPoint;
    SelectedMode : Integer;
    CurrentEntity : TDXFEntity;
    DrawingClickNb : Integer;
    CurrentColor : Integer;  
    procedure Select(XLeft,Yleft,XRight,YRight : integer);
  public
    { Déclarations publiques }
   
  end;

var
  Form1: TForm1;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$R *.dfm}

uses
  FileDXF,
  ProgressForm,

  // Added for objects adding
  ObjectsDXF,
  MathsDXF;

// ActDezoomerExecute
//
procedure TForm1.ActDezoomerExecute(Sender: TObject);
begin
  GLCamera.SceneScale := GLCamera.SceneScale * 0.85;
  if GLCamera.SceneScale < 0 then GLCamera.SceneScale := 0.001;
end;

// ActInitExecute
//
procedure TForm1.ActInitExecute(Sender: TObject);
begin
  with GLCamera do begin
    Position.SetPoint(0, 0, -10);
    SceneScale := 1;
  end;
  BlockComboBox.Items.Clear;
end; 

// ActLoadDXFExecute
//
procedure TForm1.ActLoadDXFExecute(Sender: TObject);
var
  DXFFileStream : TFileStream;
begin
  if OpenDXFDialog.Execute then begin
    ActInit.Execute;  
    DXFFileStream := TFileStream.Create(OpenDXFDialog.FileName, fmOpenRead or fmShareDenyWrite);
    Progress.Show;
    DXFFile.OnProgress := Progress.UpdateProgress;
    Enabled := False;
    try
      // read the DXF file
      DXFFile.LoadFromStream(DXFFileStream);
      GLScene.Objects.AddChild(DXFFile.DirectOpenGL);
      // center the drawing
      with DXFFile.DXF.Header do
        GLCamera.Position.SetPoint(GLCamera.Position.X + ((ExtMin.X - ExtMax.X) / 2),
          GLCamera.Position.Y - ((ExtMin.Y - ExtMax.Y) / 2),
          GLCamera.Position.Z + ((ExtMin.Z - ExtMax.Z) / 2));
      // show the name of the open file in title and status bar
      Caption := 'ManuCAD - ' + OpenDXFDialog.FileName;
      StatusBar.Panels.Items[0].Text := OpenDXFDialog.FileName;
      // enable actions
      Enregistrer1.Enabled := True;
      Enregistrersous1.Enabled := True;
    finally
      DXFFileStream.Free;
      Progress.Hide;
      Enabled := True;
    end;
  end;
end;

// ActPrintExecute
//
procedure TForm1.ActPrintExecute(Sender: TObject);
var
  Bit : TBitmap;
begin
  if PrintDXFDialog.Execute then begin
    Bit := GLViewer.Buffer.CreateSnapShot.Create32BitsBitmap;
    with Printer do begin
      BeginDoc;
      Canvas.StretchDraw(Rect(0, 0, 5000, 5000), Bit);
      EndDoc;
    end;
    FreeAndNil(Bit);
  end;
end;

// ActQuitExecute
//
procedure TForm1.ActQuitExecute(Sender: TObject);
begin
  if MessageDlg('Voulez vous vraiment quitter ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    Close;
  end;
end;

// ActZoomerExecute
//
procedure TForm1.ActZoomerExecute(Sender: TObject);
begin
  GLCamera.SceneScale := GLCamera.SceneScale * 1.15;
end;

// ColorButtonClick
//
procedure TForm1.ColorButtonClick(Sender: TObject);
var
  i : Integer;
begin
  if ColorDXFDialog.Execute then
    for i := 0 to Def_Cols do
      if DXF_Layer_Colours[i] = ColorDXFDialog.Color then begin
        CurrentColor := i;
        Break;
      end;
end;

// Enregistrersous1Click
//
procedure TForm1.Enregistrersous1Click(Sender: TObject);
var
  DXFFileStream : TFileStream;
begin
  if SaveDXFDialog.Execute then begin
    DXFFileStream := TFileStream.Create(SaveDXFDialog.FileName,
      fmCreate or fmOpenWrite or fmShareExclusive);
    Progress.Show;
    DXFFile.OnProgress := Progress.UpdateProgress;
    Enabled := False;
    try
      DXFFile.DXF.SaveToStream(DXFFileStream);
    finally
      DXFFileStream.Free;
      Progress.Hide;
      Enabled := True;
    end;
  end;
end;

// FormCreate
//
procedure TForm1.FormCreate(Sender: TObject);
begin
  // set the decimal separator as in the DXF Files
  FormatSettings.DecimalSeparator := '.';

  // sets the default mode
  SelectedMode := 1;

  // sets the default color
  CurrentColor := 3;

  // create the DXF file
  DXFFile := TGLDXFFile.Create;
  ActInit.Execute;
  // read the DXF file
  DXFFile.NewDXF;
  GLScene.Objects.AddChild(DXFFile.DirectOpenGL);
  // show the name of the open file in title and status bar
  Caption := 'ManuCAD - Nouveau fichier';
  StatusBar.Panels.Items[0].Text := 'Nouveau fichier';
  // enable actions
  Enregistrer1.Enabled := True;
  Enregistrersous1.Enabled := True;
end;

// FormDestroy
//
procedure TForm1.FormDestroy(Sender: TObject);
begin
  // destroy the DXF File
  DXFFile.Free;
end;

// FormKeyDown
//
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  with GLCamera do begin
    case Key of
      VK_LEFT       : Position.X := Position.X + 50/(37*SceneScale);
      VK_RIGHT      : Position.X := Position.X - 50/(37*SceneScale);
      VK_UP         : Position.Y := Position.Y + 50/(37*SceneScale);
      VK_DOWN       : Position.Y := Position.Y - 50/(37*SceneScale);
      VK_ADD        : ActZoomer.Execute;
      VK_SUBTRACT   : ActDezoomer.Execute;
    end;
  end;
end;

// FormMouseWheel
//
procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  with GLCamera do begin
    SceneScale := SceneScale + (WheelDelta/200)*SceneScale;
    if SceneScale < 0 then SceneScale := 0.001;
  end;
end;

// GLViewerMouseDown
//
procedure TForm1.GLViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  OldMousePosition.X := X;
  OldMousePosition.Y := Y;
  DownPosition.X := X;
  DownPosition.Y := Y;
end;

// GLViewerMouseMove
//
procedure TForm1.GLViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  AVect, AVectResult : TVector4f;
  ATempVect : TVector3f;
  Line : TDXFLine;
  Polyl : TDXFPolyline;   
  Ark : TDXFArc;       
  Circl : TDXFCircle;
  
begin
  MakeVector(AVect, X, GLViewer.Height - Y, 0);
  GLViewer.Buffer.ScreenVectorIntersectWithPlaneXY(AVect, 0, AVectResult);

  with StatusBar.Panels do begin
    Items[1].Text := 'X : ' + FormatFloat('0.0', -AVectResult.X);
    Items[2].Text := 'Y : ' + FormatFloat('0.0', AVectResult.Y);
    Items[3].Text := 'Z : ' + FormatFloat('0.0', AVectResult.Z);
  end;

  with GLCamera do begin
    case SelectedMode of
      1 : begin
            // Move
            if ssLeft in Shift then begin
              Position.X := Position.X - (OldMousePosition.X - X)/(37*SceneScale);
              Position.Y := Position.Y - (OldMousePosition.Y - Y)/(37*SceneScale);
            end;
          end;
      2 : begin
            // Select

          end;
      3 : begin
            // Zoom
            if ssLeft in Shift then begin
              SceneScale := SceneScale + ((OldMousePosition.Y - Y)/100)*SceneScale;
              if SceneScale < 0 then SceneScale := 0.001;
            end;
          end;
      4 : begin
            // Line
            if DrawingClickNb = 1 then begin
              Line := TDXFLine(CurrentEntity);
              Line.EndPoint := AffineVectorMake(AVectResult);
              Line.EndPoint.X := -AVectResult.X;
              GLViewer.Refresh;
            end;
          end;
      5 : begin
            // PolyLine
            if DrawingClickNb > 0 then begin
              Polyl := TDXFPolyline(CurrentEntity);
              with Polyl do begin
                PointList[Length(PointList) - 1].Primary := AffineVectorMake(AVectResult);
                PointList[Length(PointList) - 1].Primary.X := -AVectResult.X;
              end;
              GLViewer.Refresh;
            end;
          end;
      6 : begin
            // Arc
            case DrawingClickNb of
              1 : begin
                    Ark := TDXFArc(CurrentEntity);
                    ATempVect := AffineVectorMake(AVectResult);
                    ATempVect.X := -ATempVect.X;
                    Ark.Radius := VectorDistance(Ark.Primary, ATempVect);
                    GLViewer.Refresh;
                  end;
              2 : begin
                    Ark := TDXFArc(CurrentEntity);
                    ATempVect := AffineVectorMake(AVectResult);
                    ATempVect.X := -ATempVect.X;
                    Ark.StartAngle := Angle(Ark.Primary, ATempVect);
                    GLViewer.Refresh;
                  end;
              3 : begin
                    Ark := TDXFArc(CurrentEntity);
                    ATempVect := AffineVectorMake(AVectResult);
                    ATempVect.X := -ATempVect.X;
                    Ark.EndAngle := Angle(Ark.Primary, ATempVect);
                    GLViewer.Refresh;
                  end;
            end;
          end;
      7 : begin
            // Circle
            if DrawingClickNb = 1 then begin
              Circl := TDXFCircle(CurrentEntity);
              ATempVect := AffineVectorMake(AVectResult);
              ATempVect.X := -ATempVect.X;
              Circl.Radius := VectorDistance(Circl.Primary, ATempVect);
              GLViewer.Refresh;
            end;
          end;
      9 : begin
            // Rotation
            GLCamera.TargetObject := DXFFile.DirectOpenGL;
            GLCamera.MoveAroundTarget(OldMousePosition.Y - Y, OldMousePosition.X - X);
          end;
    end;
  end;
  OldMousePosition.X := X;
  OldMousePosition.Y := Y;
end;

// GLViewerMouseUp
//
procedure TForm1.GLViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AVect, AVectResult : TVector4f;
  ATempVect : TVector3f;
  Line : TDXFLine;
  Polyl : TDXFPolyline;
  Ark : TDXFArc;
  Circl : TDXFCircle;
  Blk : TDXFInsert;

begin
  MakeVector(AVect, X, GLViewer.Height - Y, 0);
  GLViewer.Buffer.ScreenVectorIntersectWithPlaneXY(AVect, 0, AVectResult);
  case SelectedMode of
    2 : begin
          // Select
          Select(DownPosition.X, DownPosition.Y, X, Y)
        end;

    4 : begin
          // Draw line
          if Button = TMouseButton.mbLeft then begin
            if DrawingClickNb = 0 then begin
              Line := TDXFLine.Create(DXFFile.DXF.Layers);
              Line.ColorNum := CurrentColor;
              Line.Layer := DXFFile.DXF.Layers.Layer[0];
              Line.Primary := AffineVectorMake(AVectResult);
              Line.Primary.X := -AVectResult.X;
              DXFFile.DXF.Entities.AddEntity(Line);
              CurrentEntity := Line;
              DrawingClickNb := 1;
            end else begin
              Line := TDXFLine(CurrentEntity);
              Line.EndPoint := AffineVectorMake(AVectResult);
              Line.EndPoint.X := -AVectResult.X;
              GLViewer.Refresh;
              DrawingClickNb := 0;
            end;
          end;
        end;

    5 : begin
        // Draw polyline
          if Button = TMouseButton.mbLeft then begin
            if DrawingClickNb = 0 then begin
              Polyl := TDXFPolyline.Create(DXFFile.DXF.Layers);
              Polyl.ColorNum := CurrentColor;
              Polyl.Layer := DXFFile.DXF.Layers.Layer[0];
              SetAffineVector(Polyl.Primary, 0, 0, 0);
              with Polyl do begin
                SetLength(PointList, 2);
                PointList[0] := TDXFVertex.Create(DXFFile.DXF.Layers);
                PointList[0].Layer := DXFFile.DXF.Layers.Layer[0];
                PointList[0].Primary := AffineVectorMake(AVectResult);
                PointList[0].Primary.X := -AVectResult.X;
                PointList[1] := TDXFVertex.Create(DXFFile.DXF.Layers);
                PointList[1].Layer := DXFFile.DXF.Layers.Layer[0];
                PointList[1].Primary := AffineVectorMake(AVectResult);
                PointList[1].Primary.X := -AVectResult.X;
              end;
              DXFFile.DXF.Entities.AddEntity(Polyl);
              CurrentEntity := Polyl;
            end else begin
              Polyl := TDXFPolyline(CurrentEntity);
              with Polyl do begin
                PointList[Length(PointList) - 1].Primary := AffineVectorMake(AVectResult);
                PointList[Length(PointList) - 1].Primary.X := -AVectResult.X;
                SetLength(PointList, Length(PointList) + 1);
                PointList[Length(PointList) - 1] := TDXFVertex.Create(DXFFile.DXF.Layers);
                PointList[Length(PointList) - 1].Layer := DXFFile.DXF.Layers.Layer[0];
                PointList[Length(PointList) - 1].Primary := AffineVectorMake(AVectResult);
                PointList[Length(PointList) - 1].Primary.X := -AVectResult.X;
              end;
              GLViewer.Refresh;
            end;
            inc(DrawingClickNb);
          end else if Button = TMouseButton.mbRight then begin
            Polyl := TDXFPolyline(CurrentEntity);
            Polyl.PointList[Length(Polyl.PointList) - 1].Free;
            SetLength(Polyl.PointList, Length(Polyl.PointList) - 1);
            DrawingClickNb := 0;
            GLViewer.Refresh;
          end;
        end;

    6 : // Draw arc
        if Button = TMouseButton.mbLeft then begin
          case DrawingClickNb of
            0 : begin
                  Ark := TDXFArc.Create(DXFFile.DXF.Layers);
                  Ark.ColorNum := CurrentColor;
                  Ark.Layer := DXFFile.DXF.Layers.Layer[0];
                  Ark.Primary := AffineVectorMake(AVectResult);
                  Ark.Primary.X := -AVectResult.X;
                  Ark.StartAngle := 0;
                  Ark.EndAngle := 360;
                  DXFFile.DXF.Entities.AddEntity(Ark);
                  CurrentEntity := Ark;
                  DrawingClickNb := 1;
                end;
            1 : begin
                  Ark := TDXFArc(CurrentEntity);
                  ATempVect := AffineVectorMake(AVectResult);
                  ATempVect.X := -ATempVect.X;
                  Ark.Radius := VectorDistance(Ark.Primary, ATempVect);
                  DrawingClickNb := 2;
                  GLViewer.Refresh;
                end;
            2 : begin
                  Ark := TDXFArc(CurrentEntity);
                  ATempVect := AffineVectorMake(AVectResult);
                  ATempVect.X := -ATempVect.X;
                  Ark.StartAngle := Angle(Ark.Primary, ATempVect);
                  DrawingClickNb := 3;
                  GLViewer.Refresh;
                end;
            3 : begin
                  Ark := TDXFArc(CurrentEntity);
                  ATempVect := AffineVectorMake(AVectResult);
                  ATempVect.X := -ATempVect.X;
                  Ark.EndAngle := Angle(Ark.Primary, ATempVect);
                  DrawingClickNb := 0;
                  GLViewer.Refresh;
                end;
          end;
        end;

    7 : // Draw arc
        if Button = TMouseButton.mbLeft then begin
          if DrawingClickNb = 0 then begin
            Circl := TDXFCircle.Create(DXFFile.DXF.Layers);
            Circl.ColorNum := CurrentColor;
            Circl.Layer := DXFFile.DXF.Layers.Layer[0];
            Circl.Primary := AffineVectorMake(AVectResult);
            Circl.Primary.X := -AVectResult.X;
            DXFFile.DXF.Entities.AddEntity(Circl);
            CurrentEntity := Circl;
            DrawingClickNb := 1;
          end else begin
            Circl := TDXFCircle(CurrentEntity);
            ATempVect := AffineVectorMake(AVectResult);
            ATempVect.X := -ATempVect.X;
            Circl.Radius := VectorDistance(Circl.Primary, ATempVect);
            DrawingClickNb := 0;
            GLViewer.Refresh;
          end;
        end;

    8 : // Draw Block
        if Button = TMouseButton.mbLeft then begin
          Blk := TDXFInsert.Create(DXFFile.DXF.Layers, DXFFile.DXF.Blocks);
          Blk.ColorNum := CurrentColor;
          blk.BlockName := BlockComboBox.Text;
          Blk.Layer := DXFFile.DXF.Layers.Layer[0];
          Blk.Primary := AffineVectorMake(AVectResult);
          Blk.Primary.X := -AVectResult.X;
          DXFFile.DXF.Entities.AddEntity(Blk);
//          CurrentEntity := Blk;
          GLViewer.Refresh;
        end;
  end;
end;

// InitButtonClick
//
procedure TForm1.InitButtonClick(Sender: TObject);
begin
  ActInit.Execute;
end;

// Select
//
procedure TForm1.Select(XLeft, Yleft, XRight, YRight : integer);
var
  AVect, AVectLeft, AVectRight : TVector4f;
  i : Integer;
begin

  MakeVector(AVect, XLeft, GLViewer.Height - YLeft, 0);
  GLViewer.Buffer.ScreenVectorIntersectWithPlaneXY(AVect, 0, AVectLeft);
  AVectLeft.X := -AVectLeft.X;

  MakeVector(AVect, XRight, GLViewer.Height - YRight, 0);
  GLViewer.Buffer.ScreenVectorIntersectWithPlaneXY(AVect, 0, AVectRight);
  AVectRight.X := -AVectRight.X;

  for i := 0 to DXFFile.DXF.Entities.Count-1 do begin
    With DXFFile.DXF.Entities do begin
      if (Entity[i] is TDXFPoint) then begin
        if  (TDXFPoint(Entity[i]).Primary.X > AVectLeft.X)
        and (TDXFPoint(Entity[i]).Primary.Y < AVectLeft.Y)
        and (TDXFPoint(Entity[i]).Primary.X < AVectRight.X)
        and (TDXFPoint(Entity[i]).Primary.Y > AVectRight.Y)
        then TDXFPoint(Entity[i]).ColorNum := 6;
      end;
    end;
  end;
  GLViewer.Refresh;

end;

// ToolBarButtonSelect
//
procedure TForm1.ToolBarButtonSelect(Sender: TObject);
var
  i : Integer;
begin
  SelectedMode := TComponent(Sender).Tag;
  case SelectedMode of
    8 : begin
          for i := 0 to DXFFile.DXF.Blocks.Count - 1 do begin
            BlockComboBox.Items.Append(DXFFile.DXF.Blocks.Block[i].BlockName);
          end;
          BlockComboBox.Visible := True;
        end;
    else begin
      BlockComboBox.Visible := False;
      GLCamera.TargetObject := nil;
    end;
  end;
end;

// ActSaveExecute
//
procedure TForm1.ActSaveExecute(Sender: TObject);
begin
  //
end;

// ActSaveasExecute
//
procedure TForm1.ActSaveasExecute(Sender: TObject);
var
  DXFFileStream : TFileStream;
begin
  if SaveDXFDialog.Execute then begin
    DXFFileStream := TFileStream.Create(SaveDXFDialog.FileName,
      fmCreate or fmOpenWrite or fmShareExclusive);
    Progress.Show;
    DXFFile.OnProgress := Progress.UpdateProgress;
    Enabled := False;
    try
      DXFFile.DXF.SaveToStream(DXFFileStream);
    finally
      DXFFileStream.Free;
      Progress.Hide;
      Enabled := True;
    end;
  end;
end;

end.

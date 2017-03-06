unit Unit1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
 SysUtils,Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  //GLS
  GLLCLViewer, GLScene, GLObjects, GLGeomObjects, GLSkydome, GLAsyncTimer,
  GLCadencer, GLCoordinates, GLCrossPlatform, GLSmoothNavigator, GLNavigator,
  GLCameraController, GLDCE, GLBaseClasses;

type
  TAccumObject = class
    private
      FOnUse: boolean;
      FObject: TGLCube;
      FID: integer;
      procedure SetOnUse(AValue: boolean);
    public
      constructor Create(AID: integer);
      property OnUse: boolean read FOnUse write SetOnUse;
      property ID: integer read FID write FID;
      destructor Destroy; override;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    GLScene: TGLScene;
    SceneViewer: TGLSceneViewer;
    camMain: TGLCamera;
    dcObjects: TGLDummyCube;
    dscTerrain: TGLDisk;
    lsLight: TGLLightSource;
    esdSky: TGLEarthSkyDome;
    btnInitAccum: TButton;
    btnAddObject: TButton;
    btnDelObject: TButton;
    btnDeinitAccum: TButton;
    FPSTimer: TGLAsyncTimer;
    pnlFPS: TPanel;
    Cadencer: TGLCadencer;
    procedure FormResize(Sender: TObject);
    procedure btnInitAccumClick(Sender: TObject);
    procedure btnAddObjectClick(Sender: TObject);
    procedure btnDelObjectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDeinitAccumClick(Sender: TObject);
    procedure FPSTimerTimer(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  MAX_OBJECTS = 10;

var
  frmMain: TfrmMain;
  Objects: array[0..MAX_OBJECTS - 1] of TAccumObject;
  AccumInit: boolean = false;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function PlusMinusRandom(AValue: Single): Single;
begin
  if Random(100) <= 50 then Result := Random(Trunc(AValue * 100)) / 100 else Result := - Random(Trunc(AValue * 100)) / 100;
end;

constructor TAccumObject.Create;
begin
  FObject := TGLCube.CreateAsChild(frmMain.dcObjects);
  FOnUse := false;
  with FObject do begin
    CubeDepth := 0.5;
    CubeHeight := 0.1;
    CubeWidth := 0.1;
    Visible := false;
  end;
end;

procedure TAccumObject.SetOnUse(AValue: boolean);
begin
  if AValue <> FOnUse then FOnUse := AValue;
  FObject.Visible := FOnUse;
end;

destructor TAccumObject.Destroy;
begin
  FreeAndNil(FObject);
  inherited Destroy;
end;

function GetNonUseObject: TAccumObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to MAX_OBJECTS - 1 do
    if not Objects[i].OnUse then begin
      Result := Objects[i];
      Objects[i].FObject.Position.X := PlusMinusRandom(0.8);
      Objects[i].FObject.Position.Y := PlusMinusRandom(0.8);
      Result.OnUse := true;
      Break;
    end;
end;

function KillUseObject: integer;
var
  i: integer;
begin
  for i := 0 to MAX_OBJECTS - 1 do
    if Objects[i].OnUse then begin
      Objects[i].OnUse := false;
      Result := i;
      Break;
      Exit;
    end else Result := -1;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  camMain.SceneScale := SceneViewer.Width / 666;
end;

procedure TfrmMain.btnInitAccumClick(Sender: TObject);
var
  i: integer;
begin
  if AccumInit then begin
    Application.MessageBox('Àêêóìóëÿòîð îáúåêòîâ óæå èíèöèàëèçèðîâàí!', PChar(Caption), 0);
    Exit;
  end;
  for i := 0 to MAX_OBJECTS - 1 do Objects[i] := TAccumObject.Create(i);
  AccumInit := true;
end;

procedure TfrmMain.btnAddObjectClick(Sender: TObject);
begin
  if not AccumInit then begin
    Application.MessageBox('Àêêóìóëÿòîð îáúåêòîâ íå èíèöèàëèçèðîâàí!', PChar(Caption), 0);
    Exit;
  end;
  if GetNonUseObject = nil then Application.MessageBox('Â àêêóìóëÿòîðå íåò ñâîáîäíûõ îáúåêòîâ!', PChar(Caption), 0);
end;

procedure TfrmMain.btnDelObjectClick(Sender: TObject);
begin
  if not AccumInit then begin
    Application.MessageBox('Àêêóìóëÿòîð îáúåêòîâ íå èíèöèàëèçèðîâàí!', PChar(Caption), 0);
    Exit;
  end;
  if KillUseObject = -1 then Application.MessageBox('Â àêêóìóëÿòîðå íåò çàíÿòûõ îáúåêòîâ!', PChar(Caption), 0);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Randomize;
  FPSTimer.OnTimer(FPSTimer);
end;

procedure TfrmMain.btnDeinitAccumClick(Sender: TObject);
var
  i: integer;
begin
  if not AccumInit then begin
    Application.MessageBox('Àêêóìóëÿòîð îáúåêòîâ íå èíèöèàëèçèðîâàí!', PChar(Caption), 0);
    Exit;
  end;
  for i := 0 to MAX_OBJECTS - 1 do Objects[i].Free;
  AccumInit := false;
end;

procedure TfrmMain.FPSTimerTimer(Sender: TObject);
begin
  pnlFPS.Caption := Format('%0.f FPS', [SceneViewer.FramesPerSecond]);
  SceneViewer.ResetPerformanceMonitor;
end;

procedure TfrmMain.CadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  SceneViewer.Invalidate;
end;

end.

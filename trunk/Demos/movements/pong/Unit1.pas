{: This demo is a remake of good old pong game...<p>

	Aim of the game is to prevent the ball from bouncing out of the board,
	each time the ball bumps on your pad you score a frag (er... point ;).<br>
	Move the pad with your mouse.<p>

   The demo makes use of stencil-based shadow volumes.
}
unit Unit1;

interface

uses
  Windows, Forms, GLScene, GLObjects, GLMisc, GLTexture, VectorGeometry, ExtCtrls,
  Classes, Controls, GLCadencer, GLWin32Viewer, GLSpaceText, GLShadowPlane,
  GLShadowVolume, GLCrossPlatform;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Plane1: TGLPlane;
    Cube1: TGLCube;
    Cube2: TGLCube;
    Cube3: TGLCube;
    Ball: TGLSphere;
    DummyCube1: TGLDummyCube;
	 GLLightSource1: TGLLightSource;
	 GLMaterialLibrary1: TGLMaterialLibrary;
    Pad: TGLCube;
    SpaceText1: TGLSpaceText;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLShadowVolume: TGLShadowVolume;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
	 { Déclarations privées }
	 ballVector : TAffineVector;
	 score : Integer;
	 gameOver : Boolean;
	 procedure ResetGame;
  public
	 { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses SysUtils, Dialogs;

procedure TForm1.FormCreate(Sender: TObject);
begin
	Randomize;
	GLSceneViewer1.Cursor:=crNone;
	ResetGame;
end;

procedure TForm1.ResetGame;
var
	angle : Single;
begin
	// places the ball in the mat center, resets score and ball speed
	angle:=DegToRad(45+Random(90));
	MakeVector(ballVector, 4*cos(angle), 4*sin(angle), 0);
	score:=0;
	gameOver:=False;
	Ball.Position.AsVector:=NullHmgPoint;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
	cPadMinMax = 6.25;
var
	px : Single;
begin
	// the pad's position is directly calculated from the mouse position
	px:=(x-(GLSceneViewer1.Width/2))*0.035;
	if px<-cPadMinMax then
		px:=-cPadMinMax
	else if px>cPadMinMax then
		px:=cPadMinMax;
	Pad.Position.X:=px;
//   GLCadencer1.Reset;
   // update the whole stuff now!
   GLCadencer1.Progress;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
	newBallPos : TVector;
begin
	// gameOver is True as soon as the ball is behind the pad, but we don't end
	// the game immediately so the user can realize he has lost
	if (not gameOver) and (deltaTime>0) then begin
		// calc expected new ball pos (if no bump occurs)
		// ( note : VectorCombine(v1, v2, f1, f2)=v1*f1+v2*f2 )
		newBallPos:=VectorCombine(Ball.Position.AsVector, ballVector, 1, deltaTime);
		// check collision with edges
		if newBallPos[0]<-7.05 then
			ballVector[0]:=-ballVector[0]
		else if newBallPos[0]>7.05 then
			ballVector[0]:=-ballVector[0]
		else if newBallPos[1]>4.55 then
			ballVector[1]:=-ballVector[1];
		// check collision with pad
		if newBallPos[1]<-4 then begin
			if (newBallPos[0]>Pad.Position.X-1.25) and (newBallPos[0]<Pad.Position.X+1.25) then begin
				// when ball bumps the pad, it is accelerated and the vector
				// is slightly randomized
				ballVector[1]:=-ballVector[1];
				ballVector[0]:=ballVector[0]+(Random(100)-50)/50;
				ballVector[1]:=ballVector[1]+0.1;
				// ...and of course a point is scored !
				Inc(score);
				SpaceText1.Text:=Format('%.3d', [score]);
			end else begin
				// ball missed !
				gameOver:=True;
            Exit;
         end
		end;
	end;
	// move the ball
	with Ball.Position do
		AsVector:=VectorCombine(AsVector, ballVector, 1, deltaTime);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
	// update performance monitor
	Caption:=Format('%s : %.2f FPS', [Name, GLSceneViewer1.FramesPerSecond]);
	GLSceneViewer1.ResetPerformanceMonitor;
	// display score window when game is over and the ball is well out of the board
	if gameOver and (Ball.Position.Y<-6) then begin
		// stop the timer to avoid stacking up Timer events
		// while the user makes up his mind...
		Timer1.Enabled:=False;
		if MessageDlg('Score : '+IntToStr(score)+#13#10#13#10+'Play again ?',
						  mtInformation, [mbYes, mbNo], 0)=mrYes then begin
			ResetGame;
			Timer1.Enabled:=True;
		end else Close;
	end;
end;

end.

unit mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLProc,
  Arrow, StdCtrls, ComCtrls, LCLType, LCLIntf, InterfaceBase, lazdeviceapis,
  Menus, ExtDlgs, LMessages, GLScene_Viewer_Form, GLScene_Core,
  GLScene_Objects, GLScene_MaterialEx, GLScene_Base_Classes, GLScene_Cadencer,
  //cthreads
  Unix, BaseUnix;

type

  { TForm1 }

  TForm1 = class(TGLSceneForm)//{GLScene}
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLMaterialLibraryEx1: TGLMaterialLibraryEx;
    GLScene1: TGLScene;
  {  procedure Arrow1Click(Sender: TObject);
    procedure Arrow1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Arrow1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Arrow1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);       }
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure btnShowInfoClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);

  //  procedure FormPaint(Sender: TObject);
 //   procedure MenuItem1Click(Sender: TObject);
  protected
    procedure OnSurfaceCreated(Sender: TObject);
    procedure OnSurfaceChanged(Sender: TObject);
    procedure OnSurfaceDestroyed(Sender: TObject);
  private
    { private declarations }
 //   procedure LMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure LMSize(var Message: TLMSize); message LM_SIZE;

  public
    { public declarations }
    ClickCounter: Integer;

  //  procedure HandleMessageDialogFinished(Sender: TObject; AResult: Integer);
  end;

  { TThread1 }

var
  Form1: TForm1;
  OGLContext:Pointer;


implementation

{ TSubControl }

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClick(Sender: TObject);
begin
  //   CDWidgetset.StartEGL;
  DebugLn(Format('Form click #%d', [ClickCounter]));
  Inc(ClickCounter);
  //   CDWidgetset.FinishEGL;
//  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  DebugLn('FormPaint');
end;

{procedure TForm1.Arrow1Click(Sender: TObject);
begin
  Caption := 'Clicked Arrow';
  DebugLn('Clicked Arrow');
end;

procedure TForm1.Arrow1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Down X=%d Y=%d', [X, Y]));
end;

procedure TForm1.Arrow1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Move X=%d Y=%d', [X, Y]));
end;

procedure TForm1.Arrow1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Up X=%d Y=%d', [X, Y]));
end;  }

procedure TForm1.btnShowInfoClick(Sender: TObject);
//var
//  i: Integer;
begin
  //for i := 0 to Screen.Fonts.Count - 1 do
  //  DebugLn(Screen.Fonts.Strings[i]);
  DebugLn('Device.Manufacturer='+Device.Manufacturer);
  DebugLn('Device.Model='+Device.Model);
  Device.Vibrate(2000);
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
   DebugLn('AsyncTimer');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   DebugLn('OnCreate');

   //  DebugLn('Failed to create surface to draw');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DebugLn('OnDestroy');
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('MouseDown x=%d y=%d', [x, y]));
  //fTimer.FTimerThread.Resume;

end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
    DebugLn('GLCadencer1Progress');
 // Invalidate;
 // GLCube1.Turn(20*deltaTime);
end;

{procedure TForm1.LMPaint(var Message: TLMPaint);
begin
  DebugLn('OnPaint');
   // Buffer.Render;

end; }

procedure TForm1.LMSize(var Message: TLMSize);
begin
 // DebugLn('OnSize: Width'+inttostr(Width)+'Height'+inttostr(Height));

end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('MouseMove x=%d y=%d', [x, y]));
end;

{procedure TForm1.FormPaint(Sender: TObject);
var
  lPoints: array[0..2] of TPoint;
begin
  Canvas.Brush.Color := clRed;
  lPoints[0] := Point(67,57);
  lPoints[1] := Point(11,29);
  lPoints[2] := Point(67,1);
  Canvas.Polygon(lPoints);

  Canvas.Brush.Color := clRed;
  Canvas.Rectangle(10, 10, 100, 100);
  Canvas.Brush.Color := clGreen;
  Canvas.Rectangle(100, 100, 200, 200);
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(200, 200, 300, 300);
end;       }

{procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  DebugLn('[TForm1.MenuItem1Click]');
end; }

{procedure TForm1.HandleMessageDialogFinished(Sender: TObject; AResult: Integer);
begin
  DebugLn(Format('[TForm1.HandleMessageDialogFinished] AResult=%d', [AResult]));
end;    }

procedure TForm1.OnSurfaceCreated(Sender: TObject);
var
  LNumElements: Integer;
begin
  DebugLn('OnSurfaceCreated');
 // CDWidgetset.StartEGL();
 // SetLength(LConfigs, LNumElements);
 // CDWidgetset.eglGetConfigs(@LConfigs[0], Length(LConfigs), @LNumElements) ;
 { OGLContext:= CDWidgetset.CreateContext;
  CDWidgetset.CreateSurface();
  CDWidgetset.PurgeBuffers();   }
end;

procedure TForm1.OnSurfaceChanged(Sender: TObject);
begin
  DebugLn('OnSurfaceChanged');
end;

procedure TForm1.OnSurfaceDestroyed(Sender: TObject);
begin
  DebugLn('OnSurfaceDestroyed');
 // CDWidgetset.ClearBuffers();
 { CDWidgetset.DestroySurface();
  CDWidgetset.DestroyContext(nil);  }
 // CDWidgetset.FinishEGL();
end;

initialization



end.


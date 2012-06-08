unit mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLProc,
  Arrow, StdCtrls, ComCtrls, LCLType, LCLIntf, InterfaceBase, lazdeviceapis,
  Menus, ExtDlgs, LMessages, customdrawnint, GLScene_Viewer_Form, GLScene_Core,
  GLScene_Objects;

type

  { TForm1 }

  TForm1 = class(TGLSceneForm)//{GLScene}
    GLScene1: TGLScene;
  {  procedure Arrow1Click(Sender: TObject);
    procedure Arrow1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Arrow1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Arrow1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);       }
    procedure btnShowInfoClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);

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
  TTimerThread = class(TThread)

  end;

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

procedure TForm1.FormCreate(Sender: TObject);
var
      FTimerThread: TThread;
 {   MajorVersion, MinorVersion, Err: EGLInt;
  FDisplay: EGLDisplay;
  FSurface: EGLSurface;
  FConfig: EGLConfig;
    LNumElements: Integer;
    Attribute: array[0..12] of Integer = (
   // EGL_BUFFER_SIZE,16,

    EGL_RED_SIZE, 5,
    EGL_GREEN_SIZE, 6,
    EGL_BLUE_SIZE, 5,
    EGL_ALPHA_SIZE,0,
    EGL_DEPTH_SIZE, 0,
    EGL_STENCIL_SIZE,0,
  //  EGL_SURFACE_TYPE,EGL_WINDOW_BIT,
  //  EGL_RENDERABLE_TYPE,EGL_OPENGL_ES2_BIT,
    0);
      LConfigs: array of EGLConfig;}

     //     vzHandle: TLibHandle = 0;
begin
   DebugLn('OnCreate');
 {  FTimerThread := TTimerThread.Create(False);
   FTimerThread.FreeOnTerminate := False;
  FTimerThread.Priority := tpTimeCritical; }
 //  CDWidgetset.OnSurfaceCreated := @OnSurfaceCreated;
//CDWidgetset.OnSurfaceChanged := @OnSurfaceChanged;
//CDWidgetset.OnSurfaceDestroyed := @OnSurfaceDestroyed;

 //  CDWidgetset.StartEGL;
 //  CDWidgetset.FinishEGL;
//   OGLContext:= CDWidgetset.CreateContext;
 ///  CDWidgetset.DestroyContext(OGLContext);

  //  vzHandle := LoadLibrary(PChar('libz.so'));
    //  DebugLn('libz:'+inttostr(vzHandle));
   // FDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  //  if eglInitialize(FDisplay, @MajorVersion, @MinorVersion) = 0 then
   //   DebugLn('Failed to initialize OpenGL ES');

  // DebugLn('MajorVersion:'+inttostr(MajorVersion)+' MinorVersion:'+inttostr(MinorVersion));

 //  if eglChooseConfig(FDisplay, nil, nil, 0, @LNumElements) = EGL_TRUE then
  //  begin
  //    SetLength(LConfigs, LNumElements);
   //   if eglGetConfigs(FDisplay, @LConfigs[0], Length(LConfigs), @LNumElements) = EGL_TRUE then
   //     DebugLn('eglGetConfigs');
  //  end;

//  if eglChooseConfig(FDisplay, @Attribute[0], @FConfig, 1, @LNumElements) <> EGL_TRUE then
  //  DebugLn('Failed to accept attributes');
 // FSurface := eglCreateWindowSurface(FDisplay, LConfigs[0], nil, nil);
 //  Err := eglGetError;
//	if Err <> EGL_SUCCESS then
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
end;

{procedure TForm1.LMPaint(var Message: TLMPaint);
begin
  DebugLn('OnPaint');
   // Buffer.Render;

end; }

procedure TForm1.LMSize(var Message: TLMSize);
begin
  DebugLn('OnSize');

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
  LConfigs: array of EGLConfig;
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


//
// This unit is part of the GLScene Project, http://glscene.org
//
{ 
  FireMonkey fit GLScene viewer
  The whole history is logged in previous version of the unit
}

unit GLWinFMXViewer;

interface

{$I GLScene.inc}

uses
  WinAPI.Windows,
  // System
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  // FMX
  FMX.Forms,
  FMX.Platform.Win,
  FMX.Types,
  FMX.Types3D,
  FMX.Controls,
  FMX.Graphics,
  
  OpenGLTokens,
  GLScene,
  GLWin32Context,
  GLTextureFormat,
  GLContext;

type

  TGLSceneViewport = class(TControl)
  private
     
    FGLSBuffer: TGLSceneBuffer;
    FFMXBuffer: TBitmap;
    FFMXContext: TContext3D;
    FMultisample: FMX.Types3D.TMultisample;
    FParentHandle: HWND;
    FOwnDC: HDC;
    FDrawing: Boolean;
    FPostRender: TNotifyEvent;
    procedure SetBuffer(const Value: TGLSceneBuffer);
    function GetGLSceneCamera: TGLCamera;
    procedure SetGLSceneCamera(const Value: TGLCamera);
    procedure CopyBuffer(Sender: TObject);
    procedure SetBeforeRender(const Value: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetAfterRender(const Value: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
  protected
    
    procedure Paint; override;
  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Realign; override; - E2179, removed override;
    procedure Realign;
  published
    

    { Triggered before the scene's objects get rendered.
       You may use this event to execute your own OpenGL rendering. }
    property BeforeRender: TNotifyEvent read GetBeforeRender write
      SetBeforeRender;
    { Triggered just after all the scene's objects have been rendered.
       The OpenGL context is still active in this event, and you may use it
       to execute your own OpenGL rendering.<p> }
    property PostRender: TNotifyEvent read FPostRender write FPostRender;
    { Called after rendering.
      You cannot issue OpenGL calls in this event, if you want to do your own
      OpenGL stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;
    { Access to buffer properties. }
    property Buffer: TGLSceneBuffer read FGLSBuffer write SetBuffer;
    { Camera from which the scene is rendered. }
    property GLSceneCamera: TGLCamera read GetGLSceneCamera
      write SetGLSceneCamera;
  end;

implementation

{ TGLSceneViewport }

constructor TGLSceneViewport.Create(AOwner: TComponent);
var
  FMXH: TFmxHandle;
begin
  inherited Create(AOwner);
  FGLSBuffer := TGLSceneBuffer.Create(Self);
  FGLSBuffer.ContextOptions := FGLSBuffer.ContextOptions + [roDestinationAlpha] - [roDoubleBuffer] - [roNoSwapBuffers] + [roDebugContext];
  FGLSBuffer.BackgroundAlpha := 1.0;
  FGLSBuffer.AccumBufferBits := 32;
  FGLSBuffer.PostRender := CopyBuffer;
  if Owner is TCommonCustomForm then
  begin
    FMXH := THandle(Owner); /// TCommonCustomForm(Owner).Handle;
    FParentHandle := FMXH; /// FmxHandleToHWND(FMXH);
  end;
  Width := 100;
  Height := 100;
  FFMXBuffer := TBitmap.Create(100, 100);
  FMultisample := TMultisample.msNone;
  FFMXContext := TContextManager.DefaultContextClass.Create;
///  FFMXContext := TContextManager.DefaultContextClass.CreateFromTexture(FFMXBuffer.Texture,FMultisample,False);

end;

destructor TGLSceneViewport.Destroy;
begin
  FreeAndNil(FGLSBuffer);
  if FOwnDC <> 0 then
  begin
    ReleaseDC(FParentHandle, FOwnDC);
    FOwnDC := 0;
  end;
  FreeAndNil(FFMXBuffer);
  FreeAndNil(FFMXContext);
  FreeAndNil(FMultisample);
  inherited;
end;

procedure TGLSceneViewport.Realign;
begin
  inherited Realign;

  if FFMXContext <> nil then
  begin
    FGLSBuffer.DestroyRC; // Yar: Painfull, but Service Context, which is shared handles, will be no so much
    FFMXBuffer.SetSize(Trunc(Width), Trunc(Height));
    FFMXContext.SetSize(Trunc(Width), Trunc(Height));
    AlignObjects(Self, Margins, FFMXBuffer.Width, FFMXBuffer.Height, FLastWidth, FLastHeight, FDisableAlign);
  end;
end;

procedure TGLSceneViewport.CopyBuffer(Sender: TObject);
var
  tempBuffer: TGLEnum;
begin
  // Flip GL framebuffer
  if GL.ARB_framebuffer_object or GL.EXT_framebuffer_blit  then
  begin
    if Buffer.RenderingContext.AntiAliasing in [aaDefault, aaNone] then
      tempBuffer := GL_AUX0
    else
      tempBuffer := GL_LEFT;
    GL.ReadBuffer(GL_FRONT);
    GL.DrawBuffer(tempBuffer);
    FGLSBuffer.RenderingContext.GLStates.ReadFrameBuffer := 0;
    FGLSBuffer.RenderingContext.GLStates.DrawFrameBuffer := 0;
    GL.BlitFramebuffer(
      0, FGLSBuffer.Height-1, FGLSBuffer.Width-1, 0,
      0, 0,                   FGLSBuffer.Width-1, FGLSBuffer.Height-1,
      GL_COLOR_BUFFER_BIT, GL_NEAREST);
    GL.ReadBuffer(tempBuffer);
    GL.DrawBuffer(GL_FRONT);
  end
  else
  begin
    FFMXBuffer.FlipHorizontal;
    GL.ReadBuffer(GL_FRONT);
  end;

  // Read framebuffer to operative memory
  // FFMXBuffer.Startline - E2003 Undeclared identifier: 'StartLine', changed to
  GL.ReadPixels(0, 0, FGLSBuffer.Width, FGLSBuffer.Height,
      GL_BGRA, GL_UNSIGNED_BYTE, FFMXBuffer.ClassInfo);
  GL.Finish;

  inherited Canvas.DrawBitmap(
    FFMXBuffer, RectF(0, 0, FFMXBuffer.Width, FFMXBuffer.Height),
    RectF(0, 0, FFMXBuffer.Width, FFMXBuffer.Height), AbsoluteOpacity, True);

  if Assigned(FPostRender) then
    FPostRender(Self);
end;

procedure TGLSceneViewport.Paint;
var
  R: TRectF;
begin
  if (csDesigning in ComponentState) then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := TStrokeDash.sdDash;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := TStrokeDash.sdSolid;
  end;

  if FDrawing then Exit;

  if (FGLSBuffer.Width <> FFMXBuffer.Width)
    or (FGLSBuffer.Height <> FFMXBuffer.Height) then
    Realign;

  if FGLSBuffer.RenderingContext = nil then
  begin
    if FParentHandle <> 0 then
    begin
      FGLSBuffer.Resize(0, 0, Trunc(Width), Trunc(Height));
      FOwnDC := GetDC(FParentHandle);
      FGLSBuffer.CreateRC(FOwnDC, True, 1);
      FFMXContext.BeginScene;
      FFMXContext.Clear([TClearTarget.ctColor], TAlphaColor($FF000000), 1.0, 0);
      FFMXContext.EndScene;
      FDrawing := True;
      try
        FGLSBuffer.Render;
      finally
        FDrawing := False;
      end;
    end;
  end
  else
  begin
    FDrawing := True;
    try
      if FFMXContext.BeginScene then
      begin
        FGLSBuffer.Render;
        FFMXContext.EndScene;
      end;
    finally
      FDrawing := False;
    end;
  end;
end;

procedure TGLSceneViewport.SetBeforeRender(const Value: TNotifyEvent);
begin
  FGLSBuffer.BeforeRender := Value;
end;

function TGLSceneViewport.GetBeforeRender: TNotifyEvent;
begin
  Result := FGLSBuffer.BeforeRender;
end;

procedure TGLSceneViewport.SetAfterRender(const Value: TNotifyEvent);
begin
  FGLSBuffer.AfterRender := Value;
end;

function TGLSceneViewport.GetAfterRender: TNotifyEvent;
begin
 Result := FGLSBuffer.AfterRender;
end;

procedure TGLSceneViewport.SetBuffer(const Value: TGLSceneBuffer);
begin
  FGLSBuffer.Assign(Value);
end;

function TGLSceneViewport.GetGLSceneCamera: TGLCamera;
begin
  Result := FGLSBuffer.Camera;
end;

procedure TGLSceneViewport.SetGLSceneCamera(const Value: TGLCamera);
begin
  FGLSBuffer.Camera := Value;
end;

initialization

RegisterFmxClasses([TGLSceneViewport]);

end.
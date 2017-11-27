//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   A shader that renders hidden (back-faced) lines differently from visible
   (front) lines. Polygon offset is used to displace fragments depths a little
   so that there is no z-fighting in rendering the same geometry multiple times. 
                  
    
}
unit VXS.HiddenLineShader;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL, 
  Winapi.OpenGLext, 
  System.Classes,
  
  VXS.Material, 
  VXS.CrossPlatform, 
  VXS.Scene, 
  VXS.Color,
  VXS.BaseClasses, 
  VXS.RenderContextInfo, 
  VXS.State, 
  VXS.Context;

type
  TVXLineSettings = class(TVXUpdateAbleObject)
  private
    
    FColor: TVXColor;
    FWidth: Single;
    FPattern: GLushort;

    FForceMaterial: Boolean;

    procedure SetPattern(const value: GLushort);
    procedure SetColor(const v: TVXColor);
    procedure SetWidth(const Value: Single);
    procedure SetForceMaterial(v: boolean);

  public
    
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TVXRenderContextInfo);
    procedure UnApply(var rci: TVXRenderContextInfo);

  published
    
    property Width: Single read FWidth write SetWidth;
    property Color: TVXColor read FColor write SetColor;
    property Pattern: GLushort read FPattern write SetPattern default $FFFF;
    { Set ForceMaterial to true to enforce the application of the line settings
       for objects that sets their own color, line width and pattern. }
    property ForceMaterial: Boolean read FForceMaterial write SetForceMaterial
      default false;
  end;

  TVXHiddenLineShader = class(TVXShader)
  private
    FPassCount: integer;

    FLineSmooth: Boolean;
    FSolid: Boolean;

    FBackGroundColor: TVXColor;

    FFrontLine: TVXLineSettings;
    FBackLine: TVXLineSettings;

    FLighting: Boolean;
    FShadeModel: TVXShadeModel;

    procedure SetlineSmooth(v: boolean);
    procedure SetSolid(v: boolean);
    procedure SetBackgroundColor(AColor: TVXColor);
    procedure SetLighting(v: boolean);
    procedure SetShadeModel(const val: TVXShadeModel);

  protected
    procedure DoApply(var rci: TVXRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;

  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    
    property FrontLine: TVXLineSettings read FFrontLine write FFrontLine;
    property BackLine: TVXLineSettings read FBackLine write FBackLine;
    { Line smoothing control }
    property LineSmooth: Boolean read FlineSmooth write SetlineSmooth default
      false;
    { Solid controls if you can see through the front-line wireframe. }
    property Solid: Boolean read FSolid write SetSolid default false;
    { Color used for solid fill. }
    property BackgroundColor: TVXColor read FBackgroundColor write
      SetBackgroundColor;
    { When Solid is True, determines if lighting or background color is used. }
    property SurfaceLit: Boolean read FLighting write SetLighting default true;
    { Shade model. 
       Default is "Smooth".  }
    property ShadeModel: TVXShadeModel read FShadeModel write SetShadeModel
      default smDefault;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVXLineSettings ------------------
// ------------------

constructor TVXLineSettings.Create(AOwner: TPersistent);
begin
  inherited;
  FColor := TVXColor.Create(Self);
  FColor.Initialize(clrGray20);
  FWidth := 2;
  Pattern := $FFFF;
  ForceMaterial := false;
end;

destructor TVXLineSettings.Destroy;
begin
  FColor.Free;
  inherited;
end;

procedure TVXLineSettings.SetPattern(const value: GLushort);
begin
  if FPattern <> value then
  begin
    FPattern := Value;
    NotifyChange(self);
  end;
end;

procedure TVXLineSettings.SetColor(const v: TVXColor);
begin
  FColor.Color := v.Color;
  NotifyChange(Self);
end;

// SetWidth
//

procedure TVXLineSettings.SetWidth(const Value: Single);
begin
  FWidth := Value;
  NotifyChange(Self);
end;

var
  IgnoreMatSave: boolean;

procedure TVXLineSettings.Apply(var rci: TVXRenderContextInfo);
begin
  rci.VXStates.LineWidth := Width;
  glColor4fv(Color.AsAddress);
  if Pattern <> $FFFF then
  begin
    rci.VXStates.Enable(stLineStipple);
    rci.VXStates.LineStippleFactor := 1;
    rci.VXStates.LineStipplePattern := Pattern;
  end
  else
    rci.VXStates.Disable(stLineStipple);

  if ForceMaterial then
  begin
    IgnoreMatSave := rci.ignoreMaterials;
    rci.ignoreMaterials := true;
  end;
end;

procedure TVXLineSettings.UnApply(var rci: TVXRenderContextInfo);
begin
  if ForceMaterial then
    rci.ignoreMaterials := IgnoreMatSave;
end;

// SetForceMaterial
//

procedure TVXLineSettings.SetForceMaterial(v: boolean);
begin
  if FForceMaterial <> v then
  begin
    FForceMaterial := v;
    NotifyChange(self);
  end;
end;

// ------------------
// ------------------ TVXHiddenLineShader ------------------
// ------------------

constructor TVXHiddenLineShader.Create(AOwner: TComponent);
begin
  inherited;
  FFrontLine := TVXLineSettings.Create(self);
  FBackLine := TVXLineSettings.Create(self);
  FSolid := false;

  FBackgroundColor := TVXColor.Create(Self);
  FBackgroundColor.Initialize(clrBtnFace);

  FLineSmooth := False;
  FLighting := true;
  FShadeModel := smDefault;
end;

destructor TVXHiddenLineShader.Destroy;
begin
  FFrontLine.Free;
  FBackLine.Free;
  FBackgroundColor.Free;
  inherited;
end;

// DoApply
//

procedure TVXHiddenLineShader.DoApply(var rci: TVXRenderContextInfo; Sender:
  TObject);
begin
  FPassCount := 1;

  if solid then
    with rci.VxStates do
    begin
      // draw filled front faces in first pass
      PolygonMode := pmFill;
      CullFaceMode := cmBack;

      if FLighting then
      begin
        case ShadeModel of
          smDefault, smSmooth: glShadeModel(GL_SMOOTH);
          smFlat: glShadeModel(GL_FLAT);
        end
      end
      else
      begin
        Disable(stLighting);
        glColor4fv(FBackgroundColor.AsAddress); // use background color
      end;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetFill);
    end
  else
    with rci.VxStates do
    begin
      Disable(stLighting);
      // draw back lines in first pass
      FBackLine.Apply(rci);
      CullFaceMode := cmFront;
      PolygonMode := pmLines;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetLine);
    end;

  rci.VXStates.SetPolygonOffset(1, 2);
end;

// DoUnApply
//

function TVXHiddenLineShader.DoUnApply(var rci: TVXRenderContextInfo): Boolean;

  procedure SetLineSmoothBlend;
  begin
    with rci.VxStates do
    begin
      LineStippleFactor := 1;
      LineStipplePattern := $FFFF;
      if LineSmooth then
      begin
        LineSmoothHint := hintNicest;
        Enable(stLineSmooth);
      end
      else
        Disable(stLineSmooth);

      if LineSmooth or (FBackLine.FColor.Alpha < 1)
        or (FFrontLine.FColor.Alpha < 1) then
      begin
        Enable(stBlend);
        SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end
      else
        Disable(stBlend);
    end;
  end;

begin
  case FPassCount of
    1:
      with rci.VxStates do begin
        // draw front line in 2nd pass
        FPassCount := 2;

        FBackLine.UnApply(rci);
        FFrontLine.Apply(rci);

        SetLineSmoothBlend;

        if solid and FLighting then
          Disable(stLighting);

        PolygonMode := pmLines;
        CullFaceMode := cmBack;

        if solid then
          rci.VXStates.Disable(stPolygonOffsetFill)
        else
          rci.VXStates.Disable(stPolygonOffsetLine);

        Result := True;
      end;
    2:
      begin
        FFrontLine.UnApply(rci);
        rci.VXStates.PolygonMode := pmFill;
        Result := false;
      end;
  else
    Assert(False);
    Result := False;
  end;
end;

procedure TVXHiddenLineShader.SetBackgroundColor(AColor: TVXColor);
begin
  FBackgroundColor.Color := AColor.Color;
  NotifyChange(Self);
end;

procedure TVXHiddenLineShader.SetlineSmooth(v: boolean);
begin
  if FlineSmooth <> v then
  begin
    FlineSmooth := v;
    NotifyChange(self);
  end;
end;

procedure TVXHiddenLineShader.SetLighting(v: boolean);
begin
  if FLighting <> v then
  begin
    FLighting := v;
    NotifyChange(self);
  end;
end;

procedure TVXHiddenLineShader.SetSolid(v: boolean);
begin
  if FSolid <> v then
  begin
    FSolid := v;
    NotifyChange(self);
  end;
end;

procedure TVXHiddenLineShader.SetShadeModel(const val: TVXShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;

end.


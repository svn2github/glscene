//
// VKScene project, http://glscene.sourceforge.net 
//
{
   A shader that renders hidden (back-faced) lines differently from visible
   (front) lines. Polygon offset is used to displace fragments depths a little
   so that there is no z-fighting in rendering the same geometry multiple times. 
                  
    
}
unit VKS.HiddenLineShader;

interface

{$I VKScene.inc}

uses
  System.Classes,
  //VKS
  VKS.Material, Winapi.OpenGL, Winapi.OpenGLext,  VKS.CrossPlatform, VKS.Scene, VKS.Color,
  VKS.BaseClasses, VKS.RenderContextInfo, VKS.State, VKS.Context;

type
  TVKLineSettings = class(TVKUpdateAbleObject)
  private
    { Private Declarations }
    FColor: TVKColor;
    FWidth: Single;
    FPattern: GLushort;

    FForceMaterial: Boolean;

    procedure SetPattern(const value: GLushort);
    procedure SetColor(const v: TVKColor);
    procedure SetWidth(const Value: Single);
    procedure SetForceMaterial(v: boolean);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TVKRenderContextInfo);
    procedure UnApply(var rci: TVKRenderContextInfo);

  published
    { Published Declarations }
    property Width: Single read FWidth write SetWidth;
    property Color: TVKColor read FColor write SetColor;
    property Pattern: GLushort read FPattern write SetPattern default $FFFF;
    { Set ForceMaterial to true to enforce the application of the line settings
       for objects that sets their own color, line width and pattern. }
    property ForceMaterial: Boolean read FForceMaterial write SetForceMaterial
      default false;
  end;

  TVKHiddenLineShader = class(TVKShader)
  private
    FPassCount: integer;

    FLineSmooth: Boolean;
    FSolid: Boolean;

    FBackGroundColor: TVKColor;

    FFrontLine: TVKLineSettings;
    FBackLine: TVKLineSettings;

    FLighting: Boolean;
    FShadeModel: TVKShadeModel;

    procedure SetlineSmooth(v: boolean);
    procedure SetSolid(v: boolean);
    procedure SetBackgroundColor(AColor: TVKColor);
    procedure SetLighting(v: boolean);
    procedure SetShadeModel(const val: TVKShadeModel);

  protected
    procedure DoApply(var rci: TVKRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TVKRenderContextInfo): Boolean; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published Declarations }
    property FrontLine: TVKLineSettings read FFrontLine write FFrontLine;
    property BackLine: TVKLineSettings read FBackLine write FBackLine;
    { Line smoothing control }
    property LineSmooth: Boolean read FlineSmooth write SetlineSmooth default
      false;
    { Solid controls if you can see through the front-line wireframe. }
    property Solid: Boolean read FSolid write SetSolid default false;
    { Color used for solid fill. }
    property BackgroundColor: TVKColor read FBackgroundColor write
      SetBackgroundColor;
    { When Solid is True, determines if lighting or background color is used. }
    property SurfaceLit: Boolean read FLighting write SetLighting default true;
    { Shade model. 
       Default is "Smooth".  }
    property ShadeModel: TVKShadeModel read FShadeModel write SetShadeModel
      default smDefault;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKLineSettings ------------------
// ------------------

// Create
//

constructor TVKLineSettings.Create(AOwner: TPersistent);
begin
  inherited;
  FColor := TVKColor.Create(Self);
  FColor.Initialize(clrGray20);
  FWidth := 2;
  Pattern := $FFFF;
  ForceMaterial := false;
end;

// Destroy
//

destructor TVKLineSettings.Destroy;
begin
  FColor.Free;
  inherited;
end;

// SetPattern
//

procedure TVKLineSettings.SetPattern(const value: GLushort);
begin
  if FPattern <> value then
  begin
    FPattern := Value;
    NotifyChange(self);
  end;
end;

// SetColor
//

procedure TVKLineSettings.SetColor(const v: TVKColor);
begin
  FColor.Color := v.Color;
  NotifyChange(Self);
end;

// SetWidth
//

procedure TVKLineSettings.SetWidth(const Value: Single);
begin
  FWidth := Value;
  NotifyChange(Self);
end;

var
  IgnoreMatSave: boolean;

  // Apply
  //

procedure TVKLineSettings.Apply(var rci: TVKRenderContextInfo);
begin
  rci.VKStates.LineWidth := Width;
  glColor4fv(Color.AsAddress);
  if Pattern <> $FFFF then
  begin
    rci.VKStates.Enable(stLineStipple);
    rci.VKStates.LineStippleFactor := 1;
    rci.VKStates.LineStipplePattern := Pattern;
  end
  else
    rci.VKStates.Disable(stLineStipple);

  if ForceMaterial then
  begin
    IgnoreMatSave := rci.ignoreMaterials;
    rci.ignoreMaterials := true;
  end;
end;

// UnApply
//

procedure TVKLineSettings.UnApply(var rci: TVKRenderContextInfo);
begin
  if ForceMaterial then
    rci.ignoreMaterials := IgnoreMatSave;
end;

// SetForceMaterial
//

procedure TVKLineSettings.SetForceMaterial(v: boolean);
begin
  if FForceMaterial <> v then
  begin
    FForceMaterial := v;
    NotifyChange(self);
  end;
end;

// ------------------
// ------------------ TVKHiddenLineShader ------------------
// ------------------

// Create
//

constructor TVKHiddenLineShader.Create(AOwner: TComponent);
begin
  inherited;
  FFrontLine := TVKLineSettings.Create(self);
  FBackLine := TVKLineSettings.Create(self);
  FSolid := false;

  FBackgroundColor := TVKColor.Create(Self);
  FBackgroundColor.Initialize(clrBtnFace);

  FLineSmooth := False;
  FLighting := true;
  FShadeModel := smDefault;
end;

// Destroy
//

destructor TVKHiddenLineShader.Destroy;
begin
  FFrontLine.Free;
  FBackLine.Free;
  FBackgroundColor.Free;
  inherited;
end;

// DoApply
//

procedure TVKHiddenLineShader.DoApply(var rci: TVKRenderContextInfo; Sender:
  TObject);
begin
  FPassCount := 1;

  if solid then
    with rci.VKStates do
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
    with rci.VKStates do
    begin
      Disable(stLighting);
      // draw back lines in first pass
      FBackLine.Apply(rci);
      CullFaceMode := cmFront;
      PolygonMode := pmLines;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetLine);
    end;

  rci.VKStates.SetPolygonOffset(1, 2);
end;

// DoUnApply
//

function TVKHiddenLineShader.DoUnApply(var rci: TVKRenderContextInfo): Boolean;

  procedure SetLineSmoothBlend;
  begin
    with rci.VKStates do
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
      with rci.VKStates do begin
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
          rci.VKStates.Disable(stPolygonOffsetFill)
        else
          rci.VKStates.Disable(stPolygonOffsetLine);

        Result := True;
      end;
    2:
      begin
        FFrontLine.UnApply(rci);
        rci.VKStates.PolygonMode := pmFill;
        Result := false;
      end;
  else
    Assert(False);
    Result := False;
  end;
end;

// SetBackgroundColor
//

procedure TVKHiddenLineShader.SetBackgroundColor(AColor: TVKColor);
begin
  FBackgroundColor.Color := AColor.Color;
  NotifyChange(Self);
end;

// SetlineSmooth
//

procedure TVKHiddenLineShader.SetlineSmooth(v: boolean);
begin
  if FlineSmooth <> v then
  begin
    FlineSmooth := v;
    NotifyChange(self);
  end;
end;

// SetLighting
//

procedure TVKHiddenLineShader.SetLighting(v: boolean);
begin
  if FLighting <> v then
  begin
    FLighting := v;
    NotifyChange(self);
  end;
end;

// SetSolid
//

procedure TVKHiddenLineShader.SetSolid(v: boolean);
begin
  if FSolid <> v then
  begin
    FSolid := v;
    NotifyChange(self);
  end;
end;

// SetShadeModel
//

procedure TVKHiddenLineShader.SetShadeModel(const val: TVKShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;

end.


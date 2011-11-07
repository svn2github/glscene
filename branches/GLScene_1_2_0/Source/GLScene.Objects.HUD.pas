//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLHUDObjects<p>

   GLScene objects that get rendered in 2D coordinates<p>

 <b>History : </b><font size=-1><ul>
      <li>06/06/11 - Yar - Transition to indirect rendering objects
      <li>15/11/10 - FP - Restore DepthTest at the end of RenderTextAtPosition
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
                           Fixed light state changing
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>15/03/08 - DaStr - Bugfixed TGLAbsoluteHUDText.DoRender()
                              (thanks Nicoara Adrian) (BugtrackerID = 1914823)
      <li>18/09/07 - DaStr - Added TGLResolutionIndependantHUDText and
                              TGLAbsoluteHUDText to the list of registered classes
                             Cleaned up "uses" section
      <li>07/09/07 - DaStr - AlphaChannel is now applied to ActualPrimaryMaterial
                             Added TGLResolutionIndependantHUDText,
                                   TGLAbsoluteHUDText
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>23/02/07 - DaStr - Added default values to TGLHUDSprite.Width & Height
      <li>15/02/07 - DaStr - Added default values to TGLHUDText.Alignment & Layout
      <li>28/06/04 - LR - Change TTextLayout to TGLTextLayout for Linux
      <li>27/11/02 - EG - HUDSprite and HUDText now honour renderDPI
      <li>23/11/02 - EG - Added X/YTiles to HUDSprite
      <li>12/05/02 - EG - ModulateColor for HUDText (Nelson Chu)
      <li>20/12/01 - EG - PolygonMode properly adjusted for HUDText
      <li>18/07/01 - EG - VisibilityCulling compatibility changes
      <li>20/06/01 - EG - Default hud sprite size is now 16x16
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
      <li>15/01/01 - EG - Creation
 </ul></font>
}
unit GLScene.Objects.HUD;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene.Core,
  GLScene.Base.Vector.Geometry,
  GLScene.Objects,
  GLScene.BitmapFont,
  GLScene.Platform,
  GLScene.Base.Color,
  GLScene.Base.Context.Info,
  GLScene.Base.Transformation,
  GLScene.Mesh,
  GLScene.DrawTechnique;

type

  // TGLHUDSprite
  //
{: A rectangular area, NOT perspective projected.<p>
     (x, y) coordinates map directly to the viewport (in pixels) and refer
     the center of the area.<br>
     The coordinate system is that of an equivalent TCanvas, ie. top-left
     point is the origin (0, 0).<p>
     The z component is ignored and Z-Buffer is disabled when rendering.<p>
     <b>Using TGLHUDSprite in 2D only scenes :</b><br>
     The most convenient way to use a TGLHUDSprite as a simple 2D sprite with
     blending capabilities (transparency or additive), is to set the texture
     mode to tmModulate, in FrontProperties, to use the Emission color to
     control coloring/intensity, and finally use the Diffuse color's alpha
     to control transparency (while setting the other RGB components to 0).<br>
     You can also control aplha-blending by defining a <1 value in the sprite's
     AlphaChannel field. This provides you with hardware accelerated,
     alpha-blended blitting.<p>
     Note : since TGLHUDSprite works in absolute coordinates, TGLProxyObject
     can't be used to duplicate an hud sprite. }
  TGLHUDSprite = class(TGLCustomSceneObjectEx)
  private
    { Private Declarations }
    FWidth: Single;
    FHeight: Single;
    FRotation: Single;
    FMirrorU: Boolean;
    FMirrorV: Boolean;
    FXTiles, FYTiles: Integer;
    FModulateColor: TGLColor;
    procedure SetWidth(const val: Single);
    procedure SetHeight(const val: Single);
    procedure SetRotation(const val: Single);
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
    procedure SetXTiles(const val: Integer);
    procedure SetYTiles(const val: Integer);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function GetAlphaChannel: Single;
    procedure SetAlphaChannel(const Value: Single);
    procedure SetModulateColor(const Value: TGLColor);
  protected
    { Protected Declarations }
    procedure BuildMesh; override; stdcall;
    procedure OnColorChange(Sender: TObject);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    procedure SetSize(const AWidth, AHeight: Single);
    // : Set width and height to "size"
    procedure SetSquareSize(const ASize: Single);
  published
    { Published Declarations }
    { : Sprite Width in 3D world units. }
    property Width: Single read FWidth write SetWidth stored StoreWidth;
    { : Sprite Height in 3D world units. }
    property Height: Single read FHeight write SetHeight stored StoreHeight;
    { : This the ON-SCREEN rotation of the sprite.<p>
      Rotatation=0 is handled faster. }
    property Rotation: Single read FRotation write SetRotation;
    { : Fake property for backward compatibility. }
    property AlphaChannel: Single read GetAlphaChannel write SetAlphaChannel;
    { : Reverses the texture coordinates in the U and V direction to mirror
      the texture. }
    property MirrorU: Boolean read FMirrorU write SetMirrorU default False;
    property MirrorV: Boolean read FMirrorV write SetMirrorV default False;

    property XTiles: Integer read FXTiles write SetXTiles default 1;
    property YTiles: Integer read FYTiles write SetYTiles default 1;

    property ModulateColor: TGLColor read FModulateColor write SetModulateColor;

    property Material;
    property MaterialLibrary;
    property LibMaterialName;
    property ObjectsSorting;
    property Position;
    property Scale;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
    property Hint;
  end;

  // TGLHUDText
  //
  {: A 2D text displayed and positionned in 2D coordinates.<p>
     The HUDText uses a character font defined and stored by a TGLBitmapFont
     component. The text can be scaled and rotated (2D), the layout and
     alignment can also be controled. }
  TGLHUDText = class(TGLAbstractText)
  private
    { Private Declarations }
    FRotation: Single;
  protected
    { Protected Declarations }
    procedure SetRotation(const val: Single);
    function GetTextPosition(var ARci: TRenderContextInfo): TAffineVector; virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

  published
    { Published Declarations }
    {: Rotation angle in degrees (2d). }
    property Rotation: Single read FRotation write SetRotation;

    property BitmapFont;
    property Text;
    property Alignment;
    property Layout;
    property ModulateColor;
  end;

  {: Position (X, Y and X) is in absolute coordinates. This component converts
     them to screen coordinates and renderes text there. }
  TGLAbsoluteHUDText = class(TGLHUDText)
  protected
    { Protected Declarations }
    function GetTextPosition(var ARci: TRenderContextInfo): TAffineVector; override;
  end;

  {: Position (X and Y) is expected in a [0..1] range (from Screen size)
     This component converts this position to the actual screen position and
     renders the text there. This way a HUD text always appears to be in the
     the same place, regardless of the currect screen resolution.
     Note: this still does not solve the font scaling problem. }
  TGLResolutionIndependantHUDText = class(TGLHUDText)
  protected
    { Protected Declarations }
    function GetTextPosition(var ARci: TRenderContextInfo): TAffineVector; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
{$IFDEF GLS_DELPHI}
  GLScene.Base.Vector.Types,
{$ENDIF}
{$IFDEF GLS_SERVICE_CONTEXT}
  SyncObjs,
{$ENDIF}
  SysUtils,
  GLScene.Base.Context,
  GLScene.Base.GLStateMachine,
  GLScene.Shader.Parameter;

{$IFDEF GLS_REGION}{$REGION 'TGLHUDSprite'}{$ENDIF}

// Create
//

constructor TGLHUDSprite.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  Width := 16;
  Height := 16;
  FRotation := 0;
  FMirrorU := False;
  FMirrorV := False;
  FXTiles := 1;
  FYTiles := 1;
  FModulateColor := TGLColor.CreateInitialized(Self, clrGray80);
  FModulateColor.OnNotifyChange := OnColorChange;
end;

// SetXTiles
//

procedure TGLHUDSprite.SetXTiles(const val: Integer);
begin
  if val <> FXTiles then
  begin
    FXTiles := val;
    StructureChanged;
  end;
end;

// SetYTiles
//

procedure TGLHUDSprite.SetYTiles(const val: Integer);
begin
  if val <> FYTiles then
  begin
    FYTiles := val;
    StructureChanged;
  end;
end;

procedure TGLHUDSprite.Assign(Source: TPersistent);
var
  LSprite: TGLHUDSprite;
begin
  if Source is TGLHUDSprite then
  begin
    LSprite := TGLHUDSprite(Source);
    FWidth := LSprite.FWidth;
    FHeight := LSprite.FHeight;
    FRotation := LSprite.FRotation;
    FModulateColor.Assign(LSprite.FModulateColor);
    FMirrorU := LSprite.FMirrorU;
    FMirrorV := LSprite.FMirrorV;
    FXTiles := LSprite.FXTiles;
    FYTiles := LSprite.FYTiles;
  end;
  inherited Assign(Source);
end;

procedure TGLHUDSprite.BuildMesh;
var
  vx, vy, vx1, vy1: Single;
begin
  // precalc coordinates
  vx := -Width * 0.5;
  vx1 := vx + Width;
  vy := Height * 0.5;
  vy1 := vy - Height;

  with FBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType2f);
      DeclareAttribute(attrColor, GLSLType4f);
      DeclareAttribute(attrTexCoord0, GLSLType2f);
      DeclareAttribute(attrNormal, GLSLType3f);

      BeginAssembly(mpTRIANGLES);

      Attribute3f(attrNormal, ZVector);
      Attribute4f(attrColor, FModulateColor.Color);

      Attribute2f(attrTexCoord0, NullTexPoint);
      Attribute2f(attrPosition, vx, vy1);
      EmitVertex;

      Attribute2f(attrTexCoord0, FXTiles, 0);
      Attribute2f(attrPosition, vx1, vy1);
      EmitVertex;

      Attribute2f(attrTexCoord0, 0, FYTiles);
      Attribute2f(attrPosition, vx, vy);
      EmitVertex;
      EmitVertex;

      Attribute2f(attrTexCoord0, FXTiles, 0);
      Attribute2f(attrPosition, vx1, vy1);
      EmitVertex;

      Attribute2f(attrTexCoord0, FXTiles, FYTiles);
      Attribute2f(attrPosition, vx1, vy);
      EmitVertex;

      EndAssembly;
      WeldVertices;
    finally
      UnLock;
    end;
  end;

  inherited;
end;

destructor TGLHUDSprite.Destroy;
begin
  FModulateColor.Destroy;
  inherited;
end;

procedure TGLHUDSprite.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);

  procedure PrepareSelf;
  var
    f: Single;
    M: TMatrix;
  begin
    if ocStructure in Changes then
    begin
{$IFDEF GLS_SERVICE_CONTEXT}
      if not (osStreamDraw in ObjectStyle) and IsServiceContextAvaible then
      begin
        if not Assigned(FFinishEvent) then
        begin
          FFinishEvent := TFinishTaskEvent.Create;
          AddTaskForServiceContext(BuildMesh, FFinishEvent);
        end
        else if FFinishEvent.WaitFor(0) = wrSignaled then
        begin
          FFinishEvent.ResetEvent;
          AddTaskForServiceContext(BuildMesh, FFinishEvent);
        end;
        exit;
      end
      else
{$ENDIF GLS_SERVICE_CONTEXT}
        BuildMesh;
    end;

    if ARenderSelf then
    begin
      ARci.PipelineTransformation.Push;
      ARci.PipelineTransformation.ModelMatrix := IdentityHmgMatrix;
      ARci.PipelineTransformation.ViewMatrix := IdentityHmgMatrix;
      M := TGLSceneBuffer(ARci.buffer).BaseProjectionMatrix;
      M := MatrixMultiply(
        CreateScaleMatrix(AffineVectorMake(2 / ARci.viewPortSize.cx, 2 / ARci.viewPortSize.cy, 1)),
        M);
      f := ARci.renderDPI / 96;
      with Position do
        M := MatrixMultiply(
          CreateTranslationMatrix(
          AffineVectorMake(X * f - ARci.viewPortSize.cx / 2, ARci.viewPortSize.cy / 2 - Y * f, Z)),
          M);
      if FRotation <> 0 then
        M := MatrixMultiply(
          CreateRotationMatrixZ(DegToRad(FRotation)),
          M);
      M := MatrixMultiply(
        CreateScaleMatrix(AffineVectorMake(Scale.DirectX * f, Scale.DirectY * f, 1)),
        M);
      ARci.PipelineTransformation.ProjectionMatrix := M;
      FTransformation := ARci.PipelineTransformation.StackTop;
      ARci.PipelineTransformation.Pop;

      ARci.drawList.Add(@FBatch);
    end;
  end;

begin
  PrepareSelf;

  if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

function TGLHUDSprite.GetAlphaChannel: Single;
begin
  Result := FModulateColor.Alpha;
end;

procedure TGLHUDSprite.OnColorChange(Sender: TObject);
begin
  StructureChanged;
end;

// SetWidth
//

procedure TGLHUDSprite.SetWidth(const val: Single);
begin
  if FWidth <> val then
  begin
    FWidth := val;
    NotifyChange(Self);
  end;
end;

// SetHeight
//

procedure TGLHUDSprite.SetAlphaChannel(const Value: Single);
begin
  FModulateColor.Alpha := Value;
end;

procedure TGLHUDSprite.SetHeight(const val: Single);
begin
  if FHeight <> val then
  begin
    FHeight := val;
    NotifyChange(Self);
  end;
end;

// SetRotation
//

procedure TGLHUDSprite.SetRotation(const val: Single);
begin
  if FRotation <> val then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

// SetMirrorU
//

procedure TGLHUDSprite.SetMirrorU(const val: Boolean);
begin
  FMirrorU := val;
  NotifyChange(Self);
end;

// SetMirrorV
//

procedure TGLHUDSprite.SetMirrorV(const val: Boolean);
begin
  FMirrorV := val;
  NotifyChange(Self);
end;

procedure TGLHUDSprite.SetModulateColor(const Value: TGLColor);
begin
  FModulateColor := Value;
end;

// SetSize
//

procedure TGLHUDSprite.SetSize(const AWidth, AHeight: Single);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  NotifyChange(Self);
end;

// SetSquareSize
//

procedure TGLHUDSprite.SetSquareSize(const ASize: Single);
begin
  FWidth := ASize;
  FHeight := ASize;
  NotifyChange(Self);
end;

// StoreHeight
//

function TGLHUDSprite.StoreHeight: Boolean;
begin
  Result := Abs(Height - 16) > 0.001;
end;

// StoreWidth
//

function TGLHUDSprite.StoreWidth: Boolean;
begin
  Result := Abs(Height - 16) > 0.001;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLHUDText'}{$ENDIF}

// Create
//

constructor TGLHUDText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDeferredDraw, osNoVisibilityCulling];
  FModulateColor := TGLColor.CreateInitialized(Self, clrWhite);
end;

// Destroy
//

destructor TGLHUDText.Destroy;
begin
  FreeBatches;
  FModulateColor.Free;
  BitmapFont := nil;
  FFinishEvent.Free;
  inherited;
end;

// SetRotation
//

procedure TGLHUDText.SetRotation(const val: Single);
begin
  FRotation := val;
  NotifyChange(Self);
end;

// DoRender
//

procedure TGLHUDText.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);

  procedure PrepareSelf;
  var
    I: Integer;
    f: Single;
    p: TAffineVector;
    M: TMatrix;
  begin
    if ocStructure in Changes then
    begin
{$IFDEF GLS_SERVICE_CONTEXT}
      if not (osStreamDraw in ObjectStyle) and IsServiceContextAvaible then
      begin
        if not Assigned(FFinishEvent) then
        begin
          FFinishEvent := TFinishTaskEvent.Create;
          AddTaskForServiceContext(DoBuild, FFinishEvent);
        end
        else if FFinishEvent.WaitFor(0) = wrSignaled then
        begin
          FFinishEvent.ResetEvent;
          AddTaskForServiceContext(DoBuild, FFinishEvent);
        end;
        exit;
      end
      else
{$ENDIF GLS_SERVICE_CONTEXT}
        DoBuild;
    end;

    if ARenderSelf then
    begin
      ARci.PipelineTransformation.Push;
      ARci.PipelineTransformation.ModelViewMatrix := IdentityHmgMatrix;
      M := TGLSceneBuffer(ARci.buffer).BaseProjectionMatrix;
      M := MatrixMultiply(
        CreateScaleMatrix(AffineVectorMake(2 / ARci.viewPortSize.cx, 2 / ARci.viewPortSize.cy, 1)),
        M);
      f := ARci.renderDPI / 96;
      p := GetTextPosition(ARci);
        M := MatrixMultiply(
          CreateTranslationMatrix(
          AffineVectorMake(p[0] * f - ARci.viewPortSize.cx / 2,
          ARci.viewPortSize.cy / 2 - p[1] * f, p[2])),
          M);
      if FRotation <> 0 then
        M := MatrixMultiply(
          CreateRotationMatrixZ(DegToRad(FRotation)),
          M);
      M := MatrixMultiply(
        CreateScaleMatrix(AffineVectorMake(Scale.DirectX * f, Scale.DirectY * f, 1)),
        M);
      ARci.PipelineTransformation.ProjectionMatrix := M;
      FTransformation := ARci.PipelineTransformation.StackTop;
      ARci.PipelineTransformation.Pop;

      for I := High(FBatches) downto 0 do
      begin
        FBatches[I].Transformation := @FTransformation;
        ARci.drawList.Add(@FBatches[I]);
      end;
    end;
  end;

begin
  PrepareSelf;

  if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

function TGLHUDText.GetTextPosition(var ARci: TRenderContextInfo): TAffineVector;
begin
  Result := Position.AsAffineVector;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLResolutionIndependantHUDText'}{$ENDIF}

// Create
//

constructor TGLResolutionIndependantHUDText.Create(AOwner: TComponent);
begin
  inherited;
  Position.X := 0.5;
  Position.Y := 0.5;
end;

function TGLResolutionIndependantHUDText.GetTextPosition(var ARci: TRenderContextInfo): TAffineVector;
begin
  Result := AffineVectorMake(
    Position.X * ARci.viewPortSize.cx,
    Position.Y * ARci.viewPortSize.cy,
    Position.Z);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLAbsoluteHUDText'}{$ENDIF}

function TGLAbsoluteHUDText.GetTextPosition(var ARci: TRenderContextInfo): TAffineVector;
begin
  Result := TGLSceneBuffer(ARci.buffer).WorldToScreen(Self.AbsoluteAffinePosition);
  Result[1] := ARci.viewPortSize.cy - Result[1];
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

initialization

   // class registrations
  RegisterClasses([TGLHUDText, TGLHUDSprite, TGLResolutionIndependantHUDText,
    TGLAbsoluteHUDText]);

end.


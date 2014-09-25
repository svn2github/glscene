//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLThorFX<p>

  <b>History : </b><font size=-1><ul>
  <li>13/02/07 - aidave - Updated Target.Style to csPoint
  <li>23/12/04 - PhP - GLScenestyled Header
  <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
  <li>06/04/04 - PhP - Removed property Paused use of property Disabled instead
  <li>04/15/03 - Added initialization to CalcThor, to fix an error
  Thanks to Martin Kirsch for this solution
  <li>12/08/01 - EG - Dropped unused Handle allocation (leftover from FirexFX)
  Fixed leaks (colors)
  <li>09/03/01 - René Lindsay - unit created
  </ul></font>
}
unit GLThorFX;

interface

uses Classes, GLScene, GLMisc, XCollection, VectorGeometry, GLTexture,
  GLCadencer;

type
  PThorpoint = ^TThorpoint;

  TThorpoint = record
    Position: TVector; // Position
    Size: Single; // particle size
  end;

  PThorpointArray = ^TThorpointArray;
  TThorpointArray = array [0 .. MAXINT shr 6] of TThorpoint;

  TGLBThorFX = class;

  TCalcPointEvent = procedure(Sender: TObject; PointNo: Integer; var X: Single;
    var Y: Single; var Z: Single) of object;

  // TGLThorFXManager
  //
  { : Thor special effect manager. }
  TGLThorFXManager = class(TGLCadenceAbleComponent)
  private
    { Private Declarations }
    FClients: TList;
    FThorpoints: PThorpointArray;
    FTarget: TGLCoordinates;
    FCadencer: TGLCadencer;
    FMaxpoints: Integer;
    FGlowSize: Single;
    FVibrate: Single;
    FWildness: Single;
    NP: Integer;
    FInnerColor, FOuterColor, FCoreColor: TGLColor;
    FDisabled, FCore, FGlow: Boolean;
    FOnCalcPoint: TCalcPointEvent;
  protected
    { Protected Declarations }
    procedure RegisterClient(AClient: TGLBThorFX);
    procedure DeRegisterClient(AClient: TGLBThorFX);
    procedure DeRegisterAllClients;
    procedure SetTarget(const Val: TGLCoordinates);
    procedure SetCadencer(const Val: TGLCadencer);
    procedure SetMaxpoints(const Val: Integer);
    function StoreGlowSize: Boolean;
    function StoreVibrate: Boolean;
    procedure SetInnerColor(const Val: TGLcolor);
    procedure SetOuterColor(const Val: TGLcolor);
    procedure SetCoreColor(const Val: TGLcolor);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ThorInit;
    procedure CalcThor;
    procedure CalcFrac(Left, Right: Integer; Lh, Rh: Single; Xyz: Integer);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoProgress(const ProgressTime: TProgressTimes); override;
  published
    { Published Declarations }
    property Target: TGLCoordinates read FTarget write SetTarget;
    property Cadencer: TGLCadencer read FCadencer write SetCadencer;
    property Maxpoints: Integer read FMaxpoints write SetMaxpoints default 256;
    property GlowSize: Single read FGlowSize write FGlowSize
      stored StoreGlowSize;
    property Vibrate: Single read FVibrate write FVibrate stored StoreVibrate;
    property InnerColor: TGLcolor read FInnerColor write SetInnerColor;
    property OuterColor: TGLcolor read FOuterColor write SetOuterColor;
    // default clrWhite;
    property CoreColor: TGLcolor read FCoreColor write SetCoreColor;
    // default clrWhite;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Core: Boolean read FCore write FCore;
    property Glow: Boolean read FGlow write FGlow;
    property Wildness: Single read FWildness write FWildness;
    property OnCalcPoint: TCalcPointEvent read FOnCalcPoint write FOnCalcPoint;
  end;

  // TGLBThorFX
  //
  { : Thor special effect }
  TGLBThorFX = class(TGLObjectPostEffect)
  private
    { Private Declarations }
    FManager: TGLThorFXManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
    FTarget: TGLCoordinates;
  protected
    { Protected Declarations }
    procedure SetManager(const Val: TGLThorFXManager);
    procedure WriteToFiler(Writer: TWriter); override;
    procedure ReadFromFiler(Reader: TReader); override;
    procedure Loaded; override;
    procedure SetTarget(const Val: TGLCoordinates);
  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    procedure Render(SceneBuffer: TGLSceneBuffer;
      var Rci: TRenderContextInfo); override;
  published
    { Published Declarations }
    { : Refers the collision manager. }
    property Manager: TGLThorFXManager read FManager write SetManager;
  end;

  { : Returns or creates the TGLBThorFX within the given object's effects.<p> }
function GetOrCreateThorFX(Obj: TGLBaseSceneObject; const Name: String = '')
  : TGLBThorFX;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, VectorLists;


// ------------------
// ------------------ TGLThorFXManager ------------------
// ------------------

// Create
//
constructor TGLThorFXManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  RegisterManager(Self);
  FTarget := TGLCoordinates.CreateInitialized(Self, VectorMake(0, 1, 0));
  FTarget.Style := CsPoint;
  FMaxpoints := 64;
  FGlowSize := 0.2;
  FVibrate := 0;
  FWildness := 1;
  FInnerColor := TGLColor.Create(Self);
  FInnerColor.Initialize(ClrWhite);
  FOuterColor := TGLColor.Create(Self);
  FOuterColor.Initialize(ClrBlue);
  FOuterColor.Alpha := 0;
  FCoreColor := TGLColor.Create(Self);
  FCoreColor.Initialize(ClrWhite);
  FCore := True;
  FGlow := True;
  ThorInit;
end;

// Destroy
//
destructor TGLThorFXManager.Destroy;
begin
  DeRegisterAllClients;
  DeRegisterManager(Self);
  FreeMem(FThorpoints);
  FreeAndNil(FClients);
  FreeAndNil(FInnerColor);
  FreeAndNil(FOuterColor);
  FreeAndNil(FCoreColor);
  FreeAndNil(FTarget);
  inherited Destroy;
end;

// RegisterClient
//
procedure TGLThorFXManager.RegisterClient(AClient: TGLBThorFX);
begin
  if Assigned(AClient) then
    if FClients.IndexOf(AClient) < 0 then
    begin
      FClients.Add(AClient);
      AClient.FManager := Self;
    end;
end;

// DeRegisterClient
//
procedure TGLThorFXManager.DeRegisterClient(AClient: TGLBThorFX);
begin
  if Assigned(AClient) then
  begin
    AClient.FManager := nil;
    FClients.Remove(AClient);
  end;
end;

// DeRegisterAllClients
//
procedure TGLThorFXManager.DeRegisterAllClients;
var
  I: Integer;
begin
  // Fast deregistration
  for I := 0 to FClients.Count - 1 do
    TGLBThorFX(FClients[I]).FManager := nil;
  FClients.Clear;
end;

procedure TGLThorFXManager.SetTarget(const Val: TGLCoordinates);
begin
  FTarget.Assign(Val);
  ThorInit;
end;

// SetCadencer
//
procedure TGLThorFXManager.SetCadencer(const Val: TGLCadencer);
begin
  if FCadencer <> Val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := Val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

// SetMaxpoints
//
procedure TGLThorFXManager.SetMaxpoints(const Val: Integer);
begin
  if FMaxpoints <> Val then
  begin
    FMaxpoints := Val;
    ThorInit;
  end;
end;

// StoreGlowSize
//
function TGLThorFXManager.StoreGlowSize: Boolean;
begin
  Result := (FGlowSize <> 1);
end;

// StoreGlowSize
//
function TGLThorFXManager.StoreVibrate: Boolean;
begin
  Result := (FVibrate <> 1);
end;

// SetInnerColor
//
procedure TGLThorFXManager.SetInnerColor(const Val: TGLcolor);
begin
  if FInnerColor <> Val then
  begin
    FInnerColor.Color := Val.Color;
    ThorInit;
  end;
end;

// SetOuterColor
//
procedure TGLThorFXManager.SetOuterColor(const Val: TGLcolor);
begin
  if FOuterColor <> Val then
  begin
    FOuterColor.Color := Val.Color;
    ThorInit;
  end;
end;

// SetOuterColor
//
procedure TGLThorFXManager.SetCoreColor(const Val: TGLcolor);
begin
  if FCoreColor <> Val then
  begin
    FCoreColor.Color := Val.Color;
    ThorInit;
  end;
end;

// Notification
//
procedure TGLThorFXManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = OpRemove) and (AComponent = FCadencer) then
    Cadencer := nil;
  inherited;
end;

// DoProgress
//
procedure TGLThorFXManager.DoProgress(const ProgressTime: TProgressTimes);
var
  I: Integer;

begin
  if not FDisabled then
    CalcThor;
  // Invalidate all clients
  for I := 0 to FClients.Count - 1 do
    TGLBThorFX(FClients[I]).OwnerBaseSceneObject.NotifyChange
      (TGLBThorFX(FClients[I]));
end;

// ThorInit
//
procedure TGLThorFXManager.ThorInit;
begin
  ReallocMem(FThorpoints, FMaxpoints * Sizeof(TThorpoint));
end;

// CalcThor
//
procedure TGLThorFXManager.CalcThor;
var
  N: Integer;
  Vec, Axs, Nvec: TVector;
  Dist: Single;
  A, B: Single;
  Len: Single;
begin
  // initialise all points with valid data
  for N := 0 to Maxpoints - 1 do
    SetVector(FThorpoints[N].Position, 0, 0, 0);

  // ------------------Calculate fractal (wildness)---------------
  // SetVector(FThorpoints[0].Position,0,0,0);
  SetVector(FThorpoints[Maxpoints - 1].Position, 0, 0, 0);

  CalcFrac(0, Maxpoints - 1, 0, 0, 0);
  CalcFrac(0, Maxpoints - 1, 0, 0, 1);
  // CalcFrac(0,maxpoints-1,0,FTarget.z,2);

  // ---------------Rotate Vector to target-------------
  SetVector(Nvec, FTarget.X, FTarget.Y, FTarget.Z);
  Len := VectorLength(Nvec);
  NormalizeVector(Nvec);
  A := ArcCos(Nvec.Coord[2]);
  B := ArcTan2(Nvec.Coord[0], Nvec.Coord[1]);

  N := 0;
  While (N < Maxpoints) do
  begin
    Dist := N / Maxpoints * Len;
    Vec := FThorpoints[N].Position;
    Vec.Coord[2] := Dist;

    if Assigned(OnCalcPoint) then
      OnCalcPoint(Self, N, Vec.Coord[0], Vec.Coord[1], Vec.Coord[2]);
    // Let user mess around with point position

    SetVector(Axs, 1, 0, 0); // Rotate up
    RotateVector(Vec, Axs, A);
    SetVector(Axs, 0, 0, 1); // Rotate to the sides
    RotateVector(Vec, Axs, B);
    FThorpoints[N].Position := Vec;
    Inc(N);
  end;
  // ----------------------------------------------------
  NP := Maxpoints;
end;

procedure TGLThorFXManager.CalcFrac(Left, Right: Integer; Lh, Rh: Single;
  Xyz: Integer);
var
  Midh: Single;
  Mid: Integer;
  Res: Integer;
  FracScale: Single;
begin
  Mid := (Left + Right) div 2;
  Res := (Left + Right) mod 2;
  FracScale := (Right - Left) / Maxpoints;
  Midh := (Lh + Rh) / 2 + (FracScale * FWildness * Random) -
    (FracScale * FWildness) / 2;
  FThorpoints[Mid].Position.Coord[Xyz] := Midh +
    (FVibrate * Random - (FVibrate / 2));
  // if res=1 then FThorpoints[right-1].Position[xyz]:=
  // (FThorpoints[right].Position[xyz]+midh)/(right-mid)*(right-mid-1);
  if Res = 1 then
    FThorpoints[Right - 1].Position.Coord[Xyz] :=
    FThorpoints[Right].Position.Coord[Xyz];

  if (Mid - Left) > 1 then
    CalcFrac(Left, Mid, Lh, Midh, Xyz);
  if (Right - Mid) > 1 then
    CalcFrac(Mid, Right, Midh, Rh, Xyz);
end;

// ------------------
// ------------------ TGLBThorFX ------------------
// ------------------

// Create
//
constructor TGLBThorFX.Create(AOwner: TXCollection);
begin
  inherited Create(AOwner);
  FTarget := TGLCoordinates.CreateInitialized(Self, VectorMake(0, 1, 0));
  FTarget.Style := CsPoint;
end;

// Destroy
//
destructor TGLBThorFX.Destroy;
begin
  Manager := nil;
  FreeAndNil(FTarget);
  inherited Destroy;
end;

// FriendlyName
//
class function TGLBThorFX.FriendlyName: String;
begin
  Result := 'ThorFX';
end;

// FriendlyDescription
//
class function TGLBThorFX.FriendlyDescription: String;
begin
  Result := 'Thor FX';
end;

// WriteToFiler
//
procedure TGLBThorFX.WriteToFiler(Writer: TWriter);
begin
  with Writer do
  begin
    WriteInteger(0); // ArchiveVersion 0
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

// ReadFromFiler
//
procedure TGLBThorFX.ReadFromFiler(Reader: TReader);
begin
  with Reader do
  begin
    Assert(ReadInteger = 0);
    FManagerName := ReadString;
    Manager := nil;
  end;
end;

// Loaded
//
procedure TGLBThorFX.Loaded;
var
  Mng: TComponent;

begin
  inherited;
  if FManagerName <> '' then
  begin
    Mng := FindManager(TGLThorFXManager, FManagerName);
    if Assigned(Mng) then
      Manager := TGLThorFXManager(Mng);
    FManagerName := '';
  end;
end;

// Assign
//
procedure TGLBThorFX.Assign(Source: TPersistent);
begin
  if Source is TGLBThorFX then
  begin
    Manager := TGLBThorFX(Source).Manager;
  end;
  inherited Assign(Source);
end;

// SetTarget
//
procedure TGLBThorFX.SetTarget(const Val: TGLCoordinates);
begin
  FTarget.Assign(Val);
end;

// SetManager
//
procedure TGLBThorFX.SetManager(const Val: TGLThorFXManager);
begin
  if Val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterClient(Self);
    if Assigned(Val) then
      Val.RegisterClient(Self);
  end;
end;

// Render
//
procedure TGLBThorFX.Render(SceneBuffer: TGLSceneBuffer;
  var Rci: TRenderContextInfo);
var
  N: Integer;
  I: Integer;
  // absPos :TVector;
  InnerColor: TVector;
  LastTr: TAffineVector;
  DistList: TSingleList;
  ObjList: TList;
  Fp: PThorpoint;
  Mat: TMatrix;

  Vx, Vy: TVector;
  M: Integer;
  Icol, Ocol, Ccol: TColorVector;
  Ppos, Ppos2: TAffineVector;
begin
  if Manager = nil then
    Exit;

  GlPushAttrib(GL_ALL_ATTRIB_BITS);
  GlPushMatrix;
  // we get the object position and apply translation...
  // absPos:=OwnerBaseSceneObject.AbsolutePosition;
  // ...should be removed when absolute coords will be handled directly
  // in the point system (and will also make a better flame effect)

  GlDisable(GL_CULL_FACE);
  GlDisable(GL_TEXTURE_2D);
  GlDisable(GL_LIGHTING);
  GlBlendFunc(GL_SRC_ALPHA, GL_ONE);
  GlEnable(GL_BLEND);

  N := Manager.NP;

  if N > 1 then
  begin
    DistList := TSingleList.Create;
    ObjList := TList.Create;
    for I := 0 to N - 1 do
    begin
      Fp := @(Manager.FThorpoints[I]);
      DistList.Add(VectorDotProduct(Rci.CameraDirection, Fp.Position));
      ObjList.Add(Fp);
    end;
    QuickSortLists(0, N - 1, DistList, ObjList);

    GlGetFloatv(GL_MODELVIEW_MATRIX, @Mat);
    for M := 0 to 2 do
    begin
      Vx.Coord[M] := Mat.Coord[M].Coord[0] * Manager.GlowSize;
      Vy.Coord[M] := Mat.Coord[M].Coord[1] * Manager.GlowSize;
    end;

    GlPushMatrix;
    LastTr := NullVector;
    SetVector(InnerColor, Manager.FInnerColor.Color);

    // ---------------
    GlPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or
      GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
    GlBlendFunc(GL_SRC_ALPHA, GL_ONE);
    GlEnable(GL_BLEND);
    GlEnable(GL_LINE_SMOOTH);
    GlDisable(GL_LIGHTING);
    GlDepthFunc(GL_LEQUAL);
    // Stops particles at same distanceform overwriting each-other
    GlLineWidth(3);
    Icol := Manager.FInnerColor.Color;
    Ocol := Manager.FOuterColor.Color;
    Ccol := Manager.FCoreColor.Color;

    // ---Core Line---
    if Manager.FCore then
    begin
      GlDisable(GL_BLEND);
      GlColor4fv(@Ccol);
      GlBegin(GL_LINE_STRIP);
      for I := 0 to N - 1 do
      begin
        Fp := @(Manager.FThorpoints[I]);
        SetVector(Ppos, Fp.Position);
        GlVertex3f(Ppos.Coord[0], Ppos.Coord[1], Ppos.Coord[2]);
      end;
      GlEnd;
    end; // Core;

    // ---Point Glow---
    if Manager.FGlow then
    begin
      GlEnable(GL_BLEND);
      for I := N - 1 downto 0 do
      begin
        Fp := PThorpoint(ObjList[I]);
        SetVector(Ppos, Fp.Position);
        Fp := @(Manager.FThorpoints[I]);
        SetVector(Ppos2, Fp.Position);
        GlBegin(GL_TRIANGLE_FAN);
        GlColor4fv(@Icol);
        GlVertex3f(Ppos.Coord[0], Ppos.Coord[1], Ppos.Coord[2]); // middle1
        GlColor4fv(@Ocol);
        GlVertex3f(Vx.Coord[0] + Vy.Coord[0] + Ppos.Coord[0],
          Vx.Coord[1] + Vy.Coord[1] + Ppos.Coord[1], Vx.Coord[2] + Vy.Coord[2] +
          Ppos.Coord[2]); // TopRight
        GlVertex3f(Vx.Coord[0] * 1.4 + Ppos.Coord[0],
          Vx.Coord[1] * 1.4 + Ppos.Coord[1], Vx.Coord[2] * 1.4 + Ppos.Coord[2]);
        // Right1
        GlVertex3f(Vx.Coord[0] - Vy.Coord[0] + Ppos.Coord[0],
          Vx.Coord[1] - Vy.Coord[1] + Ppos.Coord[1], Vx.Coord[2] - Vy.Coord[2] +
          Ppos.Coord[2]); // BottomRight
        GlVertex3f(-Vy.Coord[0] * 1.4 + Ppos.Coord[0],
          -Vy.Coord[1] * 1.4 + Ppos.Coord[1], -Vy.Coord[2] * 1.4 + Ppos.Coord[2]
          ); // bottom1
        GlVertex3f(-Vx.Coord[0] - Vy.Coord[0] + Ppos.Coord[0],
          -Vx.Coord[1] - Vy.Coord[1] + Ppos.Coord[1], -Vx.Coord[2] - Vy.Coord[2]
          + Ppos.Coord[2]); // BottomLeft
        GlVertex3f(-Vx.Coord[0] * 1.4 + Ppos.Coord[0],
          -Vx.Coord[1] * 1.4 + Ppos.Coord[1], -Vx.Coord[2] * 1.4 + Ppos.Coord
          [2]); // left1
        GlVertex3f(-Vx.Coord[0] + Vy.Coord[0] + Ppos.Coord[0],
          -Vx.Coord[1] + Vy.Coord[1] + Ppos.Coord[1], -Vx.Coord[2] + Vy.Coord[2]
          + Ppos.Coord[2]); // TopLeft
        GlVertex3f(Vy.Coord[0] * 1.4 + Ppos.Coord[0],
          Vy.Coord[1] * 1.4 + Ppos.Coord[1], Vy.Coord[2] * 1.4 + Ppos.Coord[2]);
        // top1
        GlVertex3f(Vx.Coord[0] + Vy.Coord[0] + Ppos.Coord[0],
          Vx.Coord[1] + Vy.Coord[1] + Ppos.Coord[1], Vx.Coord[2] + Vy.Coord[2] +
          Ppos.Coord[2]); // TopRight
        GlEnd;
      end; // Glow
    end;

    GlPopAttrib;
    // (GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
    GlPopMatrix;

    ObjList.Free;
    DistList.Free;
  end;
  GlPopMatrix;
  GlPopAttrib;
end;

// GetOrCreateThorFX
//
function GetOrCreateThorFX(Obj: TGLBaseSceneObject; const Name: String = '')
  : TGLBThorFX;
var
  I: Integer;
begin
  with Obj.Effects do
  begin
    if name = '' then
    begin
      I := IndexOfClass(TGLBThorFX);
      if I >= 0 then
        Result := TGLBThorFX(Items[I])
      else
        Result := TGLBThorFX.Create(Obj.Effects);
    end
    else
    begin
      I := IndexOfName(name);
      if I >= 0 then
        Result := (Items[I] as TGLBThorFX)
      else
      begin
        Result := TGLBThorFX.Create(Obj.Effects);
        Result.Name := name;
      end;
    end;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// class registrations
RegisterXCollectionItemClass(TGLBThorFX);

end.

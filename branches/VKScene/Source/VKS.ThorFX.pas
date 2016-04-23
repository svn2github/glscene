//
// VKScene project, http://glscene.sourceforge.net 
//

unit VKS.ThorFX;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,

  VKS.Scene, VKS.XCollection, VKS.VectorGeometry,
  Winapi.OpenGL, Winapi.OpenGLext,  VKS.Context, VKS.VectorLists, VKS.VectorTypes,
  VKS.Cadencer, VKS.Color, VKS.BaseClasses, VKS.Coordinates,
  VKS.RenderContextInfo, VKS.Manager, VKS.State, VKS.TextureFormat;

type
  PThorpoint = ^TThorpoint;

  TThorpoint = record
    Position: TVector; // Position
    Size: single; // particle size
  end;

  PThorpointArray = ^TThorpointArray;
  TThorpointArray = array [0 .. MAXINT shr 6] of TThorpoint;

  TVKBThorFX = class;

  TCalcPointEvent = procedure(Sender: TObject; PointNo: integer; var x: single;
    var y: single; var z: single) of object;

  // TVKThorFXManager
  //
  { Thor special effect manager. }
  TVKThorFXManager = class(TVKCadenceAbleComponent)
  private
    { Private Declarations }
    FClients: TList;
    FThorpoints: PThorpointArray;
    FTarget: TVKCoordinates;
    FCadencer: TVKCadencer;
    FMaxpoints: integer;
    FGlowSize: single;
    FVibrate: single;
    FWildness: single;
    NP: integer;
    FInnerColor, FOuterColor, FCoreColor: TVKColor;
    FDisabled, FCore, FGlow: boolean;
    FOnCalcPoint: TCalcPointEvent;
  protected
    { Protected Declarations }
    procedure RegisterClient(aClient: TVKBThorFX);
    procedure DeRegisterClient(aClient: TVKBThorFX);
    procedure DeRegisterAllClients;
    procedure SetTarget(const val: TVKCoordinates);
    procedure SetCadencer(const val: TVKCadencer);
    procedure SetMaxpoints(const val: integer);
    function StoreGlowSize: boolean;
    function StoreVibrate: boolean;
    procedure SetInnerColor(const val: TVKColor);
    procedure SetOuterColor(const val: TVKColor);
    procedure SetCoreColor(const val: TVKColor);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ThorInit;
    procedure CalcThor;
    procedure CalcFrac(left, right: integer; lh, rh: single; xyz: integer);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TProgressTimes); override;
  published
    { Published Declarations }
    property Target: TVKCoordinates read FTarget write SetTarget;
    property Cadencer: TVKCadencer read FCadencer write SetCadencer;
    property Maxpoints: integer read FMaxpoints write SetMaxpoints default 256;
    property GlowSize: single read FGlowSize write FGlowSize
      stored StoreGlowSize;
    property Vibrate: single read FVibrate write FVibrate stored StoreVibrate;
    property InnerColor: TVKColor read FInnerColor write SetInnerColor;
    property OuterColor: TVKColor read FOuterColor write SetOuterColor;
    // default clrWhite;
    property CoreColor: TVKColor read FCoreColor write SetCoreColor;
    // default clrWhite;
    property Disabled: boolean read FDisabled write FDisabled;
    property Core: boolean read FCore write FCore;
    property Glow: boolean read FGlow write FGlow;
    property Wildness: single read FWildness write FWildness;
    property OnCalcPoint: TCalcPointEvent read FOnCalcPoint write FOnCalcPoint;
  end;

  // TVKBThorFX
  //
  { Thor special effect }
  TVKBThorFX = class(TVKObjectPostEffect)
  private
    { Private Declarations }
    FManager: TVKThorFXManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
    FTarget: TVKCoordinates;
  protected
    { Protected Declarations }
    procedure SetManager(const val: TVKThorFXManager);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure SetTarget(const val: TVKCoordinates);
  public
    { Public Declarations }
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    procedure Render(var rci: TVKRenderContextInfo); override;
  published
    { Published Declarations }
    { Refers the collision manager. }
    property Manager: TVKThorFXManager read FManager write SetManager;
  end;

  { Returns or creates the TVKBThorFX within the given object's effects.  }
function GetOrCreateThorFX(obj: TVKBaseSceneObject; const name: String = '')
  : TVKBThorFX;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKThorFXManager ------------------
// ------------------

// Create
//
constructor TVKThorFXManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  RegisterManager(Self);
  FTarget := TVKCoordinates.CreateInitialized(Self, VectorMake(0, 1, 0));
  FTarget.Style := csPoint;
  FMaxpoints := 64;
  FGlowSize := 0.2;
  FVibrate := 0;
  FWildness := 1;
  FInnerColor := TVKColor.Create(Self);
  FInnerColor.Initialize(clrWhite);
  FOuterColor := TVKColor.Create(Self);
  FOuterColor.Initialize(clrBlue);
  FOuterColor.Alpha := 0;
  FCoreColor := TVKColor.Create(Self);
  FCoreColor.Initialize(clrWhite);
  FCore := True;
  FGlow := True;
  ThorInit;
end;

// Destroy
//
destructor TVKThorFXManager.Destroy;
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
procedure TVKThorFXManager.RegisterClient(aClient: TVKBThorFX);
begin
  if Assigned(aClient) then
    if FClients.IndexOf(aClient) < 0 then
    begin
      FClients.Add(aClient);
      aClient.FManager := Self;
    end;
end;

// DeRegisterClient
//
procedure TVKThorFXManager.DeRegisterClient(aClient: TVKBThorFX);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FClients.Remove(aClient);
  end;
end;

// DeRegisterAllClients
//
procedure TVKThorFXManager.DeRegisterAllClients;
var
  i: integer;
begin
  // Fast deregistration
  for i := 0 to FClients.Count - 1 do
    TVKBThorFX(FClients[i]).FManager := nil;
  FClients.Clear;
end;

procedure TVKThorFXManager.SetTarget(const val: TVKCoordinates);
begin
  FTarget.Assign(val);
  ThorInit;
end;

// SetCadencer
//
procedure TVKThorFXManager.SetCadencer(const val: TVKCadencer);
begin
  if FCadencer <> val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

// SetMaxpoints
//
procedure TVKThorFXManager.SetMaxpoints(const val: integer);
begin
  if FMaxpoints <> val then
  begin
    FMaxpoints := val;
    ThorInit;
  end;
end;

// StoreGlowSize
//
function TVKThorFXManager.StoreGlowSize: boolean;
begin
  Result := (FGlowSize <> 1);
end;

// StoreGlowSize
//
function TVKThorFXManager.StoreVibrate: boolean;
begin
  Result := (FVibrate <> 1);
end;

// SetInnerColor
//
procedure TVKThorFXManager.SetInnerColor(const val: TVKColor);
begin
  if FInnerColor <> val then
  begin
    FInnerColor.color := val.color;
    ThorInit;
  end;
end;

// SetOuterColor
//
procedure TVKThorFXManager.SetOuterColor(const val: TVKColor);
begin
  if FOuterColor <> val then
  begin
    FOuterColor.color := val.color;
    ThorInit;
  end;
end;

// SetOuterColor
//
procedure TVKThorFXManager.SetCoreColor(const val: TVKColor);
begin
  if FCoreColor <> val then
  begin
    FCoreColor.color := val.color;
    ThorInit;
  end;
end;

// Notification
//
procedure TVKThorFXManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FCadencer) then
    Cadencer := nil;
  inherited;
end;

// DoProgress
//
procedure TVKThorFXManager.DoProgress(const progressTime: TProgressTimes);
var
  i: integer;

begin
  if not FDisabled then
    CalcThor;
  // Invalidate all clients
  for i := 0 to FClients.Count - 1 do
    TVKBThorFX(FClients[i]).OwnerBaseSceneObject.NotifyChange
      (TVKBThorFX(FClients[i]));
end;

// ThorInit
//
procedure TVKThorFXManager.ThorInit;
begin
  ReallocMem(FThorpoints, FMaxpoints * Sizeof(TThorpoint));
end;

// CalcThor
//
procedure TVKThorFXManager.CalcThor;
var
  N: integer;
  vec, axs, nvec: TVector;
  dist: single;
  a, b: single;
  len: single;
begin
  // initialise all points with valid data
  for N := 0 to Maxpoints - 1 do
    SetVector(FThorpoints^[N].Position, 0, 0, 0);

  // ------------------Calculate fractal (wildness)---------------
  // SetVector(FThorpoints[0].Position,0,0,0);
  SetVector(FThorpoints^[Maxpoints - 1].Position, 0, 0, 0);

  CalcFrac(0, Maxpoints - 1, 0, 0, 0);
  CalcFrac(0, Maxpoints - 1, 0, 0, 1);
  // CalcFrac(0,maxpoints-1,0,FTarget.z,2);

  // ---------------Rotate Vector to target-------------
  SetVector(nvec, FTarget.x, FTarget.y, FTarget.z);
  len := VectorLength(nvec);
  NormalizeVector(nvec);
  a := ArcCosine(nvec.Z);
  b := ArcTangent2(nvec.X, nvec.Y);

  N := 0;
  While (N < Maxpoints) do
  begin
    dist := N / Maxpoints * len;
    vec := FThorpoints^[N].Position;
    vec.Z := dist;

    if Assigned(OnCalcPoint) then
      OnCalcPoint(Self, N, vec.X, vec.Y, vec.Z);
    // Let user mess around with point position

    SetVector(axs, 1, 0, 0); // Rotate up
    RotateVector(vec, axs, a);
    SetVector(axs, 0, 0, 1); // Rotate to the sides
    RotateVector(vec, axs, b);
    FThorpoints^[N].Position := vec;
    inc(N);
  end;
  // ----------------------------------------------------
  NP := Maxpoints;
end;

procedure TVKThorFXManager.CalcFrac(left, right: integer; lh, rh: single;
  xyz: integer);
var
  midh: single;
  mid: integer;
  res: integer;
  fracScale: single;
begin
  mid := (left + right) div 2;
  res := (left + right) mod 2;
  fracScale := (right - left) / Maxpoints;
  midh := (lh + rh) / 2 + (fracScale * FWildness * random) -
    (fracScale * FWildness) / 2;
  FThorpoints^[mid].Position.V[xyz] := midh +
    (FVibrate * random - (FVibrate / 2));
  // if res=1 then FThorpoints[right-1].Position[xyz]:=
  // (FThorpoints[right].Position[xyz]+midh)/(right-mid)*(right-mid-1);
  if res = 1 then
    FThorpoints^[right - 1].Position.V[xyz] := FThorpoints^[right].Position.V[xyz];
  if (mid - left) > 1 then
    CalcFrac(left, mid, lh, midh, xyz);
  if (right - mid) > 1 then
    CalcFrac(mid, right, midh, rh, xyz);
end;

// ------------------
// ------------------ TVKBThorFX ------------------
// ------------------

// Create
//
constructor TVKBThorFX.Create(AOwner: TVKXCollection);
begin
  inherited Create(AOwner);
  FTarget := TVKCoordinates.CreateInitialized(Self, VectorMake(0, 1, 0));
  FTarget.Style := csPoint;
end;

// Destroy
//
destructor TVKBThorFX.Destroy;
begin
  Manager := nil;
  FreeAndNil(FTarget);
  inherited Destroy;
end;

// FriendlyName
//
class function TVKBThorFX.FriendlyName: String;
begin
  Result := 'ThorFX';
end;

// FriendlyDescription
//
class function TVKBThorFX.FriendlyDescription: String;
begin
  Result := 'Thor FX';
end;

// WriteToFiler
//
procedure TVKBThorFX.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    // ArchiveVersion 1, added inherited call
    WriteInteger(1);
    inherited;
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

// ReadFromFiler
//
procedure TVKBThorFX.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0 .. 1]);
    if archiveVersion >= 1 then
      inherited;
    FManagerName := ReadString;
    Manager := nil;
  end;
end;

// Loaded
//
procedure TVKBThorFX.Loaded;
var
  mng: TComponent;

begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TVKThorFXManager, FManagerName);
    if Assigned(mng) then
      Manager := TVKThorFXManager(mng);
    FManagerName := '';
  end;
end;

// Assign
//
procedure TVKBThorFX.Assign(Source: TPersistent);
begin
  if Source is TVKBThorFX then
  begin
    Manager := TVKBThorFX(Source).Manager;
  end;
  inherited Assign(Source);
end;

// SetTarget
//
procedure TVKBThorFX.SetTarget(const val: TVKCoordinates);
begin
  FTarget.Assign(val);
end;

// SetManager
//
procedure TVKBThorFX.SetManager(const val: TVKThorFXManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterClient(Self);
    if Assigned(val) then
      val.RegisterClient(Self);
  end;
end;

// Render
//
procedure TVKBThorFX.Render(var rci: TVKRenderContextInfo);
var
  N: integer;
  i: integer;
  // absPos :TVector;
  InnerColor: TVector;
  distList: TSingleList;
  objList: TList;
  fp: PThorpoint;
  mat: TMatrix;

  vx, vy: TVector;
  m: integer;
  Icol, Ocol, Ccol: TColorVector;
  Ppos, Ppos2: TAffineVector;
begin
  if Manager = nil then
    Exit;

  rci.PipelineTransformation.Push;
  // we get the object position and apply translation...
  // absPos:=OwnerBaseSceneObject.AbsolutePosition;
  // ...should be removed when absolute coords will be handled directly
  // in the point system (and will also make a better flame effect)

  rci.GLStates.Disable(stCullFace);
  rci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
  rci.GLStates.Disable(stLighting);
  rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
  rci.GLStates.Enable(stBlend);

  N := Manager.NP;

  if N > 1 then
  begin
    distList := TSingleList.Create;
    objList := TList.Create;
    for i := 0 to N - 1 do
    begin
      fp := @(Manager.FThorpoints[i]);
      distList.Add(VectorDotProduct(rci.cameraDirection, fp^.Position));
      objList.Add(fp);
    end;
    QuickSortLists(0, N - 1, distList, objList);

    mat := rci.PipelineTransformation.ModelViewMatrix;
    for m := 0 to 2 do
    begin
      vx.V[m] := mat.V[m].X * Manager.GlowSize;
      vy.V[m] := mat.V[m].Y * Manager.GlowSize;
    end;

    SetVector(InnerColor, Manager.FInnerColor.color);

    // ---------------
    rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
    rci.GLStates.Enable(stBlend);
    rci.GLStates.Enable(stLineSmooth);
    rci.GLStates.Disable(stLighting);
    // Stops particles at same distanceform overwriting each-other
    rci.GLStates.DepthFunc := cfLEqual;
    rci.GLStates.LineWidth := 3;
    Icol := Manager.FInnerColor.color;
    Ocol := Manager.FOuterColor.color;
    Ccol := Manager.FCoreColor.color;

    // ---Core Line---
    if Manager.FCore then
    begin
      rci.GLStates.Disable(stBlend);
      glColor4fv(@Ccol);
      glBegin(GL_LINE_STRIP);
      for i := 0 to N - 1 do
      begin
        fp := @(Manager.FThorpoints[i]);
        SetVector(Ppos, fp^.Position);
        glVertex3f(Ppos.X, Ppos.Y, Ppos.Z);
      end;
      glEnd;
    end; // Core;

    // ---Point Glow---
    if Manager.FGlow then
    begin
      rci.GLStates.Enable(stBlend);
      for i := N - 1 downto 0 do
      begin
        fp := PThorpoint(objList[i]);
        SetVector(Ppos, fp^.Position);
        fp := @(Manager.FThorpoints[i]);
        SetVector(Ppos2, fp^.Position);
        glBegin(GL_TRIANGLE_FAN);
        glColor4fv(@Icol);
        glVertex3f(Ppos.X, Ppos.Y, Ppos.Z); // middle1
        glColor4fv(@Ocol);
        glVertex3f(Vx.X + Vy.X + Ppos.X,
          Vx.Y + Vy.Y + Ppos.Y, Vx.Z + Vy.Z +
          Ppos.Z); // TopRight
        glVertex3f(Vx.X * 1.4 + Ppos.X,
          Vx.Y * 1.4 + Ppos.Y, Vx.Z * 1.4 + Ppos.Z);
        // Right1
        glVertex3f(Vx.X - Vy.X + Ppos.X,
          Vx.Y - Vy.Y + Ppos.Y, Vx.Z - Vy.Z +
          Ppos.Z); // BottomRight
        glVertex3f(-Vy.X * 1.4 + Ppos.X,
          -Vy.Y * 1.4 + Ppos.Y, -Vy.Z * 1.4 + Ppos.Z
          ); // bottom1
        glVertex3f(-Vx.X - Vy.X + Ppos.X,
          -Vx.Y - Vy.Y + Ppos.Y, -Vx.Z - Vy.Z
          + Ppos.Z); // BottomLeft
        glVertex3f(-Vx.X * 1.4 + Ppos.X,
          -Vx.Y * 1.4 + Ppos.Y, -Vx.Z * 1.4 + Ppos.Z); // left1
        glVertex3f(-Vx.X + Vy.X + Ppos.X,
          -Vx.Y + Vy.Y + Ppos.Y, -Vx.Z + Vy.Z
          + Ppos.Z); // TopLeft
        glVertex3f(Vy.X * 1.4 + Ppos.X,
          Vy.Y * 1.4 + Ppos.Y, Vy.Z * 1.4 + Ppos.Z);
        // top1
        glVertex3f(Vx.X + Vy.X + Ppos.X,
          Vx.Y + Vy.Y + Ppos.Y, Vx.Z + Vy.Z +
          Ppos.Z); // TopRight
        glEnd;
      end; // Glow
    end;

    objList.Free;
    distList.Free;
  end;
  rci.PipelineTransformation.Pop;
end;

// GetOrCreateThorFX
//
function GetOrCreateThorFX(obj: TVKBaseSceneObject; const name: String = '')
  : TVKBThorFX;
var
  i: integer;
begin
  with obj.Effects do
  begin
    if name = '' then
    begin
      i := IndexOfClass(TVKBThorFX);
      if i >= 0 then
        Result := TVKBThorFX(Items[i])
      else
        Result := TVKBThorFX.Create(obj.Effects);
    end
    else
    begin
      i := IndexOfName(name);
      if i >= 0 then
        Result := (Items[i] as TVKBThorFX)
      else
      begin
        Result := TVKBThorFX.Create(obj.Effects);
        Result.name := name;
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
RegisterXCollectionItemClass(TVKBThorFX);

finalization

UnregisterXCollectionItemClass(TVKBThorFX);

end.

//
// This unit is part of the GLScene Project, http://glscene.org
//
{: Base classes for GLScene.<p>

   <b>History : </b><font size=-1><ul>
      <li>14/10/10 - Yar - Added TGLSAbstractManager, TGLAbstractName
      <li>05/10/08 - DanB - Creation, from GLMisc.pas + other places
   </ul></font>
}

unit BaseClasses;

interface

uses
  Classes,
  PersistentClasses,
  GLCrossPlatform;

type

  // TProgressTimes
  //
  TProgressTimes = record
    deltaTime, newTime: Double
  end;

  // TGLProgressEvent
  //
  {: Progression event for time-base animations/simulations.<p>
     deltaTime is the time delta since last progress and newTime is the new
     time after the progress event is completed. }
  TGLProgressEvent = procedure(Sender: TObject; const deltaTime, newTime: Double) of object;

  IGLNotifyAble = interface(IInterface)
    ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
    procedure NotifyChange(Sender: TObject);
  end;

  IGLProgessAble = interface(IInterface)
    ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
    procedure DoProgress(const progressTime: TProgressTimes);
  end;

  // TGLUpdateAbleObject
  //
  {: An abstract class describing the "update" interface.<p> }
  TGLUpdateAbleObject = class(TGLInterfacedPersistent, IGLNotifyAble)
  private
    { Private Declarations }
    FOwner: TPersistent;
    FUpdating: Integer;
    FOnNotifyChange: TNotifyEvent;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); virtual;

    procedure NotifyChange(Sender: TObject); virtual;
    function GetOwner: TPersistent; override;

    property Updating: Integer read FUpdating;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner: TPersistent read FOwner;
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

  // TGLCadenceAbleComponent
  //
  {: A base class describing the "cadenceing" interface.<p> }
  TGLCadenceAbleComponent = class(TGLComponent, IGLProgessAble)
  public
    { Public Declarations }
    procedure DoProgress(const progressTime: TProgressTimes); virtual;
  end;

  // TGLUpdateAbleComponent
  //
  {: A base class describing the "update" interface.<p> }
  TGLUpdateAbleComponent = class(TGLCadenceAbleComponent, IGLNotifyAble)
  public
    { Public Declarations }
    procedure NotifyChange(Sender: TObject); virtual;
  end;

  // TNotifyCollection
  //
  TNotifyCollection = class(TOwnedCollection)
  private
    { Private Declarations }
    FOnNotifyChange: TNotifyEvent;

  protected
    { Protected Declarations }
    procedure Update(item: TCollectionItem); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

  TGLAbstractNameClass = class of TGLAbstractName;

  // TGLSAbstractManager
  //
  {: Base class of GLScene managers. <p>}
  TGLSAbstractManager = class(TPersistent)
  protected
    { Protected Declarations }
    // Design time notifications
    class procedure NotifyProjectOpened; virtual; abstract;
    class procedure NotifyProjectClosed; virtual; abstract;
    class procedure NotifyContextCreated; virtual; abstract;
    class procedure NotifyBeforeCompile; virtual; abstract;
    class function FirstOne: Boolean; virtual;
  public
    { Public Declarations }
    class function FillResourceList(AList: TStringList): Boolean; virtual; abstract;
    class procedure MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass); virtual; abstract;
    // Brackets for thread safe work.
    class procedure BeginWork; virtual;
    class procedure EndWork; virtual;
    class procedure CheckCall; virtual;
  end;

  TGLSAbstractManagerClass = class of TGLSAbstractManager;

  {: GLScene manager items name interface. <p>}
  IGLName = interface(IInterface)
    function GetValue: string;
    procedure SetValue(AName: string);
    function GetHash: Integer;
    function GetManager: TGLSAbstractManagerClass;
    function GetInheritorClass: TGLAbstractNameClass;
    function GetIndex: Integer;
  end;

  // TGLAbstractName
  //
  TGLAbstractName = class(TInterfacedObject, IGLName)
  private
    FValue: string;
    FHashCode: Integer;
    FIndex: Integer;
  protected
    function GetValue: string;
    procedure SetValue(AName: string);
    function GetHash: Integer;
    function GetInheritorClass: TGLAbstractNameClass; virtual; abstract;
    procedure SetIndex(AIndex: Integer);
    function GetIndex: Integer;
  public
    constructor Create; virtual;
{$IFDEF GLS_MULTITHREAD}final;{$ENDIF}
    destructor Destroy; override;
{$IFDEF GLS_MULTITHREAD}final;{$ENDIF}    
    function GetManager: TGLSAbstractManagerClass; virtual; abstract;
    property Value: string read GetValue write SetValue;
    property HashCode: Integer read GetHash;
    property IndexInManagerArray: Integer read GetIndex;
  end;

procedure RegisterGLSceneManager(AManager: TGLSAbstractManagerClass);
procedure NotifyGLSceneManagersContextCreated;
procedure NotifyGLSceneManagersBeforeCompile;
procedure NotifyGLSceneManagersProjectOpened;
procedure NotifyGLSceneManagersProjectClosed;
function UpdateGLSceneManagersResourceList: Boolean;

var
  vManagersResourceList: string;

implementation

uses
  SysUtils,
  GLSLog;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLUpdateAbleObject'}{$ENDIF}
//---------------------- TGLUpdateAbleObject -----------------------------------------

// Create
//

constructor TGLUpdateAbleObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

// NotifyChange
//

procedure TGLUpdateAbleObject.NotifyChange(Sender: TObject);
begin
  if FUpdating = 0 then
  begin
    if Assigned(Owner) then
    begin
      if Owner is TGLUpdateAbleObject then
        TGLUpdateAbleObject(Owner).NotifyChange(Self)
      else if Owner is TGLUpdateAbleComponent then
        TGLUpdateAbleComponent(Owner).NotifyChange(Self);
    end;
    if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
  end;
end;

// GetOwner
//

function TGLUpdateAbleObject.GetOwner: TPersistent;
begin
  Result := Owner;
end;

// BeginUpdate
//

procedure TGLUpdateAbleObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

// EndUpdate
//

procedure TGLUpdateAbleObject.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating <= 0 then
  begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  end;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION 'TGLUpdateAbleObject'}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLCadenceAbleComponent'}{$ENDIF}
// ------------------
// ------------------ TGLCadenceAbleComponent ------------------
// ------------------

// DoProgress
//

procedure TGLCadenceAbleComponent.DoProgress(const progressTime: TProgressTimes);
begin
  // nothing
end;

// ------------------
// ------------------ TGLUpdateAbleObject ------------------
// ------------------

// NotifyChange
//

procedure TGLUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TGLUpdateAbleComponent) then
      (Owner as TGLUpdateAbleComponent).NotifyChange(Self);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION 'TGLUpdateAbleObject'}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TNotifyCollection'}{$ENDIF}
// ------------------
// ------------------ TNotifyCollection ------------------
// ------------------

// Create
//

constructor TNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  if Assigned(AOwner) and (AOwner is TGLUpdateAbleComponent) then
    OnNotifyChange := TGLUpdateAbleComponent(AOwner).NotifyChange;
end;

// Update
//

procedure TNotifyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION 'TNotifyCollection'}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLSAbstractManager'}{$ENDIF}
// ------------------
// ------------------ TGLSAbstractManager ------------------
// ------------------

type
  TGLSManagerState = record
    ManagerClass: TGLSAbstractManagerClass;
    Working: Boolean;
{$IFDEF GLS_MULTITHREAD}
    Lock: TRTLCriticalSection;
{$ENDIF}
  end;

var
  aGLSceneManagers: array of TGLSManagerState;

procedure RegisterGLSceneManager(AManager: TGLSAbstractManagerClass);
var
  I: Integer;
begin
  SetLength(aGLSceneManagers, Length(aGLSceneManagers) + 1);
  if AManager.FirstOne then
  begin
    for I := High(aGLSceneManagers) downto 1 do
      aGLSceneManagers[I] := aGLSceneManagers[I-1];
    I := 0;
  end
  else
    I := High(aGLSceneManagers);
  aGLSceneManagers[I].ManagerClass := AManager;
  aGLSceneManagers[I].Working := False;
{$IFDEF GLS_MULTITHREAD}
  InitializeCriticalSection(aGLSceneManagers[I].Lock);
{$ENDIF}
end;

{$IFDEF GLS_MULTITHREAD}
procedure ReleaseGLSceneManagers;
var
  I: Integer;
begin
  for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
    DeleteCriticalSection(aGLSceneManagers[I].Lock);
end;
{$ENDIF}

class function TGLSAbstractManager.FirstOne: Boolean;
begin
  Result := False;
end;

class procedure TGLSAbstractManager.BeginWork;
var
  I: Integer;
  bPrev: Boolean;
begin
  for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
  begin
    if Self.ClassName = aGLSceneManagers[I].ManagerClass.ClassName then
    begin
{$IFDEF GLS_MULTITHREAD}
      EnterCriticalSection(aGLSceneManagers[I].Lock);
{$ENDIF}
      bPrev := aGLSceneManagers[I].Working;
      aGLSceneManagers[I].Working := True;
      if bPrev then
        GLSLogger.LogError(Format('Excessive call %s.BeginWork', [Self.ClassName]));
      exit;
    end;
  end;
end;

class procedure TGLSAbstractManager.EndWork;
var
  I: Integer;
  bPrev: Boolean;
begin
  for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
  begin
    if Self.ClassName = aGLSceneManagers[I].ManagerClass.ClassName then
    begin
{$IFDEF GLS_MULTITHREAD}
      LeaveCriticalSection(aGLSceneManagers[I].Lock);
{$ENDIF}
      bPrev := aGLSceneManagers[I].Working;
      aGLSceneManagers[I].Working := False;
      if not bPrev then
        GLSLogger.LogError(Format('Excessive call %s.EndWork', [Self.ClassName]));
      exit;
    end;
  end;
end;

class procedure TGLSAbstractManager.CheckCall;
var
  I: Integer;
begin
  for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
  begin
    if Self.ClassName = aGLSceneManagers[I].ManagerClass.ClassName then
      if not aGLSceneManagers[I].Working then
      begin
        GLSLogger.LogError(Format('This method must be call between %0:s.BeginWork and %0:s.EndWork', [Self.ClassName]));
        Abort;
      end
      else
        exit;
  end;
end;

procedure NotifyGLSceneManagersContextCreated;
var
  I: Integer;
begin
  SetExeDirectory;
  for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
    aGLSceneManagers[I].ManagerClass.NotifyContextCreated;
end;

procedure NotifyGLSceneManagersBeforeCompile;
var
  I: Integer;
begin
  SetExeDirectory;
  for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
    aGLSceneManagers[I].ManagerClass.NotifyBeforeCompile;
end;

procedure NotifyGLSceneManagersProjectOpened;
var
  I: Integer;
begin
  SetExeDirectory;
  for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
    aGLSceneManagers[I].ManagerClass.NotifyProjectOpened;
end;

procedure NotifyGLSceneManagersProjectClosed;
var
  I: Integer;
begin
  SetExeDirectory;
  for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
    aGLSceneManagers[I].ManagerClass.NotifyProjectClosed;
end;

function UpdateGLSceneManagersResourceList: Boolean;
var
  I, C: Integer;
  lList: TStringList;
begin
  vManagersResourceList := '';
  if Length(aGLSceneManagers)>0 then
  begin
    lList := TStringList.Create;
    C := 0;
    for I := High(aGLSceneManagers) downto Low(aGLSceneManagers) do
      if aGLSceneManagers[I].ManagerClass.FillResourceList(lList) then
        Inc(C);
    Result := C > 0;
    if Result then
      vManagersResourceList := lList.Text;
    lList.Destroy;
  end
  else
    Result := False;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION 'TGLSAbstractManager'}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLAbstractName'}{$ENDIF}
// ------------------
// ------------------ TGLAbstractName ------------------
// ------------------

constructor TGLAbstractName.Create;
begin
  inherited;
  FIndex := -1;
end;

destructor TGLAbstractName.Destroy;
begin
  FIndex := -1;
  inherited;
end;

function TGLAbstractName.GetValue: string;
begin
  Result := FValue;
end;

procedure TGLAbstractName.SetValue(AName: string);
var
  I, N: Integer;
begin
  GetManager.MakeUniqueItemName(AName, TGLAbstractNameClass(Self.ClassType));
  FValue := AName;
  N := Length(AName);
  FHashCode := N;
  for I := 1 to N do
    FHashCode := (FHashCode shl 1) + Integer(AName[i]);
end;

function TGLAbstractName.GetHash: Integer;
begin
  Result := FHashCode;
end;

procedure TGLAbstractName.SetIndex(AIndex: Integer);
begin
  FIndex := AIndex;
end;

function TGLAbstractName.GetIndex: Integer;
begin
  Result := FIndex;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization

finalization

{$IFDEF GLS_MULTITHREAD}
procedure ReleaseGLSceneManagers;
{$ENDIF}

end.


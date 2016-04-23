//
// VKScene project, http://glscene.sourceforge.net
//

unit VKS.BaseClasses;

interface

uses
  System.Classes, System.SysUtils,
  //VKS
  VKS.Strings, VKS.PersistentClasses, VKS.CrossPlatform;

type

  // TProgressTimes
  //
  TProgressTimes = record
    deltaTime, newTime: Double
  end;

  // TVKProgressEvent
  //
  { Progression event for time-base animations/simulations. 
     deltaTime is the time delta since last progress and newTime is the new
     time after the progress event is completed. }
  TVKProgressEvent = procedure(Sender: TObject; const deltaTime, newTime: Double) of object;

  IGLNotifyAble = interface(IInterface)
    ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
    procedure NotifyChange(Sender: TObject);
  end;

  IGLProgessAble = interface(IInterface)
    ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
    procedure DoProgress(const progressTime: TProgressTimes);
  end;

  // TVKUpdateAbleObject
  //
  { An abstract class describing the "update" interface.  }
  TVKUpdateAbleObject = class(GLinterfacedPersistent, IGLNotifyAble)
  private
    { Private Declarations }
    FOwner: TPersistent;
    FUpdating: Integer;
    FOnNotifyChange: TNotifyEvent;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); virtual;

    procedure NotifyChange(Sender: TObject); virtual;
    procedure Notification(Sender: TObject; Operation: TOperation); virtual;
    function GetOwner: TPersistent; override;

    property Updating: Integer read FUpdating;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner: TPersistent read FOwner;
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

  // TVKCadenceAbleComponent
  //
  { A base class describing the "cadenceing" interface.  }
  TVKCadenceAbleComponent = class(TVKComponent, IGLProgessAble)
  public
    { Public Declarations }
    procedure DoProgress(const progressTime: TProgressTimes); virtual;
  end;

  // TVKUpdateAbleComponent
  //
  { A base class describing the "update" interface.  }
  TVKUpdateAbleComponent = class(TVKCadenceAbleComponent, IGLNotifyAble)
  public
    { Public Declarations }
    procedure NotifyChange(Sender: TObject); virtual;
  end;

  // TVKNotifyCollection
  //
  TVKNotifyCollection = class(TOwnedCollection)
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

implementation

{$IFDEF VKS_REGIONS}{$REGION 'TVKUpdateAbleObject'}{$ENDIF}
//---------------------- TVKUpdateAbleObject -----------------------------------------

// Create
//

constructor TVKUpdateAbleObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

// NotifyChange
//

procedure TVKUpdateAbleObject.NotifyChange(Sender: TObject);
begin
  if FUpdating = 0 then
  begin
    if Assigned(Owner) then
    begin
      if Owner is TVKUpdateAbleObject then
        TVKUpdateAbleObject(Owner).NotifyChange(Self)
      else if Owner is TVKUpdateAbleComponent then
        TVKUpdateAbleComponent(Owner).NotifyChange(Self);
    end;
    if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
  end;
end;

// Notification
//

procedure TVKUpdateAbleObject.Notification(Sender: TObject; Operation: TOperation);
begin
end;

// GetOwner
//

function TVKUpdateAbleObject.GetOwner: TPersistent;
begin
  Result := Owner;
end;

// BeginUpdate
//

procedure TVKUpdateAbleObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

// EndUpdate
//

procedure TVKUpdateAbleObject.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating <= 0 then
  begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  end;
end;
{$IFDEF VKS_REGIONS}{$ENDREGION 'TVKUpdateAbleObject'}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKCadenceAbleComponent'}{$ENDIF}
// ------------------
// ------------------ TVKCadenceAbleComponent ------------------
// ------------------

// DoProgress
//

procedure TVKCadenceAbleComponent.DoProgress(const progressTime: TProgressTimes);
begin
  // nothing
end;

// ------------------
// ------------------ TVKUpdateAbleObject ------------------
// ------------------

// NotifyChange
//

procedure TVKUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TVKUpdateAbleComponent) then
      (Owner as TVKUpdateAbleComponent).NotifyChange(Self);
end;
{$IFDEF VKS_REGIONS}{$ENDREGION 'TVKUpdateAbleObject'}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TVKNotifyCollection'}{$ENDIF}
// ------------------
// ------------------ TVKNotifyCollection ------------------
// ------------------

// Create
//

constructor TVKNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  if Assigned(AOwner) and (AOwner is TVKUpdateAbleComponent) then
    OnNotifyChange := TVKUpdateAbleComponent(AOwner).NotifyChange;
end;

// Update
//

procedure TVKNotifyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;
{$IFDEF VKS_REGIONS}{$ENDREGION 'TVKNotifyCollection'}{$ENDIF}

end.


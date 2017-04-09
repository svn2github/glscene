//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Base classes for VKScene
  The history is logged in a former GLS version of the unit.
}
unit GLX.BaseClasses;

interface

uses
  System.Classes,
  System.SysUtils,
  
  GLX.Strings,
  GLX.PersistentClasses,
  GLX.CrossPlatform;

type

  // TProgressTimes
  //
  TProgressTimes = record
    deltaTime, newTime: Double
  end;

  // TGLProgressEvent
  //
  { Progression event for time-base animations/simulations.
     deltaTime is the time delta since last progress and newTime is the new
     time after the progress event is completed. }
  TGLProgressEvent = procedure(Sender: TObject; const deltaTime, newTime: Double) of object;

  IVKNotifyAble = interface(IInterface)
    ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
    procedure NotifyChange(Sender: TObject);
  end;

  IVKProgessAble = interface(IInterface)
    ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
    procedure DoProgress(const progressTime: TProgressTimes);
  end;

  // TGLUpdateAbleObject
  //
  { An abstract class describing the "update" interface.  }
  TGLUpdateAbleObject = class(TGLInterfacedPersistent, IVKNotifyAble)
  private
    
    FOwner: TPersistent;
    FUpdating: Integer;
    FOnNotifyChange: TNotifyEvent;

  public
    
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

  // TGLCadenceAbleComponent
  //
  { A base class describing the "cadenceing" interface.  }
  TGLCadenceAbleComponent = class(TGLComponent, IVKProgessAble)
  public
    
    procedure DoProgress(const progressTime: TProgressTimes); virtual;
  end;

  // TGLUpdateAbleComponent
  //
  { A base class describing the "update" interface.  }
  TGLUpdateAbleComponent = class(TGLCadenceAbleComponent, IVKNotifyAble)
  public
    
    procedure NotifyChange(Sender: TObject); virtual;
  end;

  // TGLNotifyCollection
  //
  TGLNotifyCollection = class(TOwnedCollection)
  private
    
    FOnNotifyChange: TNotifyEvent;

  protected
    
    procedure Update(item: TCollectionItem); override;

  public
    
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

implementation

{$IFDEF VKS_REGIONS}{$REGION 'TGLUpdateAbleObject'}{$ENDIF}
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

// Notification
//

procedure TGLUpdateAbleObject.Notification(Sender: TObject; Operation: TOperation);
begin
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
{$IFDEF VKS_REGIONS}{$ENDREGION 'TGLUpdateAbleObject'}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TGLCadenceAbleComponent'}{$ENDIF}
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
{$IFDEF VKS_REGIONS}{$ENDREGION 'TGLUpdateAbleObject'}{$ENDIF}

{$IFDEF VKS_REGIONS}{$REGION 'TGLNotifyCollection'}{$ENDIF}
// ------------------
// ------------------ TGLNotifyCollection ------------------
// ------------------

// Create
//

constructor TGLNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  if Assigned(AOwner) and (AOwner is TGLUpdateAbleComponent) then
    OnNotifyChange := TGLUpdateAbleComponent(AOwner).NotifyChange;
end;

// Update
//

procedure TGLNotifyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;
{$IFDEF VKS_REGIONS}{$ENDREGION 'TGLNotifyCollection'}{$ENDIF}

end.


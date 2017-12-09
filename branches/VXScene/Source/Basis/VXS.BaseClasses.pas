//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Base classes for VXScene
  The history is logged in a former version of the unit.
}
unit VXS.BaseClasses;

interface

uses
  System.Classes,
  System.SysUtils,
  
  VXS.Strings,
  VXS.PersistentClasses,
  VXS.CrossPlatform;

type

  TProgressTimes = record
    deltaTime, newTime: Double
  end;

  { Progression event for time-base animations/simulations.
     deltaTime is the time delta since last progress and newTime is the new
     time after the progress event is completed. }
  TVXProgressEvent = procedure(Sender: TObject; const deltaTime, newTime: Double) of object;

  IVKNotifyAble = interface(IInterface)
    ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
    procedure NotifyChange(Sender: TObject);
  end;

  IVKProgessAble = interface(IInterface)
    ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
    procedure DoProgress(const progressTime: TProgressTimes);
  end;

  { An abstract class describing the "update" interface.  }
  TVXUpdateAbleObject = class(TVXInterfacedPersistent, IVKNotifyAble)
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

  { A base class describing the "cadenceing" interface.  }
  TVXCadenceAbleComponent = class(TVXComponent, IVKProgessAble)
  public
    procedure DoProgress(const progressTime: TProgressTimes); virtual;
  end;

  { A base class describing the "update" interface.  }
  TVXUpdateAbleComponent = class(TVXCadenceAbleComponent, IVKNotifyAble)
  public
    procedure NotifyChange(Sender: TObject); virtual;
  end;

  TVXNotifyCollection = class(TOwnedCollection)
  private
    FOnNotifyChange: TNotifyEvent;
  protected
    procedure Update(item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

//==================================================================
implementation
//==================================================================

{$IFDEF VXS_REGIONS}{$REGION 'TVXUpdateAbleObject'}{$ENDIF}
//---------------------- TVXUpdateAbleObject -----------------------------------------

constructor TVXUpdateAbleObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TVXUpdateAbleObject.NotifyChange(Sender: TObject);
begin
  if FUpdating = 0 then
  begin
    if Assigned(Owner) then
    begin
      if Owner is TVXUpdateAbleObject then
        TVXUpdateAbleObject(Owner).NotifyChange(Self)
      else if Owner is TVXUpdateAbleComponent then
        TVXUpdateAbleComponent(Owner).NotifyChange(Self);
    end;
    if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
  end;
end;

procedure TVXUpdateAbleObject.Notification(Sender: TObject; Operation: TOperation);
begin
end;

function TVXUpdateAbleObject.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TVXUpdateAbleObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TVXUpdateAbleObject.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating <= 0 then
  begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  end;
end;
{$IFDEF VXS_REGIONS}{$ENDREGION 'TVXUpdateAbleObject'}{$ENDIF}

{$IFDEF VXS_REGIONS}{$REGION 'TVXCadenceAbleComponent'}{$ENDIF}
// ------------------
// ------------------ TVXCadenceAbleComponent ------------------
// ------------------

procedure TVXCadenceAbleComponent.DoProgress(const progressTime: TProgressTimes);
begin
  // nothing
end;

// ------------------
// ------------------ TVXUpdateAbleObject ------------------
// ------------------

procedure TVXUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TVXUpdateAbleComponent) then
      (Owner as TVXUpdateAbleComponent).NotifyChange(Self);
end;
{$IFDEF VXS_REGIONS}{$ENDREGION 'TVXUpdateAbleObject'}{$ENDIF}

{$IFDEF VXS_REGIONS}{$REGION 'TVXNotifyCollection'}{$ENDIF}
// ------------------
// ------------------ TVXNotifyCollection ------------------
// ------------------

constructor TVXNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  if Assigned(AOwner) and (AOwner is TVXUpdateAbleComponent) then
    OnNotifyChange := TVXUpdateAbleComponent(AOwner).NotifyChange;
end;

procedure TVXNotifyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;
{$IFDEF VXS_REGIONS}{$ENDREGION 'TVXNotifyCollection'}{$ENDIF}

end.



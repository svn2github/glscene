//
// This unit is part of the GLScene Project   
//
// FRTextureEdit
{ : Egg<p>

  Basic editing frame for TVKTexture<p>

  <b>History : </b><font size=-1><ul>
  <li>19/12/14 - PW - Upgraded to support FMX
  <li>05/10/08 - DanB - Removed Kylix support
  <li>24/03/08 - DaStr - Moved TVKMinFilter and TVKMagFilter from VKS.Utils.pas
                  to GLGraphics.pas (BugTracker ID = 1923844)
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>19/12/06 - DaStr - SBEditImageClick() now calls DoOnChange
                 TRTextureEdit.CBImageClassChange - TVKTextureImageClass(tic).Create()
                 now gets the correct variable as its owner (BugTracker ID = 1603743)
                 All comboboxes get their Items using RTTI
                (thanks to dikoe Kenguru for the reminder and Roman Ganz for the code)
  <li>03/07/04 - LR  - Make change for Linux
  <li>17/03/00 - Egg - Added ImageAlpha combo
  <li>13/03/00 - Egg - Creation
  </ul></font>
}
{ TODO : Replace STImageClass with a dropdown (polymorphism) }
unit FRTextureEdit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.TypInfo,
  System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox,
   
  VKS.Graphics, VKS.TextureFormat, VKS.Texture, VKS.TextureImageEditors;

type
  TRTextureEdit = class(TFrame)
    LabelImage: TLabel;
    SBEditImage: TSpeedButton;
    CBMagFilter: TComboBox;
    LabelImageAlpha: TLabel;
    LabelTextureWrap: TLabel;
    CBMinFilter: TComboBox;
    CBTextureMode: TComboBox;
    LabelMagFilter: TLabel;
    LabelMinFilter: TLabel;
    CBTextureWrap: TComboBox;
    CBDisabled: TCheckBox;
    CBImageClass: TComboBox;
    CBImageAlpha: TComboBox;
    LabelFilterQuality: TLabel;
    CBFilteringQuality: TComboBox;
    LabelTextureMode: TLabel;
    procedure CBMagFilterChange(Sender: TObject);
    procedure CBMinFilterChange(Sender: TObject);
    procedure CBTextureModeChange(Sender: TObject);
    procedure CBTextureWrapChange(Sender: TObject);
    procedure CBDisabledClick(Sender: TObject);

    procedure SBEditImageClick(Sender: TObject);
    procedure CBImageClassChange(Sender: TObject);
    procedure CBImageAlphaChange(Sender: TObject);
    procedure CBFilteringQualityChange(Sender: TObject);
  private
    { Private declarations }
    FTexture: TVKTexture;
    FOnChange: TNotifyEvent;
    Changeing: Boolean;
  protected
    { Protected declarations }
    procedure SetTexture(const val: TVKTexture);
    procedure DoOnChange; dynamic;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Texture: TVKTexture read FTexture write SetTexture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.fmx}

// Create
//
constructor TRTextureEdit.Create(AOwner: TComponent);
var
  I : Integer;
begin
  inherited;
  FTexture := TVKTexture.Create(Self);
  SetTexture(FTexture);
  SetGLTextureImageClassesToStrings(CBImageClass.Items);
  for I := 0 to Integer(High(TVKTextureImageAlpha)) do
    CBImageAlpha.Items.Add(GetEnumName(TypeInfo(TVKTextureImageAlpha), I));
  for I := 0 to Integer(High(TVKMagFilter)) do
    CBMagFilter.Items.Add(GetEnumName(TypeInfo(TVKMagFilter), I));
  for I := 0 to Integer(High(TVKMinFilter)) do
    CBMinFilter.Items.Add(GetEnumName(TypeInfo(TVKMinFilter), I));
  for I := 0 to Integer(High(TVKTextureFilteringQuality)) do
    CBFilteringQuality.Items.Add
      (GetEnumName(TypeInfo(TVKTextureFilteringQuality), I));
  for I := 0 to Integer(High(TVKTextureMode)) do
    CBTextureMode.Items.Add(GetEnumName(TypeInfo(TVKTextureMode), I));
  for I := 0 to Integer(High(TVKTextureWrap)) do
    CBTextureWrap.Items.Add(GetEnumName(TypeInfo(TVKTextureWrap), I));
end;

// Destroy
//
destructor TRTextureEdit.Destroy;
begin
  FTexture.Free;
  inherited;
end;

// SetTexture
//
procedure TRTextureEdit.SetTexture(const val: TVKTexture);
begin
  FTexture.Assign(val);
  changeing := True;
  try
    with CBImageClass do
      ItemIndex := Items.IndexOfObject(Pointer(FTexture.Image.ClassType));
    CBImageAlpha.ItemIndex := Integer(FTexture.ImageAlpha);
    CBMagFilter.ItemIndex := Integer(FTexture.MagFilter);
    CBMinFilter.ItemIndex := Integer(FTexture.MinFilter);
    CBFilteringQuality.ItemIndex := Integer(FTexture.FilteringQuality);
    CBTextureMode.ItemIndex := Integer(FTexture.TextureMode);
    CBTextureWrap.ItemIndex := Integer(FTexture.TextureWrap);
    CBDisabled.IsChecked := FTexture.Disabled;
  finally
    Changeing := False;
    DoOnChange;
  end;
end;

// DoOnChange
//
procedure TRTextureEdit.DoOnChange;
begin
  if (not Changeing) and Assigned(FOnChange) then
    OnChange(Self);
end;

// CBImageClassChange
//
procedure TRTextureEdit.CBImageClassChange(Sender: TObject);
var
  tic: TVKTextureImageClass;
  ti: TVKTextureImage;
begin
  if not changeing then
  begin
    with CBImageClass do
      tic := TVKTextureImageClass(Items.Objects[ItemIndex]);
    if FTexture.Image.ClassType <> tic then
    begin
      ti := TVKTextureImageClass(tic).Create(FTexture);
      FTexture.Image := ti;
      ti.Free;
    end;
    DoOnChange;
  end;
end;

// CBImageAlphaChange
//
procedure TRTextureEdit.CBImageAlphaChange(Sender: TObject);
begin
  FTexture.ImageAlpha := TVKTextureImageAlpha(CBImageAlpha.ItemIndex);
  DoOnChange;
end;

// CBMagFilterChange
//
procedure TRTextureEdit.CBMagFilterChange(Sender: TObject);
begin
  FTexture.MagFilter := TVKMagFilter(CBMagFilter.ItemIndex);
  DoOnChange;
end;

// CBMinFilterChange
//
procedure TRTextureEdit.CBMinFilterChange(Sender: TObject);
begin
  FTexture.MinFilter := TVKMinFilter(CBMinFilter.ItemIndex);
  DoOnChange;
end;

// CBTextureModeChange
//
procedure TRTextureEdit.CBTextureModeChange(Sender: TObject);
begin
  FTexture.TextureMode := TVKTextureMode(CBTextureMode.ItemIndex);
  DoOnChange;
end;

// CBTextureWrapChange
//
procedure TRTextureEdit.CBTextureWrapChange(Sender: TObject);
begin
  FTexture.TextureWrap := TVKTextureWrap(CBTextureWrap.ItemIndex);
  DoOnChange;
end;

// CBDisabledClick
//
procedure TRTextureEdit.CBDisabledClick(Sender: TObject);
begin
  FTexture.Disabled := CBDisabled.IsChecked;
  DoOnChange;
end;

// SBEditImageClick
//
procedure TRTextureEdit.SBEditImageClick(Sender: TObject);
begin
  EditGLTextureImage(FTexture.Image);
  DoOnChange;
end;

procedure TRTextureEdit.CBFilteringQualityChange(Sender: TObject);
begin
  FTexture.FilteringQuality := TVKTextureFilteringQuality
    (CBFilteringQuality.ItemIndex);
  DoOnChange;
end;

end.

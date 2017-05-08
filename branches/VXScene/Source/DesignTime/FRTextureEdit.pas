//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Basic editing frame for TVXTexture
}

{ TODO : Replace STImageClass with a dropdown (polymorphism) }

unit FRTextureEdit;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.TypInfo,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Controls.Presentation,
  VXS.Graphics,
  VXS.TextureFormat,
  VXS.Texture,
  VXS.TextureImageEditors;

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
    FTexture: TVXTexture;
    FOnChange: TNotifyEvent;
    Changeing: Boolean;
  protected
    procedure SetTexture(const val: TVXTexture);
    procedure DoOnChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Texture: TVXTexture read FTexture write SetTexture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

//=====================================================================
implementation
//=====================================================================

{$R *.fmx}

constructor TRTextureEdit.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FTexture := TVXTexture.Create(Self);
  SetTexture(FTexture);
  SetTextureImageClassesToStrings(CBImageClass.Items);
  for I := 0 to Integer(High(TVXTextureImageAlpha)) do
    CBImageAlpha.Items.Add(GetEnumName(TypeInfo(TVXTextureImageAlpha), I));
  for I := 0 to Integer(High(TVXMagFilter)) do
    CBMagFilter.Items.Add(GetEnumName(TypeInfo(TVXMagFilter), I));
  for I := 0 to Integer(High(TVXMinFilter)) do
    CBMinFilter.Items.Add(GetEnumName(TypeInfo(TVXMinFilter), I));
  for I := 0 to Integer(High(TVXTextureFilteringQuality)) do
    CBFilteringQuality.Items.Add
      (GetEnumName(TypeInfo(TVXTextureFilteringQuality), I));
  for I := 0 to Integer(High(TVXTextureMode)) do
    CBTextureMode.Items.Add(GetEnumName(TypeInfo(TVXTextureMode), I));
  for I := 0 to Integer(High(TVXTextureWrap)) do
    CBTextureWrap.Items.Add(GetEnumName(TypeInfo(TVXTextureWrap), I));
end;

destructor TRTextureEdit.Destroy;
begin
  FTexture.Free;
  inherited;
end;

procedure TRTextureEdit.SetTexture(const val: TVXTexture);
begin
  FTexture.Assign(val);
  Changeing := True;
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

procedure TRTextureEdit.DoOnChange;
begin
  if (not Changeing) and Assigned(FOnChange) then
    OnChange(Self);
end;

procedure TRTextureEdit.CBImageClassChange(Sender: TObject);
var
  tic: TVXTextureImageClass;
  ti: TVXTextureImage;
begin
  if not Changeing then
  begin
    with CBImageClass do
      tic := TVXTextureImageClass(Items.Objects[ItemIndex]);
    if FTexture.Image.ClassType <> tic then
    begin
      ti := TVXTextureImageClass(tic).Create(FTexture);
      FTexture.Image := ti;
      ti.Free;
    end;
    DoOnChange;
  end;
end;

procedure TRTextureEdit.CBImageAlphaChange(Sender: TObject);
begin
  FTexture.ImageAlpha := TVXTextureImageAlpha(CBImageAlpha.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBMagFilterChange(Sender: TObject);
begin
  FTexture.MagFilter := TVXMagFilter(CBMagFilter.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBMinFilterChange(Sender: TObject);
begin
  FTexture.MinFilter := TVXMinFilter(CBMinFilter.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBTextureModeChange(Sender: TObject);
begin
  FTexture.TextureMode := TVXTextureMode(CBTextureMode.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBTextureWrapChange(Sender: TObject);
begin
  FTexture.TextureWrap := TVXTextureWrap(CBTextureWrap.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBDisabledClick(Sender: TObject);
begin
  FTexture.Disabled := CBDisabled.IsChecked;
  DoOnChange;
end;

procedure TRTextureEdit.SBEditImageClick(Sender: TObject);
begin
  EditTextureImage(FTexture.Image);
  DoOnChange;
end;

procedure TRTextureEdit.CBFilteringQualityChange(Sender: TObject);
begin
  FTexture.FilteringQuality := TVXTextureFilteringQuality
    (CBFilteringQuality.ItemIndex);
  DoOnChange;
end;

end.

//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Basic editing frame for TGLTexture
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
  GLX.Graphics,
  GLX.TextureFormat,
  GLX.Texture,
  GLX.TextureImageEditors;

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
    
    FTexture: TGLTexture;
    FOnChange: TNotifyEvent;
    Changeing: Boolean;
  protected
    
    procedure SetTexture(const val: TGLTexture);
    procedure DoOnChange; dynamic;
  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Texture: TGLTexture read FTexture write SetTexture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.fmx}

// Create
//
constructor TRTextureEdit.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FTexture := TGLTexture.Create(Self);
  SetTexture(FTexture);
  SetTextureImageClassesToStrings(CBImageClass.Items);
  for I := 0 to Integer(High(TGLTextureImageAlpha)) do
    CBImageAlpha.Items.Add(GetEnumName(TypeInfo(TGLTextureImageAlpha), I));
  for I := 0 to Integer(High(TGLMagFilter)) do
    CBMagFilter.Items.Add(GetEnumName(TypeInfo(TGLMagFilter), I));
  for I := 0 to Integer(High(TGLMinFilter)) do
    CBMinFilter.Items.Add(GetEnumName(TypeInfo(TGLMinFilter), I));
  for I := 0 to Integer(High(TGLTextureFilteringQuality)) do
    CBFilteringQuality.Items.Add
      (GetEnumName(TypeInfo(TGLTextureFilteringQuality), I));
  for I := 0 to Integer(High(TGLTextureMode)) do
    CBTextureMode.Items.Add(GetEnumName(TypeInfo(TGLTextureMode), I));
  for I := 0 to Integer(High(TGLTextureWrap)) do
    CBTextureWrap.Items.Add(GetEnumName(TypeInfo(TGLTextureWrap), I));
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
procedure TRTextureEdit.SetTexture(const val: TGLTexture);
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
  tic: TGLTextureImageClass;
  ti: TGLTextureImage;
begin
  if not Changeing then
  begin
    with CBImageClass do
      tic := TGLTextureImageClass(Items.Objects[ItemIndex]);
    if FTexture.Image.ClassType <> tic then
    begin
      ti := TGLTextureImageClass(tic).Create(FTexture);
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
  FTexture.ImageAlpha := TGLTextureImageAlpha(CBImageAlpha.ItemIndex);
  DoOnChange;
end;

// CBMagFilterChange
//
procedure TRTextureEdit.CBMagFilterChange(Sender: TObject);
begin
  FTexture.MagFilter := TGLMagFilter(CBMagFilter.ItemIndex);
  DoOnChange;
end;

// CBMinFilterChange
//
procedure TRTextureEdit.CBMinFilterChange(Sender: TObject);
begin
  FTexture.MinFilter := TGLMinFilter(CBMinFilter.ItemIndex);
  DoOnChange;
end;

// CBTextureModeChange
//
procedure TRTextureEdit.CBTextureModeChange(Sender: TObject);
begin
  FTexture.TextureMode := TGLTextureMode(CBTextureMode.ItemIndex);
  DoOnChange;
end;

// CBTextureWrapChange
//
procedure TRTextureEdit.CBTextureWrapChange(Sender: TObject);
begin
  FTexture.TextureWrap := TGLTextureWrap(CBTextureWrap.ItemIndex);
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
  EditTextureImage(FTexture.Image);
  DoOnChange;
end;

procedure TRTextureEdit.CBFilteringQualityChange(Sender: TObject);
begin
  FTexture.FilteringQuality := TGLTextureFilteringQuality
    (CBFilteringQuality.ItemIndex);
  DoOnChange;
end;

end.

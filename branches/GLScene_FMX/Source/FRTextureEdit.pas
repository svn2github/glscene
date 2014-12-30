//
// This unit is part of the GLScene Project, http://glscene.org
//
// FRTextureEdit
{ : Egg<p>

  Basic editing frame for TGLTexture<p>

  <b>History : </b><font size=-1><ul>
  <li>19/12/14 - PW - Upgraded to support FMX
  <li>05/10/08 - DanB - Removed Kylix support
  <li>24/03/08 - DaStr - Moved TGLMinFilter and TGLMagFilter from GLS.Utils.pas
                  to GLGraphics.pas (BugTracker ID = 1923844)
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>19/12/06 - DaStr - SBEditImageClick() now calls DoOnChange
                 TRTextureEdit.CBImageClassChange - TGLTextureImageClass(tic).Create()
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
   
  GLS.Graphics, GLS.TextureFormat, GLS.Texture, GLS.TextureImageEditors;

type
  TRTextureEdit = class(TFrame)
    LabelImage: TLabel;
    LabelImageAlpha: TLabel;
    LabelMagFilter: TLabel;
    LabelMinFilter: TLabel;
    LabelFilterQuality: TLabel;
    LabelTextureMode: TLabel;
    LabelTextureWrap: TLabel;
    CBImageClass: TComboBox;
    CBImageAlpha: TComboBox;
    CBMagFilter: TComboBox;
    CBMinFilter: TComboBox;
    CBFilteringQuality: TComboBox;
    CBTextureMode: TComboBox;
    CBTextureWrap: TComboBox;
    CBDisabled: TCheckBox;
    SBEditImage: TSpeedButton;
    procedure CBImageClassChange(Sender: TObject);
    procedure CBImageAlphaChange(Sender: TObject);
    procedure CBMagFilterChange(Sender: TObject);
    procedure CBMinFilterChange(Sender: TObject);
    procedure CBFilteringQualityChange(Sender: TObject);
    procedure CBTextureModeChange(Sender: TObject);
    procedure CBTextureWrapChange(Sender: TObject);
    procedure SBEditImageClick(Sender: TObject);
    procedure CBDisabledClick(Sender: TObject);
  private
    { Private declarations }
    FTexture: TGLTexture;
    FOnChange: TNotifyEvent;
    Changeing: Boolean;
  protected
    { Protected declarations }
    procedure SetTexture(const val: TGLTexture);
    procedure DoOnChange; dynamic;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Texture: TGLTexture read FTexture write SetTexture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.fmx}

{ TRTextureEdit }

procedure TRTextureEdit.CBDisabledClick(Sender: TObject);
begin
  FTexture.Disabled := CBDisabled.IsChecked;
  DoOnChange;
end;

procedure TRTextureEdit.CBFilteringQualityChange(Sender: TObject);
begin
  FTexture.FilteringQuality := TGLTextureFilteringQuality
    (CBFilteringQuality.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBImageAlphaChange(Sender: TObject);
begin
  FTexture.ImageAlpha := TGLTextureImageAlpha(CBImageAlpha.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBImageClassChange(Sender: TObject);
var
  tic: TGLTextureImageClass;
  ti: TGLTextureImage;
begin
  if not changeing then
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

procedure TRTextureEdit.CBMagFilterChange(Sender: TObject);
begin
  FTexture.MagFilter := TGLMagFilter(CBMagFilter.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBMinFilterChange(Sender: TObject);
begin
  FTexture.MinFilter := TGLMinFilter(CBMinFilter.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBTextureModeChange(Sender: TObject);
begin
  FTexture.TextureMode := TGLTextureMode(CBTextureMode.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBTextureWrapChange(Sender: TObject);
begin
  FTexture.TextureWrap := TGLTextureWrap(CBTextureWrap.ItemIndex);
  DoOnChange;
end;

constructor TRTextureEdit.Create(AOwner: TComponent);
var
  I : Integer;
begin
  inherited;
  FTexture := TGLTexture.Create(Self);
  SetTexture(FTexture);
  SetGLTextureImageClassesToStrings(CBImageClass.Items);

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

destructor TRTextureEdit.Destroy;
begin
  FTexture.Free;
  inherited;
end;

procedure TRTextureEdit.DoOnChange;
begin
  if (not Changeing) and Assigned(FOnChange) then
    OnChange(Self);
end;

procedure TRTextureEdit.SBEditImageClick(Sender: TObject);
begin
  EditGLTextureImage(FTexture.Image);
  DoOnChange;
end;

procedure TRTextureEdit.SetTexture(const val: TGLTexture);
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

end.

object RTextureEdit: TRTextureEdit
  Left = 0
  Top = 0
  Width = 227
  Height = 162
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object Label2: TLabel
    Left = 0
    Top = 1
    Width = 29
    Height = 13
    Caption = 'Image'
  end
  object SBEditImage: TSpeedButton
    Left = 209
    Top = 0
    Width = 17
    Height = 21
    Hint = 'Edit image'
    Anchors = [akTop, akRight]
    Caption = '...'
    OnClick = SBEditImageClick
  end
  object Label3: TLabel
    Left = 0
    Top = 52
    Width = 43
    Height = 13
    Caption = 'MagFilter'
  end
  object Label4: TLabel
    Left = 0
    Top = 76
    Width = 39
    Height = 13
    Caption = 'MinFilter'
  end
  object Label1: TLabel
    Left = 0
    Top = 100
    Width = 66
    Height = 13
    Caption = 'Texture Mode'
  end
  object Label5: TLabel
    Left = 0
    Top = 124
    Width = 65
    Height = 13
    Caption = 'Texture Wrap'
  end
  object Label6: TLabel
    Left = 0
    Top = 28
    Width = 56
    Height = 13
    Caption = 'ImageAlpha'
  end
  object CBMagFilter: TComboBox
    Left = 72
    Top = 48
    Width = 154
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = CBMagFilterChange
    Items.Strings = (
      'maNearest'
      'maLinear')
  end
  object CBMinFilter: TComboBox
    Left = 72
    Top = 72
    Width = 154
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = CBMinFilterChange
    Items.Strings = (
      'miNearest'
      'miLinear'
      'miNearestMipmapNearest'
      'miLinearMipmapNearest'
      'miNearestMipmapLinear'
      'miLinearMipmapLinear')
  end
  object CBTextureMode: TComboBox
    Left = 72
    Top = 96
    Width = 154
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = CBTextureModeChange
    Items.Strings = (
      'tmDecal'
      'tmModulate'
      'tmBlend'
      'tmReplace')
  end
  object CBTextureWrap: TComboBox
    Left = 72
    Top = 120
    Width = 154
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = CBTextureWrapChange
    Items.Strings = (
      'twBoth'
      'twNone'
      'twVertical'
      'twHorizontal')
  end
  object CBDisabled: TCheckBox
    Left = 0
    Top = 144
    Width = 73
    Height = 17
    Caption = 'Disabled'
    TabOrder = 4
    OnClick = CBDisabledClick
  end
  object CBImageClass: TComboBox
    Left = 72
    Top = 0
    Width = 135
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 5
    OnChange = CBImageClassChange
  end
  object CBImageAlpha: TComboBox
    Left = 72
    Top = 24
    Width = 154
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    OnChange = CBImageAlphaChange
    Items.Strings = (
      'tiaDefault'
      'tiaAlphaFromIntensity'
      'tiaSuperBlackTransparent'
      'tiaLuminance')
  end
end

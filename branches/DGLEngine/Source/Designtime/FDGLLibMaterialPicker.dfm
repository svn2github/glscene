object DGLLibMaterialPicker: TDGLLibMaterialPicker
  Left = 326
  Top = 157
  BorderStyle = bsDialog
  Caption = 'Library Material Picker'
  ClientHeight = 380
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 191
    Top = 8
    Width = 78
    Height = 13
    Caption = 'Material Preview'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 88
    Height = 13
    Caption = 'Available Materials'
  end
  object LBMaterials: TListBox
    Left = 8
    Top = 24
    Width = 177
    Height = 337
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBMaterialsClick
    OnDblClick = LBMaterialsDblClick
    OnKeyPress = LBMaterialsKeyPress
  end
  object BBOk: TBitBtn
    Left = 326
    Top = 336
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object BBCancel: TBitBtn
    Left = 245
    Top = 336
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 191
    Top = 27
    Width = 210
    Height = 206
    TabOrder = 3
    object CBBackground: TComboBox
      Left = 64
      Top = 3
      Width = 142
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'on a pattern background'
        'on a white background'
        'on a black background'
        'on a blue background'
        'on a red background'
        'on a green background')
    end
    object CBObject: TComboBox
      Left = 2
      Top = 3
      Width = 60
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      Items.Strings = (
        'Cube'
        'Sphere'
        'Cone'
        'Teapot')
    end
  end
end

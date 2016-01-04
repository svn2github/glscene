object DGLLibMaterialPicker: TDGLLibMaterialPicker
  Left = 326
  Top = 157
  BorderStyle = bsDialog
  Caption = 'Library Material Picker'
  ClientHeight = 405
  ClientWidth = 230
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
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 88
    Height = 13
    Caption = 'Available Materials'
  end
  object LBMaterials: TListBox
    Left = 8
    Top = 27
    Width = 214
    Height = 337
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBMaterialsClick
    OnDblClick = LBMaterialsDblClick
    OnKeyPress = LBMaterialsKeyPress
  end
  object BBOk: TBitBtn
    Left = 147
    Top = 372
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object BBCancel: TBitBtn
    Left = 66
    Top = 372
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
end

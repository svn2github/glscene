object JPEGForm: TJPEGForm
  Left = 226
  Top = 156
  Width = 282
  Height = 145
  Caption = 'JPEG file parameters'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF888888888888888888888888888FFFF
    7777777777777777777777777778000000000000000000000000000000780FFF
    FFFFFFFFFFFFFFFFFFFFFFFFF0780FFFFFFFFFFFFFFFFFFFF9FFFFFFF0780FFF
    FFFFFFFFFFFFFFFFF19FFFFFF0780FFFFFFFFFFFFFFFFFFFF919FFFFF0780F91
    919191919191919191919FFFF0780F191919191919191919191919FFF0780F91
    91919191919191919191919FF0780F191919191919191919191919FFF0780F91
    919191919191919191919FFFF0780FFFFFFFFFFFFFFFFFFFF919FFFFF0780FFF
    FFF0FFFFFFFFFFFFF19FFFFFF0780FFFFF06FFFFFFFFFFFFF9FFFFFFF0780FFF
    F060FFFFFFFFFFFFFFFFFFFFF0780FFF06060606060606060606060FF0780FF0
    60606060606060606060606FF0780F0606060606060606060606060FF0780FF0
    60606060606060606060606FF0780FFF06060606060606060606060FF0780FFF
    F060FFFFFFFFFFFFFFFFFFFFF0780FFFFF06FFFFFFFFFFFFFFFFFFFFF0780FFF
    FFF0FFFFFFFFFFFFFFFFFFFFF07F0FFFFFFFFFFFFFFFFFFFFFFFFFFFF0FF0FFF
    FFFFFFFFFFFFFFFFFFFFFFFFF0FF000FFFFFFFFFF00000000000000000FFFF0F
    FFFFFFFFF0FFFFFFFFFFFFFFFFFFFF00FFFFFFFF00FFFFFFFFFFFFFFFFFFFFF0
    000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 0
    Width = 212
    Height = 13
    Caption = ' "this software is based in part on the work of'
  end
  object Label2: TLabel
    Left = 32
    Top = 16
    Width = 207
    Height = 16
    Caption = 'the Independent JPEG Group"'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Scale: TComboBox
    Left = 23
    Top = 35
    Width = 36
    Height = 21
    Hint = 'Scale'
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      '1:1'
      '1:2'
      '1:4'
      '1:8')
  end
  object PixelFormat: TComboBox
    Left = 62
    Top = 35
    Width = 49
    Height = 21
    Hint = 'Pixel Format'
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      '24 bit'
      '8 bit')
  end
  object ColorSpace: TComboBox
    Left = 114
    Top = 35
    Width = 62
    Height = 21
    Hint = 'Color space'
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      'RGB'
      'Grayscale')
  end
  object Performance: TComboBox
    Left = 179
    Top = 35
    Width = 75
    Height = 21
    Hint = 'Performance'
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      'Quality'
      'Speed')
  end
  object ProgressiveDisplay: TCheckBox
    Left = 26
    Top = 57
    Width = 103
    Height = 14
    Caption = 'Progressive JPEG'
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object IncrementalDisplay: TCheckBox
    Left = 149
    Top = 60
    Width = 108
    Height = 14
    Caption = 'Incremental Display'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object OKBtn: TBitBtn
    Left = 176
    Top = 80
    Width = 75
    Height = 25
    TabOrder = 6
    OnClick = OKBtnClick
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 24
    Top = 80
    Width = 75
    Height = 25
    TabOrder = 7
    OnClick = CancelBtnClick
    Kind = bkCancel
  end
end

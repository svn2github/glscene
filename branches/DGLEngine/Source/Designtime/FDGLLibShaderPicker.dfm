object DGLLibShaderPicker: TDGLLibShaderPicker
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Library Shader Picker'
  ClientHeight = 440
  ClientWidth = 262
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 85
    Height = 13
    Caption = 'Available Shaders'
  end
  object LBShaders: TListBox
    Left = 9
    Top = 24
    Width = 240
    Height = 377
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = LBShadersDblClick
  end
  object BBCancel: TBitBtn
    Left = 93
    Top = 407
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object BBOk: TBitBtn
    Left = 174
    Top = 407
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
end

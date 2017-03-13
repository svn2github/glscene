object MainFrm: TMainFrm
  Left = 268
  Top = 144
  Width = 464
  Height = 469
  Caption = 'MainFrm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TestLoadMap: TButton
    Left = 192
    Top = 136
    Width = 75
    Height = 25
    Caption = 'TestLoadMap'
    TabOrder = 0
    OnClick = TestLoadMapClick
  end
  object OD: TOpenDialog
    Filter = 'MapFile|*.map'
    Left = 208
    Top = 184
  end
  object D: TIdDecoderMIME
    FillChar = '='
    Left = 528
    Top = 64
  end
end

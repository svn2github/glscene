object MainFrm: TMainFrm
  Left = 318
  Top = 167
  Width = 371
  Height = 402
  Caption = 'MainFrm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MM
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Field: TMemo
    Left = 0
    Top = 0
    Width = 355
    Height = 344
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = FieldChange
  end
  object Encoder: TIdEncoderMIME
    FillChar = '='
    Left = 440
    Top = 32
  end
  object Decoder: TIdDecoderMIME
    FillChar = '='
    Left = 440
    Top = 80
  end
  object OD: TOpenDialog
    Filter = 'Map file|*.map'
    Left = 280
    Top = 40
  end
  object SD: TSaveDialog
    Filter = 'Map file|*.map'
    Left = 280
    Top = 88
  end
  object MM: TMainMenu
    Left = 240
    Top = 40
    object BtnFile: TMenuItem
      Caption = #1060#1072#1081#1083
      object BtnOpen: TMenuItem
        Caption = #1054#1090#1082#1088#1099#1090#1100' '#1092#1072#1081#1083
        OnClick = BtnOpenClick
      end
      object BtnClose: TMenuItem
        Caption = #1047#1072#1082#1088#1099#1090#1100' '#1092#1072#1081#1083
        Enabled = False
      end
      object none1: TMenuItem
        Caption = '-'
      end
      object BtnSave: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
        Enabled = False
        OnClick = BtnSaveClick
      end
      object BtnSaveAs: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1082#1072#1082' ...'
        Enabled = False
        OnClick = BtnSaveAsClick
      end
      object none2: TMenuItem
        Caption = '-'
      end
      object BtnExit: TMenuItem
        Caption = #1042#1099#1093#1086#1076
      end
    end
    object BtnSet: TMenuItem
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      object BtnFont: TMenuItem
        Caption = #1064#1088#1080#1092#1090
        OnClick = BtnFontClick
      end
    end
  end
  object FD: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 240
    Top = 88
  end
end

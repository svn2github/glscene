object VectorEditorForm: TVectorEditorForm
  Left = 411
  Top = 108
  BorderStyle = bsDialog
  Caption = 'XYZ editor'
  ClientHeight = 163
  ClientWidth = 275
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelX: TLabel
    Left = 16
    Top = 72
    Width = 39
    Height = 16
    Caption = 'X axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelY: TLabel
    Left = 16
    Top = 104
    Width = 38
    Height = 16
    Caption = 'Y axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelZ: TLabel
    Left = 16
    Top = 136
    Width = 37
    Height = 16
    Caption = 'Z axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object IMx: TImage
    Left = 160
    Top = 72
    Width = 16
    Height = 16
    Picture.Data = {
      07544269746D617076010000424D760100000000000076000000280000002000
      000010000000010004000000000000010000130B0000130B0000100000000000
      0000000000000000800000800000008080008000000080008000808000007F7F
      7F00BFBFBF000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF003333333333333333333333FFFFF3333333333999993333333333F77777FF
      F33333399999999933333337777FF377FF3333993370739993333377FF373F37
      7FF3399993000339993337777F777F3377F3393999707333993337F777373333
      37FF993399933333399377F3777FF333377F993339903333399377F33737FF33
      377F993333707333399377F333377FF3377F993333101933399377F333777FFF
      377F993333000993399377FF3377737FF7733993330009993933373FF3777377
      F7F339993300039999333773FF777F777733339993707339933333773FF7FFF7
      7333333999999999333333377733377733333333399999333333333337777733
      3333}
    Transparent = True
    Visible = False
  end
  object IMy: TImage
    Left = 160
    Top = 104
    Width = 16
    Height = 16
    Picture.Data = {
      07544269746D617076010000424D760100000000000076000000280000002000
      000010000000010004000000000000010000130B0000130B0000100000000000
      0000000000000000800000800000008080008000000080008000808000007F7F
      7F00BFBFBF000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF003333333333333333333333FFFFF3333333333999993333333333F77777FF
      F33333399999999933333337777FF377FF3333993370739993333377FF373F37
      7FF3399993000339993337777F777F3377F3393999707333993337F777373333
      37FF993399933333399377F3777FF333377F993339903333399377F33737FF33
      377F993333707333399377F333377FF3377F993333101933399377F333777FFF
      377F993333000993399377FF3377737FF7733993330009993933373FF3777377
      F7F339993300039999333773FF777F777733339993707339933333773FF7FFF7
      7333333999999999333333377733377733333333399999333333333337777733
      3333}
    Transparent = True
    Visible = False
  end
  object IMz: TImage
    Left = 160
    Top = 136
    Width = 16
    Height = 16
    Picture.Data = {
      07544269746D617076010000424D760100000000000076000000280000002000
      000010000000010004000000000000010000130B0000130B0000100000000000
      0000000000000000800000800000008080008000000080008000808000007F7F
      7F00BFBFBF000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF003333333333333333333333FFFFF3333333333999993333333333F77777FF
      F33333399999999933333337777FF377FF3333993370739993333377FF373F37
      7FF3399993000339993337777F777F3377F3393999707333993337F777373333
      37FF993399933333399377F3777FF333377F993339903333399377F33737FF33
      377F993333707333399377F333377FF3377F993333101933399377F333777FFF
      377F993333000993399377FF3377737FF7733993330009993933373FF3777377
      F7F339993300039999333773FF777F777733339993707339933333773FF7FFF7
      7333333999999999333333377733377733333333399999333333333337777733
      3333}
    Transparent = True
    Visible = False
  end
  object SBPlusX: TSpeedButton
    Left = 11
    Top = 8
    Width = 33
    Height = 23
    Caption = '+ X'
    Transparent = False
    OnClick = SBPlusXClick
  end
  object SBMinusX: TSpeedButton
    Left = 11
    Top = 33
    Width = 33
    Height = 23
    Caption = '- X'
    Transparent = False
    OnClick = SBMinusXClick
  end
  object SBPlusY: TSpeedButton
    Left = 45
    Top = 8
    Width = 33
    Height = 23
    Caption = '+ Y'
    Transparent = False
    OnClick = SBPlusYClick
  end
  object SBMinusY: TSpeedButton
    Left = 45
    Top = 33
    Width = 33
    Height = 23
    Caption = '- Y'
    Transparent = False
    OnClick = SBMinusYClick
  end
  object SBPlusZ: TSpeedButton
    Left = 84
    Top = 8
    Width = 33
    Height = 23
    Caption = '+ Z'
    Transparent = False
    OnClick = SBPlusZClick
  end
  object SBMinusZ: TSpeedButton
    Left = 84
    Top = 32
    Width = 33
    Height = 23
    Caption = '- Z'
    Transparent = False
    OnClick = SBMinusZClick
  end
  object SBNull: TSpeedButton
    Left = 138
    Top = 8
    Width = 47
    Height = 23
    Caption = '0; 0; 0'
    Transparent = False
    OnClick = SBNullClick
  end
  object SBUnit: TSpeedButton
    Left = 138
    Top = 32
    Width = 47
    Height = 23
    Caption = '1; 1; 1'
    Transparent = False
    OnClick = SBUnitClick
  end
  object SBNormalize: TSpeedButton
    Left = 202
    Top = 8
    Width = 63
    Height = 23
    Caption = 'Normalize'
    Transparent = False
    OnClick = SBNormalizeClick
  end
  object Bevel1: TBevel
    Left = 16
    Top = 62
    Width = 249
    Height = 5
    Shape = bsTopLine
  end
  object SBInvert: TSpeedButton
    Left = 202
    Top = 32
    Width = 63
    Height = 23
    Caption = 'Invert'
    Transparent = False
    OnClick = SBInvertClick
  end
  object EDx: TEdit
    Left = 64
    Top = 71
    Width = 89
    Height = 21
    TabOrder = 0
    Text = '0'
    OnChange = EDxChange
  end
  object EDy: TEdit
    Left = 64
    Top = 103
    Width = 89
    Height = 21
    TabOrder = 1
    Text = '0'
    OnChange = EDyChange
  end
  object EDz: TEdit
    Left = 64
    Top = 135
    Width = 89
    Height = 21
    TabOrder = 2
    Text = '0'
    OnChange = EDzChange
  end
  object BBok: TBitBtn
    Left = 192
    Top = 72
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
  end
  object BBcancel: TBitBtn
    Left = 192
    Top = 104
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 4
  end
end

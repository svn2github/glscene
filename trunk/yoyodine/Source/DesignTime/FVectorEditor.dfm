object VectorEditorForm: TVectorEditorForm
  Left = 411
  Top = 108
  BorderStyle = bsDialog
  Caption = 'XYZ editor'
  ClientHeight = 210
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 89
    Width = 47
    Height = 19
    Caption = 'X axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 10
    Top = 128
    Width = 46
    Height = 19
    Caption = 'Y axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 10
    Top = 167
    Width = 45
    Height = 19
    Caption = 'Z axis'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object IMx: TImage
    Left = 187
    Top = 89
    Width = 20
    Height = 19
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
    Left = 187
    Top = 128
    Width = 20
    Height = 20
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
    Left = 187
    Top = 167
    Width = 20
    Height = 20
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
  object SpeedButton1: TSpeedButton
    Left = 10
    Top = 10
    Width = 40
    Height = 28
    Caption = '+ X'
    Flat = True
    Transparent = False
    OnClick = TBxClick
  end
  object SBmX: TSpeedButton
    Left = 10
    Top = 39
    Width = 40
    Height = 29
    Caption = '- X'
    Flat = True
    Transparent = False
    OnClick = SBmXClick
  end
  object SpeedButton3: TSpeedButton
    Left = 52
    Top = 10
    Width = 40
    Height = 28
    Caption = '+ Y'
    Flat = True
    Transparent = False
    OnClick = TByClick
  end
  object SBmY: TSpeedButton
    Left = 52
    Top = 39
    Width = 40
    Height = 29
    Caption = '- Y'
    Flat = True
    Transparent = False
    OnClick = SBmYClick
  end
  object SpeedButton5: TSpeedButton
    Left = 94
    Top = 10
    Width = 40
    Height = 28
    Caption = '+ Z'
    Flat = True
    Transparent = False
    OnClick = TBzClick
  end
  object SBmZ: TSpeedButton
    Left = 94
    Top = 39
    Width = 40
    Height = 29
    Caption = '- Z'
    Flat = True
    Transparent = False
    OnClick = SBmZClick
  end
  object SpeedButton7: TSpeedButton
    Left = 160
    Top = 10
    Width = 58
    Height = 28
    Caption = '0; 0; 0'
    Flat = True
    Transparent = False
    OnClick = TBnullClick
  end
  object SBUnit: TSpeedButton
    Left = 160
    Top = 39
    Width = 58
    Height = 29
    Caption = '1; 1; 1'
    Flat = True
    Transparent = False
    OnClick = SBUnitClick
  end
  object SpeedButton9: TSpeedButton
    Left = 239
    Top = 10
    Width = 77
    Height = 28
    Caption = 'Normalize'
    Flat = True
    Transparent = False
    OnClick = SpeedButton9Click
  end
  object Bevel1: TBevel
    Left = 10
    Top = 76
    Width = 306
    Height = 6
    Shape = bsTopLine
  end
  object SBInvert: TSpeedButton
    Left = 239
    Top = 39
    Width = 77
    Height = 29
    Caption = 'Invert'
    Flat = True
    Transparent = False
    OnClick = SBInvertClick
  end
  object EDx: TEdit
    Left = 69
    Top = 87
    Width = 109
    Height = 24
    TabOrder = 0
    Text = '0'
    OnChange = EDxChange
  end
  object EDy: TEdit
    Left = 69
    Top = 127
    Width = 109
    Height = 24
    TabOrder = 1
    Text = '0'
    OnChange = EDyChange
  end
  object EDz: TEdit
    Left = 69
    Top = 166
    Width = 109
    Height = 24
    TabOrder = 2
    Text = '0'
    OnChange = EDzChange
  end
  object BBok: TBitBtn
    Left = 226
    Top = 89
    Width = 93
    Height = 30
    TabOrder = 3
    Kind = bkOK
  end
  object BBcancel: TBitBtn
    Left = 226
    Top = 128
    Width = 93
    Height = 31
    TabOrder = 4
    Kind = bkCancel
  end
end

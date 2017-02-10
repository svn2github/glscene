object Form2: TForm2
  Left = 747
  Top = 290
  Caption = 'Form2'
  ClientHeight = 138
  ClientWidth = 346
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
  object SpeedButton1: TSpeedButton
    Left = 96
    Top = 104
    Width = 153
    Height = 30
    Caption = #1057#1086#1079#1076#1072#1090#1100
    Flat = True
    OnClick = SpeedButton1Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 16
    Width = 337
    Height = 81
    Caption = ' '#1053#1086#1074#1099#1081' '#1087#1088#1086#1077#1082#1090' '
    TabOrder = 0
    object Label1: TLabel
      Left = 32
      Top = 40
      Width = 42
      Height = 13
      Caption = #1064#1080#1088#1080#1085#1072':'
    end
    object Label2: TLabel
      Left = 176
      Top = 40
      Width = 41
      Height = 13
      Caption = #1042#1099#1089#1086#1090#1072':'
    end
    object sp_w: TSpinEdit
      Left = 80
      Top = 36
      Width = 65
      Height = 22
      Increment = 50
      MaxLength = 3
      MaxValue = 600
      MinValue = 50
      TabOrder = 0
      Value = 50
    end
    object sp_h: TSpinEdit
      Left = 224
      Top = 36
      Width = 65
      Height = 22
      Increment = 50
      MaxLength = 3
      MaxValue = 600
      MinValue = 50
      TabOrder = 1
      Value = 50
    end
  end
end

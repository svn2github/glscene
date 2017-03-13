object FrmMenu: TFrmMenu
  Left = 296
  Top = 151
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'ODE Car arcade Demo by k00m.'
  ClientHeight = 209
  ClientWidth = 338
  Color = 4210688
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
    Left = 240
    Top = 8
    Width = 60
    Height = 16
    Caption = 'Car Color'
    Font.Charset = ANSI_CHARSET
    Font.Color = clSilver
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object RBlue: TRadioButton
    Left = 248
    Top = 32
    Width = 81
    Height = 17
    Caption = 'Blue'
    Checked = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    TabStop = True
  end
  object RGreen: TRadioButton
    Left = 248
    Top = 56
    Width = 89
    Height = 17
    Caption = 'Green'
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object RRed: TRadioButton
    Left = 248
    Top = 80
    Width = 89
    Height = 17
    Caption = 'Red'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object RYellow: TRadioButton
    Left = 248
    Top = 104
    Width = 97
    Height = 17
    Caption = 'Yellow'
    Font.Charset = ANSI_CHARSET
    Font.Color = clYellow
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  object RSpecial1: TRadioButton
    Left = 248
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Special 1'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
  end
  object RSpecial2: TRadioButton
    Left = 248
    Top = 152
    Width = 105
    Height = 17
    Caption = 'Special 2'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 190
    Width = 338
    Height = 19
    Panels = <>
  end
  object Button1: TButton
    Left = 8
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Start Demo'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Close Demo'
    TabOrder = 8
    OnClick = Button2Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 225
    Height = 121
    Caption = ' Info '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 65
      Height = 16
      Caption = 'Keyboard:'
    end
    object Label3: TLabel
      Left = 8
      Top = 32
      Width = 174
      Height = 16
      Caption = 'Arrow = left,right,front,back'
    end
    object Label4: TLabel
      Left = 8
      Top = 48
      Width = 122
      Height = 16
      Caption = 'B = Break 2 wheels'
    end
    object Label5: TLabel
      Left = 8
      Top = 64
      Width = 153
      Height = 16
      Caption = 'Space = Break 4 wheels'
    end
    object Label6: TLabel
      Left = 8
      Top = 96
      Width = 102
      Height = 16
      Caption = 'Esc = Exit Demo'
    end
    object Label7: TLabel
      Left = 8
      Top = 80
      Width = 93
      Height = 16
      Caption = 'R = Return Car'
    end
  end
end

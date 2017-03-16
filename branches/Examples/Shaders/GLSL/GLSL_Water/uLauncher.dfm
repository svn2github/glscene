object LauncherFrm: TLauncherFrm
  Left = 598
  Top = 387
  BorderStyle = bsDialog
  Caption = 'Launcher'
  ClientHeight = 118
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 100
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Screen resolution'
  end
  object Label2: TLabel
    Left = 30
    Top = 34
    Width = 83
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Refresh rate'
  end
  object Label3: TLabel
    Left = 30
    Top = 56
    Width = 83
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Bits per pixel'
  end
  object ComboBox1: TComboBox
    Left = 116
    Top = 10
    Width = 145
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = '1024 x 768'
    Items.Strings = (
      '1024 x 768'
      '800 x 600'
      '640 x 480')
  end
  object ComboBox2: TComboBox
    Left = 116
    Top = 32
    Width = 145
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = '100 Hrz'
    Items.Strings = (
      '100 Hrz'
      '85 Hrz'
      '75 Hrz'
      '60 Hrz')
  end
  object ComboBox3: TComboBox
    Left = 116
    Top = 54
    Width = 145
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = '32 bpp'
    Items.Strings = (
      '32 bpp'
      '16 bpp')
  end
  object StartBtn: TButton
    Left = 178
    Top = 84
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 3
    OnClick = StartBtnClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 272
    Height = 118
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    ExplicitTop = 8
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 194
    Top = 36
  end
end

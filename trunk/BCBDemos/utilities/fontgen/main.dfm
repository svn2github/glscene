object frmMain: TfrmMain
  Left = 277
  Top = 178
  BorderStyle = bsSingle
  Caption = 'FontGen for GLScene'
  ClientHeight = 447
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 110
  TextHeight = 16
  object GroupBox1: TGroupBox
    Left = 7
    Top = 10
    Width = 447
    Height = 134
    Caption = '1. Select Windows font :'
    TabOrder = 0
    object Panel1: TPanel
      Left = 12
      Top = 25
      Width = 422
      Height = 50
      BevelOuter = bvLowered
      Caption = 'Sample AaBbCcDd'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Button1: TButton
      Left = 175
      Top = 86
      Width = 92
      Height = 31
      Caption = 'Select...'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 7
    Top = 148
    Width = 447
    Height = 208
    Caption = '2. Check glyphs'
    TabOrder = 1
    object Image1: TImage
      Left = 2
      Top = 18
      Width = 443
      Height = 188
      Align = alClient
      Center = True
    end
  end
  object GroupBox3: TGroupBox
    Left = 7
    Top = 357
    Width = 447
    Height = 80
    Caption = '3. Save font:'
    TabOrder = 2
    object Button2: TButton
      Left = 185
      Top = 30
      Width = 92
      Height = 30
      Caption = 'Save...'
      TabOrder = 0
      OnClick = Button2Click
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 264
    Top = 82
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'gsf'
    FileName = 'font'
    Filter = 'GlScene Font|*.gsf|All Files|*.*'
    Title = 'Save font'
    Left = 290
    Top = 310
  end
end

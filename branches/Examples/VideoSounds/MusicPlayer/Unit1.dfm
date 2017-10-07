object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'Music Player'
  ClientHeight = 397
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 44
    Top = 301
    Width = 64
    Height = 13
    Caption = 'Track Length'
  end
  object Label4: TLabel
    Left = 164
    Top = 301
    Width = 98
    Height = 13
    Caption = 'Sound Reproduction'
  end
  object Label1: TLabel
    Left = 63
    Top = 320
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 226
    Top = 320
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label5: TLabel
    Left = 8
    Top = 108
    Width = 184
    Height = 13
    Caption = 'Use FromStream for quick reproduction'
  end
  object Label6: TLabel
    Left = 380
    Top = 339
    Width = 193
    Height = 13
    Alignment = taRightJustify
    Caption = 'AutoPlay-False- press Play after selection'
    Layout = tlCenter
  end
  object Button1: TButton
    Left = 40
    Top = 339
    Width = 75
    Height = 25
    Caption = 'Play'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 129
    Top = 339
    Width = 75
    Height = 25
    Caption = 'Pause'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 226
    Top = 339
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = Button3Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 17
    Width = 281
    Height = 73
    Caption = 'Sound Manager'
    TabOrder = 3
    object RadioButton1: TRadioButton
      Left = 16
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Bass'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 16
      Top = 39
      Width = 113
      Height = 17
      Caption = 'Fmod'
      TabOrder = 1
      OnClick = RadioButton2Click
    end
  end
  object ListBox1: TListBox
    Left = 312
    Top = 48
    Width = 233
    Height = 233
    ItemHeight = 13
    TabOrder = 4
    OnDblClick = ListBox1DblClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 135
    Width = 281
    Height = 66
    Caption = 'Playing Mode'
    TabOrder = 5
    object RadioButton3: TRadioButton
      Left = 16
      Top = 16
      Width = 113
      Height = 17
      Caption = 'FromLib'
      TabOrder = 0
      OnClick = RadioButton3Click
    end
    object RadioButton4: TRadioButton
      Left = 16
      Top = 39
      Width = 113
      Height = 17
      Caption = 'FromStream'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = RadioButton4Click
    end
  end
  object Button4: TButton
    Left = 329
    Top = 17
    Width = 41
    Height = 25
    Caption = 'up'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 407
    Top = 17
    Width = 40
    Height = 25
    Caption = 'down'
    TabOrder = 7
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 477
    Top = 17
    Width = 44
    Height = 25
    Caption = 'delete'
    TabOrder = 8
    OnClick = Button6Click
  end
  object ScrollBar1: TScrollBar
    Left = 0
    Top = 380
    Width = 611
    Height = 17
    Align = alBottom
    PageSize = 0
    TabOrder = 9
    OnScroll = ScrollBar1Scroll
    ExplicitTop = 353
    ExplicitWidth = 572
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 207
    Width = 281
    Height = 74
    Caption = 'Properties'
    TabOrder = 10
    object CheckBox2: TCheckBox
      Left = 16
      Top = 40
      Width = 119
      Height = 17
      Caption = 'Repeat Playing List'
      TabOrder = 0
      OnClick = CheckBox2Click
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 17
      Width = 97
      Height = 17
      Caption = 'AutoPlay'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 408
    Top = 120
  end
  object GLSMBASS1: TGLSMBASS
    Active = True
    MaxChannels = 32
    MasterVolume = 1.000000000000000000
    Sources = <>
    Cadencer = GLCadencer1
    Left = 408
    Top = 56
  end
  object SoundLib: TGLSoundLibrary
    Samples = <>
    Left = 480
    Top = 56
  end
  object GLSMFMOD1: TGLSMFMOD
    MasterVolume = 1.000000000000000000
    Sources = <>
    Cadencer = GLCadencer1
    Left = 336
    Top = 56
  end
  object GLScene1: TGLScene
    Left = 336
    Top = 120
  end
  object MainMenu1: TMainMenu
    Left = 336
    Top = 176
    object N1: TMenuItem
      Caption = 'File'
      object N2: TMenuItem
        Caption = 'Add in List...'
        OnClick = N2Click
      end
      object N3: TMenuItem
        Caption = 'Clean List'
        OnClick = N3Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'All Music|*.mp3;*.wav;|*.wav|*.wav|*.mp3|*.mp3'
    Left = 480
    Top = 120
  end
end

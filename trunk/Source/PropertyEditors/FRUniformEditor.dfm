object ShaderUniformEditor: TShaderUniformEditor
  Left = 432
  Top = 332
  BorderStyle = bsDialog
  Caption = 'Autofill of uniforms'
  ClientHeight = 317
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Labe1: TLabel
    Left = 8
    Top = 5
    Width = 88
    Height = 13
    Caption = 'Available Uniforms'
  end
  object Label1: TLabel
    Left = 223
    Top = 5
    Width = 78
    Height = 13
    Caption = 'Avaiblable Value'
  end
  object Label2: TLabel
    Left = 223
    Top = 97
    Width = 79
    Height = 13
    Caption = 'Texture Sampler'
  end
  object Label3: TLabel
    Left = 223
    Top = 143
    Width = 176
    Height = 13
    Caption = 'Swizzle of texture color components '
  end
  object Label4: TLabel
    Left = 223
    Top = 51
    Width = 131
    Height = 13
    Caption = 'Texture Image/Attachment'
  end
  object LBUniforms: TListBox
    Left = 8
    Top = 24
    Width = 209
    Height = 285
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBUniformsClick
    OnKeyPress = LBUniformsKeyPress
  end
  object BBOk: TBitBtn
    Left = 357
    Top = 284
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object AutoSetBox: TComboBox
    Left = 223
    Top = 24
    Width = 209
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = AutoSetBoxChange
  end
  object SamplerBox: TComboBox
    Left = 223
    Top = 116
    Width = 209
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
    OnChange = SamplerBoxChange
  end
  object Panel1: TPanel
    Left = 223
    Top = 162
    Width = 209
    Height = 116
    TabOrder = 4
    object RedGroup: TRadioGroup
      Left = 8
      Top = 8
      Width = 50
      Height = 100
      Caption = 'RED'
      Color = clRed
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Items.Strings = (
        'R'
        'G'
        'B'
        'A'
        '0'
        '1')
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      OnClick = ColorGroupClick
    end
    object GreenGroup: TRadioGroup
      Tag = 1
      Left = 55
      Top = 8
      Width = 50
      Height = 100
      Caption = 'GREEN'
      Color = clGreen
      Items.Strings = (
        'R'
        'G'
        'B'
        'A'
        '0'
        '1')
      ParentBackground = False
      ParentColor = False
      TabOrder = 1
      OnClick = ColorGroupClick
    end
    object BlueGroup: TRadioGroup
      Tag = 2
      Left = 102
      Top = 8
      Width = 50
      Height = 100
      Caption = 'BLUE'
      Color = clBlue
      Items.Strings = (
        'R'
        'G'
        'B'
        'A'
        '0'
        '1')
      ParentBackground = False
      ParentColor = False
      TabOrder = 2
      OnClick = ColorGroupClick
    end
    object AlphaGroup: TRadioGroup
      Tag = 3
      Left = 150
      Top = 8
      Width = 50
      Height = 100
      Caption = 'ALPHA'
      Color = clWhite
      Items.Strings = (
        'R'
        'G'
        'B'
        'A'
        '0'
        '1')
      ParentBackground = False
      ParentColor = False
      TabOrder = 3
      OnClick = ColorGroupClick
    end
  end
  object TextureBox: TComboBox
    Left = 223
    Top = 70
    Width = 209
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 5
    OnChange = TextureBoxChange
  end
end

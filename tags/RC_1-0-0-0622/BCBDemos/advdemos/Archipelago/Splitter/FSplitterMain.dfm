object Form1: TForm1
  Left = 192
  Top = 126
  Width = 374
  Height = 275
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 110
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 20
    Width = 340
    Height = 38
    Caption = 
      'This utility will generate 16 1024x1024 BMP '#13#10'textures from the ' +
      #39'TextureMap.jpg'#39' files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LAAction: TLabel
    Left = 118
    Top = 177
    Width = 53
    Height = 16
    Caption = 'LAAction'
    Visible = False
  end
  object EDFile: TEdit
    Left = 20
    Top = 10
    Width = 326
    Height = 24
    Enabled = False
    TabOrder = 0
    Text = 'TextureMap.jpg'
    Visible = False
  end
  object Button1: TButton
    Left = 20
    Top = 197
    Width = 80
    Height = 31
    Caption = 'Split'
    TabOrder = 1
    OnClick = Button1Click
  end
  object EDTileSize: TEdit
    Left = 20
    Top = 30
    Width = 70
    Height = 24
    Enabled = False
    TabOrder = 2
    Text = '1024'
    Visible = False
  end
  object EDMask: TEdit
    Left = 20
    Top = 39
    Width = 326
    Height = 24
    Enabled = False
    TabOrder = 3
    Text = 'Tex_%d_%d.bmp'
    Visible = False
  end
  object ProgressBar: TProgressBar
    Left = 118
    Top = 197
    Width = 228
    Height = 31
    Min = 0
    Max = 16
    Smooth = True
    TabOrder = 4
  end
  object RBFull: TRadioButton
    Left = 59
    Top = 79
    Width = 277
    Height = 21
    Caption = 'Full Resolution (64 MB graphics memory)'
    Checked = True
    TabOrder = 5
    TabStop = True
  end
  object RBHalf: TRadioButton
    Left = 59
    Top = 108
    Width = 277
    Height = 21
    Caption = 'Medium Resolution (16 MB)'
    TabOrder = 6
  end
  object RBLow: TRadioButton
    Left = 59
    Top = 138
    Width = 277
    Height = 21
    Caption = 'Low Resolution (4 MB)'
    TabOrder = 7
  end
end

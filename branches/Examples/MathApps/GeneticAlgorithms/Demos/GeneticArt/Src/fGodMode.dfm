object frmGodMode: TfrmGodMode
  Left = 282
  Top = 114
  Width = 674
  Height = 578
  Caption = 'Ken'#39's God Mode'
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
  object Image1: TImage
    Left = 240
    Top = 8
    Width = 417
    Height = 249
  end
  object Shape1: TShape
    Left = 8
    Top = 8
    Width = 225
    Height = 145
    Brush.Color = clBlack
  end
  object Label1: TLabel
    Left = 8
    Top = 264
    Width = 93
    Height = 13
    Caption = 'Ling strength sliders'
  end
  object Label2: TLabel
    Left = 296
    Top = 264
    Width = 73
    Height = 13
    Caption = 'Genotype code'
  end
  object Image: TImage32
    Left = 16
    Top = 16
    Width = 210
    Height = 128
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object ScrollBox_LinkSliders: TScrollBox
    Left = 8
    Top = 280
    Width = 281
    Height = 257
    TabOrder = 1
  end
  object Memo_GenotypeCode: TMemo
    Left = 296
    Top = 280
    Width = 361
    Height = 257
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button_Zoom: TButton
    Left = 88
    Top = 160
    Width = 75
    Height = 21
    Caption = '&Zoom'
    TabOrder = 3
    OnClick = Button_ZoomClick
  end
  object Timer_RedrawEvent: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer_RedrawEventTimer
    Left = 168
    Top = 192
  end
end

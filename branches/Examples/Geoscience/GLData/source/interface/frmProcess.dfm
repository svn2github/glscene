object formProcess: TformProcess
  Left = 15
  Top = 302
  Caption = 'Process'
  ClientHeight = 299
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 453
    Height = 56
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object lblValue: TLabel
      Left = 87
      Top = 32
      Width = 35
      Height = 13
      Alignment = taRightJustify
      Caption = 'a Value'
    end
    object cbDataMapping: TComboBox
      Left = 4
      Top = 6
      Width = 246
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Cartesian'
      OnChange = cbDataMappingChange
      Items.Strings = (
        'Cartesian'
        'Cylindrical to Cartesian'
        'Spherical to Cartesian'
        'Prolate Spheroidal to Cartesian'
        'Oblate Spheroidal to Cartesian'
        'Bipolar Cylindrical to Cartesian')
    end
    object geParameterA: TGEFloatEdit
      Left = 129
      Top = 28
      Width = 121
      Height = 21
      Text = '0'
      ReturnStop = False
      UseMinBound = False
      UseMaxBound = False
      Validate = vdNone
      TabOrder = 1
    end
  end
  object memProcessReport: TMemo
    Left = 0
    Top = 101
    Width = 453
    Height = 198
    Align = alClient
    ReadOnly = True
    TabOrder = 1
    ExplicitHeight = 215
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 56
    Width = 453
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object bCancel: TBitBtn
      Left = 376
      Top = 4
      Width = 75
      Height = 25
      DoubleBuffered = True
      Kind = bkCancel
      ParentDoubleBuffered = False
      TabOrder = 0
    end
    object bOK: TBitBtn
      Left = 299
      Top = 4
      Width = 75
      Height = 25
      DoubleBuffered = True
      Enabled = False
      Kind = bkOK
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object pbProcess: TProgressBar
      Left = 0
      Top = 32
      Width = 453
      Height = 13
      Align = alBottom
      Smooth = True
      TabOrder = 2
    end
    object bProcess: TBitBtn
      Left = 220
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Process'
      DoubleBuffered = True
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
        000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
        0000FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00000000000000000000000000000000000000000000000000000000000000
        0000FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00
        FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00
        FF00000000000000000000000000000000000000000000000000000000000000
        0000FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
        0000FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
        000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = bProcessClick
    end
  end
end

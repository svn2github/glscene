object AGr32ViewerForm: TAGr32ViewerForm
  Left = 64
  Top = 5
  Caption = 'AStar Sprite Viewer'
  ClientHeight = 530
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000009900000000000000000000000
    0000090009990000000000000000000000009099000099990000000000000000
    0009099999990000999000000000000000909999999999990009999000000000
    0909990000000000999000099000000090990099999999990019191000000009
    0990999999999999190191919000009099099999999999999190191900000009
    9909999999999999191091900000009999099999999999999190190000000009
    9990999999999999190190000000000000909999999999999109000000000000
    0000999999999999190000000000000000009999999999919100000000000000
    0000099999999999100000000000000000000999999999919000000000000000
    0000099999999999100000000000000000000999999999919000000000000000
    0000009999999919000000000000000000000099999999910000000000000000
    0000009999999919000000000000000000000099999991910000000000000000
    0000000999999910000000000000000000000009999991900000000000000000
    0000000999999910000000000000000000000009999991900000000000000000
    0000000099991900000000000000000000000000990091000000000000000000
    000000000099000000000000000000000000000000000000000000000000FFC7
    FFFFFF80FFFFFF000FFFFE0001FFFC00001FF8000007F0000003E0000003C000
    0003800000078000000F8000001F8000003FE000007FFC0000FFFE0001FFFF00
    03FFFF0003FFFF0003FFFF0003FFFF8007FFFF8007FFFF8007FFFF8007FFFFC0
    0FFFFFC00FFFFFC00FFFFFC00FFFFFE01FFFFFE01FFFFFF03FFFFFF87FFF}
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label22: TLabel
    Left = 136
    Top = 32
    Width = 18
    Height = 13
    Caption = '321'
  end
  object Label23: TLabel
    Left = 104
    Top = 32
    Width = 18
    Height = 13
    Caption = '123'
  end
  object Label34: TLabel
    Left = 112
    Top = 40
    Width = 18
    Height = 13
    Caption = '123'
  end
  object Label35: TLabel
    Left = 144
    Top = 40
    Width = 18
    Height = 13
    Caption = '321'
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 511
    Width = 688
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Panels = <
      item
        Alignment = taCenter
        Text = 'Edit Mode'
        Width = 80
      end
      item
        Alignment = taCenter
        Text = '0.192 MS / 3 = 0.066 MS'
        Width = 150
      end
      item
        Alignment = taCenter
        Text = 'Unit # 1'
        Width = 80
      end
      item
        Text = 'Scenario #1'
        Width = 80
      end
      item
        Alignment = taCenter
        Text = 'X: 1200 Y: 1200'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = 'X: 120 Y: 120'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = 'Hints'
        Width = 50
      end>
    UseSystemFont = False
    ExplicitTop = 518
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 517
    Height = 511
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 518
    object Image32: TImage32
      Left = 0
      Top = 0
      Width = 512
      Height = 512
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
      OnMouseDown = Image32MouseDown
      OnMouseMove = Image32MouseMove
    end
  end
  object Panel1: TPanel
    Left = 517
    Top = 0
    Width = 171
    Height = 511
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 2
    ExplicitHeight = 518
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 75
      Height = 16
      Caption = 'Unit Status'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Unit1Status: TLabel
      Left = 8
      Top = 32
      Width = 43
      Height = 13
      Caption = 'Patrolling'
    end
    object Unit2Status: TLabel
      Left = 8
      Top = 64
      Width = 85
      Height = 13
      Caption = 'Targetunwalkable'
    end
    object Unit3Status: TLabel
      Left = 8
      Top = 96
      Width = 39
      Height = 13
      Caption = 'Blocked'
    end
    object Unit4Status: TLabel
      Left = 8
      Top = 128
      Width = 45
      Height = 13
      Caption = 'Deployed'
    end
    object Unit5Status: TLabel
      Left = 8
      Top = 160
      Width = 20
      Height = 13
      Caption = 'Lost'
    end
    object Unit6Status: TLabel
      Left = 8
      Top = 192
      Width = 49
      Height = 13
      Caption = 'Patrolling'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object Unit1Time: TLabel
      Left = 70
      Top = 48
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = '1.093'
    end
    object Unit2Time: TLabel
      Left = 70
      Top = 80
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = '1.093'
    end
    object Unit3Time: TLabel
      Left = 70
      Top = 112
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = '3.093'
    end
    object Unit4Time: TLabel
      Left = 70
      Top = 144
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = '1.093'
    end
    object Unit5Time: TLabel
      Left = 64
      Top = 176
      Width = 33
      Height = 13
      Alignment = taRightJustify
      Caption = '11.093'
    end
    object Unit6Time: TLabel
      Left = 70
      Top = 208
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = '1.093'
    end
    object TerrainValueAlphaValueLabel: TLabel
      Left = 120
      Top = 280
      Width = 21
      Height = 13
      Caption = '0.00'
    end
    object LayerLoadBtn: TSpeedButton
      Left = 8
      Top = 344
      Width = 73
      Height = 22
      Caption = 'Load Layer'
      OnClick = LayerLoadBtnClick
    end
    object Label7: TLabel
      Left = 20
      Top = 264
      Width = 93
      Height = 13
      Caption = 'Terrain Value Alpha'
    end
    object Label8: TLabel
      Left = 16
      Top = 312
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object Label9: TLabel
      Left = 80
      Top = 312
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object TVAMinLabel: TLabel
      Left = 40
      Top = 312
      Width = 12
      Height = 13
      Caption = '45'
    end
    object TVAMaxLabel: TLabel
      Left = 104
      Top = 312
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Label10: TLabel
      Left = 104
      Top = 16
      Width = 13
      Height = 13
      Caption = 'X: '
    end
    object Label11: TLabel
      Left = 136
      Top = 16
      Width = 13
      Height = 13
      Caption = 'Y: '
    end
    object Unit1BaseYLabel: TLabel
      Left = 136
      Top = 32
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit1BaseXLabel: TLabel
      Left = 104
      Top = 32
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit2BaseYLabel: TLabel
      Left = 136
      Top = 64
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit2BaseXLabel: TLabel
      Left = 104
      Top = 64
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit3BaseYLabel: TLabel
      Left = 136
      Top = 96
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit3BaseXLabel: TLabel
      Left = 104
      Top = 96
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit4BaseYLabel: TLabel
      Left = 136
      Top = 128
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit4BaseXLabel: TLabel
      Left = 104
      Top = 128
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit5BaseYLabel: TLabel
      Left = 136
      Top = 160
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit5BaseXLabel: TLabel
      Left = 104
      Top = 160
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit6BaseXLabel: TLabel
      Left = 104
      Top = 192
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit6BaseYLabel: TLabel
      Left = 136
      Top = 192
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit1TargetXLabel: TLabel
      Left = 104
      Top = 48
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit1TargetYLabel: TLabel
      Left = 136
      Top = 48
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit2TargetXLabel: TLabel
      Left = 104
      Top = 80
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit2TargetYLabel: TLabel
      Left = 136
      Top = 80
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit3TargetXLabel: TLabel
      Left = 104
      Top = 112
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit3TargetYLabel: TLabel
      Left = 136
      Top = 112
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit4TargetXLabel: TLabel
      Left = 104
      Top = 144
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit4TargetYLabel: TLabel
      Left = 136
      Top = 144
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit5TargetXLabel: TLabel
      Left = 104
      Top = 176
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit5TargetYLabel: TLabel
      Left = 136
      Top = 176
      Width = 18
      Height = 13
      Caption = '321'
    end
    object Unit6TargetXLabel: TLabel
      Left = 104
      Top = 208
      Width = 18
      Height = 13
      Caption = '123'
    end
    object Unit6TargetYLabel: TLabel
      Left = 136
      Top = 208
      Width = 18
      Height = 13
      Caption = '321'
    end
    object GridPaintBtn: TSpeedButton
      Left = 8
      Top = 400
      Width = 73
      Height = 22
      Caption = 'Paint Grid'
      OnClick = GridPaintBtnClick
    end
    object LayerAlphaTB: TTrackBar
      Left = 84
      Top = 344
      Width = 80
      Height = 33
      Max = 255
      PageSize = 64
      Frequency = 64
      TabOrder = 0
      OnChange = LayerAlphaTBChange
    end
    object TerrainValueAlphaValueTB: TTrackBar
      Left = 8
      Top = 280
      Width = 100
      Height = 33
      Max = 100
      PageSize = 1
      Frequency = 10
      TabOrder = 1
      OnChange = TerrainValueAlphaValueTBChange
    end
    object GrPathPanel: TPanel
      Left = 8
      Top = 232
      Width = 73
      Height = 22
      Caption = 'Path Color'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = GrPathPanelClick
    end
    object GridSizeRG: TRadioGroup
      Left = 88
      Top = 400
      Width = 73
      Height = 92
      Caption = 'Grid Size'
      ItemIndex = 4
      Items.Strings = (
        '64x64'
        '32x32'
        '20x20'
        '16x16'
        '10x10'
        '8x8')
      TabOrder = 3
    end
    object GridColorPanel: TPanel
      Left = 8
      Top = 464
      Width = 73
      Height = 14
      TabOrder = 4
      OnClick = GridColorPanelClick
    end
    object GridTB: TTrackBar
      Left = 3
      Top = 424
      Width = 80
      Height = 33
      Max = 255
      PageSize = 64
      Frequency = 64
      TabOrder = 5
      OnChange = GridTBChange
    end
  end
  object OpenDialog: TOpenPictureDialog
    Left = 496
    Top = 216
  end
  object ColorDialog1: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 489
    Top = 170
  end
end

object MFMakerForm: TMFMakerForm
  Left = 200
  Top = 112
  Caption = 'MF Texture Maker'
  ClientHeight = 244
  ClientWidth = 426
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
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00F777
    7777777777777777777777777777F8088888888800000000888888888087F070
    88888800AAA22AAA008888880707F808888800AAAAA22AAAAA0088888087F888
    8800AAAAAAA22AAAAAAA08888887F8888800AAAAAAA22AAAAAAAA0888887F888
    80AA2AAAA222222AAAAA2A088887F8880AAAA2A22AA22AA22AA2AAA08887F888
    0AAAAA2AAAA22AAAA22AAAA08887F880AAAAA2A2AAA22AAAA22AAAAA0887F880
    AAAA2AA2AAA22AAA2AA2AAAA0887F880AAAA2AAA2A2222AA2AA2AAAA0887F80A
    AAA2AAAAA2AAAA22AAAA2AAAA087F80AAAA2AAAA2AAAAAA2AAAA2AAAA087F80A
    AAA2AAA2AAAAAAAA2AAA2AAAA087F80222222222AAAAAAAA222222222087F802
    22222222AAAAAAAA222222222087F80AAAA2AAA2AAAAAAAA2AAA2AAAA087F80A
    AAA2AAAA2AAAAAA2AAAA2AAAA087F80AAAA2AAAA22AAAA2A2AAA2AAAA087F880
    AAAA2AA2AA2222AA2AA2AAAA0887F880AAAA2AA2AAA22AAAA2A2AAAA0887F880
    AAAAA22AAAA22AAAAA2AAAAA0887F8880AAAA22AAAA22AAAA2A2AAA08887F888
    0AAA2AA22AA22AA22AAA2AA08887F88880A2AAAAA222222AAAAAA0088887F888
    880AAAAAAAA22AAAAAAAA0888887F8888880AAAAAAA22AAAAAAA08888887F808
    888800AAAAA22AAAAA0088888087F07088888800AAA22AAA008888880707F808
    8888888800000000888888888087FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Memo_Octaves: TMemo
    Left = 160
    Top = 14
    Width = 89
    Height = 169
    Hint = 'Dont Step on Me!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'clear'
      'bubble'
      'n|4|0.5'
      'n|8|0.25'
      'n|16|0.125'
      'n|32|0.061'
      'n|64|0.0305'
      'n|128|0.0150'
      'rescale')
    ParentFont = False
    TabOrder = 2
    WordWrap = False
  end
  object Image32_Terrain: TImage32
    Left = 0
    Top = 0
    Width = 128
    Height = 128
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnMouseDown = Image32_TerrainMouseDown
    OnMouseMove = Image32_TerrainMouseMove
  end
  object Panel1: TPanel
    Left = 257
    Top = 0
    Width = 177
    Height = 254
    TabOrder = 1
    object SaveBtn: TSpeedButton
      Left = 96
      Top = 80
      Width = 41
      Height = 22
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object ExitBtn: TSpeedButton
      Left = 96
      Top = 104
      Width = 41
      Height = 22
      Hint = 'New Clear'
      Caption = 'Exit'
      OnClick = ExitBtnClick
    end
    object HelpBtn: TSpeedButton
      Left = 144
      Top = 8
      Width = 25
      Height = 22
      Caption = '?'
      OnClick = HelpBtnClick
    end
    object Button_GO: TSpeedButton
      Left = 96
      Top = 32
      Width = 41
      Height = 22
      Caption = 'GO'
      OnClick = Button_GOClick
    end
    object EdgesBtn: TSpeedButton
      Left = 96
      Top = 56
      Width = 41
      Height = 22
      Caption = 'Edges'
      OnClick = EdgesBtnClick
    end
    object ClearBtn: TSpeedButton
      Left = 96
      Top = 8
      Width = 41
      Height = 22
      Caption = 'Clear'
      OnClick = ClearBtnClick
    end
    object GoTB: TTrackBar
      Left = 146
      Top = 48
      Width = 24
      Height = 80
      Min = 1
      Orientation = trVertical
      Position = 2
      TabOrder = 0
      ThumbLength = 14
      TickMarks = tmTopLeft
    end
    object SizeRG: TRadioGroup
      Left = 8
      Top = 8
      Width = 81
      Height = 81
      Caption = 'Size'
      ItemIndex = 1
      Items.Strings = (
        '64x64'
        '128x128'
        '256x256')
      TabOrder = 1
      OnClick = SizeRGClick
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 127
      Width = 161
      Height = 121
      Caption = 'Image Mouse Height altering'
      TabOrder = 2
      DesignSize = (
        161
        121)
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 33
        Height = 13
        Caption = 'Radius'
      end
      object Label2: TLabel
        Left = 8
        Top = 56
        Width = 29
        Height = 13
        Caption = 'Depth'
      end
      object ScrollBar_Radius: TScrollBar
        Left = 6
        Top = 32
        Width = 147
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Min = 1
        PageSize = 0
        Position = 20
        TabOrder = 0
      end
      object ScrollBar_Depth: TScrollBar
        Left = 8
        Top = 72
        Width = 145
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Min = 1
        PageSize = 0
        Position = 7
        TabOrder = 1
      end
      object CheckBox_Box: TCheckBox
        Left = 8
        Top = 96
        Width = 97
        Height = 17
        Caption = 'Box [x] or Circle'
        TabOrder = 2
      end
    end
    object CheckBox_RenderWater: TCheckBox
      Left = 144
      Top = 31
      Width = 25
      Height = 17
      Caption = '_'
      TabOrder = 3
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'rnd'
    Filter = 'Cloud base (*.clb)|*.clb'
    Left = 136
    Top = 32
  end
end

object NanMakerForm: TNanMakerForm
  Left = 237
  Top = 110
  Caption = 'Nan Maker'
  ClientHeight = 245
  ClientWidth = 357
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
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000BBBB000000000000000BBB
    B00000000000B00000000003333000B0000000000000B00000000000030000B0
    000000000000B000000000000300000B000000000000B000000000003000000B
    00000000000BBB000000000030000000B000000000B0B0B00000000300000000
    BBBB00000B00B00B00003333000000000B00BBBBB7B303B0B333003000000000
    0B00BBBBBB7B0B0B333300300000000000B0BBBBBBB7B0B3B333030000000000
    00B0BBBBBB3B0B3B3333030000000000000BBBBBBBB3B3B3B333300000000000
    000000808080008808000000000000000000FFFF078888708880000000000000
    0000FFF087888077088800000000000000000F08F78880777080000000000000
    00000F7FF7800077708000000000000000000F7FF87777777780000000000000
    00000F70000FF000008800000000000000000F700E0FF0E00788000000000000
    00000F77EE0FF0EE708000000000000000000087777FF7777800000000000000
    000000088F8FF777800000000000000000000000078FF7780000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFC3FFF87F81FE103FC3FC087FE3FE18FFE3FF1C7FE3FE3C7FC
    1FE3E0F80F07E0000007F000000FF000000FF800001FF800001FFC00003FFE00
    007FFE0000FFFE00007FFF0000FFFF0000FFFF0000FFFF00007FFF00007FFF00
    00FFFF8001FFFFC003FFFFE007FFFFF80FFFFFFFFFFFFFFFFFFFFFFFFFFF}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 128
    Height = 128
    AutoSize = True
    OnMouseUp = Image1MouseUp
  end
  object Panel1: TPanel
    Left = 252
    Top = 0
    Width = 105
    Height = 245
    Align = alRight
    TabOrder = 0
    object SaveBtn: TSpeedButton
      Left = 56
      Top = 208
      Width = 41
      Height = 22
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object ExitBtn: TSpeedButton
      Left = 56
      Top = 232
      Width = 41
      Height = 22
      Caption = 'Exit'
      OnClick = ExitBtnClick
    end
    object HelpBtn: TSpeedButton
      Left = 8
      Top = 232
      Width = 41
      Height = 22
      Caption = '?'
      OnClick = HelpBtnClick
    end
    object StopBtn: TSpeedButton
      Left = 8
      Top = 208
      Width = 41
      Height = 22
      Caption = 'Stop'
      OnClick = StopBtnClick
    end
    object ClearBtn: TSpeedButton
      Left = 8
      Top = 76
      Width = 41
      Height = 22
      Hint = 'New Clear'
      Caption = 'Clear'
      OnClick = ClearBtnClick
    end
    object TrackBar1: TTrackBar
      Left = 8
      Top = 168
      Width = 90
      Height = 33
      Min = 1
      Position = 2
      TabOrder = 0
    end
    object SizeRG: TRadioGroup
      Left = 8
      Top = 8
      Width = 81
      Height = 65
      Caption = 'Size'
      ItemIndex = 1
      Items.Strings = (
        '64x64'
        '128x128'
        '256x256')
      TabOrder = 1
      OnClick = SizeRGClick
    end
    object CARG: TRadioGroup
      Left = 8
      Top = 98
      Width = 89
      Height = 64
      Caption = 'Run 1 of these'
      Columns = 2
      ItemIndex = 5
      Items.Strings = (
        'CA'
        'DLA'
        'DLT'
        'C2'
        'C3'
        'NA')
      TabOrder = 2
      OnClick = CARGClick
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'rnd'
    Filter = 'Cloud base (*.clb)|*.clb'
    Left = 136
    Top = 32
  end
end

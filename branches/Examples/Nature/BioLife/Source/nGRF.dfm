object GRFForm: TGRFForm
  Left = 161
  Top = 66
  HelpContext = 5000
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Grass Rabbit Fox'
  ClientHeight = 233
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007777
    BBBBBB99992222222222BBBB77777777BBBBBB99992222222222BBBB77777777
    BBBBBB99992222222222BBBB77777777BBBBBB99992222222222BBBB77777777
    7777992222222222222277777777777777779922222222222222777777777777
    7777992222222222222277777777777777779922222222222222777777777777
    777777222222222222BBBBBBBBBB7777777777222222222222BBBBBBBBBBBBBB
    222277BBBB222222222222227777BBBB222277BBBB222222222222227777BBBB
    222277BBBB222222222222227777BBBB222277BBBB222222222222227777BBBB
    777777BBBB222222222299997777BBBB777777BBBB222222222299997777BBBB
    777777BBBB222222222299997777BBBB777777BBBB2222222222999977777777
    7777772222222222222277772222777777777722222222222222777722227777
    777777222222222222227777222277777777772222222222222277772222BBBB
    7777779999BBBBBBBB2222222222BBBB7777779999BBBBBBBB2222222222BBBB
    222299777777777777BB22222222BBBB222299777777777777BB22222222BBBB
    222299777777777777BB22222222BBBB222299777777777777BB22222222BBBB
    BBBBBBBBBB77777777BB22222222BBBBBBBBBBBBBB77777777BB22222222BBBB
    BBBBBBBBBB77777777BB22222222BBBBBBBBBBBBBB77777777BB222222220000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 121
    Height = 217
    Hint = 'Grow'
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 54
    Height = 24
    Caption = 'Grass'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 32
    Top = 48
    Width = 33
    Height = 13
    Caption = 'Energy'
  end
  object Label4: TLabel
    Left = 16
    Top = 136
    Width = 78
    Height = 13
    Caption = 'Growth Minimum'
  end
  object Label6: TLabel
    Left = 16
    Top = 176
    Width = 81
    Height = 13
    Caption = 'Growth Maximum'
  end
  object Label5: TLabel
    Left = 16
    Top = 96
    Width = 70
    Height = 13
    Caption = 'Growth Energy'
  end
  object Bevel2: TBevel
    Left = 136
    Top = 8
    Width = 121
    Height = 137
    Hint = 'Run'
  end
  object Label7: TLabel
    Left = 144
    Top = 16
    Width = 60
    Height = 24
    Caption = 'Rabbit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 144
    Top = 80
    Width = 60
    Height = 13
    Caption = 'Max Percent'
  end
  object Label9: TLabel
    Left = 168
    Top = 48
    Width = 33
    Height = 13
    Caption = 'Energy'
  end
  object Label10: TLabel
    Left = 144
    Top = 96
    Width = 104
    Height = 13
    Caption = 'Energy per Movement'
  end
  object Bevel3: TBevel
    Left = 264
    Top = 8
    Width = 121
    Height = 137
    Hint = 'Hunt'
  end
  object Label11: TLabel
    Left = 272
    Top = 96
    Width = 104
    Height = 13
    Caption = 'Energy per Movement'
  end
  object Label12: TLabel
    Left = 296
    Top = 48
    Width = 33
    Height = 13
    Caption = 'Energy'
  end
  object Label13: TLabel
    Left = 272
    Top = 80
    Width = 60
    Height = 13
    Caption = 'Max Percent'
  end
  object Label14: TLabel
    Left = 280
    Top = 16
    Width = 37
    Height = 24
    Caption = 'Fox'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 80
    Width = 60
    Height = 13
    Caption = 'Max Percent'
  end
  object GrassNEdit: TEdit
    Left = 80
    Top = 16
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 0
    Text = '5000'
  end
  object GrassEEdit: TEdit
    Left = 80
    Top = 40
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 1
    Text = '15'
  end
  object GrassGMinEdit: TEdit
    Left = 80
    Top = 152
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 2
    Text = '3'
  end
  object GrassGMaxEdit: TEdit
    Left = 80
    Top = 192
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 3
    Text = '8'
  end
  object GrassGEEdit: TEdit
    Left = 80
    Top = 112
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 4
    Text = '3'
  end
  object RabbitNEdit: TEdit
    Left = 208
    Top = 16
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 5
    Text = '500'
  end
  object RabbitEEdit: TEdit
    Left = 208
    Top = 40
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 6
    Text = '15'
  end
  object RabbitEMEdit: TEdit
    Left = 208
    Top = 112
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 7
    Text = '2'
  end
  object FoxEMEdit: TEdit
    Left = 336
    Top = 112
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 8
    Text = '10'
  end
  object FoxEEdit: TEdit
    Left = 336
    Top = 40
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 9
    Text = '20'
  end
  object FoxNEdit: TEdit
    Left = 336
    Top = 16
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 10
    Text = '50'
  end
  object GRFCheckBox: TCheckBox
    Left = 144
    Top = 152
    Width = 97
    Height = 17
    Hint = 'Run forever...'
    HelpContext = 5004
    Caption = 'Reincarnation'
    TabOrder = 11
  end
  object GRFImageSizeRG: TRadioGroup
    Left = 264
    Top = 152
    Width = 121
    Height = 73
    Hint = 'sorta screen size'
    HelpContext = 5000
    Caption = 'Image Size'
    ItemIndex = 0
    Items.Strings = (
      '403x268 {98x60}'
      '639x480 {157x113}'
      '799x600 [197x143]')
    TabOrder = 12
  end
  object GRFHelpBtn: TBitBtn
    Left = 136
    Top = 176
    Width = 65
    Height = 25
    Hint = 'GRF Help|Grass Rabbit Fox Help or F1 something'
    HelpContext = 5000
    TabOrder = 13
    OnClick = GRFHelpBtnClick
    Kind = bkHelp
  end
  object RabbitMPEdit: TEdit
    Left = 208
    Top = 72
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 15
    Text = '5'
  end
  object FoxMPEdit: TEdit
    Left = 336
    Top = 72
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 16
    Text = '1'
  end
  object GrassMPEdit: TEdit
    Left = 80
    Top = 72
    Width = 41
    Height = 21
    HelpContext = 5000
    TabOrder = 17
    Text = '50'
  end
  object CancelBitBtn: TBitBtn
    Left = 136
    Top = 200
    Width = 65
    Height = 25
    Hint = 'land'
    HelpContext = 5000
    TabOrder = 18
    OnClick = CancelBitBtnClick
    Kind = bkClose
  end
  object GRFOKBtn: TBitBtn
    Left = 200
    Top = 200
    Width = 57
    Height = 25
    Hint = 'Grow to eat'
    HelpContext = 5000
    TabOrder = 14
    OnClick = GRFOKBtnClick
    Kind = bkOK
  end
end

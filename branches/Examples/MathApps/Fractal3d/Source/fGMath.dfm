object MathForm: TMathForm
  Left = 220
  Top = 128
  Hint = 'Baker'#39's Dozen'
  HelpContext = 2000
  Caption = 'Math Graphics'
  ClientHeight = 348
  ClientWidth = 301
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
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00CCCC
    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    CCCCFCFCCCCCCCCCCCCCCCCFCCCCCCCCFCCCCCCFCCFCCCCCCCCCCCCCCFCCCCCC
    CCCCCCCCCCCCCCCCCFCFCCCCCCFCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFCCCCCC
    CCCCCFCCCCCCCCFCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFC1111111CCCCC11
    11111CCFCCCCC1111111111111CCC11111111111CCC111111111111111CCC111
    11111111111111111111111111CCCC1111111111111111111111111111CCCCC1
    1111111111111111111111111CCCCCCC11111111111111111111111CCCCCCCFC
    CC1111111111111111111CCCFCCCCCCCCCCCCCCCCCCCCCC11111CCCCCCFCCCCC
    CCCCCCCCCCCCCCCC1111CCCCCCCCEEEEEEEEEEEEEEEEEEEE1111EEEEEEEEEEEE
    EEEEEEEEEEEEEEEEE1111EEEEEEEEEEEEEEEEEEEEEEEEEEEEE1111EEEEEEEEEE
    EEEEEEEEEEEEEEEEEEE111EEEEEEEEEEEEEEEEEEEEEEEE2EEEEE111EEEE2EEEE
    EEEEEEEEEEEEEE2EEEEE111EEE22EEEEEEEEEEEEEE2EEE22EEEEE111EE22EEEE
    BBBBEEEEE22E1122E11EEE11E222EEEBBBBBBEEEE2221122211EEE11E222EEEB
    BBBBBEEEEE222EE222EEEE11E222EEEBBBBBBEEEEE2222E222E11E112222EEEB
    BBBBBEEEEEE2222E22211112222EEEEEBBBBEEEEEEEE22222E22122222EEEEEE
    EEEEEEEEEEEEE222222222222EEEEEEEEEEEEEEEEEEEEEEE222222EEEEEE0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 297
    Height = 353
    Hint = 'Graphics generated from Math'
    HelpContext = 2000
    ActivePage = RandomTS
    HotTrack = True
    MultiLine = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object PopulationTS: TTabSheet
      HelpContext = 2200
      Caption = '&Population'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label19: TLabel
        Left = 16
        Top = 264
        Width = 3
        Height = 13
        Caption = 'r'
      end
      object Label20: TLabel
        Left = 80
        Top = 264
        Width = 6
        Height = 13
        Caption = 'd'
      end
      object MGPRG: TRadioGroup
        Left = 16
        Top = 16
        Width = 257
        Height = 233
        HelpContext = 2210
        Caption = 'Population Equation'
        ItemIndex = 0
        Items.Strings = (
          'Bifurcation rx(1-x)  (x=0.5) (r=0.95, d=0.005)'
          'Bifurcation:  expanded (r=3.55, d=0.0005)'
          'Bifurcation: Input  [r] [d]'
          'Fiegenbaum #'#39's... see FX text and DIY Memo'
          'Fiegenbaum x(1-x)  (x=0.5) (r=0.95, d=0.005)'
          'Fiegenbaum: expanded (r=3.55, d=0.0005)'
          'Fiegenbaum: Input  [r] [d]'
          'Malthusian: Pn+1 = R*Pn*(1-Pn)'
          'Malthusian View 2')
        TabOrder = 0
      end
      object MGPHelp: TBitBtn
        Left = 144
        Top = 264
        Width = 64
        Height = 25
        HelpContext = 2200
        Kind = bkHelp
        NumGlyphs = 2
        TabOrder = 1
        OnClick = MGPHelpClick
      end
      object MGPOK: TBitBtn
        Left = 208
        Top = 264
        Width = 64
        Height = 25
        Hint = 'Run Display|Run Population selection onto Display'
        HelpContext = 2200
        Caption = '&Display'
        Default = True
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 1
        NumGlyphs = 2
        TabOrder = 2
        OnClick = MGPOKClick
      end
      object PopEdit1: TEdit
        Left = 24
        Top = 264
        Width = 49
        Height = 21
        HelpContext = 2201
        TabOrder = 3
        Text = '3.55'
      end
      object PopEdit2: TEdit
        Left = 88
        Top = 264
        Width = 49
        Height = 21
        HelpContext = 2201
        TabOrder = 4
        Text = '0.0005'
      end
    end
    object AttractorsTS: TTabSheet
      HelpContext = 2100
      Caption = '&Attractors'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label2: TLabel
        Left = 8
        Top = 264
        Width = 73
        Height = 13
        Caption = '[ ]...Click Image'
      end
      object Label3: TLabel
        Left = 88
        Top = 264
        Width = 46
        Height = 13
        Caption = 'to Display'
      end
      object Label4: TLabel
        Left = 80
        Top = 276
        Width = 54
        Height = 13
        Caption = 'Next Image'
      end
      object MGARG: TRadioGroup
        Left = 88
        Top = 16
        Width = 97
        Height = 105
        HelpContext = 2110
        Caption = 'Strange Attractors'
        Items.Strings = (
          'Lorenz (x,y,z)'
          'Vortex (x,z)'
          'Duffing'
          'Rossler'
          'Henon Stairs')
        TabOrder = 0
        OnClick = MGARGClick
      end
      object MGLCRG: TRadioGroup
        Left = 0
        Top = 16
        Width = 89
        Height = 105
        HelpContext = 2120
        Caption = 'Cycles of Limit'
        ItemIndex = 0
        Items.Strings = (
          'Rayleigh'
          'Vanderpol'
          'Brusselator'
          'All 3 in 1')
        TabOrder = 1
        OnClick = MGLCRGClick
      end
      object MGAOK: TBitBtn
        Left = 208
        Top = 264
        Width = 64
        Height = 25
        Hint = 'Run Display|Run Attractor selection onto Display'
        HelpContext = 2100
        Caption = '&Display'
        Default = True
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 1
        NumGlyphs = 2
        TabOrder = 2
        OnClick = MGAOKClick
      end
      object MGAHelp: TBitBtn
        Left = 144
        Top = 264
        Width = 64
        Height = 25
        HelpContext = 2100
        Kind = bkHelp
        NumGlyphs = 2
        TabOrder = 3
        OnClick = MGAHelpClick
      end
      object AHREdit1: TEdit
        Left = 112
        Top = 208
        Width = 33
        Height = 21
        HelpContext = 2113
        TabOrder = 4
        Text = '1.42'
      end
      object AHREdit2: TEdit
        Left = 176
        Top = 208
        Width = 33
        Height = 21
        HelpContext = 2113
        TabOrder = 5
        Text = '0.02'
      end
      object AHREdit3: TEdit
        Left = 216
        Top = 208
        Width = 33
        Height = 21
        HelpContext = 2113
        TabOrder = 6
        Text = '3'
      end
      object KEdit1: TEdit
        Left = 112
        Top = 160
        Width = 33
        Height = 21
        HelpContext = 2111
        TabOrder = 7
        Text = '1.3'
      end
      object KEdit2: TEdit
        Left = 144
        Top = 160
        Width = 33
        Height = 21
        HelpContext = 2111
        Color = clAqua
        TabOrder = 8
        Text = '0.1'
      end
      object KEdit3: TEdit
        Left = 176
        Top = 160
        Width = 33
        Height = 21
        HelpContext = 2111
        TabOrder = 9
        Text = '0.01'
      end
      object KEdit4: TEdit
        Left = 216
        Top = 160
        Width = 33
        Height = 21
        HelpContext = 2111
        TabOrder = 10
        Text = '30'
      end
      object KEdit5: TEdit
        Left = 248
        Top = 160
        Width = 41
        Height = 21
        HelpContext = 2111
        TabOrder = 11
        Text = '3000'
      end
      object k2Edit1: TEdit
        Left = 112
        Top = 184
        Width = 33
        Height = 21
        HelpContext = 2112
        Color = clAqua
        TabOrder = 12
        Text = '0.3'
      end
      object k2Edit2: TEdit
        Left = 144
        Top = 184
        Width = 33
        Height = 21
        HelpContext = 2112
        TabOrder = 13
        Text = '1.75'
      end
      object k2Edit3: TEdit
        Left = 176
        Top = 184
        Width = 33
        Height = 21
        HelpContext = 2112
        TabOrder = 14
        Text = '0.02'
      end
      object k2Edit4: TEdit
        Left = 216
        Top = 184
        Width = 33
        Height = 21
        HelpContext = 2112
        TabOrder = 15
        Text = '36'
      end
      object k2Edit5: TEdit
        Left = 247
        Top = 184
        Width = 41
        Height = 21
        HelpContext = 2112
        TabOrder = 16
        Text = '3000'
      end
      object MGASARG: TRadioGroup
        Left = 0
        Top = 155
        Width = 113
        Height = 73
        HelpContext = 2150
        Caption = 'Stranger   Attractors'
        Items.Strings = (
          'Kaneko [ ]'
          'Kaneko.2 [ ]'
          'Henon Range [ ]')
        TabOrder = 17
        OnClick = MGASARGClick
      end
      object MGACRG: TRadioGroup
        Left = 184
        Top = 16
        Width = 105
        Height = 137
        HelpContext = 2130
        Caption = 'Strange Chemicals'
        Items.Strings = (
          'A straw'
          'Beckon'
          'Corkscrew'
          'D  ...3+V'
          'E  ...3'
          'F  ...2+V'
          'Google 2'
          'Alphabet Soup')
        TabOrder = 18
        OnClick = MGACRGClick
      end
    end
    object RandomTS: TTabSheet
      HelpContext = 2250
      Caption = '&Random'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label83: TLabel
        Left = 8
        Top = 264
        Width = 3
        Height = 13
        Caption = 'r'
      end
      object Label84: TLabel
        Left = 56
        Top = 264
        Width = 6
        Height = 13
        Caption = 'd'
      end
      object Label142: TLabel
        Left = 104
        Top = 264
        Width = 3
        Height = 13
        Caption = 't'
      end
      object MGBMRG: TRadioGroup
        Left = 56
        Top = 8
        Width = 177
        Height = 233
        HelpContext = 2254
        Caption = 'Random ( Brownian )  Motion'
        ItemIndex = 0
        Items.Strings = (
          'One D Random [r]'
          'Two D Linear [r]'
          'Fractal <t>, Gaussian [r] [d] [t]'
          'White , 1/f,  Brownian [r] [d]'
          'Cellar Walk (Click it to Stop)'
          'Wrapped (Click it to Stop)'
          'Zapped (Click it to Stop)'
          'HiFi Lines (Click it to Stop)'
          'Chaser (Click it to Stop)')
        TabOrder = 0
      end
      object MGBMrEdit: TEdit
        Left = 16
        Top = 264
        Width = 41
        Height = 21
        HelpContext = 2252
        TabOrder = 1
        Text = '1.1'
      end
      object MGBMdEdit: TEdit
        Left = 64
        Top = 264
        Width = 33
        Height = 21
        HelpContext = 2252
        TabOrder = 2
        Text = '2.2'
      end
      object MGBMHelpBtn: TBitBtn
        Left = 144
        Top = 264
        Width = 64
        Height = 25
        HelpContext = 2250
        Kind = bkHelp
        NumGlyphs = 2
        TabOrder = 3
        OnClick = MGBMHelpBtnClick
      end
      object MGBMOKBtn: TBitBtn
        Left = 208
        Top = 264
        Width = 64
        Height = 25
        Hint = 'Run Display|Run Random selection onto Display'
        HelpContext = 2250
        Caption = '&Display'
        Default = True
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 1
        NumGlyphs = 2
        TabOrder = 4
        OnClick = MGBMOKBtnClick
      end
      object RTimeEdit: TEdit
        Left = 112
        Top = 264
        Width = 25
        Height = 21
        TabOrder = 5
        Text = '50'
      end
    end
    object IteratedFunctionsTS: TTabSheet
      Hint = 'Pete and Repeate were sitting on a log...'
      HelpContext = 2400
      Caption = '&Fun'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MGIFRG: TRadioGroup
        Left = 168
        Top = 0
        Width = 121
        Height = 249
        Hint = 'Pete and Repeate were sitting on a log...'
        HelpContext = 2430
        Caption = 'Iterated Functions'
        ItemIndex = 0
        Items.Strings = (
          'Fern'
          '4 3D Ferns'
          'Tree'
          'Sierpinski what'
          'Sierpinski when'
          'Determined Fern'
          'Determined Triangle'
          'Sierpinski Triangle'
          'Sierpinski Arches'
          'Sierpinski Forest')
        TabOrder = 0
        OnClick = MGIFRGClick
      end
      object MGVRG: TRadioGroup
        Left = 8
        Top = 0
        Width = 153
        Height = 137
        Hint = 'Pete fell off...'
        HelpContext = 2410
        Caption = 'Vistas'
        Items.Strings = (
          'Hello, Buttes'
          'Puerto Rico Peak'
          'Forest for the Trees'
          'Lights on, Nobody home'
          'Earth Viewed from Moon')
        TabOrder = 1
        OnClick = MGVRGClick
      end
      object MGCirclesRG: TRadioGroup
        Left = 8
        Top = 160
        Width = 153
        Height = 89
        Hint = 'Who was left ?'
        HelpContext = 2420
        Caption = 'Trees and Circles'
        Items.Strings = (
          'Tree'
          'Apolliania'#39's Circles [1..8]'
          'Pharoah'#39's Collar Boned')
        TabOrder = 2
        OnClick = MGCirclesRGClick
      end
      object MGIFHelp: TBitBtn
        Left = 144
        Top = 264
        Width = 64
        Height = 25
        HelpContext = 2400
        Kind = bkHelp
        NumGlyphs = 2
        TabOrder = 3
        OnClick = MGIFHelpClick
      end
      object MGIFOK: TBitBtn
        Left = 208
        Top = 264
        Width = 64
        Height = 25
        Hint = 'Run Display|Run IF selection onto Display'
        HelpContext = 2405
        Caption = '&Display'
        Default = True
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 1
        NumGlyphs = 2
        TabOrder = 4
        OnClick = MGIFOKClick
      end
      object ApollianiaEdit: TEdit
        Left = 24
        Top = 264
        Width = 49
        Height = 21
        Hint = 'Apolliania needs input'
        HelpContext = 2420
        TabOrder = 5
        Text = '4'
      end
      object SierpinskiEdit: TEdit
        Left = 88
        Top = 264
        Width = 49
        Height = 21
        Hint = 'Sierpinski needs input'
        HelpContext = 2420
        TabOrder = 6
        Text = '4'
      end
    end
    object BumpTS: TTabSheet
      Hint = 'Bumps and Grinds'
      HelpContext = 2450
      Caption = '&Bumps'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object FOK: TSpeedButton
        Left = 216
        Top = 208
        Width = 73
        Height = 22
        Hint = 'Run Display|Run Bumps selection onto Display'
        Caption = 'Display'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        NumGlyphs = 2
        OnClick = FOKClick
      end
      object Label21: TLabel
        Left = 8
        Top = 240
        Width = 7
        Height = 13
        Caption = 'X'
      end
      object Label22: TLabel
        Left = 80
        Top = 240
        Width = 7
        Height = 13
        Caption = 'Y'
      end
      object Label23: TLabel
        Left = 148
        Top = 240
        Width = 12
        Height = 13
        Caption = 'Zx'
      end
      object Label24: TLabel
        Left = 220
        Top = 240
        Width = 12
        Height = 13
        Caption = 'Zy'
      end
      object Label87: TLabel
        Left = 8
        Top = 264
        Width = 3
        Height = 13
        Caption = 'r'
      end
      object Label88: TLabel
        Left = 56
        Top = 264
        Width = 6
        Height = 13
        Caption = 'd'
      end
      object Label99: TLabel
        Left = 162
        Top = 264
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'h'
      end
      object Label100: TLabel
        Left = 204
        Top = 264
        Width = 2
        Height = 13
        Caption = 'i'
      end
      object FRotate: TSpeedButton
        Left = 104
        Top = 208
        Width = 24
        Height = 22
        Hint = 'Click to change Rotation of Cube'
        Glyph.Data = {
          EE000000424DEE0000000000000076000000280000000F0000000F0000000100
          0400000000007800000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888808888
          8881808888808888808188088000008808808880077777008880888088888770
          888088088FFFF87708808808FFFFFF8708820008FFFFFF8700008808FFFFFF87
          08808808FFFFFF87088088808FFFF8808880888008888800888E880880000088
          088080888880888880808888888088888880}
        OnClick = FRotateClick
      end
      object FClear: TSpeedButton
        Left = 2
        Top = 208
        Width = 24
        Height = 22
        Hint = 'Reset'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
          333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
          0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333337F33333337F333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333FF7F33333337FFFBBB0FFFFFFFF0BB37777F3333333777F3BB0FFFFFFFF
          0BBB3777F3333FFF77773330FFFF000003333337F333777773333330FFFF0FF0
          33333337F3337F37F3333330FFFF0F0B33333337F3337F77FF333330FFFF003B
          B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
          3BB33773333773333773B333333B3333333B7333333733333337}
        NumGlyphs = 2
        OnClick = FClearClick
      end
      object Label144: TLabel
        Left = 250
        Top = 264
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'k'
      end
      object Label145: TLabel
        Left = 122
        Top = 264
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'g'
      end
      object GLBtn: TSpeedButton
        Left = 136
        Top = 204
        Width = 73
        Height = 33
        Glyph.Data = {
          B60C0000424DB60C000000000000360400002800000044000000200000000100
          0800000000008008000000000000000000000001000000010000FFFFFF00F7FF
          FF00F7F7FF00FFFFF700FFF7F700F7F7F700EFF7F700EFEFF700F7F7EF00EFEF
          EF00E7EFEF00E7E7EF00F7EFE700EFEFE700E7E7E700DEDEE700EFE7DE00DEDE
          DE00D6D6DE00DED6D600D6D6D600D6CED600CECED600E7DECE00CECECE00C6C6
          CE00E7D6C600C6C6C600BDBDC600E7D6BD00DED6BD00DECEBD00D6C6BD00BDBD
          BD00BDB5BD00B5B5BD00DECEB500BDB5B500B5B5B500ADADB500D6C6AD00ADAD
          AD00ADA5AD00D6BDA500CEBDA500A5A5A500A59CA5009C9CA500D6BD9C00CEBD
          9C00BDAD9C009C9C9C009C949C0094949C00CEB594009C94940094949400948C
          94008C8C9400CEB58C00C6AD8C00B5A58C00948C8C008C8C8C008C848C008484
          8C00C6AD84008C84840084848400847B84007B7B8400C6A57B00BDA57B00847B
          7B007B7B7B007B737B00BDA57300BD9C73007B73730073737300BDA56B00BD9C
          6B00B59C6B009C8C6B00B5946300A58C6300736B63006B6B6300B5945A00AD94
          5A00AD8C5A00A58C5A006B635A00AD8C5200A58C5200A58452006B635200AD8C
          4A00A5844A009C844A00635A4A00A58442009C7B42006B5A42005A524200524A
          42009C7B3900947B3900947339009C7B3100947B3100947331008C7331008C6B
          3100634A31005A4A3100524A31004A423100947329008C6B2900846B29008C63
          2900846329005A4A2900524229004A392900423929008C6B2100846B21008463
          21007B6321007B5A2100634A21005A4A21004A39210042392100846318007B5A
          1800735A18005242180042311800846310007B5A1000735A10006B5210003129
          10007B5A0800735A0800735208006B520800634A080052390800423108003129
          08003121080029210800735A0000735200006B5200006B4A0000634A00006342
          00005A420000524200005A390000523900004A3900004A310000423100004229
          0000392900003129000031210000292100002118000021100000181000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000090E151B21262627
          26221C18110E0500000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000040A1829344987ABA9
          A6A5A6A5A7AAAA864F40382D1C11050000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000918277DA8A295
          8D7A786F6F6F6F6F6F70777A8193A2A87D4038210E0000000000000000000000
          00000000000000000000000000000000000000000000000000000009217CA690
          7F787877787777776F6F6F6F6C6F6F76777A7A807A8EA67D412D110000000000
          0000000000000000000000000000000000000000000000000000000000021B98
          A080717778777A817A7866523117050000000000000410314D5A6F78959A4427
          0E00000000000000000000000000000000000000000000000000000000000000
          0E73A06F6F6F7077787A775F2C021B2100000000000000000000000000000005
          2862A05C38180000000000000000000000000000000000000000000000000000
          00000011A8816F6C6C6C6F716C5200000008A837000000000000000000000000
          0000000000000531B03418000000000000000000000000000000000000000000
          0000000000000EA27877706F6F6C6B551B1409000008A2380E110F0500000009
          11110500060E0000000E050000960F09110B0000090000050505080508050000
          00000000000000000005A377777777776F7760857B433A190305A24456643A2D
          090016566839330E26390900113F1B0000112D4E4F3F2A142F18002D35373535
          35370000000000000000000000AA7A777A777171706C66545981AD491C089FA0
          7763A05C270E816651938729923F09004CAF1B0014A07F7778A15763AC19509D
          9C8F8E8888AA00000000000000000000008D7A7F78787777778D0900000062AB
          4311957B070024A73F5DA5180E09A12389410D004CAE1B058181705217308877
          991B3C81885D5952479400000000000000000000A5787877817F787FA2140000
          0000007775229F370000008E3F714B403F3F3F398840090051AE1C6682A21300
          00052477AC193688AD19000000000000000000000000000077777879787A8881
          570900000000003CAA2DA03F0900008A3A786C615E5A88358D3F110052AE1B77
          8E3F05000E3313779B1B3677AC1B000000000000000000000000000076707780
          7A88888E3E05000000000017952E88412A1453A0235D752F1225942677692E14
          5DAF169296370000528E88789A18288199190000000000000000000000000000
          6F6F77807A8188943F0900000000001D922D77A27E8B92A708109F918B78A505
          6F9591869374058EA23F05001E3C363682042477AA1900000000000000000000
          000000006A6C71787981898E4A140000000000429E21480D3C623D0100000852
          6655020036063662830500889545120000000E2A290E1F70AA1B000000000000
          00000000000000004D6C6F7878818889AF33090000000B6FA70E000000000000
          000000000000000000000000000000529DAD3711050A8EA249091771AA190000
          0000000000000000000000001F6E6C7677787A8895413419141B7F881C000000
          000000000000000000000000000000000000000C8EA08C41389F7A9419001A77
          AA19000000000000000000000000000000486D6C70778081889DAC8CAA958932
          000000000000000000000000000000000000000000000000178E929E8F888821
          00001A71AB1400000000000000000000000000000000636C6C6F7778788E7677
          7058090000000000000000000000000000000000000000000000000000063B54
          5E5802000000173671000000000000000000000000000000000000656A6F7071
          7781A3462E120500000000000000000000000000000000000000000000000009
          1B84180000000000000000000000000000000000000000000000000000000000
          596A6F76707781A0A64433190E00000000000000000000000000000000000000
          050E26678E1B0000000000000000000000000000000000000000000000000000
          00000000002B6A6C6F6F7177819FA856403421140E0603000000000000000004
          0911182738A47A6A0A0000000000000000000000000000000000000000000000
          0000000000000000000008526D6F6F6F7777788EA0A5974F3F39332D27262526
          27292D333A72A58E716A20000000000000000000000000000000000000000000
          000000000000000000000000000000000C526A6F7077777878787881929FA2A2
          A2A2A3A2A2A19E92817877622801000000000000000000000000000000000000
          000000000000000000000000000000000000000000000024586F777877777777
          7770766F6C6F6F6F6F7671776354060000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000041E
          425A666C6F6F6F6F6C6A655B5131100000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        OnClick = GLBtnClick
      end
      object FractalRG: TRadioGroup
        Left = 0
        Top = 0
        Width = 289
        Height = 201
        Hint = 'Bumps and Grinds'
        HelpContext = 2453
        Caption = 'Bumps and Grinds'
        ItemIndex = 0
        Items.Strings = (
          'Sin Shadow'
          'Sin Shadow [r] [d] [g] [h] [i] (3.0, 0.3, 90, 20,{1or-1})'
          'cosine Shadow'
          'cosine Shadow[r] [d] [g] [h] [i] (3.0, 0.3, 90, 20,{1or-1})'
          'Sin Lattice (grid) [g] [h] [i] [k]  (16,8,30,30)'
          'Rotating Cube (* Rotator) (Click to Stop)'
          'Falling Column (Click to Stop)'
          'Diffusion Limited Aggregration [i] [0..5] (Click to Stop)'
          '(DLA) Bottom Center [i] [0..5] (Click to Stop)'
          '(DLA) Centroid [i] [0..5] (Click to Stop)')
        TabOrder = 0
      end
      object FHelp: TBitBtn
        Left = 32
        Top = 208
        Width = 65
        Height = 22
        HelpContext = 2450
        Kind = bkHelp
        NumGlyphs = 2
        TabOrder = 1
        OnClick = FHelpClick
      end
      object FXEdit: TEdit
        Left = 16
        Top = 240
        Width = 57
        Height = 21
        HelpContext = 2454
        TabOrder = 2
        Text = '0'
      end
      object FYEdit: TEdit
        Left = 88
        Top = 240
        Width = 57
        Height = 21
        HelpContext = 2455
        TabOrder = 3
        Text = '0'
      end
      object FZxEdit: TEdit
        Left = 160
        Top = 240
        Width = 57
        Height = 21
        HelpContext = 2456
        TabOrder = 4
        Text = '640'
      end
      object FZyEdit: TEdit
        Left = 232
        Top = 240
        Width = 57
        Height = 21
        HelpContext = 2457
        TabOrder = 5
        Text = '480'
      end
      object FrEdit: TEdit
        Left = 16
        Top = 264
        Width = 41
        Height = 21
        Hint = 'Rate'
        HelpContext = 2458
        TabOrder = 6
        Text = '3.0'
      end
      object FdEdit: TEdit
        Left = 64
        Top = 264
        Width = 41
        Height = 21
        HelpContext = 2459
        TabOrder = 7
        Text = '0.3'
      end
      object FhEdit: TEdit
        Left = 168
        Top = 264
        Width = 33
        Height = 21
        HelpContext = 2460
        TabOrder = 8
        Text = '20'
      end
      object FiEdit: TEdit
        Left = 208
        Top = 264
        Width = 33
        Height = 21
        Hint = 'Increase'
        HelpContext = 2461
        TabOrder = 9
        Text = '2'
      end
      object FgEdit: TEdit
        Left = 128
        Top = 264
        Width = 33
        Height = 21
        HelpContext = 2459
        TabOrder = 10
        Text = '90'
      end
      object FkEdit: TEdit
        Left = 256
        Top = 264
        Width = 33
        Height = 21
        HelpContext = 2460
        TabOrder = 11
        Text = '20'
      end
    end
    object CurvesTS: TTabSheet
      HelpContext = 2300
      Caption = '&Curves'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Bevel1: TBevel
        Left = 204
        Top = 8
        Width = 85
        Height = 281
      end
      object MGCVKRG: TRadioGroup
        Left = 8
        Top = 152
        Width = 97
        Height = 137
        HelpContext = 2320
        Caption = 'Von Koch'
        Items.Strings = (
          'Snowflake'
          'Gosper'
          'Koch Hex 8'
          'Koch 3'
          'Koch 8'
          'Koch 18'
          'Koch 32'
          'Koch 50')
        TabOrder = 0
        OnClick = MGCVKRGClick
      end
      object MGCPRG: TRadioGroup
        Left = 104
        Top = 8
        Width = 97
        Height = 49
        HelpContext = 2330
        Caption = 'Peano'
        ItemIndex = 0
        Items.Strings = (
          'Peano'
          'Peano Mod')
        TabOrder = 1
        OnClick = MGCPRGClick
      end
      object MGCCSRG: TRadioGroup
        Left = 8
        Top = 8
        Width = 97
        Height = 137
        HelpContext = 2310
        Caption = 'Cesaro and Snow'
        Items.Strings = (
          'Cesaro'
          'Cesaro MD'
          'Cesaro x2'
          'Polya'
          '!Polya Gosper'
          'Snow 7'
          'Snowhall'
          'Snow 13')
        TabOrder = 2
        OnClick = MGCCSRGClick
      end
      object MGCHRG: TRadioGroup
        Left = 104
        Top = 64
        Width = 97
        Height = 81
        HelpContext = 2340
        Caption = 'Hilbert'
        Items.Strings = (
          'Hilbert'
          'Hilbert 2'
          'Hilbert 3D')
        TabOrder = 3
        OnClick = MGCHRGClick
      end
      object MGCSRG: TRadioGroup
        Left = 104
        Top = 152
        Width = 97
        Height = 73
        HelpContext = 2350
        Caption = 'Sierpinski'
        Items.Strings = (
          'Sierpinski'
          'Sier Box'
          'Sier Gasket')
        TabOrder = 4
        OnClick = MGCSRGClick
      end
      object MGCDCRG: TRadioGroup
        Left = 104
        Top = 232
        Width = 97
        Height = 57
        HelpContext = 2360
        Caption = 'Dragon Curves'
        Items.Strings = (
          'H - H Dragon'
          'Twin Dragon')
        TabOrder = 5
        OnClick = MGCDCRGClick
      end
      object MGCHelp: TBitBtn
        Left = 210
        Top = 232
        Width = 71
        Height = 25
        HelpContext = 2300
        Kind = bkHelp
        NumGlyphs = 2
        TabOrder = 6
        OnClick = MGCHelpClick
      end
      object MGCOK: TBitBtn
        Left = 210
        Top = 256
        Width = 71
        Height = 25
        Hint = 'Run Display|Run Curves selection onto Display'
        HelpContext = 2300
        Caption = '&Display'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 8
        NumGlyphs = 2
        TabOrder = 7
        OnClick = MGCOKClick
      end
      object MGCurvesRG: TRadioGroup
        Left = 208
        Top = 16
        Width = 73
        Height = 217
        HelpContext = 2370
        Caption = 'Iterations'
        ItemIndex = 0
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8')
        TabOrder = 8
      end
    end
    object TurtlesTS: TTabSheet
      Hint = 'Turtle graphic lines'
      HelpContext = 3100
      Caption = 'Turt&les'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label85: TLabel
        Left = 88
        Top = 0
        Width = 193
        Height = 13
        Caption = 'PI / ??    OR Turns n circle  OR Degrees'
      end
      object Label89: TLabel
        Left = 0
        Top = 24
        Width = 65
        Height = 13
        Caption = 'Axiom (string):'
      end
      object Label90: TLabel
        Left = 16
        Top = 96
        Width = 87
        Height = 13
        Caption = 'Production rule # :'
      end
      object TFileSave: TSpeedButton
        Left = 192
        Top = 264
        Width = 24
        Height = 24
        Hint = 'Save}Save a Turtle File'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
          7700333333337777777733333333008088003333333377F73377333333330088
          88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
          000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
          FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
          99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        OnClick = TFileSaveClick
      end
      object Label101: TLabel
        Left = 8
        Top = 240
        Width = 7
        Height = 13
        Caption = 'X'
      end
      object Label102: TLabel
        Left = 80
        Top = 240
        Width = 7
        Height = 13
        Caption = 'Y'
      end
      object Label103: TLabel
        Left = 148
        Top = 240
        Width = 12
        Height = 13
        Caption = 'Zx'
      end
      object Label104: TLabel
        Left = 220
        Top = 240
        Width = 12
        Height = 13
        Caption = 'Zy'
      end
      object TMouseBtn: TSpeedButton
        Left = 216
        Top = 264
        Width = 24
        Height = 24
        Hint = 'Mouse Coordinates|View Vista X,Y Coordinates'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333337773F333333333330FFF03333333333733373F3333333330FFFFF03
          33333337F33337F3333333330FFFFF0333333337F33337F3333333330FFFFF03
          33333337FFFFF7F3333333330777770333333337777777F3333333330FF7FF03
          33333337F37F37F3333333330FF7FF03333333373F7FF7333333333330000033
          33333333777773FFF33333333330330007333333337F37777F33333333303033
          307333333373733377F33333333303333303333333F7F33337F3333333330733
          330333333F777F3337F333333370307330733333F77377FF7733333337033300
          0733333F77333777733333337033333333333337733333333333}
        NumGlyphs = 2
        OnClick = TreeMouserClick
      end
      object TClearBtn: TSpeedButton
        Left = 0
        Top = 264
        Width = 24
        Height = 24
        Hint = 'Clear all data'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
          333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
          0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333337F33333337F333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333FF7F33333337FFFBBB0FFFFFFFF0BB37777F3333333777F3BB0FFFFFFFF
          0BBB3777F3333FFF77773330FFFF000003333337F333777773333330FFFF0FF0
          33333337F3337F37F3333330FFFF0F0B33333337F3337F77FF333330FFFF003B
          B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
          3BB33773333773333773B333333B3333333B7333333733333337}
        NumGlyphs = 2
        OnClick = TClearBtnClick
      end
      object TFileOpen: TSpeedButton
        Left = 24
        Top = 264
        Width = 24
        Height = 24
        Hint = 'Load|Load a Turtle file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
          0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
          B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
          FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
          FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
          FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
          0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
          0555555555777777755555555555555555555555555555555555}
        NumGlyphs = 2
        OnClick = TFileOpenClick
      end
      object Label105: TLabel
        Left = 0
        Top = 109
        Width = 88
        Height = 13
        Caption = 'Production (string):'
      end
      object Label129: TLabel
        Left = 0
        Top = 60
        Width = 179
        Height = 13
        Caption = 'Production rule alpha(s)  (string of all) :'
      end
      object Label5: TLabel
        Left = 168
        Top = 96
        Width = 48
        Height = 13
        Caption = 'Computed'
      end
      object Label6: TLabel
        Left = 168
        Top = 109
        Width = 63
        Height = 13
        Caption = 'String Length'
      end
      object Label138: TLabel
        Left = 240
        Top = 149
        Width = 48
        Height = 13
        Caption = 'Do f level:'
      end
      object TPIEdit: TEdit
        Left = 88
        Top = 16
        Width = 41
        Height = 21
        Hint = 'Enter this or degrees'
        HelpContext = 3111
        TabOrder = 0
        Text = '3'
      end
      object TDegreeEdit: TEdit
        Left = 240
        Top = 16
        Width = 41
        Height = 21
        Hint = 'Enter this or PI'#39's divisor'
        HelpContext = 3111
        TabOrder = 1
        Text = '60'
      end
      object THelpBtn: TBitBtn
        Left = 240
        Top = 264
        Width = 24
        Height = 24
        Hint = 'Help|Turtle Help'
        HelpContext = 3100
        Glyph.Data = {
          DE010000424DDE01000000000000760000002800000024000000120000000100
          0400000000006801000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333336633
          3333333333333FF3333333330000333333364463333333333333388F33333333
          00003333333E66433333333333338F38F3333333000033333333E66333333333
          33338FF8F3333333000033333333333333333333333338833333333300003333
          3333446333333333333333FF3333333300003333333666433333333333333888
          F333333300003333333E66433333333333338F38F333333300003333333E6664
          3333333333338F38F3333333000033333333E6664333333333338F338F333333
          0000333333333E6664333333333338F338F3333300003333344333E666433333
          333F338F338F3333000033336664333E664333333388F338F338F33300003333
          E66644466643333338F38FFF8338F333000033333E6666666663333338F33888
          3338F3330000333333EE666666333333338FF33333383333000033333333EEEE
          E333333333388FFFFF8333330000333333333333333333333333388888333333
          0000}
        NumGlyphs = 2
        TabOrder = 2
        OnClick = THelpBtnClick
      end
      object TurtleOK: TBitBtn
        Left = 264
        Top = 264
        Width = 24
        Height = 24
        Hint = 'Run Display|Run Turtles selection onto Display'
        HelpContext = 3101
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 5
        NumGlyphs = 2
        TabOrder = 3
        OnClick = TurtleOKClick
      end
      object TurtleRG: TRadioGroup
        Left = 0
        Top = 144
        Width = 233
        Height = 89
        Hint = 'Level|Intensity of turtle'
        HelpContext = 3104
        Caption = 'Iterations'
        Columns = 5
        ItemIndex = 0
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15'
          '16'
          '17'
          '18'
          '19'
          '20'
          '25'
          '30'
          '60'
          '90'
          '180')
        TabOrder = 4
      end
      object TAxiomEdit: TEdit
        Left = 0
        Top = 40
        Width = 289
        Height = 21
        Hint = 'Base data|Iterations are inserted into Axiom'
        HelpContext = 3110
        TabOrder = 5
        Text = 'F'
      end
      object TProjEdit: TEdit
        Left = 0
        Top = 120
        Width = 289
        Height = 21
        Hint = 'Production rules'
        HelpContext = 3106
        TabOrder = 6
        Text = 'F-F++F-F'
      end
      object TurtleFileEdit: TEdit
        Left = 50
        Top = 264
        Width = 139
        Height = 21
        Hint = 'File Name'
        HelpContext = 3102
        TabOrder = 7
        Text = 'Turtle.FLA'
      end
      object TPruleEdit: TEdit
        Left = 104
        Top = 96
        Width = 33
        Height = 21
        Hint = 'Current  Rule'
        HelpContext = 3107
        TabOrder = 8
        Text = '1'
      end
      object TurtleUpDown: TUpDown
        Left = 137
        Top = 96
        Width = 24
        Height = 21
        Hint = 'Rotate rules|Change which Production rule is displayed'
        HelpContext = 3107
        Associate = TPruleEdit
        Min = 1
        Max = 52
        Position = 1
        TabOrder = 9
        OnClick = TurtleUpDownClick
      end
      object TXMinEdit: TEdit
        Left = 16
        Top = 240
        Width = 57
        Height = 21
        HelpContext = 3103
        TabOrder = 10
        Text = '0'
      end
      object TYMinEdit: TEdit
        Left = 88
        Top = 240
        Width = 57
        Height = 21
        HelpContext = 3103
        TabOrder = 11
        Text = '0'
      end
      object TZxmaxEdit: TEdit
        Left = 160
        Top = 240
        Width = 57
        Height = 21
        HelpContext = 3103
        TabOrder = 12
        Text = '640'
      end
      object TZymaxEdit: TEdit
        Left = 232
        Top = 240
        Width = 57
        Height = 21
        HelpContext = 3103
        TabOrder = 13
        Text = '480'
      end
      object TKanEdit: TEdit
        Left = 0
        Top = 72
        Width = 289
        Height = 21
        HelpContext = 3109
        TabOrder = 14
        Text = 'F'
      end
      object TCircledEdit: TEdit
        Left = 160
        Top = 16
        Width = 41
        Height = 21
        Hint = 'Enter this or degrees'
        HelpContext = 3111
        TabOrder = 15
        Text = '6'
      end
      object TStrLenEdit: TEdit
        Left = 232
        Top = 96
        Width = 55
        Height = 21
        HelpContext = 3108
        TabOrder = 16
        Text = '0'
      end
      object DOfLevelEdit: TEdit
        Left = 240
        Top = 168
        Width = 41
        Height = 21
        Hint = 'f Pen up|When to start using f as a Pen up Command'
        HelpContext = 3105
        TabOrder = 17
        Text = '0'
      end
    end
    object DragonTS: TTabSheet
      Caption = '&Dragons'
      ImageIndex = 13
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label25: TLabel
        Left = 232
        Top = 0
        Width = 35
        Height = 13
        Caption = 'X scale'
      end
      object Label26: TLabel
        Left = 232
        Top = 24
        Width = 35
        Height = 13
        Caption = 'Y scale'
      end
      object Label27: TLabel
        Left = 232
        Top = 48
        Width = 36
        Height = 13
        Caption = 'X offset'
      end
      object Label28: TLabel
        Left = 232
        Top = 72
        Width = 36
        Height = 13
        Caption = 'Y offset'
      end
      object Label31: TLabel
        Left = 232
        Top = 96
        Width = 27
        Height = 13
        Caption = 'Alpha'
      end
      object Label32: TLabel
        Left = 232
        Top = 120
        Width = 22
        Height = 13
        Caption = 'Beta'
      end
      object Label29: TLabel
        Left = 232
        Top = 144
        Width = 36
        Height = 13
        Caption = 'Gamma'
      end
      object Label30: TLabel
        Left = 280
        Top = 168
        Width = 7
        Height = 13
        Caption = 'P'
      end
      object Label67: TLabel
        Left = 280
        Top = 192
        Width = 8
        Height = 13
        Caption = 'Q'
      end
      object Label36: TLabel
        Left = 232
        Top = 216
        Width = 43
        Height = 13
        Caption = 'Iterations'
      end
      object Label74: TLabel
        Left = 80
        Top = 80
        Width = 90
        Height = 24
        Caption = '2D Dragon'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clCaptionText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Label79: TLabel
        Left = 128
        Top = 104
        Width = 39
        Height = 13
        Caption = '2D: A=0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clCaptionText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object FernSaver: TSpeedButton
        Left = 144
        Top = 216
        Width = 24
        Height = 22
        Hint = 'Save}Save a Vista File'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
          7700333333337777777733333333008088003333333377F73377333333330088
          88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
          000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
          FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
          99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        OnClick = FernSaverClick
      end
      object FernLoader: TSpeedButton
        Left = 0
        Top = 216
        Width = 24
        Height = 22
        Hint = 'Load|Load a Vista file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
          0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
          B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
          FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
          FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
          FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
          0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
          0555555555777777755555555555555555555555555555555555}
        NumGlyphs = 2
        OnClick = FernLoaderClick
      end
      object Label33: TLabel
        Left = 8
        Top = 8
        Width = 90
        Height = 24
        Caption = '3D Dragon'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clCaptionText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object IterationEdit: TEdit
        Left = 176
        Top = 216
        Width = 49
        Height = 21
        HelpContext = 2691
        TabOrder = 0
        Text = '26000'
      end
      object QEdit: TEdit
        Left = 176
        Top = 192
        Width = 97
        Height = 21
        HelpContext = 2690
        TabOrder = 1
        Text = '0.967049'
      end
      object PEdit: TEdit
        Left = 176
        Top = 168
        Width = 97
        Height = 21
        HelpContext = 2689
        TabOrder = 2
        Text = '1.646009'
      end
      object GammaEdit: TEdit
        Left = 176
        Top = 144
        Width = 49
        Height = 21
        HelpContext = 2688
        TabOrder = 3
        Text = '25.0'
      end
      object BetaEdit: TEdit
        Left = 176
        Top = 120
        Width = 49
        Height = 21
        HelpContext = 2687
        TabOrder = 4
        Text = '115.0'
      end
      object AlphaEdit: TEdit
        Left = 176
        Top = 96
        Width = 49
        Height = 21
        HelpContext = 2686
        TabOrder = 5
        Text = '30.0'
      end
      object OffEditY: TEdit
        Left = 176
        Top = 72
        Width = 49
        Height = 21
        HelpContext = 2685
        TabOrder = 6
        Text = '-180'
      end
      object OffEditX: TEdit
        Left = 176
        Top = 48
        Width = 49
        Height = 21
        HelpContext = 2684
        TabOrder = 7
        Text = '0'
      end
      object FDEditY: TEdit
        Left = 176
        Top = 24
        Width = 49
        Height = 21
        HelpContext = 2683
        TabOrder = 8
        Text = '50'
      end
      object FDEditX: TEdit
        Left = 176
        Top = 0
        Width = 49
        Height = 21
        HelpContext = 2682
        TabOrder = 9
        Text = '40'
      end
      object MGFernOK: TBitBtn
        Left = 2
        Top = 264
        Width = 56
        Height = 25
        Hint = 'Display Fern 2 or 3D'
        HelpContext = 2640
        Caption = '&Fern'
        Default = True
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 1
        NumGlyphs = 2
        TabOrder = 10
        OnClick = MGFernOKClick
      end
      object MGDragon3DOK: TBitBtn
        Left = 226
        Top = 264
        Width = 63
        Height = 25
        Hint = 'Display Dragon {3D (use Alpha,B,G)} or Outline'
        HelpContext = 2660
        Caption = '&Dragon'
        Default = True
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 1
        NumGlyphs = 2
        TabOrder = 11
        OnClick = MGDragon3DOKClick
      end
      object FernNameEdit: TEdit
        Left = 30
        Top = 216
        Width = 111
        Height = 21
        HelpContext = 2681
        TabOrder = 12
        Text = 'Fern.FLF or Dragon.FLD'
      end
      object DColor1: TPanel
        Left = 8
        Top = 240
        Width = 49
        Height = 17
        Hint = 'Click Me'
        HelpContext = 2601
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '1'
        Color = clRed
        TabOrder = 13
        OnClick = V1ColorboxClick
      end
      object DColor2: TPanel
        Left = 60
        Top = 240
        Width = 49
        Height = 17
        Hint = 'Drink Me'
        HelpContext = 2601
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '2'
        Color = clLime
        TabOrder = 14
        OnClick = V2ColorboxClick
      end
      object DColor3: TPanel
        Left = 172
        Top = 240
        Width = 49
        Height = 17
        Hint = 'Eat Me'
        HelpContext = 2601
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '3'
        Color = clBlue
        TabOrder = 15
        OnClick = V3ColorboxClick
      end
      object DColor4: TPanel
        Left = 224
        Top = 240
        Width = 49
        Height = 17
        Hint = 'Click Me'
        HelpContext = 2601
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '4'
        Color = clYellow
        TabOrder = 16
        OnClick = V4ColorboxClick
      end
      object BitBtn1: TBitBtn
        Left = 120
        Top = 264
        Width = 50
        Height = 25
        Hint = 'Help'
        HelpContext = 2600
        Kind = bkHelp
        NumGlyphs = 2
        TabOrder = 17
        OnClick = MGTreeHelpClick
      end
    end
    object TreeTS: TTabSheet
      HelpContext = 2600
      Caption = '&Trees'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label7: TLabel
        Left = 44
        Top = 80
        Width = 58
        Height = 13
        Caption = 'Stem Height'
      end
      object Label8: TLabel
        Left = 44
        Top = 104
        Width = 55
        Height = 13
        Caption = 'Stem Width'
      end
      object Label9: TLabel
        Left = 156
        Top = 24
        Width = 48
        Height = 13
        Caption = 'Left Alpha'
      end
      object Label10: TLabel
        Left = 156
        Top = 48
        Width = 55
        Height = 13
        Caption = 'Right Alpha'
      end
      object Label11: TLabel
        Left = 156
        Top = 80
        Width = 85
        Height = 13
        Caption = 'Left Branch Angle'
      end
      object Label12: TLabel
        Left = 156
        Top = 104
        Width = 92
        Height = 13
        Caption = 'Right Branch Angle'
      end
      object Label13: TLabel
        Left = 44
        Top = 144
        Width = 45
        Height = 13
        Caption = 'Branches'
      end
      object TreeMouser: TSpeedButton
        Left = 120
        Top = 224
        Width = 50
        Height = 25
        Hint = 'Mouse Coordinates|View Vista X,Y Coordinates'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333337773F333333333330FFF03333333333733373F3333333330FFFFF03
          33333337F33337F3333333330FFFFF0333333337F33337F3333333330FFFFF03
          33333337FFFFF7F3333333330777770333333337777777F3333333330FF7FF03
          33333337F37F37F3333333330FF7FF03333333373F7FF7333333333330000033
          33333333777773FFF33333333330330007333333337F37777F33333333303033
          307333333373733377F33333333303333303333333F7F33337F3333333330733
          330333333F777F3337F333333370307330733333F77377FF7733333337033300
          0733333F77333777733333337033333333333337733333333333}
        NumGlyphs = 2
        OnClick = TreeMouserClick
      end
      object TreeLoader: TSpeedButton
        Left = 0
        Top = 264
        Width = 24
        Height = 25
        Hint = 'Load|Load a Vista file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
          0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
          B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
          FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
          FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
          FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
          0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
          0555555555777777755555555555555555555555555555555555}
        NumGlyphs = 2
        OnClick = TreeLoaderClick
      end
      object TreeSaver: TSpeedButton
        Left = 144
        Top = 264
        Width = 24
        Height = 25
        Hint = 'Save}Save a Vista File'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
          7700333333337777777733333333008088003333333377F73377333333330088
          88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
          000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
          FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
          99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        OnClick = TreeSaverClick
      end
      object Label75: TLabel
        Left = 44
        Top = 48
        Width = 38
        Height = 13
        Caption = 'Y Offset'
      end
      object Label76: TLabel
        Left = 44
        Top = 24
        Width = 38
        Height = 13
        Caption = 'X Offset'
      end
      object Label34: TLabel
        Left = 224
        Top = 8
        Width = 61
        Height = 13
        Caption = 'Randomosity'
      end
      object MGTreeOK: TBitBtn
        Left = 226
        Top = 264
        Width = 55
        Height = 25
        Hint = 'Display Tree'
        HelpContext = 2620
        Caption = '&Tree'
        Default = True
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        ModalResult = 1
        NumGlyphs = 2
        TabOrder = 8
        OnClick = MGTreeOKClick
      end
      object TreeSHEdit: TEdit
        Left = 0
        Top = 77
        Width = 41
        Height = 21
        HelpContext = 2623
        TabOrder = 0
        Text = '90'
      end
      object TreeSWEdit: TEdit
        Left = 0
        Top = 105
        Width = 41
        Height = 21
        HelpContext = 2624
        TabOrder = 1
        Text = '20'
      end
      object TreeLAEdit: TEdit
        Left = 112
        Top = 21
        Width = 41
        Height = 21
        HelpContext = 2625
        TabOrder = 2
        Text = '2.0'
      end
      object TreeRAEdit: TEdit
        Left = 112
        Top = 49
        Width = 41
        Height = 21
        HelpContext = 2626
        TabOrder = 3
        Text = '2.2'
      end
      object TreeLBAEdit: TEdit
        Left = 112
        Top = 77
        Width = 41
        Height = 21
        HelpContext = 2627
        TabOrder = 4
        Text = '20'
      end
      object TreeRBAEdit: TEdit
        Left = 112
        Top = 105
        Width = 41
        Height = 21
        HelpContext = 2628
        TabOrder = 5
        Text = '28'
      end
      object TreeRLEdit: TEdit
        Left = 0
        Top = 141
        Width = 41
        Height = 21
        HelpContext = 2629
        TabOrder = 6
        Text = '14'
      end
      object MGTreeHelp: TBitBtn
        Left = 176
        Top = 264
        Width = 50
        Height = 25
        Hint = 'Help'
        HelpContext = 2600
        Kind = bkHelp
        NumGlyphs = 2
        TabOrder = 7
        OnClick = MGTreeHelpClick
      end
      object TDColor1: TPanel
        Left = 216
        Top = 224
        Width = 49
        Height = 17
        Hint = 'Click Me'
        HelpContext = 2601
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '1'
        Color = clRed
        TabOrder = 9
        OnClick = V1ColorboxClick
      end
      object TDColor4: TPanel
        Left = 216
        Top = 168
        Width = 49
        Height = 17
        Hint = 'Click Me'
        HelpContext = 2601
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '4'
        Color = clYellow
        TabOrder = 10
        OnClick = V4ColorboxClick
      end
      object TDColor3: TPanel
        Left = 216
        Top = 187
        Width = 49
        Height = 17
        Hint = 'Eat Me'
        HelpContext = 2601
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '3'
        Color = clBlue
        TabOrder = 11
        OnClick = V3ColorboxClick
      end
      object TDColor2: TPanel
        Left = 216
        Top = 205
        Width = 49
        Height = 17
        Hint = 'Drink Me'
        HelpContext = 2601
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '2'
        Color = clLime
        TabOrder = 12
        OnClick = V2ColorboxClick
      end
      object TreeNameEdit: TEdit
        Left = 32
        Top = 264
        Width = 111
        Height = 21
        HelpContext = 2630
        TabOrder = 13
        Text = 'Tree.FLT'
      end
      object TreeYOEdit: TEdit
        Left = 0
        Top = 48
        Width = 41
        Height = 21
        HelpContext = 2622
        TabOrder = 14
        Text = '-135'
      end
      object TreeXOEdit: TEdit
        Left = 0
        Top = 19
        Width = 41
        Height = 21
        HelpContext = 2621
        TabOrder = 15
        Text = '0'
      end
      object Edit10: TEdit
        Left = 256
        Top = 104
        Width = 33
        Height = 21
        TabOrder = 16
        Text = '10'
      end
      object Edit28: TEdit
        Left = 256
        Top = 80
        Width = 33
        Height = 21
        TabOrder = 17
        Text = '10'
      end
      object Edit29: TEdit
        Left = 256
        Top = 48
        Width = 33
        Height = 21
        TabOrder = 18
        Text = '10'
      end
      object Edit30: TEdit
        Left = 256
        Top = 24
        Width = 33
        Height = 21
        TabOrder = 19
        Text = '10'
      end
      object RadioGroup1: TRadioGroup
        Left = 16
        Top = 184
        Width = 185
        Height = 33
        Caption = 'Tree Colors'
        Columns = 2
        ItemIndex = 1
        Items.Strings = (
          '16 Colors'
          '4 Colors:')
        TabOrder = 20
      end
    end
    object Sky2DTS: TTabSheet
      HelpContext = 2700
      Caption = '&Sky 2D'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Bevel2: TBevel
        Left = 8
        Top = 16
        Width = 265
        Height = 113
        Hint = 'Arrays 0..3 = 4 levels'
      end
      object Label58: TLabel
        Left = 16
        Top = 22
        Width = 6
        Height = 13
        Caption = 'a'
      end
      object Label59: TLabel
        Left = 80
        Top = 22
        Width = 6
        Height = 13
        Caption = 'b'
      end
      object Label60: TLabel
        Left = 16
        Top = 54
        Width = 6
        Height = 13
        Caption = 'c'
      end
      object Label64: TLabel
        Left = 80
        Top = 54
        Width = 6
        Height = 13
        Caption = 'd'
      end
      object Label65: TLabel
        Left = 208
        Top = 24
        Width = 6
        Height = 13
        Caption = 'e'
      end
      object Label66: TLabel
        Left = 208
        Top = 56
        Width = 3
        Height = 13
        Caption = 'f'
      end
      object Label68: TLabel
        Left = 208
        Top = 88
        Width = 6
        Height = 13
        Caption = 'p'
      end
      object Label69: TLabel
        Left = 64
        Top = 112
        Width = 18
        Height = 13
        Caption = '0..3'
      end
      object Label70: TLabel
        Left = 16
        Top = 144
        Width = 37
        Height = 13
        Caption = 'X Scale'
      end
      object Label71: TLabel
        Left = 80
        Top = 144
        Width = 37
        Height = 13
        Caption = 'Y Scale'
      end
      object Label72: TLabel
        Left = 144
        Top = 144
        Width = 38
        Height = 13
        Caption = 'X Offset'
      end
      object Label73: TLabel
        Left = 208
        Top = 144
        Width = 38
        Height = 13
        Caption = 'Y Offset'
      end
      object Label77: TLabel
        Left = 208
        Top = 180
        Width = 36
        Height = 13
        Caption = 'Horizon'
      end
      object Label78: TLabel
        Left = 128
        Top = 200
        Width = 43
        Height = 13
        Caption = 'Iterations'
      end
      object SkyMouser: TSpeedButton
        Left = 128
        Top = 240
        Width = 49
        Height = 25
        Hint = 'Mouse Coordinates|View Vista X,Y Coordinates'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333337773F333333333330FFF03333333333733373F3333333330FFFFF03
          33333337F33337F3333333330FFFFF0333333337F33337F3333333330FFFFF03
          33333337FFFFF7F3333333330777770333333337777777F3333333330FF7FF03
          33333337F37F37F3333333330FF7FF03333333373F7FF7333333333330000033
          33333333777773FFF33333333330330007333333337F37777F33333333303033
          307333333373733377F33333333303333303333333F7F33337F3333333330733
          330333333F777F3337F333333370307330733333F77377FF7733333337033300
          0733333F77333777733333337033333333333337733333333333}
        NumGlyphs = 2
        OnClick = TreeMouserClick
      end
      object SkyClear: TSpeedButton
        Left = 0
        Top = 264
        Width = 25
        Height = 22
        Hint = 'Clear all data'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
          333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
          0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333337F33333337F333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333FF7F33333337FFFBBB0FFFFFFFF0BB37777F3333333777F3BB0FFFFFFFF
          0BBB3777F3333FFF77773330FFFF000003333337F333777773333330FFFF0FF0
          33333337F3337F37F3333330FFFF0F0B33333337F3337F77FF333330FFFF003B
          B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
          3BB33773333773333773B333333B3333333B7333333733333337}
        NumGlyphs = 2
        OnClick = SkyClearClick
      end
      object SkyLoader: TSpeedButton
        Left = 40
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Load|Load a Vista file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
          0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
          B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
          FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
          FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
          FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
          0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
          0555555555777777755555555555555555555555555555555555}
        NumGlyphs = 2
        OnClick = SkyLoaderClick
      end
      object SkySaver: TSpeedButton
        Left = 192
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Save}Save a Vista File'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
          7700333333337777777733333333008088003333333377F73377333333330088
          88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
          000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
          FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
          99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        OnClick = SkySaverClick
      end
      object SkyShow: TSpeedButton
        Left = 256
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Run Display|Run Sky 2D selection onto Display'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        NumGlyphs = 2
        OnClick = SkyShowClick
      end
      object SkyHelp: TSpeedButton
        Left = 224
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Help|Vista Help on command'
        Glyph.Data = {
          CE070000424DCE07000000000000360000002800000024000000120000000100
          1800000000009807000000000000000000000000000000000000008080008080
          0080800080800080800080800080800080808080008080000080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080FFFFFFFFFFFF008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080808080008000
          0080000080800000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080808080808080FFFFFF008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          80008080008080FFFF0080800080800080000000808000808000808000808000
          8080008080008080008080008080008080008080008080008080808080FFFFFF
          008080808080FFFFFF0080800080800080800080800080800080800080800080
          80008080008080008080008080008080008080008080FFFF0080800080800000
          8080008080008080008080008080008080008080008080008080008080008080
          008080008080808080FFFFFFFFFFFF808080FFFFFF0080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080808080808080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080800000800000808000008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          80008080FFFFFFFFFFFF00808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080808000808000808000
          8000000080800080800080800080800080800080800080800080800080800080
          80008080008080008080008080808080808080808080FFFFFF00808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          008080FFFF008080008080008000000080800080800080800080800080800080
          80008080008080008080008080008080008080008080808080FFFFFF00808080
          8080FFFFFF008080008080008080008080008080008080008080008080008080
          008080008080008080008080008080FFFF008080008080008080008000000080
          8000808000808000808000808000808000808000808000808000808000808000
          8080808080FFFFFF008080808080FFFFFF008080008080008080008080008080
          008080008080008080008080008080008080008080008080008080008080FFFF
          0080800080800080800080000000808000808000808000808000808000808000
          8080008080008080008080008080808080FFFFFF008080008080808080FFFFFF
          0080800080800080800080800080800080800080800080800080800080800080
          80008080008080008080008080FFFF0080800080800080800080000000808000
          8080008080008080008080008080008080008080008080008080008080808080
          FFFFFF008080008080808080FFFFFF0080800080800080800080800080800080
          80008080008080008080008080800000800000008080008080008080FFFF0080
          8000808000808000800000008080008080008080008080008080008080008080
          008080FFFFFF008080008080808080FFFFFF008080008080808080FFFFFF0080
          8000808000808000808000808000808000808000808080800080800080800080
          0000008080008080008080FFFF00808000808000800000008080008080008080
          008080008080008080008080808080808080FFFFFF008080008080808080FFFF
          FF008080008080808080FFFFFF00808000808000808000808000808000808000
          8080FFFF00808000808000808000800000800000800000808000808000808000
          800000008080008080008080008080008080008080808080FFFFFF0080808080
          80FFFFFFFFFFFFFFFFFF808080008080008080808080FFFFFF00808000808000
          8080008080008080008080008080008080FFFF00808000808000808000808000
          8080008080008080008080008080000080800080800080800080800080800080
          80808080FFFFFF00808000808080808080808080808000808000808000808080
          8080FFFFFF008080008080008080008080008080008080008080008080008080
          FFFF00FFFF008080008080008080008080008080008080000080800080800080
          80008080008080008080008080008080808080FFFFFFFFFFFF00808000808000
          8080008080008080008080808080008080008080008080008080008080008080
          008080008080008080008080008080008080FFFF00FFFF00FFFF00FFFF00FFFF
          0000808000808000808000808000808000808000808000808000808000808080
          8080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080808080808080808080808080808080
          008080008080008080008080008080008080}
        NumGlyphs = 2
        OnClick = SkyHelpClick
      end
      object SkyE: TEdit
        Left = 208
        Top = 35
        Width = 57
        Height = 21
        HelpContext = 2715
        TabOrder = 0
        Text = '0'
      end
      object SkyF: TEdit
        Left = 208
        Top = 70
        Width = 57
        Height = 21
        HelpContext = 2716
        TabOrder = 1
        Text = '0'
      end
      object SkyB: TEdit
        Left = 80
        Top = 35
        Width = 57
        Height = 21
        HelpContext = 2712
        TabOrder = 2
        Text = '0'
      end
      object SkyA: TEdit
        Left = 16
        Top = 35
        Width = 57
        Height = 21
        HelpContext = 2711
        TabOrder = 3
        Text = '0'
      end
      object SkyP: TEdit
        Left = 208
        Top = 102
        Width = 57
        Height = 21
        HelpContext = 2717
        TabOrder = 4
        Text = '0.01'
      end
      object SkyD: TEdit
        Left = 80
        Top = 70
        Width = 57
        Height = 21
        HelpContext = 2714
        TabOrder = 5
        Text = '0'
      end
      object SkyC: TEdit
        Left = 16
        Top = 70
        Width = 57
        Height = 21
        HelpContext = 2713
        TabOrder = 6
        Text = '0'
      end
      object Edit33: TEdit
        Left = 88
        Top = 104
        Width = 25
        Height = 21
        HelpContext = 2718
        TabOrder = 7
        Text = '0'
      end
      object SkyUpDown: TUpDown
        Left = 113
        Top = 104
        Width = 24
        Height = 21
        Hint = 'Level'
        HelpContext = 2719
        Associate = Edit33
        Max = 3
        Orientation = udHorizontal
        TabOrder = 8
        OnClick = SkyUpDownClick
      end
      object SkyYOff: TEdit
        Left = 208
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 2724
        TabOrder = 9
        Text = '2'
      end
      object SkyXOff: TEdit
        Left = 144
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 2723
        TabOrder = 10
        Text = '2'
      end
      object SkyYScale: TEdit
        Left = 80
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 2722
        TabOrder = 11
        Text = '2'
      end
      object SkyXScale: TEdit
        Left = 16
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 2721
        TabOrder = 12
        Text = '2'
      end
      object SkyHorizon: TEdit
        Left = 208
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 2725
        TabOrder = 13
        Text = '2'
      end
      object SkyIterations: TEdit
        Left = 128
        Top = 216
        Width = 49
        Height = 21
        HelpContext = 2726
        TabOrder = 14
        Text = '26000'
      end
      object SkyColor3: TPanel
        Left = 180
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 2729
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '3'
        Color = clBlue
        TabOrder = 15
        OnClick = V3ColorboxClick
      end
      object SkyColor4: TPanel
        Left = 232
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 2729
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '4'
        Color = clYellow
        TabOrder = 16
        OnClick = V4ColorboxClick
      end
      object SkyColor2: TPanel
        Left = 68
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 2729
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '2'
        Color = clLime
        TabOrder = 17
        OnClick = V2ColorboxClick
      end
      object SkyColor1: TPanel
        Left = 8
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 2729
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '1'
        Color = clRed
        TabOrder = 18
        OnClick = V1ColorboxClick
      end
      object SkyNameEdit: TEdit
        Left = 69
        Top = 264
        Width = 122
        Height = 21
        Hint = 'Numb Files (*.VLN)'
        HelpContext = 2728
        TabOrder = 19
        Text = 'Sky2D.FLS'
      end
    end
    object Numb3DTS: TTabSheet
      HelpContext = 2800
      Caption = '&Numb 3D'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Bevel3: TBevel
        Left = 8
        Top = 0
        Width = 273
        Height = 143
        Hint = 'Array Levels 0..3 = 4'
      end
      object ClearHeaded: TSpeedButton
        Left = 0
        Top = 264
        Width = 25
        Height = 22
        Hint = 'Clear all data'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
          333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
          0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333337F33333337F333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333FF7F33333337FFFBBB0FFFFFFFF0BB37777F3333333777F3BB0FFFFFFFF
          0BBB3777F3333FFF77773330FFFF000003333337F333777773333330FFFF0FF0
          33333337F3337F37F3333330FFFF0F0B33333337F3337F77FF333330FFFF003B
          B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
          3BB33773333773333773B333333B3333333B7333333733333337}
        NumGlyphs = 2
        OnClick = ClearHeadedClick
      end
      object NFOpen: TSpeedButton
        Left = 40
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Load|Load a Vista file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
          0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
          B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
          FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
          FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
          FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
          0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
          0555555555777777755555555555555555555555555555555555}
        NumGlyphs = 2
        OnClick = NFOpenClick
      end
      object NFHelp: TSpeedButton
        Left = 224
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Help|Vista Help on command'
        Glyph.Data = {
          CE070000424DCE07000000000000360000002800000024000000120000000100
          1800000000009807000000000000000000000000000000000000008080008080
          0080800080800080800080800080800080808080008080000080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080FFFFFFFFFFFF008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080808080008000
          0080000080800000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080808080808080FFFFFF008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          80008080008080FFFF0080800080800080000000808000808000808000808000
          8080008080008080008080008080008080008080008080008080808080FFFFFF
          008080808080FFFFFF0080800080800080800080800080800080800080800080
          80008080008080008080008080008080008080008080FFFF0080800080800000
          8080008080008080008080008080008080008080008080008080008080008080
          008080008080808080FFFFFFFFFFFF808080FFFFFF0080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080808080808080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080800000800000808000008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          80008080FFFFFFFFFFFF00808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080808000808000808000
          8000000080800080800080800080800080800080800080800080800080800080
          80008080008080008080008080808080808080808080FFFFFF00808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          008080FFFF008080008080008000000080800080800080800080800080800080
          80008080008080008080008080008080008080008080808080FFFFFF00808080
          8080FFFFFF008080008080008080008080008080008080008080008080008080
          008080008080008080008080008080FFFF008080008080008080008000000080
          8000808000808000808000808000808000808000808000808000808000808000
          8080808080FFFFFF008080808080FFFFFF008080008080008080008080008080
          008080008080008080008080008080008080008080008080008080008080FFFF
          0080800080800080800080000000808000808000808000808000808000808000
          8080008080008080008080008080808080FFFFFF008080008080808080FFFFFF
          0080800080800080800080800080800080800080800080800080800080800080
          80008080008080008080008080FFFF0080800080800080800080000000808000
          8080008080008080008080008080008080008080008080008080008080808080
          FFFFFF008080008080808080FFFFFF0080800080800080800080800080800080
          80008080008080008080008080800000800000008080008080008080FFFF0080
          8000808000808000800000008080008080008080008080008080008080008080
          008080FFFFFF008080008080808080FFFFFF008080008080808080FFFFFF0080
          8000808000808000808000808000808000808000808080800080800080800080
          0000008080008080008080FFFF00808000808000800000008080008080008080
          008080008080008080008080808080808080FFFFFF008080008080808080FFFF
          FF008080008080808080FFFFFF00808000808000808000808000808000808000
          8080FFFF00808000808000808000800000800000800000808000808000808000
          800000008080008080008080008080008080008080808080FFFFFF0080808080
          80FFFFFFFFFFFFFFFFFF808080008080008080808080FFFFFF00808000808000
          8080008080008080008080008080008080FFFF00808000808000808000808000
          8080008080008080008080008080000080800080800080800080800080800080
          80808080FFFFFF00808000808080808080808080808000808000808000808080
          8080FFFFFF008080008080008080008080008080008080008080008080008080
          FFFF00FFFF008080008080008080008080008080008080000080800080800080
          80008080008080008080008080008080808080FFFFFFFFFFFF00808000808000
          8080008080008080008080808080008080008080008080008080008080008080
          008080008080008080008080008080008080FFFF00FFFF00FFFF00FFFF00FFFF
          0000808000808000808000808000808000808000808000808000808000808080
          8080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080808080808080808080808080808080
          008080008080008080008080008080008080}
        NumGlyphs = 2
        OnClick = NFHelpClick
      end
      object NFSave: TSpeedButton
        Left = 192
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Save}Save a Vista File'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
          7700333333337777777733333333008088003333333377F73377333333330088
          88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
          000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
          FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
          99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        OnClick = NFSaveClick
      end
      object NFRun: TSpeedButton
        Left = 256
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Run Display|Run Numb 3D selection onto Display'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        NumGlyphs = 2
        OnClick = NFRunClick
      end
      object Label37: TLabel
        Left = 16
        Top = -2
        Width = 6
        Height = 13
        Caption = 'a'
        Transparent = True
      end
      object Label38: TLabel
        Left = 72
        Top = -2
        Width = 6
        Height = 13
        Caption = 'b'
        Transparent = True
      end
      object Label39: TLabel
        Left = 136
        Top = -2
        Width = 6
        Height = 13
        Caption = 'c'
        Transparent = True
      end
      object Label40: TLabel
        Left = 16
        Top = 30
        Width = 6
        Height = 13
        Caption = 'd'
      end
      object Label41: TLabel
        Left = 80
        Top = 66
        Width = 6
        Height = 13
        Caption = 'h'
      end
      object Label42: TLabel
        Left = 144
        Top = 176
        Width = 36
        Height = 13
        Caption = 'Gamma'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label43: TLabel
        Left = 80
        Top = 176
        Width = 22
        Height = 13
        Caption = 'Beta'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label45: TLabel
        Left = 216
        Top = 104
        Width = 50
        Height = 13
        Caption = 'p robability'
      end
      object Label46: TLabel
        Left = 16
        Top = 66
        Width = 6
        Height = 13
        Caption = 'g'
      end
      object Label47: TLabel
        Left = 136
        Top = 32
        Width = 3
        Height = 13
        Caption = 'f'
      end
      object Label48: TLabel
        Left = 72
        Top = 32
        Width = 6
        Height = 13
        Caption = 'e'
      end
      object Label49: TLabel
        Left = 216
        Top = 66
        Width = 3
        Height = 13
        Caption = 'r'
      end
      object Label50: TLabel
        Left = 216
        Top = 28
        Width = 6
        Height = 13
        Caption = 'q'
      end
      object Label51: TLabel
        Left = 216
        Top = -4
        Width = 6
        Height = 13
        Caption = 'n'
        Transparent = True
      end
      object Label52: TLabel
        Left = 136
        Top = 66
        Width = 8
        Height = 13
        Caption = 'm'
      end
      object Label53: TLabel
        Left = 208
        Top = 144
        Width = 38
        Height = 13
        Caption = 'Y Offset'
      end
      object Label54: TLabel
        Left = 144
        Top = 144
        Width = 38
        Height = 13
        Caption = 'X Offset'
      end
      object Label55: TLabel
        Left = 80
        Top = 144
        Width = 37
        Height = 13
        Caption = 'Y Scale'
      end
      object Label56: TLabel
        Left = 16
        Top = 144
        Width = 37
        Height = 13
        Caption = 'X Scale'
      end
      object Label57: TLabel
        Left = 168
        Top = 216
        Width = 42
        Height = 13
        Caption = 'Horizon'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object Label61: TLabel
        Left = 208
        Top = 176
        Width = 43
        Height = 13
        Caption = 'Iterations'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label62: TLabel
        Left = 24
        Top = 126
        Width = 50
        Height = 13
        Caption = 'Level: 0..3'
      end
      object NumbMouser: TSpeedButton
        Left = 128
        Top = 240
        Width = 49
        Height = 25
        Hint = 'Mouse Coordinates|View Vista X,Y Coordinates'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333337773F333333333330FFF03333333333733373F3333333330FFFFF03
          33333337F33337F3333333330FFFFF0333333337F33337F3333333330FFFFF03
          33333337FFFFF7F3333333330777770333333337777777F3333333330FF7FF03
          33333337F37F37F3333333330FF7FF03333333373F7FF7333333333330000033
          33333333777773FFF33333333330330007333333337F37777F33333333303033
          307333333373733377F33333333303333303333333F7F33337F3333333330733
          330333333F777F3337F333333370307330733333F77377FF7733333337033300
          0733333F77333777733333337033333333333337733333333333}
        NumGlyphs = 2
        OnClick = TreeMouserClick
      end
      object Label44: TLabel
        Left = 16
        Top = 176
        Width = 27
        Height = 13
        Caption = 'Alpha'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label80: TLabel
        Left = 16
        Top = 216
        Width = 18
        Height = 13
        Caption = 'Py'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object Label81: TLabel
        Left = 92
        Top = 216
        Width = 19
        Height = 13
        Caption = 'Qx'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object NFb: TEdit
        Left = 76
        Top = 12
        Width = 37
        Height = 21
        HelpContext = 2712
        TabOrder = 0
        Text = '0'
      end
      object NFe: TEdit
        Left = 76
        Top = 46
        Width = 37
        Height = 21
        HelpContext = 2715
        TabOrder = 1
        Text = '0'
      end
      object NFd: TEdit
        Left = 16
        Top = 46
        Width = 41
        Height = 21
        HelpContext = 2714
        TabOrder = 2
        Text = '0'
      end
      object NFg: TEdit
        Left = 16
        Top = 80
        Width = 41
        Height = 21
        HelpContext = 2821
        TabOrder = 3
        Text = '0'
      end
      object NFp: TEdit
        Left = 216
        Top = 118
        Width = 41
        Height = 21
        HelpContext = 2717
        TabOrder = 4
        Text = '0'
      end
      object NFf: TEdit
        Left = 136
        Top = 46
        Width = 41
        Height = 21
        HelpContext = 2716
        TabOrder = 5
        Text = '0'
      end
      object NFa: TEdit
        Left = 16
        Top = 12
        Width = 41
        Height = 21
        HelpContext = 2711
        TabOrder = 6
        Text = '0'
      end
      object NFc: TEdit
        Left = 136
        Top = 12
        Width = 41
        Height = 21
        HelpContext = 2713
        TabOrder = 7
        Text = '0'
      end
      object NFn: TEdit
        Left = 216
        Top = 11
        Width = 41
        Height = 21
        HelpContext = 2824
        TabOrder = 8
        Text = '0'
      end
      object NFm: TEdit
        Left = 136
        Top = 80
        Width = 41
        Height = 21
        HelpContext = 2823
        TabOrder = 9
        Text = '0'
      end
      object NFq: TEdit
        Left = 216
        Top = 46
        Width = 41
        Height = 21
        HelpContext = 2825
        TabOrder = 10
        Text = '0'
      end
      object NFr: TEdit
        Left = 216
        Top = 80
        Width = 41
        Height = 21
        HelpContext = 2826
        TabOrder = 11
        Text = '0'
      end
      object NFYScale: TEdit
        Left = 80
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 2722
        TabOrder = 12
        Text = '50'
      end
      object NFXScale: TEdit
        Left = 16
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 2721
        TabOrder = 13
        Text = '40'
      end
      object NFXOff: TEdit
        Left = 144
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 2723
        TabOrder = 14
        Text = '60'
      end
      object NFYOff: TEdit
        Left = 208
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 2724
        TabOrder = 15
        Text = '-180'
      end
      object NFBeta: TEdit
        Left = 80
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 2832
        TabOrder = 16
        Text = '70'
      end
      object NFAlpha: TEdit
        Left = 16
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 2831
        TabOrder = 17
        Text = '15'
      end
      object NFGamma: TEdit
        Left = 144
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 2833
        TabOrder = 18
        Text = '20'
      end
      object NFHorizon: TEdit
        Left = 208
        Top = 216
        Width = 57
        Height = 21
        HelpContext = 2725
        TabOrder = 19
        Text = '12'
      end
      object NFh: TEdit
        Left = 76
        Top = 80
        Width = 37
        Height = 21
        HelpContext = 2822
        TabOrder = 20
        Text = '0'
      end
      object NumbFileEdit: TEdit
        Left = 70
        Top = 264
        Width = 121
        Height = 21
        Hint = 'Numb Files (*.VLN)'
        HelpContext = 2840
        TabOrder = 21
        Text = 'NumbFile.FLN'
      end
      object NFColor2: TPanel
        Left = 68
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 2729
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '2'
        Color = clLime
        TabOrder = 22
        OnClick = V2ColorboxClick
      end
      object NFColor1: TPanel
        Left = 8
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 2729
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '1'
        Color = clRed
        TabOrder = 23
        OnClick = V1ColorboxClick
      end
      object NFColor3: TPanel
        Left = 180
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 2729
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '3'
        Color = clBlue
        TabOrder = 24
        OnClick = V3ColorboxClick
      end
      object NFColor4: TPanel
        Left = 232
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 2729
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '4'
        Color = clYellow
        TabOrder = 25
        OnClick = V4ColorboxClick
      end
      object NumbIterations: TEdit
        Left = 208
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 2726
        TabOrder = 26
        Text = '26000'
      end
      object NumbFunLevel: TEdit
        Left = 80
        Top = 120
        Width = 25
        Height = 21
        HelpContext = 2718
        TabOrder = 27
        Text = '0'
      end
      object NumbUpDown: TUpDown
        Left = 105
        Top = 120
        Width = 24
        Height = 21
        Hint = 'Change Array|Go Slow ! , I have a lot of work to do'
        HelpContext = 2719
        Associate = NumbFunLevel
        Max = 3
        Orientation = udHorizontal
        TabOrder = 28
        OnClick = NumbUpDownClick
      end
      object NFPy: TEdit
        Left = 32
        Top = 216
        Width = 57
        Height = 21
        HelpContext = 2834
        TabOrder = 29
        Text = '26000'
      end
      object NFQx: TEdit
        Left = 108
        Top = 216
        Width = 57
        Height = 21
        HelpContext = 2835
        TabOrder = 30
        Text = '26000'
      end
      object UpDowna: TUpDown
        Left = 57
        Top = 12
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 31
        OnClick = UpDownaClick
      end
      object UpDownd: TUpDown
        Left = 57
        Top = 46
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 32
        OnClick = UpDowndClick
      end
      object UpDowng: TUpDown
        Left = 57
        Top = 80
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 33
        OnClick = UpDowngClick
      end
      object UpDownh: TUpDown
        Left = 113
        Top = 80
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 34
        OnClick = UpDownhClick
      end
      object UpDowne: TUpDown
        Left = 113
        Top = 46
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 35
        OnClick = UpDowneClick
      end
      object UpDownb: TUpDown
        Left = 113
        Top = 12
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 36
        OnClick = UpDownbClick
      end
      object UpDownc: TUpDown
        Left = 177
        Top = 12
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 37
        OnClick = UpDowncClick
      end
      object UpDownf: TUpDown
        Left = 177
        Top = 46
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 38
        OnClick = UpDownfClick
      end
      object UpDownm: TUpDown
        Left = 177
        Top = 80
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 39
        OnClick = UpDownmClick
      end
      object UpDownr: TUpDown
        Left = 257
        Top = 80
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 40
        OnClick = UpDownrClick
      end
      object UpDownq: TUpDown
        Left = 257
        Top = 46
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 41
        OnClick = UpDownqClick
      end
      object UpDownn: TUpDown
        Left = 257
        Top = 11
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 42
        OnClick = UpDownnClick
      end
      object UpDownp: TUpDown
        Left = 257
        Top = 118
        Width = 16
        Height = 21
        Min = -100
        TabOrder = 43
        OnClick = UpDownpClick
      end
      object NumbRemCB: TCheckBox
        Left = 16
        Top = 102
        Width = 121
        Height = 17
        Hint = 'by 0.1 or 0.01|Checked changes 0.1 else 0.01'
        Caption = 'RapidEyeMovement'
        Checked = True
        State = cbChecked
        TabOrder = 44
        OnClick = NumbRemCBClick
      end
      object NumbAwake: TCheckBox
        Left = 136
        Top = 102
        Width = 57
        Height = 17
        Caption = 'Awake'
        TabOrder = 45
        OnClick = NumbAwakeClick
      end
    end
    object Collage: TTabSheet
      HelpContext = 3000
      Caption = 'Collag&e'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 16
        Top = -2
        Width = 6
        Height = 13
        Caption = 'a'
      end
      object Label86: TLabel
        Left = 72
        Top = -2
        Width = 6
        Height = 13
        Caption = 'b'
      end
      object Label106: TLabel
        Left = 136
        Top = -2
        Width = 6
        Height = 13
        Caption = 'c'
      end
      object Label107: TLabel
        Left = 216
        Top = -4
        Width = 6
        Height = 13
        Caption = 'n'
      end
      object Label108: TLabel
        Left = 16
        Top = 30
        Width = 6
        Height = 13
        Caption = 'd'
      end
      object Label109: TLabel
        Left = 72
        Top = 32
        Width = 6
        Height = 13
        Caption = 'e'
      end
      object Label110: TLabel
        Left = 128
        Top = 32
        Width = 3
        Height = 13
        Caption = 'f'
      end
      object Label111: TLabel
        Left = 216
        Top = 28
        Width = 6
        Height = 13
        Caption = 'q'
      end
      object Label112: TLabel
        Left = 16
        Top = 66
        Width = 6
        Height = 13
        Caption = 'g'
      end
      object Label113: TLabel
        Left = 80
        Top = 66
        Width = 6
        Height = 13
        Caption = 'h'
      end
      object Label114: TLabel
        Left = 136
        Top = 66
        Width = 8
        Height = 13
        Caption = 'm'
      end
      object Label115: TLabel
        Left = 216
        Top = 66
        Width = 3
        Height = 13
        Caption = 'r'
      end
      object Label116: TLabel
        Left = 216
        Top = 104
        Width = 6
        Height = 13
        Caption = 'p'
      end
      object Label117: TLabel
        Left = 120
        Top = 118
        Width = 18
        Height = 13
        Caption = '0..3'
      end
      object Label118: TLabel
        Left = 16
        Top = 144
        Width = 37
        Height = 13
        Caption = 'X Scale'
      end
      object Label119: TLabel
        Left = 80
        Top = 144
        Width = 37
        Height = 13
        Caption = 'Y Scale'
      end
      object Label120: TLabel
        Left = 144
        Top = 144
        Width = 38
        Height = 13
        Caption = 'X Offset'
      end
      object Label121: TLabel
        Left = 208
        Top = 144
        Width = 38
        Height = 13
        Caption = 'Y Offset'
      end
      object Label122: TLabel
        Left = 16
        Top = 176
        Width = 27
        Height = 13
        Caption = 'Alpha'
      end
      object Label123: TLabel
        Left = 80
        Top = 176
        Width = 22
        Height = 13
        Caption = 'Beta'
      end
      object Label124: TLabel
        Left = 144
        Top = 176
        Width = 36
        Height = 13
        Caption = 'Gamma'
      end
      object Label125: TLabel
        Left = 208
        Top = 180
        Width = 36
        Height = 13
        Caption = 'Horizon'
      end
      object Label126: TLabel
        Left = 16
        Top = 216
        Width = 12
        Height = 13
        Caption = 'Py'
      end
      object Label127: TLabel
        Left = 92
        Top = 216
        Width = 13
        Height = 13
        Caption = 'Qx'
      end
      object Label128: TLabel
        Left = 168
        Top = 216
        Width = 43
        Height = 13
        Caption = 'Iterations'
      end
      object SpeedButton1: TSpeedButton
        Left = 128
        Top = 240
        Width = 49
        Height = 25
        Hint = 'Mouse Coordinates|View Vista X,Y Coordinates'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333337773F333333333330FFF03333333333733373F3333333330FFFFF03
          33333337F33337F3333333330FFFFF0333333337F33337F3333333330FFFFF03
          33333337FFFFF7F3333333330777770333333337777777F3333333330FF7FF03
          33333337F37F37F3333333330FF7FF03333333373F7FF7333333333330000033
          33333333777773FFF33333333330330007333333337F37777F33333333303033
          307333333373733377F33333333303333303333333F7F33337F3333333330733
          330333333F777F3337F333333370307330733333F77377FF7733333337033300
          0733333F77333777733333337033333333333337733333333333}
        NumGlyphs = 2
        OnClick = TreeMouserClick
      end
      object CollageClear: TSpeedButton
        Left = 0
        Top = 264
        Width = 25
        Height = 22
        Hint = 'Clear all data'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
          333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
          0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333337F33333337F333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333FF7F33333337FFFBBB0FFFFFFFF0BB37777F3333333777F3BB0FFFFFFFF
          0BBB3777F3333FFF77773330FFFF000003333337F333777773333330FFFF0FF0
          33333337F3337F37F3333330FFFF0F0B33333337F3337F77FF333330FFFF003B
          B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
          3BB33773333773333773B333333B3333333B7333333733333337}
        NumGlyphs = 2
        OnClick = CollageClearClick
      end
      object CollageOpen: TSpeedButton
        Left = 40
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Load|Load a Vista file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
          0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
          B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
          FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
          FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
          FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
          0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
          0555555555777777755555555555555555555555555555555555}
        NumGlyphs = 2
        OnClick = CollageOpenClick
      end
      object CollageSave: TSpeedButton
        Left = 192
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Save}Save a Vista File'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
          7700333333337777777733333333008088003333333377F73377333333330088
          88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
          000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
          FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
          99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        OnClick = CollageSaveClick
      end
      object CollageRun: TSpeedButton
        Left = 256
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Run Display|Run Collage selection onto Display'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        NumGlyphs = 2
        OnClick = CollageRunClick
      end
      object CollageHelp: TSpeedButton
        Left = 224
        Top = 264
        Width = 24
        Height = 22
        Hint = 'Help|Vista Help on command'
        Glyph.Data = {
          CE070000424DCE07000000000000360000002800000024000000120000000100
          1800000000009807000000000000000000000000000000000000008080008080
          0080800080800080800080800080800080808080008080000080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080FFFFFFFFFFFF008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080808080008000
          0080000080800000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080808080808080FFFFFF008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          80008080008080FFFF0080800080800080000000808000808000808000808000
          8080008080008080008080008080008080008080008080008080808080FFFFFF
          008080808080FFFFFF0080800080800080800080800080800080800080800080
          80008080008080008080008080008080008080008080FFFF0080800080800000
          8080008080008080008080008080008080008080008080008080008080008080
          008080008080808080FFFFFFFFFFFF808080FFFFFF0080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080808080808080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080800000800000808000008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          80008080FFFFFFFFFFFF00808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080808000808000808000
          8000000080800080800080800080800080800080800080800080800080800080
          80008080008080008080008080808080808080808080FFFFFF00808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          008080FFFF008080008080008000000080800080800080800080800080800080
          80008080008080008080008080008080008080008080808080FFFFFF00808080
          8080FFFFFF008080008080008080008080008080008080008080008080008080
          008080008080008080008080008080FFFF008080008080008080008000000080
          8000808000808000808000808000808000808000808000808000808000808000
          8080808080FFFFFF008080808080FFFFFF008080008080008080008080008080
          008080008080008080008080008080008080008080008080008080008080FFFF
          0080800080800080800080000000808000808000808000808000808000808000
          8080008080008080008080008080808080FFFFFF008080008080808080FFFFFF
          0080800080800080800080800080800080800080800080800080800080800080
          80008080008080008080008080FFFF0080800080800080800080000000808000
          8080008080008080008080008080008080008080008080008080008080808080
          FFFFFF008080008080808080FFFFFF0080800080800080800080800080800080
          80008080008080008080008080800000800000008080008080008080FFFF0080
          8000808000808000800000008080008080008080008080008080008080008080
          008080FFFFFF008080008080808080FFFFFF008080008080808080FFFFFF0080
          8000808000808000808000808000808000808000808080800080800080800080
          0000008080008080008080FFFF00808000808000800000008080008080008080
          008080008080008080008080808080808080FFFFFF008080008080808080FFFF
          FF008080008080808080FFFFFF00808000808000808000808000808000808000
          8080FFFF00808000808000808000800000800000800000808000808000808000
          800000008080008080008080008080008080008080808080FFFFFF0080808080
          80FFFFFFFFFFFFFFFFFF808080008080008080808080FFFFFF00808000808000
          8080008080008080008080008080008080FFFF00808000808000808000808000
          8080008080008080008080008080000080800080800080800080800080800080
          80808080FFFFFF00808000808080808080808080808000808000808000808080
          8080FFFFFF008080008080008080008080008080008080008080008080008080
          FFFF00FFFF008080008080008080008080008080008080000080800080800080
          80008080008080008080008080008080808080FFFFFFFFFFFF00808000808000
          8080008080008080008080808080008080008080008080008080008080008080
          008080008080008080008080008080008080FFFF00FFFF00FFFF00FFFF00FFFF
          0000808000808000808000808000808000808000808000808000808000808080
          8080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080808080808080808080808080808080
          008080008080008080008080008080008080}
        NumGlyphs = 2
        OnClick = CollageHelpClick
      end
      object Edit1: TEdit
        Left = 16
        Top = 12
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 0
        Text = '0'
      end
      object Edit2: TEdit
        Left = 76
        Top = 12
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 1
        Text = '0'
      end
      object Edit3: TEdit
        Left = 136
        Top = 12
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 2
        Text = '0'
      end
      object Edit4: TEdit
        Left = 216
        Top = 11
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 3
        Text = '0'
      end
      object Edit5: TEdit
        Left = 16
        Top = 46
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 4
        Text = '0'
      end
      object Edit6: TEdit
        Left = 76
        Top = 46
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 5
        Text = '0'
      end
      object Edit7: TEdit
        Left = 136
        Top = 46
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 6
        Text = '0'
      end
      object Edit8: TEdit
        Left = 216
        Top = 46
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 7
        Text = '0'
      end
      object Edit9: TEdit
        Left = 16
        Top = 80
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 8
        Text = '0'
      end
      object Edit11: TEdit
        Left = 76
        Top = 80
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 9
        Text = '0'
      end
      object Edit12: TEdit
        Left = 136
        Top = 80
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 10
        Text = '0'
      end
      object Edit13: TEdit
        Left = 216
        Top = 80
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 11
        Text = '0'
      end
      object Edit14: TEdit
        Left = 216
        Top = 118
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 12
        Text = '0.01'
      end
      object CollageUpDown: TUpDown
        Left = 169
        Top = 118
        Width = 15
        Height = 21
        Hint = 'Change Array|Go Slow ! , I have a lot of work to do'
        HelpContext = 3000
        Associate = Edit15
        Max = 3
        Orientation = udHorizontal
        TabOrder = 13
        OnClick = NumbUpDownClick
      end
      object Edit15: TEdit
        Left = 144
        Top = 118
        Width = 25
        Height = 21
        HelpContext = 3000
        TabOrder = 14
        Text = '0'
      end
      object Edit16: TEdit
        Left = 16
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 15
        Text = '40'
      end
      object Edit17: TEdit
        Left = 80
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 16
        Text = '50'
      end
      object Edit18: TEdit
        Left = 144
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 17
        Text = '60'
      end
      object Edit19: TEdit
        Left = 208
        Top = 157
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 18
        Text = '-180'
      end
      object Edit20: TEdit
        Left = 16
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 19
        Text = '15'
      end
      object Edit21: TEdit
        Left = 80
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 20
        Text = '70'
      end
      object Edit22: TEdit
        Left = 144
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 21
        Text = '20'
      end
      object Edit23: TEdit
        Left = 208
        Top = 192
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 22
        Text = '12'
      end
      object Edit24: TEdit
        Left = 32
        Top = 216
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 23
        Text = '26000'
      end
      object Edit25: TEdit
        Left = 108
        Top = 216
        Width = 57
        Height = 21
        HelpContext = 3000
        TabOrder = 24
        Text = '26000'
      end
      object Edit26: TEdit
        Left = 216
        Top = 216
        Width = 49
        Height = 21
        HelpContext = 3000
        TabOrder = 25
        Text = '26000'
      end
      object Panel1: TPanel
        Left = 8
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 3000
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '1'
        Color = clRed
        TabOrder = 26
        OnClick = V1ColorboxClick
      end
      object Panel2: TPanel
        Left = 68
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 3000
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '2'
        Color = clLime
        TabOrder = 27
        OnClick = V2ColorboxClick
      end
      object Panel3: TPanel
        Left = 180
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 3000
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '3'
        Color = clBlue
        TabOrder = 28
        OnClick = V3ColorboxClick
      end
      object Panel4: TPanel
        Left = 232
        Top = 240
        Width = 49
        Height = 17
        HelpContext = 3000
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '4'
        Color = clYellow
        TabOrder = 29
        OnClick = V4ColorboxClick
      end
      object Edit27: TEdit
        Left = 70
        Top = 264
        Width = 121
        Height = 21
        Hint = 'Numb Files (*.VLN)'
        HelpContext = 3000
        TabOrder = 30
        Text = 'Collage.FLO'
      end
    end
    object DIYTS: TTabSheet
      HelpContext = 2500
      Caption = 'DI&Y'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label14: TLabel
        Left = 200
        Top = 0
        Width = 7
        Height = 13
        Caption = 'X'
      end
      object Label15: TLabel
        Left = 200
        Top = 24
        Width = 7
        Height = 13
        Caption = 'Y'
      end
      object Label16: TLabel
        Left = 196
        Top = 46
        Width = 16
        Height = 13
        Caption = 'Wx'
        Transparent = True
      end
      object DIYSetup: TSpeedButton
        Left = 0
        Top = 144
        Width = 24
        Height = 22
        Hint = 'Set up Vista'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
          333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
          0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333337F33333337F333330FFFFFFFF07333337F33333337F333330FFFFFFFF
          07333FF7F33333337FFFBBB0FFFFFFFF0BB37777F3333333777F3BB0FFFFFFFF
          0BBB3777F3333FFF77773330FFFF000003333337F333777773333330FFFF0FF0
          33333337F3337F37F3333330FFFF0F0B33333337F3337F77FF333330FFFF003B
          B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
          3BB33773333773333773B333333B3333333B7333333733333337}
        NumGlyphs = 2
        OnClick = DIYSetupClick
      end
      object DIYAdd: TSpeedButton
        Left = 156
        Top = 144
        Width = 20
        Height = 22
        Hint = 'List Landed Object|Add Landed Object to Vista List'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33333333FF33333333FF333993333333300033377F3333333777333993333333
          300033F77FFF3333377739999993333333333777777F3333333F399999933333
          33003777777333333377333993333333330033377F3333333377333993333333
          3333333773333333333F333333333333330033333333F33333773333333C3333
          330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
          993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
          333333333337733333FF3333333C333330003333333733333777333333333333
          3000333333333333377733333333333333333333333333333333}
        NumGlyphs = 2
        OnClick = DIYAddClick
      end
      object DIYBitmapLoader: TSpeedButton
        Left = 192
        Top = 120
        Width = 23
        Height = 22
        Hint = 'Load Bitmap|Load BMP for Bitmap Object and/or Painting'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
          0000377777777777777703030303030303037F7F7F7F7F7F7F7F000000000000
          00007777777777777777933393303933337073F37F37F73F3377393393303393
          379037FF7F37F37FF777379793303379793037777337F3777737339933303339
          93303377F3F7F3F77F3733993930393993303377F737F7377FF7399993303399
          999037777337F377777793993330333393307377FF37F3337FF7333993303333
          993033377F37F33377F7333993303333993033377337F3337737333333303333
          33303FFFFFF7FFFFFFF700000000000000007777777777777777030303030303
          03037F7F7F7F7F7F7F7F00000000000000007777777777777777}
        NumGlyphs = 2
        OnClick = DIYBitmapLoaderClick
      end
      object DIYClipit: TSpeedButton
        Left = 198
        Top = 144
        Width = 20
        Height = 22
        Hint = 'Copy to Clipboard'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003333330B7FFF
          FFB0333333777F3333773333330B7FFFFFB0333333777F3333773333330B7FFF
          FFB0333333777F3333773333330B7FFFFFB03FFFFF777FFFFF77000000000077
          007077777777777777770FFFFFFFF00077B07F33333337FFFF770FFFFFFFF000
          7BB07F3FF3FFF77FF7770F00F000F00090077F77377737777F770FFFFFFFF039
          99337F3FFFF3F7F777FF0F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        OnClick = DIYClipitClick
      end
      object DIYMagicLoader: TSpeedButton
        Left = 116
        Top = 144
        Width = 20
        Height = 22
        Hint = 'Load *.vl?'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000880B0000880B00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FF0000000000
          00FFFFF0000000000FFFFFF0000000000FFFFFFF00000000FFFFFFF000000F00
          FFFFFFF0000000000FFFFF077777777770FFFF00DDDD777770FFFFFFDDDDFFFF
          FFFFFFFFDDDDFCCCFFFFFFFFDDDDCCCCCFFFFFFFFFFFCCCCCFFFF999999FCCCC
          CFFFFF9999FFFCCCFFFFFFF99FFFFFFFFFFFFFFFFFFFFFFFFFFF}
        OnClick = DIYMagicLoaderClick
      end
      object Label17: TLabel
        Left = 200
        Top = 74
        Width = 7
        Height = 13
        Caption = 'Z'
      end
      object DIYHelp: TSpeedButton
        Left = 268
        Top = 144
        Width = 22
        Height = 22
        Hint = 'Help|Vista Help on command'
        Glyph.Data = {
          CE070000424DCE07000000000000360000002800000024000000120000000100
          1800000000009807000000000000000000000000000000000000008080008080
          0080800080800080800080800080800080808080008080000080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080FFFFFFFFFFFF008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080800080800080808080008000
          0080000080800000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080808080808080FFFFFF008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          80008080008080FFFF0080800080800080000000808000808000808000808000
          8080008080008080008080008080008080008080008080008080808080FFFFFF
          008080808080FFFFFF0080800080800080800080800080800080800080800080
          80008080008080008080008080008080008080008080FFFF0080800080800000
          8080008080008080008080008080008080008080008080008080008080008080
          008080008080808080FFFFFFFFFFFF808080FFFFFF0080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          0080800080800080800080800080800080800080808080808080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080800000800000808000008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          80008080FFFFFFFFFFFF00808000808000808000808000808000808000808000
          8080008080008080008080008080008080008080008080808000808000808000
          8000000080800080800080800080800080800080800080800080800080800080
          80008080008080008080008080808080808080808080FFFFFF00808000808000
          8080008080008080008080008080008080008080008080008080008080008080
          008080FFFF008080008080008000000080800080800080800080800080800080
          80008080008080008080008080008080008080008080808080FFFFFF00808080
          8080FFFFFF008080008080008080008080008080008080008080008080008080
          008080008080008080008080008080FFFF008080008080008080008000000080
          8000808000808000808000808000808000808000808000808000808000808000
          8080808080FFFFFF008080808080FFFFFF008080008080008080008080008080
          008080008080008080008080008080008080008080008080008080008080FFFF
          0080800080800080800080000000808000808000808000808000808000808000
          8080008080008080008080008080808080FFFFFF008080008080808080FFFFFF
          0080800080800080800080800080800080800080800080800080800080800080
          80008080008080008080008080FFFF0080800080800080800080000000808000
          8080008080008080008080008080008080008080008080008080008080808080
          FFFFFF008080008080808080FFFFFF0080800080800080800080800080800080
          80008080008080008080008080800000800000008080008080008080FFFF0080
          8000808000808000800000008080008080008080008080008080008080008080
          008080FFFFFF008080008080808080FFFFFF008080008080808080FFFFFF0080
          8000808000808000808000808000808000808000808080800080800080800080
          0000008080008080008080FFFF00808000808000800000008080008080008080
          008080008080008080008080808080808080FFFFFF008080008080808080FFFF
          FF008080008080808080FFFFFF00808000808000808000808000808000808000
          8080FFFF00808000808000808000800000800000800000808000808000808000
          800000008080008080008080008080008080008080808080FFFFFF0080808080
          80FFFFFFFFFFFFFFFFFF808080008080008080808080FFFFFF00808000808000
          8080008080008080008080008080008080FFFF00808000808000808000808000
          8080008080008080008080008080000080800080800080800080800080800080
          80808080FFFFFF00808000808080808080808080808000808000808000808080
          8080FFFFFF008080008080008080008080008080008080008080008080008080
          FFFF00FFFF008080008080008080008080008080008080000080800080800080
          80008080008080008080008080008080808080FFFFFFFFFFFF00808000808000
          8080008080008080008080808080008080008080008080008080008080008080
          008080008080008080008080008080008080FFFF00FFFF00FFFF00FFFF00FFFF
          0000808000808000808000808000808000808000808000808000808000808080
          8080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080008080008080008080
          0080800080800080800080800080800080800080800080800080800080800080
          8000808000808000808000808000808000808000808000808000808000808000
          8080008080008080008080008080008080808080808080808080808080808080
          008080008080008080008080008080008080}
        NumGlyphs = 2
        OnClick = DIYHelpClick
      end
      object DIYRun: TSpeedButton
        Left = 220
        Top = 144
        Width = 24
        Height = 22
        Hint = 'Run|Run Vista Memo onto Display'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
          033333777777777773333330777777703333333773F333773333333330888033
          33333FFFF7FFF7FFFFFF0000000000000003777777777777777F0FFFFFFFFFF9
          FF037F3333333337337F0F78888888887F037F33FFFFFFFFF37F0F7000000000
          8F037F3777777777F37F0F70AAAAAAA08F037F37F3333337F37F0F70ADDDDDA0
          8F037F37F3333337F37F0F70A99A99A08F037F37F3333337F37F0F70A99A99A0
          8F037F37F3333337F37F0F70AAAAAAA08F037F37FFFFFFF7F37F0F7000000000
          8F037F3777777777337F0F77777777777F037F3333333333337F0FFFFFFFFFFF
          FF037FFFFFFFFFFFFF7F00000000000000037777777777777773}
        NumGlyphs = 2
        OnClick = DIYRunClick
      end
      object Label18: TLabel
        Left = 200
        Top = 96
        Width = 6
        Height = 13
        Caption = 'L'
      end
      object DIYFont: TSpeedButton
        Left = 178
        Top = 144
        Width = 20
        Height = 22
        Hint = 'Font Selection'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333FFF33FFFFF33333300033000
          00333337773377777333333330333300033333337FF33777F333333330733300
          0333333377FFF777F33333333700000073333333777777773333333333033000
          3333333337FF777F333333333307300033333333377F777F3333333333703007
          33333333377F7773333333333330000333333333337777F33333333333300003
          33333333337777F3333333333337007333333333337777333333333333330033
          3333333333377333333333333333033333333333333733333333333333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
        OnClick = DIYFontClick
      end
      object DIYPaintBrush: TSpeedButton
        Left = 226
        Top = 168
        Width = 19
        Height = 17
        Hint = 'Bitmap Paint Brush|Select a Bitmap then use as a Paint Brush'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00559999999995
          5555555999999950555555555555555009555555555599550195555555555555
          019955555555555501905555555555550F90555555555550000555555555550F
          F055555555555008F05555555557000005555555557000075555555570000755
          5555555700075555555557000755555555550007555555555555}
        OnClick = DIYPaintBrushClick
      end
      object DIYLight: TSpeedButton
        Left = 136
        Top = 144
        Width = 20
        Height = 22
        Hint = 'Light (L)|Change light source... change shadow direction (L)'
        Enabled = False
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
          333333333337FF3333333333330003333333333333777F333333333333080333
          3333333F33777FF33F3333B33B000B33B3333373F777773F7333333BBB0B0BBB
          33333337737F7F77F333333BBB0F0BBB33333337337373F73F3333BBB0F7F0BB
          B333337F3737F73F7F3333BB0FB7BF0BB3333F737F37F37F73FFBBBB0BF7FB0B
          BBB3773F7F37337F377333BB0FBFBF0BB333337F73F333737F3333BBB0FBF0BB
          B3333373F73FF7337333333BBB000BBB33333337FF777337F333333BBBBBBBBB
          3333333773FF3F773F3333B33BBBBB33B33333733773773373333333333B3333
          333333333337F33333333333333B333333333333333733333333}
        NumGlyphs = 2
        OnClick = DIYLightClick
      end
      object DIYSave: TSpeedButton
        Left = 244
        Top = 144
        Width = 24
        Height = 22
        Hint = 'Save|Save a Vista File'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
          7700333333337777777733333333008088003333333377F73377333333330088
          88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
          000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
          FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
          99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
          99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
          99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
          93337FFFF7737777733300000033333333337777773333333333}
        NumGlyphs = 2
        OnClick = DIYSaveClick
      end
      object DIYLoad: TSpeedButton
        Left = 31
        Top = 144
        Width = 20
        Height = 22
        Hint = 'Load|Load a Vista file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
          0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
          B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
          FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
          FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
          FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
          0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
          0555555555777777755555555555555555555555555555555555}
        NumGlyphs = 2
        OnClick = DIYLoadClick
      end
      object DIYLand: TSpeedButton
        Left = 96
        Top = 144
        Width = 20
        Height = 22
        Hint = 'Place Land Forms|Place Selected Land Forms on Display'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333337773F333333333330FFF03333333333733373F3333333330FFFFF03
          33333337F33337F3333333330FFFFF0333333337F33337F3333333330FFFFF03
          33333337FFFFF7F3333333330777770333333337777777F3333333330FF7FF03
          33333337F37F37F3333333330FF7FF03333333373F7FF7333333333330000033
          33333333777773FFF33333333330330007333333337F37777F33333333303033
          307333333373733377F33333333303333303333333F7F33337F3333333330733
          330333333F777F3337F333333370307330733333F77377FF7733333337033300
          0733333F77333777733333337033333333333337733333333333}
        NumGlyphs = 2
        OnClick = DIYLandClick
      end
      object DIYGear: TSpeedButton
        Left = 71
        Top = 144
        Width = 20
        Height = 22
        Hint = 'Gears a loaded file onto display'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00370777033333
          3330337F3F7F33333F3787070003333707303F737773333373F7007703333330
          700077337F3333373777887007333337007733F773F333337733700070333333
          077037773733333F7F37703707333300080737F373333377737F003333333307
          78087733FFF3337FFF7F33300033330008073F3777F33F777F73073070370733
          078073F7F7FF73F37FF7700070007037007837773777F73377FF007777700730
          70007733FFF77F37377707700077033707307F37773F7FFF7337080777070003
          3330737F3F7F777F333778080707770333333F7F737F3F7F3333080787070003
          33337F73FF737773333307800077033333337337773373333333}
        NumGlyphs = 2
        OnClick = DIYGearClick
      end
      object DIYRotate: TSpeedButton
        Left = 248
        Top = 168
        Width = 20
        Height = 17
        Hint = 'Rotate (Z): Perspective (Z)'
        Enabled = False
        Glyph.Data = {
          EE000000424DEE0000000000000076000000280000000F0000000F0000000100
          0400000000007800000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888808888
          8881808888808888808188088000008808808880077777008880888088888770
          888088088FFFF87708808808FFFFFF8708820008FFFFFF8700008808FFFFFF87
          08808808FFFFFF87088088808FFFF8808880888008888800888E880880000088
          088080888880888880808888888088888880}
        OnClick = DIYRotateClick
      end
      object G_Math_Image: TImage
        Left = 208
        Top = 168
        Width = 16
        Height = 16
        Hint = 'Displays current loaded Bitmap'
      end
      object PolygonBtn: TSpeedButton
        Left = 270
        Top = 168
        Width = 19
        Height = 17
        Hint = 'Polygon Paintbrush'
        Enabled = False
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333FFFFF3333333333000003333333333F777773FF333333003333300
          33333337733333773F33330333333333033333733FFFFFFF73F3303300000003
          303337F37777777337F3303330CCC0333033373337777733373F0333330C0333
          33037F33337773FFF37F03333330300033037F3FFFF73777FF7F0300000307B7
          03037F77777F77777F7F030999030BBB03037F77777F77777F7F0309990307B7
          03037377777F7777737330099903300030333777777F377737F3300000033333
          3033377777733333373333033333333303333373FF33333F7333333003333300
          3333333773FFFF77333333333000003333333333377777333333}
        NumGlyphs = 2
        OnClick = PolygonBtnClick
      end
      object Label63: TLabel
        Left = 200
        Top = 58
        Width = 13
        Height = 13
        Caption = 'Hy'
        Transparent = True
      end
      object DIYAddLoad: TSpeedButton
        Left = 51
        Top = 144
        Width = 20
        Height = 22
        Hint = 'Add Load|Add Load another Vista file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555000000
          000055555F77777777775555000FFFFFFFF0555F777F5FFFF55755000F0F0000
          FFF05F777F7F77775557000F0F0FFFFFFFF0777F7F7F5FFFFFF70F0F0F0F0000
          00F07F7F7F7F777777570F0F0F0FFFFFFFF07F7F7F7F5FFFFFF70F0F0F0F0000
          00F07F7F7F7F777777570F0F0F0FFFFFFFF07F7F7F7F5FFF55570F0F0F0F000F
          FFF07F7F7F7F77755FF70F0F0F0FFFFF00007F7F7F7F5FF577770F0F0F0F00FF
          0F057F7F7F7F77557F750F0F0F0FFFFF00557F7F7F7FFFFF77550F0F0F000000
          05557F7F7F77777775550F0F0000000555557F7F7777777555550F0000000555
          55557F7777777555555500000005555555557777777555555555}
        NumGlyphs = 2
        OnClick = DIYAddLoadClick
      end
      object Label82: TLabel
        Left = 256
        Top = 74
        Width = 7
        Height = 13
        Caption = 'E'
      end
      object DIYMemo2: TMemo
        Left = 8
        Top = 256
        Width = 265
        Height = 17
        TabOrder = 16
        Visible = False
      end
      object DIYPallette: TRadioGroup
        Left = 0
        Top = 0
        Width = 193
        Height = 144
        Hint = 'Select|Select then use Mouse Lander'
        HelpContext = 2510
        Caption = 'DIY: Do It Yourself Pallette'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'TriangleZE'
          'QuadrangleZE'
          'Ellipse  (Circle)'
          'Tri Angle'
          'Rectangle'
          'Pandora'#39's Box'
          'Ramped Blank'
          'Line'
          'C Shadow [L]'
          'T Shadow [L]'
          'Mountain.FLM'
          'Turtle.FLA'
          'Tree.FLT'
          'Fern.FLF'
          'Dragon.FLD'
          'Sky Cloud.FLS'
          'Numb.FLN'
          'Collage.FLO'
          'Torso.FLW'
          'Bitmap.bmp')
        TabOrder = 0
      end
      object DIYXEdit: TEdit
        Left = 216
        Top = 0
        Width = 73
        Height = 21
        HelpContext = 2530
        TabOrder = 1
        Text = '0'
      end
      object DIYYEdit: TEdit
        Left = 216
        Top = 24
        Width = 73
        Height = 21
        HelpContext = 2530
        TabOrder = 2
        Text = '0'
      end
      object DIYZEdit: TEdit
        Left = 216
        Top = 72
        Width = 33
        Height = 21
        HelpContext = 2530
        TabOrder = 3
        Text = '6'
      end
      object DIYMemo: TMemo
        Left = 1
        Top = 192
        Width = 286
        Height = 97
        Hint = 'Vista Object List'
        HelpContext = 2520
        ScrollBars = ssVertical
        TabOrder = 4
      end
      object DIYHyEdit: TEdit
        Left = 248
        Top = 48
        Width = 41
        Height = 21
        HelpContext = 2530
        TabOrder = 5
        Text = '0'
      end
      object DIYWxEdit: TEdit
        Left = 216
        Top = 48
        Width = 33
        Height = 21
        HelpContext = 2530
        TabOrder = 6
        Text = '0'
      end
      object DIYBitmapEdit: TEdit
        Left = 216
        Top = 120
        Width = 65
        Height = 21
        HelpContext = 2560
        TabOrder = 7
        Text = 'Bitmap.bmp'
      end
      object DIYLxEdit: TEdit
        Left = 216
        Top = 96
        Width = 41
        Height = 21
        HelpContext = 2530
        TabOrder = 8
        Text = '2'
      end
      object DIYSet: TPanel
        Left = 24
        Top = 144
        Width = 6
        Height = 22
        BevelOuter = bvNone
        Color = clRed
        TabOrder = 9
      end
      object V2Colorbox: TPanel
        Left = 40
        Top = 168
        Width = 33
        Height = 17
        HelpContext = 2540
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '2'
        Color = clLime
        TabOrder = 10
        OnClick = V2ColorboxClick
      end
      object V1Colorbox: TPanel
        Left = 0
        Top = 168
        Width = 33
        Height = 17
        HelpContext = 2540
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '1'
        Color = clRed
        TabOrder = 11
        OnClick = V1ColorboxClick
      end
      object V3Colorbox: TPanel
        Left = 76
        Top = 168
        Width = 21
        Height = 17
        HelpContext = 2540
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '3'
        Color = clBlue
        TabOrder = 12
        OnClick = V3ColorboxClick
      end
      object V4Colorbox: TPanel
        Left = 99
        Top = 168
        Width = 22
        Height = 17
        HelpContext = 2540
        BevelOuter = bvLowered
        BevelWidth = 2
        Caption = '4'
        Color = clYellow
        TabOrder = 13
        OnClick = V4ColorboxClick
      end
      object DIYLyEdit: TEdit
        Left = 256
        Top = 96
        Width = 33
        Height = 21
        HelpContext = 2530
        TabOrder = 14
        Text = '2'
      end
      object DIYFileEdit: TEdit
        Left = 128
        Top = 168
        Width = 77
        Height = 21
        HelpContext = 2500
        TabOrder = 15
        Text = 'Filename.FL?'
      end
      object DIYEEdit: TEdit
        Left = 264
        Top = 72
        Width = 25
        Height = 21
        HelpContext = 2530
        TabOrder = 17
        Text = '0'
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Vista List (*.VL), Objects (*.vl?)|*.vl;*.vlt;*.vld;*.vlc;*.vlf;' +
      '*.vln'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Vista List: *.VL'
    Left = 268
    Top = 122
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      'Vista List (*.VL), Objects (*.vl?)|*.vl;*.vlt;*.vld;*.vlc;*.vlf;' +
      '*.vln'
    Options = [ofOverwritePrompt, ofHideReadOnly]
    Title = 'Vista List:  *.VL'
    Left = 228
    Top = 66
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 212
    Top = 18
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Vista BMP loader'
    Left = 268
    Top = 90
  end
  object ColorDialog1: TColorDialog
    Left = 260
    Top = 146
  end
end

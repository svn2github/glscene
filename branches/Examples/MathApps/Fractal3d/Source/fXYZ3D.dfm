object XYZ3DForm: TXYZ3DForm
  Left = 403
  Top = 118
  Width = 316
  Height = 381
  Hint = 'XYZ 3D maker'
  HelpContext = 4000
  Caption = 'XYZ: 3D'
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
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007777
    7777777777777777777777777777777777777777777777777777777777777777
    7777777777777777777777777777777777777777777777777777777777777777
    7777777777777777777777777777777777777777777777777777777777777777
    7777777777777777777777777777777777777777777777777777877777777777
    7777877777777777777788777777887777778777777777077777787777778887
    7777887777777077777778877777088887778877778708877777788777777008
    887778777800700888877887777787700F888887807777000888778777778777
    700FFF88077700EEE008878877778777777700F07770EEEEEEE0888887778877
    77777707700EEEEEEEEE008F88778887787777770EEEEEEEEEEEEE00F8708887
    88778800EEEEEEEEEEEEEEEE0F0E0888877800EEEEEEEEEEEEEEEEEEE0EEE0FF
    8880EEEEEEEEEEE0EEEEEEEEEEEEEE0FF00EEEEEEE0EEE000EE0EEEEEEEEEEE0
    0EEEEEEEEEE000EEE00EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
    EEEEEEEEEEEEEEEEEEEEEEEFFFFFEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEFFEEEE
    EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEBBBEEEEEEEEFFFFFFFFEEEEEEEFEEEEBBB
    BBEEEEEEEEEEFFFFFFFEFFFFFFEEEBBBBBEEEEEEEEEEEFFFEEEFEFFEEEEEEBBB
    BBEEEEEEEEEEEEEEEEEEEEEEEEEEEEBBBEEEEEEEEEEEEEEEEEEEEEEEEEEE0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 305
    Height = 345
    Hint = 'Morph Mountains'
    HelpContext = 4000
    ActivePage = ElevationTS
    MultiLine = True
    TabOrder = 0
    object ElevationTS: TTabSheet
      Hint = 'Digital Elevation Mapping'
      HelpContext = 4700
      Caption = 'D Elevation M'
      ImageIndex = -1
      object ELoad: TSpeedButton
        Left = 8
        Top = 232
        Width = 24
        Height = 22
        Hint = 'Load|Load a DEM.bin file'
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
        OnClick = ELoadClick
      end
      object ESave: TSpeedButton
        Left = 152
        Top = 232
        Width = 24
        Height = 22
        Hint = 'Save|Save a  FLM File'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          33333FFFFFFFFFFFFFFF000000000000000077777777777777770F7777777777
          77707F3F3333333333370F988888888888707F733FFFFFFFF3370F8800000000
          88707F337777777733370F8888888888887077777FFFFFFFF337080000000000
          88707F377777777733370F80C088888888707F777373333333370F0C00888888
          8870777733733333333700C0F8FFFFFFFFF0777FFF7FFFFFFFF70C0000000000
          00007777777777777777C0033333333333333333333333333333033333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
        OnClick = ESaveClick
      end
      object EMouse: TSpeedButton
        Left = 188
        Top = 232
        Width = 24
        Height = 22
        Hint = 'Line Of Sight'
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
      end
      object EleRun: TSpeedButton
        Left = 264
        Top = 232
        Width = 24
        Height = 22
        Hint = 'Display|Run DEM onto Display'
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
        OnClick = EleRunClick
      end
      object DemCIE: TLabel
        Left = 8
        Top = 120
        Width = 137
        Height = 13
        AutoSize = False
        Caption = '7 Contour Intervals:'
      end
      object DemMinE: TLabel
        Left = 152
        Top = 96
        Width = 137
        Height = 13
        AutoSize = False
        Caption = 'Minimum Elevation:'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object DemMaxE: TLabel
        Left = 152
        Top = 184
        Width = 137
        Height = 13
        AutoSize = False
        Caption = 'Maximum Elevation: '
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object DemiColumnsX: TLabel
        Left = 8
        Top = 170
        Width = 129
        Height = 13
        Hint = '# of columns = Width(X)'
        AutoSize = False
        Caption = '# Columns (X)'
      end
      object DemiRowsY: TLabel
        Left = 8
        Top = 186
        Width = 129
        Height = 13
        Hint = '# of rows = Height (Y)'
        AutoSize = False
        Caption = '# of Rows (Y)'
      end
      object Label4: TLabel
        Left = 8
        Top = 264
        Width = 7
        Height = 13
        Caption = 'X'
      end
      object Label5: TLabel
        Left = 80
        Top = 264
        Width = 7
        Height = 13
        Caption = 'Y'
      end
      object Label19: TLabel
        Left = 148
        Top = 264
        Width = 12
        Height = 13
        Caption = 'Zx'
      end
      object Label20: TLabel
        Left = 220
        Top = 264
        Width = 12
        Height = 13
        Caption = 'Zy'
      end
      object Label21: TLabel
        Left = 8
        Top = 288
        Width = 3
        Height = 13
        Caption = 'r'
      end
      object Label34: TLabel
        Left = 56
        Top = 288
        Width = 6
        Height = 13
        Caption = 'd'
      end
      object Label146: TLabel
        Left = 122
        Top = 288
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'g'
      end
      object Label43: TLabel
        Left = 160
        Top = 288
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'h'
      end
      object Label44: TLabel
        Left = 204
        Top = 288
        Width = 2
        Height = 13
        Caption = 'i'
      end
      object Label147: TLabel
        Left = 250
        Top = 288
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'k'
      end
      object Label6: TLabel
        Left = 8
        Top = 136
        Width = 97
        Height = 13
        AutoSize = False
        Caption = 'Set Contour Interval:'
      end
      object Label11: TLabel
        Left = 8
        Top = 210
        Width = 73
        Height = 13
        Caption = 'Null Dem Value'
      end
      object EFileNameEdit: TEdit
        Left = 30
        Top = 232
        Width = 121
        Height = 21
        HelpContext = 4100
        TabOrder = 0
        Text = 'Mountain.flm'
      end
      object EHelp: TBitBtn
        Left = 224
        Top = 232
        Width = 24
        Height = 22
        Hint = 'DEM Help'
        HelpContext = 4700
        TabOrder = 1
        OnClick = EHelpClick
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
      end
      object EColorsRG: TRadioGroup
        Left = 152
        Top = 0
        Width = 129
        Height = 89
        HelpContext = 4702
        Caption = 'Fract al go rhythym: Tints'
        ItemIndex = 0
        Items.Strings = (
          '9 Terrain Ranges'
          '7 Color Ranges'
          '1 color Range'
          'Fractal 16+256'
          'Set the 9 Intervals:')
        TabOrder = 2
      end
      object ECI1Edit: TEdit
        Left = 152
        Top = 112
        Width = 41
        Height = 21
        HelpContext = 4703
        Color = clFuchsia
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = '-7000'
      end
      object XYZDEMRG: TRadioGroup
        Left = 8
        Top = 0
        Width = 137
        Height = 113
        Hint = 'Choose ME!'
        HelpContext = 4701
        Caption = 'Digital Elevation Map'
        ItemIndex = 0
        Items.Strings = (
          'Elevation Tints'
          '(sha) Slope Tints'
          'Contours'
          '(na) Line Of Sight: A-B'
          '(na) LOS Masked Area'
          'Gl 3D'
          'GLobe wan')
        TabOrder = 4
      end
      object ECI2Edit: TEdit
        Left = 200
        Top = 112
        Width = 41
        Height = 21
        HelpContext = 4703
        Color = clBlue
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        Text = '-6000'
      end
      object ECI3Edit: TEdit
        Left = 248
        Top = 112
        Width = 41
        Height = 21
        HelpContext = 4703
        Color = clAqua
        TabOrder = 6
        Text = '-2000'
      end
      object EXEdit: TEdit
        Left = 16
        Top = 264
        Width = 57
        Height = 21
        HelpContext = 2902
        TabOrder = 7
      end
      object EYEdit: TEdit
        Left = 88
        Top = 264
        Width = 57
        Height = 21
        HelpContext = 2903
        TabOrder = 8
      end
      object EZxEdit: TEdit
        Left = 160
        Top = 264
        Width = 57
        Height = 21
        HelpContext = 2904
        TabOrder = 9
      end
      object EZyEdit: TEdit
        Left = 232
        Top = 264
        Width = 57
        Height = 21
        HelpContext = 2905
        TabOrder = 10
      end
      object EREdit: TEdit
        Left = 16
        Top = 288
        Width = 41
        Height = 21
        Hint = 'Rate'
        HelpContext = 2906
        TabOrder = 11
      end
      object EDEdit: TEdit
        Left = 64
        Top = 288
        Width = 41
        Height = 21
        HelpContext = 2907
        TabOrder = 12
      end
      object EGEdit: TEdit
        Left = 128
        Top = 288
        Width = 33
        Height = 21
        HelpContext = 2459
        TabOrder = 13
      end
      object EHEdit: TEdit
        Left = 168
        Top = 288
        Width = 33
        Height = 21
        HelpContext = 2908
        TabOrder = 14
      end
      object EIEdit: TEdit
        Left = 208
        Top = 288
        Width = 33
        Height = 21
        Hint = 'Increase'
        HelpContext = 2909
        TabOrder = 15
      end
      object EKEdit: TEdit
        Left = 256
        Top = 288
        Width = 33
        Height = 21
        HelpContext = 2460
        TabOrder = 16
      end
      object ECI6Edit: TEdit
        Left = 248
        Top = 136
        Width = 41
        Height = 21
        HelpContext = 4703
        Color = clRed
        TabOrder = 17
        Text = '1001'
      end
      object ECI5Edit: TEdit
        Left = 200
        Top = 136
        Width = 41
        Height = 21
        HelpContext = 4703
        Color = clYellow
        TabOrder = 18
        Text = '401'
      end
      object ECI4Edit: TEdit
        Left = 152
        Top = 136
        Width = 41
        Height = 21
        HelpContext = 4703
        Color = clGreen
        TabOrder = 19
        Text = '1'
      end
      object ECI8Edit: TEdit
        Left = 200
        Top = 160
        Width = 41
        Height = 21
        HelpContext = 4703
        TabOrder = 20
        Text = '3001'
      end
      object ECI7Edit: TEdit
        Left = 152
        Top = 160
        Width = 41
        Height = 21
        HelpContext = 4703
        Color = clFuchsia
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 21
        Text = '2001'
      end
      object DemCIEEdit: TEdit
        Left = 106
        Top = 137
        Width = 33
        Height = 21
        Hint = 'Set Contour Interval'
        HelpContext = 4704
        TabOrder = 22
        Text = '50'
      end
      object NullValueEdit: TEdit
        Left = 88
        Top = 208
        Width = 65
        Height = 21
        HelpContext = 4100
        TabOrder = 23
        Text = '-32767'
      end
    end
    object DEMImport: TTabSheet
      HelpContext = 4800
      Caption = 'DEM Import'
      ImageIndex = 8
      object DemiImport: TSpeedButton
        Left = 64
        Top = 292
        Width = 65
        Height = 22
        Hint = 'Select file|Import data into DTM format'
        Caption = 'Import'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          33333FFFFFFFFFFFFFFF000000000000000077777777777777770F7777777777
          77707F3F3333333333370F988888888888707F733FFFFFFFF3370F8800000000
          88707F337777777733370F8888888888887077777FFFFFFFF337080000000000
          88707F377777777733370F80C088888888707F777373333333370F0C00888888
          8870777733733333333700C0F8FFFFFFFFF0777FFF7FFFFFFFF70C0000000000
          00007777777777777777C0033333333333333333333333333333033333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
        OnClick = DemiImportClick
      end
      object Label24: TLabel
        Left = 0
        Top = 96
        Width = 43
        Height = 13
        Caption = 'Rows (Y)'
      end
      object Label25: TLabel
        Left = 0
        Top = 56
        Width = 56
        Height = 13
        Caption = 'Columns (X)'
      end
      object Label36: TLabel
        Left = 0
        Top = 136
        Width = 28
        Height = 13
        Caption = 'Left X'
      end
      object Label37: TLabel
        Left = 0
        Top = 176
        Width = 29
        Height = 13
        Caption = 'Top Y'
      end
      object Label38: TLabel
        Left = 0
        Top = 216
        Width = 35
        Height = 13
        Caption = 'Right X'
      end
      object Label39: TLabel
        Left = 0
        Top = 256
        Width = 43
        Height = 13
        Caption = 'Bottom Y'
      end
      object Label10: TLabel
        Left = 0
        Top = 40
        Width = 212
        Height = 13
        Caption = 'Etopo Inputs: South and West are Negatives'
      end
      object DehHeader: TSpeedButton
        Left = 184
        Top = 268
        Width = 81
        Height = 22
        Hint = 'View DEH Header'
        Caption = 'deh header'
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
        OnClick = DehHeaderClick
      end
      object FlmHeader: TSpeedButton
        Left = 64
        Top = 268
        Width = 81
        Height = 22
        Hint = 'View flm Header'
        Caption = 'flm header'
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
        OnClick = FlmHeaderClick
      end
      object CopyClipBtn: TSpeedButton
        Left = 240
        Top = 292
        Width = 23
        Height = 22
        Hint = 'Copy memo to Clipboard'
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
        OnClick = CopyClipBtnClick
      end
      object DehMemoPrintBtn: TSpeedButton
        Left = 264
        Top = 268
        Width = 23
        Height = 22
        Hint = 'Annotated deh'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
          0003377777777777777308888888888888807F33333333333337088888888888
          88807FFFFFFFFFFFFFF7000000000000000077777777777777770F8F8F8F8F8F
          8F807F333333333333F708F8F8F8F8F8F9F07F333333333337370F8F8F8F8F8F
          8F807FFFFFFFFFFFFFF7000000000000000077777777777777773330FFFFFFFF
          03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
          03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
          33333337F3F37F3733333330F08F0F0333333337F7337F7333333330FFFF0033
          33333337FFFF7733333333300000033333333337777773333333}
        NumGlyphs = 2
        OnClick = DehMemoPrintBtnClick
      end
      object FlmMemoPrintBtn: TSpeedButton
        Left = 144
        Top = 268
        Width = 23
        Height = 22
        Hint = 'Annotated flm'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
          0003377777777777777308888888888888807F33333333333337088888888888
          88807FFFFFFFFFFFFFF7000000000000000077777777777777770F8F8F8F8F8F
          8F807F333333333333F708F8F8F8F8F8F9F07F333333333337370F8F8F8F8F8F
          8F807FFFFFFFFFFFFFF7000000000000000077777777777777773330FFFFFFFF
          03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
          03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
          33333337F3F37F3733333330F08F0F0333333337F7337F7333333330FFFF0033
          33333337FFFF7733333333300000033333333337777773333333}
        NumGlyphs = 2
        OnClick = FlmMemoPrintBtnClick
      end
      object DemiFileEdit: TEdit
        Left = 128
        Top = 292
        Width = 89
        Height = 21
        HelpContext = 4800
        TabOrder = 0
        Text = 'Elevation.bin'
      end
      object DemiHelp: TBitBtn
        Left = 264
        Top = 292
        Width = 24
        Height = 22
        Hint = 'Import Help'
        HelpContext = 4800
        TabOrder = 1
        OnClick = DemiHelpClick
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
      end
      object DemiMemo: TMemo
        Left = 64
        Top = 56
        Width = 225
        Height = 209
        HelpContext = 4800
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
      end
      object DemiColumns: TEdit
        Left = 0
        Top = 72
        Width = 49
        Height = 21
        HelpContext = 4802
        TabOrder = 3
        Text = '0'
      end
      object DemiRows: TEdit
        Left = 0
        Top = 112
        Width = 49
        Height = 21
        HelpContext = 4802
        TabOrder = 4
        Text = '0'
      end
      object DemiLeftX1: TEdit
        Left = 0
        Top = 152
        Width = 49
        Height = 21
        HelpContext = 4802
        TabOrder = 5
        Text = '0'
      end
      object DemiTopY1: TEdit
        Left = 0
        Top = 192
        Width = 49
        Height = 21
        HelpContext = 4802
        TabOrder = 6
        Text = '0'
      end
      object DemiRightX2: TEdit
        Left = 0
        Top = 232
        Width = 49
        Height = 21
        HelpContext = 4802
        TabOrder = 7
        Text = '0'
      end
      object DemiBottomY2: TEdit
        Left = 0
        Top = 272
        Width = 49
        Height = 21
        HelpContext = 4802
        TabOrder = 8
        Text = '0'
      end
      object DemiRG: TRadioGroup
        Left = 0
        Top = 0
        Width = 297
        Height = 33
        Hint = 'Select one'
        HelpContext = 4801
        Caption = 'Digital Elevation Data Type'
        Columns = 3
        Items.Strings = (
          'Etopo 5'#39
          'NOAA 30"'
          'Gtopo 30"')
        TabOrder = 9
      end
    end
    object Tiling: TTabSheet
      Hint = 'Gather together'
      HelpContext = 4400
      Caption = 'Tiling'
      ImageIndex = -1
      object TileRun: TSpeedButton
        Left = 200
        Top = 294
        Width = 89
        Height = 22
        Hint = 'Do it|Combine or Divide the selected files'
        Caption = 'Tile the Files'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          33333FFFFFFFFFFFFFFF000000000000000077777777777777770F7777777777
          77707F3F3333333333370F988888888888707F733FFFFFFFF3370F8800000000
          88707F337777777733370F8888888888887077777FFFFFFFF337080000000000
          88707F377777777733370F80C088888888707F777373333333370F0C00888888
          8870777733733333333700C0F8FFFFFFFFF0777FFF7FFFFFFFF70C0000000000
          00007777777777777777C0033333333333333333333333333333033333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
        OnClick = TileRunClick
      end
      object TileOpen1: TSpeedButton
        Left = 184
        Top = 29
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen1Click
      end
      object TileOpen2: TSpeedButton
        Left = 216
        Top = 29
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen2Click
      end
      object TileOpen3: TSpeedButton
        Left = 248
        Top = 29
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen3Click
      end
      object TileOpen4: TSpeedButton
        Left = 184
        Top = 61
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '4'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen4Click
      end
      object TileOpen5: TSpeedButton
        Left = 216
        Top = 61
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '5'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen5Click
      end
      object TileOpen6: TSpeedButton
        Left = 248
        Top = 61
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '6'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen6Click
      end
      object TileOpen9: TSpeedButton
        Left = 248
        Top = 93
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '9'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen9Click
      end
      object TileOpen8: TSpeedButton
        Left = 216
        Top = 93
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '8'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen8Click
      end
      object TileOpen7: TSpeedButton
        Left = 184
        Top = 93
        Width = 25
        Height = 22
        Hint = 'Select|Select a Bin file'
        Caption = '7'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        NumGlyphs = 2
        ParentFont = False
        OnClick = TileOpen7Click
      end
      object Label8: TLabel
        Left = 176
        Top = 8
        Width = 102
        Height = 13
        Caption = 'File Selection Buttons'
      end
      object Label3: TLabel
        Left = 40
        Top = 264
        Width = 62
        Height = 13
        Caption = 'Set Tile Size:'
      end
      object Label7: TLabel
        Left = 8
        Top = 8
        Width = 60
        Height = 13
        Caption = 'Thin 1 out of'
      end
      object Label9: TLabel
        Left = 216
        Top = 258
        Width = 73
        Height = 13
        Caption = 'Null Dem Value'
      end
      object Label15: TLabel
        Left = 120
        Top = 8
        Width = 33
        Height = 13
        Caption = 'or Strip'
      end
      object TileHelp: TBitBtn
        Left = 168
        Top = 294
        Width = 24
        Height = 22
        Hint = 'Tiling help'
        HelpContext = 4400
        TabOrder = 0
        OnClick = TileHelpClick
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
      end
      object BinFileList: TListBox
        Left = 168
        Top = 120
        Width = 121
        Height = 137
        Hint = 'BIN files'
        HelpContext = 4400
        ItemHeight = 13
        Items.Strings = (
          'Elevation.Bin File Name'
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9')
        TabOrder = 1
      end
      object TileRG: TRadioGroup
        Left = 8
        Top = 72
        Width = 145
        Height = 193
        HelpContext = 4401
        Caption = 'Combinental Divide'
        Items.Strings = (
          'Thin 1'
          'Combine 1 and 2'
          'Combine 1 and 4'
          '-Combine 1,2,4,5'
          '-Combine 1..9'
          '-Divide 5: W and E'
          '-Divide 5: N and S'
          '-Divide 5: Ne,Se,Nw,Sw'
          'Divide 5: 1..9 files'
          '-Divide 5: set tile size ?#s')
        TabOrder = 2
      end
      object TileRowStrip: TCheckBox
        Left = 2
        Top = 24
        Width = 167
        Height = 17
        Hint = 'Delete Y Rows'
        HelpContext = 4401
        Caption = 'Combine: Strip ^ Y Rows (1+4)'
        TabOrder = 3
      end
      object TileColumnStrip: TCheckBox
        Left = 2
        Top = 40
        Width = 180
        Height = 17
        Hint = 'Delete X Columns'
        HelpContext = 4401
        Caption = 'Combine: Strip ^  X Columns (1+2)'
        TabOrder = 4
      end
      object TileThinEdit: TEdit
        Left = 72
        Top = 3
        Width = 41
        Height = 21
        HelpContext = 4401
        TabOrder = 5
        Text = '10'
      end
      object TileSizeEdit: TEdit
        Left = 104
        Top = 264
        Width = 49
        Height = 21
        HelpContext = 4401
        TabOrder = 6
        Text = '256'
      end
      object TileSizeRGNS: TRadioGroup
        Left = 8
        Top = 287
        Width = 71
        Height = 30
        HelpContext = 4401
        Caption = 'Trash side'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'N'
          'S')
        TabOrder = 7
      end
      object TileSizeRGEW: TRadioGroup
        Left = 80
        Top = 287
        Width = 71
        Height = 30
        HelpContext = 4401
        Caption = 'Trash side'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'E'
          'W')
        TabOrder = 8
      end
      object NullDemValueEdit: TEdit
        Left = 216
        Top = 272
        Width = 73
        Height = 21
        HelpContext = 4400
        TabOrder = 9
        Text = '-32767'
      end
      object TileSmooth: TCheckBox
        Left = 2
        Top = 56
        Width = 159
        Height = 17
        Hint = 'Average edges'
        HelpContext = 4401
        Caption = 'Combine Smooth^ (X and Y)'
        TabOrder = 10
      end
    end
    object TabSheet1: TTabSheet
      HelpContext = 4500
      Caption = 'Mtns'
      ImageIndex = 3
      object MtnsClear: TSpeedButton
        Left = 8
        Top = 240
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
        OnClick = MtnsClearClick
      end
      object MtnsSave: TSpeedButton
        Left = 192
        Top = 240
        Width = 24
        Height = 22
        Hint = 'Save|Save a Mtn File'
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
        OnClick = MtnsSaveClick
      end
      object MtnsFLSBtn: TSpeedButton
        Left = 264
        Top = 32
        Width = 24
        Height = 22
        Hint = 'Run Display|Run Mtn selection onto Display'
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
        OnClick = MtnsFLSBtnClick
      end
      object Label130: TLabel
        Left = 8
        Top = 272
        Width = 7
        Height = 13
        Caption = 'X'
      end
      object Label131: TLabel
        Left = 80
        Top = 272
        Width = 7
        Height = 13
        Caption = 'Y'
      end
      object Label132: TLabel
        Left = 148
        Top = 272
        Width = 12
        Height = 13
        Caption = 'Zx'
      end
      object Label133: TLabel
        Left = 220
        Top = 272
        Width = 12
        Height = 13
        Caption = 'Zy'
      end
      object Label134: TLabel
        Left = 8
        Top = 296
        Width = 3
        Height = 13
        Caption = 'r'
      end
      object Label135: TLabel
        Left = 56
        Top = 296
        Width = 6
        Height = 13
        Caption = 'd'
      end
      object Label1: TLabel
        Left = 121
        Top = 296
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'g'
      end
      object Label136: TLabel
        Left = 155
        Top = 296
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'h'
      end
      object Label137: TLabel
        Left = 194
        Top = 296
        Width = 2
        Height = 13
        Caption = 'i'
      end
      object Label2: TLabel
        Left = 235
        Top = 296
        Width = 6
        Height = 13
        Hint = 'Height'
        Caption = 'k'
      end
      object MtnsRLSBtn: TSpeedButton
        Left = 184
        Top = 148
        Width = 73
        Height = 33
        Hint = 'Run Display|Run Mtn selection onto GL Display'
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
        OnClick = MtnsRLSBtnClick
      end
      object RgbToDemBtn: TSpeedButton
        Left = 0
        Top = 200
        Width = 24
        Height = 22
        Hint = 'Load|Load a RGB .bmp file'
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
        OnClick = RgbToDemBtnClick
      end
      object Label17: TLabel
        Left = 152
        Top = 200
        Width = 131
        Height = 13
        Caption = 'Turn 24 bit RGB bmp image'
      end
      object Label26: TLabel
        Left = 152
        Top = 216
        Width = 125
        Height = 13
        Caption = 'into dem, save and display'
      end
      object MtnsRG: TRadioGroup
        Left = 0
        Top = 8
        Width = 257
        Height = 65
        Hint = 'Land Scape choice'
        HelpContext = 4500
        Caption = 'Fractal   Land Scapes'
        ItemIndex = 0
        Items.Strings = (
          'Fractal Mountains [r] [d] (1.8,1.2)   Quick 48 (10)'
          'Fractal Mountains [r] [d]                 Slow 120 (4)'
          'Fractal Mountains [r] [d]              Stalled 480 (1)')
        TabOrder = 0
      end
      object MtnsFileEdit: TEdit
        Left = 30
        Top = 240
        Width = 163
        Height = 21
        HelpContext = 2901
        TabOrder = 1
        Text = 'Mountain.flm'
      end
      object MtnsHelp: TBitBtn
        Left = 264
        Top = 240
        Width = 24
        Height = 22
        Hint = 'Mtn  Help'
        HelpContext = 4500
        TabOrder = 2
        OnClick = MtnsHelpClick
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
      end
      object MtnsXEdit: TEdit
        Left = 16
        Top = 272
        Width = 57
        Height = 21
        HelpContext = 2902
        TabOrder = 3
        Text = '0'
      end
      object MtnsYEdit: TEdit
        Left = 88
        Top = 272
        Width = 57
        Height = 21
        HelpContext = 2903
        TabOrder = 4
        Text = '0'
      end
      object MtnsZxEdit: TEdit
        Left = 160
        Top = 272
        Width = 57
        Height = 21
        HelpContext = 2904
        TabOrder = 5
        Text = '320'
      end
      object MtnsZyEdit: TEdit
        Left = 232
        Top = 272
        Width = 57
        Height = 21
        HelpContext = 2905
        TabOrder = 6
        Text = '240'
      end
      object MtnsREdit: TEdit
        Left = 16
        Top = 296
        Width = 41
        Height = 21
        Hint = 'Rate'
        HelpContext = 2906
        TabOrder = 7
        Text = '1.4'
      end
      object MtnsDEdit: TEdit
        Left = 64
        Top = 296
        Width = 41
        Height = 21
        HelpContext = 2907
        TabOrder = 8
        Text = '1.6'
      end
      object MtnsGEdit: TEdit
        Left = 128
        Top = 296
        Width = 25
        Height = 21
        HelpContext = 2459
        TabOrder = 9
        Text = '9'
      end
      object MtnsHEdit: TEdit
        Left = 163
        Top = 296
        Width = 25
        Height = 21
        HelpContext = 2908
        TabOrder = 10
        Text = '22'
      end
      object MtnsIEdit: TEdit
        Left = 198
        Top = 296
        Width = 33
        Height = 21
        Hint = 'Increase'
        HelpContext = 2909
        TabOrder = 11
        Text = '36'
      end
      object MtnsKEdit: TEdit
        Left = 240
        Top = 296
        Width = 49
        Height = 21
        HelpContext = 2460
        TabOrder = 12
        Text = '20000'
      end
      object MtnsRLSCB: TComboBox
        Left = 176
        Top = 88
        Width = 97
        Height = 21
        Hint = 'pick one to size dem'
        HelpContext = 4500
        ItemHeight = 13
        TabOrder = 13
        Text = '320x 240y'
        OnChange = MtnsRLSCBChange
        Items.Strings = (
          '320x 240y'
          '640x 480y'
          '800x 600y'
          '360x 180y'
          '720x 360y'
          '256x 256y'
          '512x 512y'
          '1024x 1024y'
          'Input Zx Zy')
      end
      object MtnsRLSRG: TRadioGroup
        Left = 0
        Top = 72
        Width = 169
        Height = 129
        Hint = 'Select|Left redo, Right more'
        HelpContext = 4500
        Caption = 'Random Land Scapes'
        ItemIndex = 0
        Items.Strings = (
          'Fractal Tin Mtns [r] [d]'
          'Folded Globe [k]'
          'DupliGlobe [k]'
          'Craters [r] [g] [i]'
          'Waves [h] [i]'
          'Rippled [g] [h] [i]'
          'Splattered [g] [h] [i]')
        TabOrder = 14
      end
      object MtnsProgressBar: TProgressBar
        Left = 174
        Top = 116
        Width = 100
        Height = 16
        Min = 0
        Max = 100
        Smooth = True
        Step = 1
        TabOrder = 15
      end
      object RgbToDemEdit: TEdit
        Left = 24
        Top = 200
        Width = 121
        Height = 21
        TabOrder = 16
        Text = 'Import Image.bmp'
      end
    end
    object TabSheet2: TTabSheet
      HelpContext = 4000
      Caption = 'Todo'
      ImageIndex = 4
      object Label12: TLabel
        Left = 8
        Top = 8
        Width = 280
        Height = 13
        Caption = 'Select file edge as '#39'Seed'#39'... add random #s to fill new image'
      end
      object Label13: TLabel
        Left = 8
        Top = 248
        Width = 100
        Height = 13
        Caption = 'TIN.. generator types'
      end
      object Label14: TLabel
        Left = 8
        Top = 232
        Width = 208
        Height = 13
        Caption = 'Splatter objects onto image using the mouse'
      end
      object Label16: TLabel
        Left = 8
        Top = 216
        Width = 263
        Height = 13
        Caption = 'Mouse an area (or 256x256 box) to Crop data into a Tile'
      end
      object MtnsRLSMouseBtn: TSpeedButton
        Left = 224
        Top = 232
        Width = 24
        Height = 22
        Hint = 'Mouse Coordinates|View Mtn X,Y Coordinates'
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
      end
      object FileSeedDemBtn: TSpeedButton
        Left = 8
        Top = 32
        Width = 24
        Height = 22
        Hint = 'Load|Load a DEM.bin file'
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
        OnClick = FileSeedDemBtnClick
      end
      object Label18: TLabel
        Left = 8
        Top = 296
        Width = 250
        Height = 13
        Caption = 'Slope %s  : 2 7 10 17 24 45 > or   5 10 15 20 25 45 >'
      end
      object Label22: TLabel
        Left = 8
        Top = 264
        Width = 125
        Height = 13
        Caption = 'LOS AtoB, Center of Circle'
      end
      object Label23: TLabel
        Left = 8
        Top = 280
        Width = 156
        Height = 13
        Caption = 'Slope Direction NSEW Up-Down'
      end
      object SpeedButton1: TSpeedButton
        Left = 90
        Top = 141
        Width = 24
        Height = 22
        Hint = 'Load|Load a DEM.bin file'
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
        OnClick = FileSeedDemBtnClick
      end
      object Label27: TLabel
        Left = 16
        Top = 64
        Width = 173
        Height = 13
        Caption = 'Plot of GeoGlobe tile to select-create'
      end
      object Label28: TLabel
        Left = 8
        Top = 96
        Width = 168
        Height = 13
        Caption = 'Roughen/Smooth/paint: Landforms'
      end
      object Label29: TLabel
        Left = 16
        Top = 192
        Width = 161
        Height = 13
        Caption = 'DEM Import.. Export as .BT format'
      end
      object FileSeedEdit: TEdit
        Left = 32
        Top = 32
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Seed.flm'
      end
      object FileSeedRG: TRadioGroup
        Left = 152
        Top = 24
        Width = 129
        Height = 29
        HelpContext = 4401
        Caption = 'Seed side'
        Columns = 4
        ItemIndex = 0
        Items.Strings = (
          'N'
          'S'
          'E'
          'W')
        TabOrder = 1
      end
      object TodoTransRG: TRadioGroup
        Left = 8
        Top = 112
        Width = 81
        Height = 73
        Caption = 'Trans Tile'
        ItemIndex = 0
        Items.Strings = (
          'Add'
          'Subtract'
          'Multiply'
          'Divide')
        TabOrder = 2
      end
      object TodoTransEdit: TEdit
        Left = 88
        Top = 118
        Width = 49
        Height = 21
        TabOrder = 3
        Text = '123'
      end
      object TodoTNameEdit: TEdit
        Left = 88
        Top = 164
        Width = 73
        Height = 21
        TabOrder = 4
        Text = 'TransTile.flm'
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Title = 'Fractl3D DTM'
    Left = 272
    Top = 80
  end
  object SaveDialog1: TSaveDialog
    Title = 'Fractl3D DTM'
    Left = 272
    Top = 32
  end
end

object AProjectOptionsForm: TAProjectOptionsForm
  Left = 212
  Top = 111
  Width = 544
  Height = 375
  Caption = 'AStar Project File (apf)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010002002020100000000000E80200002600000010101000000000002801
    00000E0300002800000020000000400000000100040000000000800200000000
    0000000000000000000000000000000000000000800000800000008080008000
    0000800080008080000080808000C0C0C0000000FF0000FF000000FFFF00FF00
    0000FF00FF00FFFF0000FFFFFF00000000000000000000000000000000000000
    088888888888888870000000000000000F8FFFFFFFFFFF887000000000000000
    0878F888888888F870700000000000000F7F8F88888888F87770000000000000
    0878F8F8888888F877700000000000000F7F8F8F888888F87770000000000000
    0878F8F8F88888F8777000000000000008788F8F8F8888F87770000000000000
    087888F8F8F888F877700000000000000878888F8F8F88F87770000000000000
    08788888F8F8F8F8777000000000000008877777777777887770000000000000
    0888888888F8F8F8777000000000000000000000000000000770000000000000
    FFFFFFFFFFFFFFFFF0700000000000007F8888F8F888888FF700000000000000
    7888888F8F888888F71111100000000078888888F8F88888F708999000000000
    7888F0F0F0F0F888F7008990000000007888FFFFFFFFF888F700899000000000
    7888888888F8F888F70899000000000078888888888F8F88F799990000000000
    7888FFF0F0F0F8F8F703B000000000007888FFFFFFFFFF8FF703B00000000000
    78888888888888F8F703B00000000000788888888888888FF03B000000000000
    7777777777777777F70B0000000000000F8F8F8F8F8F8F8F8770000000000000
    00F8F8F8F8F8F8F8F877000000000000000F8F8F8F8F8F8F8F87000000000000
    0000000000000000000000000000F00007FFF000001FF0000007F00000C3F000
    00F3F00000F3F00000E7F00000C7F00000CFF000008FF000009FF000009FF000
    009FF000009FF000009FE000009FE000009FE000001FE000000FE000000FE000
    000FE000001FE000001FE000003FE000003FE000003FE000007FE000007FF000
    007FF800007FFC00007FFE0000FF280000001000000020000000010004000000
    0000C00000000000000000000000000000000000000000000000000080000080
    00000080800080000000800080008080000080808000C0C0C0000000FF0000FF
    000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000000000000008
    88888800000000087FFFF8700000000878888F700000000878888F7000000008
    87777F70000000088888887000000000000000000000007FFFFFFFF000000078
    000088F010000078888888F010000078800088F300000077777777FB00000000
    00000000000000088888888000000000000000000000C0070000C0030000C00D
    0000C0090000C00B0000C00B0000C00B00008003000080030000800300008003
    0000800300008007000080070000C0070000E0070000}
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object AMainPageControl: TPageControl
    Left = 0
    Top = 0
    Width = 536
    Height = 348
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Project'
      object OpenapfBtn: TSpeedButton
        Left = 8
        Top = 4
        Width = 84
        Height = 22
        Caption = 'Open File.apf'
        OnClick = OpenapfBtnClick
      end
      object SaveapfBtn: TSpeedButton
        Left = 304
        Top = 4
        Width = 84
        Height = 22
        Caption = 'Save File.apf'
        OnClick = SaveapfBtnClick
      end
      object Label74: TLabel
        Left = 336
        Top = 80
        Width = 136
        Height = 13
        Caption = 'Average Terrain (H Estimate)'
      end
      object Label32: TLabel
        Left = 336
        Top = 120
        Width = 141
        Height = 13
        Caption = 'Terrain (MAG) G Cost = NoGo'
      end
      object Label97: TLabel
        Left = 336
        Top = 40
        Width = 150
        Height = 13
        Caption = 'Terrain Value Alpha ( 0.0 .. 1.0 )'
      end
      object Label98: TLabel
        Left = 336
        Top = 272
        Width = 184
        Height = 13
        Caption = '(NA) Turning penalty Alpha ( 0.0 .. 1.0 )'
      end
      object Label99: TLabel
        Left = 336
        Top = 160
        Width = 143
        Height = 13
        Caption = ' Adjacent Paths Penalty Alpha'
      end
      object Label101: TLabel
        Left = 392
        Top = 176
        Width = 114
        Height = 13
        Caption = '( 0.0 .. 1.0 ) (of 20 or 28)'
      end
      object ExitBtn: TBitBtn
        Left = 448
        Top = 4
        Width = 75
        Height = 25
        TabOrder = 0
        Kind = bkOK
      end
      object FileApfEdit: TEdit
        Left = 96
        Top = 4
        Width = 200
        Height = 21
        TabOrder = 1
      end
      object GridLinesDisplayedCB: TCheckBox
        Left = 336
        Top = 232
        Width = 145
        Height = 17
        Caption = 'Grid Lines Displayed (NA)'
        TabOrder = 2
      end
      object PathDisplayedCB: TCheckBox
        Left = 336
        Top = 200
        Width = 97
        Height = 17
        Caption = 'Path Displayed'
        TabOrder = 3
      end
      object ClaimedNodesDisplayedCB: TCheckBox
        Left = 336
        Top = 248
        Width = 169
        Height = 17
        Caption = 'Claimed Nodes Displayed (NA)'
        TabOrder = 4
      end
      object TerrainValueAlphaEdit: TEdit
        Left = 336
        Top = 58
        Width = 49
        Height = 21
        TabOrder = 5
        Text = '0.05'
      end
      object ProcessNoGoIslandsCB: TCheckBox
        Left = 336
        Top = 216
        Width = 129
        Height = 17
        Caption = 'Process NoGo Islands'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object TurningpenaltyEdit: TEdit
        Left = 336
        Top = 288
        Width = 49
        Height = 21
        TabOrder = 7
        Text = '0.5'
      end
      object AverageTerrainEdit: TEdit
        Left = 336
        Top = 96
        Width = 49
        Height = 21
        TabOrder = 8
        Text = '3'
      end
      object TextureSizeRG: TRadioGroup
        Left = 160
        Top = 40
        Width = 89
        Height = 97
        Caption = 'Texture Size'
        ItemIndex = 1
        Items.Strings = (
          '256x256'
          '512x512'
          '1024x1024'
          '2048x2048'
          '4096x4096')
        TabOrder = 9
      end
      object TileSizeRG: TRadioGroup
        Left = 256
        Top = 40
        Width = 65
        Height = 121
        Caption = 'Tile Size'
        ItemIndex = 5
        Items.Strings = (
          '32x32'
          '16x16'
          '8x8'
          '4x4'
          '2x2'
          '1x1')
        TabOrder = 10
      end
      object TerrainMagNoGoEdit: TEdit
        Left = 336
        Top = 136
        Width = 49
        Height = 21
        TabOrder = 11
        Text = '140'
      end
      object HeightfieldSizeRG: TRadioGroup
        Left = 8
        Top = 40
        Width = 89
        Height = 64
        Caption = 'Heightfield Size'
        ItemIndex = 0
        Items.Strings = (
          '256x256'
          '512x512'
          '1024x1024')
        TabOrder = 12
      end
      object RatioRG: TRadioGroup
        Left = 104
        Top = 40
        Width = 49
        Height = 97
        Caption = 'Ratio'
        ItemIndex = 1
        Items.Strings = (
          'x1'
          'x2'
          'x4'
          'x8')
        TabOrder = 13
      end
      object TerrainAPPAEdit: TEdit
        Left = 336
        Top = 176
        Width = 49
        Height = 21
        TabOrder = 14
        Text = '0.5'
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Units'
      ImageIndex = 3
      object OpenaufBtn: TSpeedButton
        Left = 4
        Top = 4
        Width = 84
        Height = 22
        Caption = 'Open File.auf'
        OnClick = OpenaufBtnClick
      end
      object SaveaufBtn: TSpeedButton
        Left = 294
        Top = 4
        Width = 84
        Height = 22
        Caption = 'Save File.auf'
        OnClick = SaveaufBtnClick
      end
      object Label81: TLabel
        Left = 432
        Top = 2
        Width = 76
        Height = 13
        Caption = '# of Units (1..5):'
      end
      object PrintaufUnitsBtn: TSpeedButton
        Left = 380
        Top = 4
        Width = 37
        Height = 22
        Caption = 'Print'
        OnClick = PrintaufUnitsBtnClick
      end
      object AUnitsPageControl: TPageControl
        Left = 0
        Top = 28
        Width = 528
        Height = 292
        ActivePage = TabSheet9
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object TabSheet9: TTabSheet
          Caption = 'Unit 1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          object Label8: TLabel
            Left = 72
            Top = 128
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label9: TLabel
            Left = 176
            Top = 88
            Width = 68
            Height = 13
            Caption = '# of Members:'
          end
          object Label7: TLabel
            Left = 80
            Top = 48
            Width = 54
            Height = 13
            Caption = 'Target: X,Y'
          end
          object Label6: TLabel
            Left = 72
            Top = 8
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label1: TLabel
            Left = 0
            Top = 88
            Width = 31
            Height = 13
            Caption = 'Speed'
          end
          object Label2: TLabel
            Left = 48
            Top = 88
            Width = 74
            Height = 13
            Caption = 'Manueverability'
          end
          object Label52: TLabel
            Left = 128
            Top = 88
            Width = 27
            Height = 13
            Caption = 'Armor'
          end
          object Openaff1Btn: TSpeedButton
            Tag = 1
            Left = 392
            Top = 28
            Width = 122
            Height = 22
            Caption = 'Target Texture .bmp'
            OnClick = Openaff1BtnClick
          end
          object Label67: TLabel
            Left = 51
            Top = 104
            Width = 18
            Height = 13
            Caption = '0..9'
          end
          object Openamd1Btn: TSpeedButton
            Tag = 1
            Left = 390
            Top = 108
            Width = 122
            Height = 22
            Caption = 'Open Actor.md2'
            OnClick = Openamd1BtnClick
          end
          object Openajp1Btn: TSpeedButton
            Tag = 1
            Left = 390
            Top = 156
            Width = 122
            Height = 22
            Caption = 'Actor Texture'
            OnClick = Openajp1BtnClick
          end
          object Label90: TLabel
            Left = 392
            Top = 82
            Width = 55
            Height = 13
            Caption = 'Actor Scale'
          end
          object OpenTexture1Btn: TSpeedButton
            Tag = 1
            Left = 392
            Top = 236
            Width = 41
            Height = 22
            Caption = 'Texture'
            OnClick = OpenTexture6BtnClick
          end
          object OpenWpn1Btn: TSpeedButton
            Left = 391
            Top = 206
            Width = 42
            Height = 22
            Caption = 'Wpn'
            OnClick = OpenWpn1BtnClick
          end
          object Label40: TLabel
            Left = 392
            Top = 2
            Width = 54
            Height = 13
            Caption = 'Target Size'
          end
          object Unit1M5YEdit: TEdit
            Left = 128
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 0
            Text = '-2'
          end
          object Unit1M4YEdit: TEdit
            Left = 128
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 1
            Text = '-2'
          end
          object Unit1M3YEdit: TEdit
            Left = 128
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 2
            Text = '0'
          end
          object Unit1M2YEdit: TEdit
            Left = 128
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 3
            Text = '0'
          end
          object Unit1M1YEdit: TEdit
            Left = 128
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 4
            Text = '2'
          end
          object Unit1M5XEdit: TEdit
            Left = 0
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 5
            Text = '-2'
          end
          object Unit1M4XEdit: TEdit
            Left = 0
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 6
            Text = '2'
          end
          object Unit1M3XEdit: TEdit
            Left = 0
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 7
            Text = '-2'
          end
          object Unit1M2XEdit: TEdit
            Left = 0
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 8
            Text = '2'
          end
          object Unit1M1XEdit: TEdit
            Left = 0
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 9
            Text = '2'
          end
          object Unit1MEdit: TEdit
            Left = 216
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 10
            Text = '5'
          end
          object Unit1TargetXEdit: TEdit
            Left = 0
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 11
            Text = '488'
          end
          object Unit1TargetYEdit: TEdit
            Left = 128
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 12
            Text = '255'
          end
          object Unit1StartYEdit: TEdit
            Left = 128
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 13
            Text = '200'
          end
          object Unit1StartXEdit: TEdit
            Left = 0
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 14
            Text = '10'
          end
          object Unit1SpeedEdit: TEdit
            Left = 0
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 15
            Text = '5'
          end
          object Unit1ManEdit: TEdit
            Left = 88
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 16
            Text = '5'
          end
          object Unit1ArmorEdit: TEdit
            Left = 128
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 17
            Text = '5'
          end
          object Unit1OperatingOrdersRG: TRadioGroup
            Left = 256
            Top = 2
            Width = 122
            Height = 95
            Caption = 'Operating Orders'
            ItemIndex = 0
            Items.Strings = (
              'Search to Target'
              'Right Flank Unit 1'
              'Left Flank Unit 1'
              'Pete and Repeat'
              'Random Walk')
            TabOrder = 18
          end
          object FileAff1Edit: TEdit
            Tag = 1
            Left = 392
            Top = 52
            Width = 122
            Height = 21
            TabOrder = 19
          end
          object Unit1SearchRG: TRadioGroup
            Left = 256
            Top = 100
            Width = 89
            Height = 89
            Caption = 'Search'
            ItemIndex = 2
            Items.Strings = (
              'Dijkstra'
              'Diagonal'
              'Manhattan'
              'BestFirst'
              'Euclidean')
            TabOrder = 20
          end
          object Unit1TieBreakerRG: TRadioGroup
            Left = 256
            Top = 190
            Width = 65
            Height = 73
            Caption = 'TieBreaker'
            ItemIndex = 0
            Items.Strings = (
              'None'
              'Straight'
              'Close'
              'Far')
            TabOrder = 21
          end
          object FileAmd1Edit: TEdit
            Tag = 1
            Left = 390
            Top = 132
            Width = 122
            Height = 21
            TabOrder = 22
          end
          object FileAjp1Edit: TEdit
            Tag = 1
            Left = 390
            Top = 180
            Width = 122
            Height = 21
            TabOrder = 23
          end
          object Unit1ActorScaleEdit: TEdit
            Left = 464
            Top = 82
            Width = 49
            Height = 21
            TabOrder = 24
            Text = '0.04'
          end
          object WpnEdit1: TEdit
            Tag = 1
            Left = 438
            Top = 206
            Width = 75
            Height = 21
            TabOrder = 25
          end
          object WpnTextureEdit1: TEdit
            Tag = 1
            Left = 438
            Top = 236
            Width = 75
            Height = 21
            TabOrder = 26
          end
          object TargetSizeEdit1: TEdit
            Left = 463
            Top = 2
            Width = 49
            Height = 21
            TabOrder = 27
            Text = '2'
          end
        end
        object TabSheet10: TTabSheet
          Caption = 'Unit 2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 1
          ParentFont = False
          object Label10: TLabel
            Left = 72
            Top = 128
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label11: TLabel
            Left = 184
            Top = 88
            Width = 68
            Height = 13
            Caption = '# of Members:'
          end
          object Label12: TLabel
            Left = 80
            Top = 48
            Width = 54
            Height = 13
            Caption = 'Target: X,Y'
          end
          object Label13: TLabel
            Left = 72
            Top = 8
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label3: TLabel
            Left = 0
            Top = 88
            Width = 31
            Height = 13
            Caption = 'Speed'
          end
          object Label4: TLabel
            Left = 128
            Top = 88
            Width = 27
            Height = 13
            Caption = 'Armor'
          end
          object Label53: TLabel
            Left = 48
            Top = 88
            Width = 74
            Height = 13
            Caption = 'Manueverability'
          end
          object Openaff2Btn: TSpeedButton
            Tag = 2
            Left = 392
            Top = 28
            Width = 122
            Height = 22
            Caption = 'Target Texture .bmp'
            OnClick = Openaff1BtnClick
          end
          object Openajp2Btn: TSpeedButton
            Tag = 2
            Left = 390
            Top = 156
            Width = 122
            Height = 22
            Caption = 'Actor Texture'
            OnClick = Openajp1BtnClick
          end
          object Openamd2Btn: TSpeedButton
            Tag = 2
            Left = 390
            Top = 108
            Width = 122
            Height = 22
            Caption = 'Open Actor.md2'
            OnClick = Openamd1BtnClick
          end
          object Label91: TLabel
            Left = 392
            Top = 82
            Width = 55
            Height = 13
            Caption = 'Actor Scale'
          end
          object OpenWpn2Btn: TSpeedButton
            Tag = 2
            Left = 391
            Top = 206
            Width = 42
            Height = 22
            Caption = 'Wpn'
            OnClick = OpenWpn1BtnClick
          end
          object OpenTexture2Btn: TSpeedButton
            Tag = 2
            Left = 392
            Top = 236
            Width = 41
            Height = 22
            Caption = 'Texture'
            OnClick = OpenTexture6BtnClick
          end
          object Label48: TLabel
            Left = 392
            Top = 2
            Width = 54
            Height = 13
            Caption = 'Target Size'
          end
          object Unit2MEdit: TEdit
            Left = 216
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 0
            Text = '5'
          end
          object Unit2TargetXEdit: TEdit
            Left = 0
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 1
            Text = '450'
          end
          object Unit2TargetYEdit: TEdit
            Left = 128
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 2
            Text = '450'
          end
          object Unit2StartYEdit: TEdit
            Left = 128
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 3
            Text = '300'
          end
          object Unit2StartXEdit: TEdit
            Left = 0
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 4
            Text = '10'
          end
          object Unit2SpeedEdit: TEdit
            Left = 0
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 5
            Text = '5'
          end
          object Unit2ArmorEdit: TEdit
            Left = 128
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 6
            Text = '5'
          end
          object Unit2ManEdit: TEdit
            Left = 88
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 7
            Text = '5'
          end
          object Unit2OperatingOrdersRG: TRadioGroup
            Left = 256
            Top = 2
            Width = 121
            Height = 95
            Caption = 'Operating Orders'
            ItemIndex = 1
            Items.Strings = (
              'Search to Target'
              'Right Flank Unit 1'
              'Left Flank Unit 1'
              'Pete and Repeat'
              'Random Walk')
            TabOrder = 8
          end
          object Unit2M1XEdit: TEdit
            Left = 0
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 9
            Text = '2'
          end
          object Unit2M2XEdit: TEdit
            Left = 0
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 10
            Text = '2'
          end
          object Unit2M3XEdit: TEdit
            Left = 0
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 11
            Text = '-2'
          end
          object Unit2M4XEdit: TEdit
            Left = 0
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 12
            Text = '2'
          end
          object Unit2M5XEdit: TEdit
            Left = 0
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 13
            Text = '-2'
          end
          object Unit2M5YEdit: TEdit
            Left = 128
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 14
            Text = '-2'
          end
          object Unit2M4YEdit: TEdit
            Left = 128
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 15
            Text = '-2'
          end
          object Unit2M3YEdit: TEdit
            Left = 128
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 16
            Text = '0'
          end
          object Unit2M2YEdit: TEdit
            Left = 128
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 17
            Text = '0'
          end
          object Unit2M1YEdit: TEdit
            Left = 128
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 18
            Text = '2'
          end
          object FileAff2Edit: TEdit
            Tag = 2
            Left = 392
            Top = 52
            Width = 122
            Height = 21
            TabOrder = 19
          end
          object Unit2SearchRG: TRadioGroup
            Left = 256
            Top = 100
            Width = 89
            Height = 89
            Caption = 'Search'
            ItemIndex = 2
            Items.Strings = (
              'Dijkstra'
              'Diagonal'
              'Manhattan'
              'BestFirst'
              'Euclidean')
            TabOrder = 20
          end
          object Unit2TieBreakerRG: TRadioGroup
            Left = 256
            Top = 190
            Width = 65
            Height = 73
            Caption = 'TieBreaker'
            ItemIndex = 0
            Items.Strings = (
              'None'
              'Straight'
              'Close'
              'Far')
            TabOrder = 21
          end
          object FileAjp2Edit: TEdit
            Tag = 1
            Left = 390
            Top = 180
            Width = 122
            Height = 21
            TabOrder = 22
          end
          object FileAmd2Edit: TEdit
            Tag = 1
            Left = 390
            Top = 132
            Width = 122
            Height = 21
            TabOrder = 23
          end
          object Unit2ActorScaleEdit: TEdit
            Left = 464
            Top = 82
            Width = 49
            Height = 21
            TabOrder = 24
            Text = '0.04'
          end
          object WpnTextureEdit2: TEdit
            Tag = 1
            Left = 438
            Top = 236
            Width = 75
            Height = 21
            TabOrder = 25
          end
          object WpnEdit2: TEdit
            Tag = 1
            Left = 438
            Top = 206
            Width = 75
            Height = 21
            TabOrder = 26
          end
          object TargetSizeEdit2: TEdit
            Left = 463
            Top = 2
            Width = 49
            Height = 21
            TabOrder = 27
            Text = '2'
          end
        end
        object TabSheet11: TTabSheet
          Caption = 'Unit 3'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 2
          ParentFont = False
          object Label14: TLabel
            Left = 72
            Top = 128
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label15: TLabel
            Left = 184
            Top = 88
            Width = 68
            Height = 13
            Caption = '# of Members:'
          end
          object Label16: TLabel
            Left = 80
            Top = 48
            Width = 54
            Height = 13
            Caption = 'Target: X,Y'
          end
          object Label17: TLabel
            Left = 72
            Top = 8
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label5: TLabel
            Left = 0
            Top = 88
            Width = 31
            Height = 13
            Caption = 'Speed'
          end
          object Label35: TLabel
            Left = 128
            Top = 88
            Width = 27
            Height = 13
            Caption = 'Armor'
          end
          object Label54: TLabel
            Left = 48
            Top = 88
            Width = 74
            Height = 13
            Caption = 'Manueverability'
          end
          object Openaff3Btn: TSpeedButton
            Tag = 3
            Left = 392
            Top = 28
            Width = 122
            Height = 22
            Caption = 'Target Texture .bmp'
            OnClick = Openaff1BtnClick
          end
          object Openajp3Btn: TSpeedButton
            Tag = 3
            Left = 390
            Top = 156
            Width = 122
            Height = 22
            Caption = 'Actor Texture'
            OnClick = Openajp1BtnClick
          end
          object Openamd3Btn: TSpeedButton
            Tag = 3
            Left = 390
            Top = 108
            Width = 122
            Height = 22
            Caption = 'Open Actor.md2'
            OnClick = Openamd1BtnClick
          end
          object Label92: TLabel
            Left = 392
            Top = 82
            Width = 55
            Height = 13
            Caption = 'Actor Scale'
          end
          object OpenWpn3Btn: TSpeedButton
            Tag = 3
            Left = 391
            Top = 206
            Width = 42
            Height = 22
            Caption = 'Wpn'
            OnClick = OpenWpn1BtnClick
          end
          object OpenTexture3Btn: TSpeedButton
            Tag = 3
            Left = 392
            Top = 236
            Width = 41
            Height = 22
            Caption = 'Texture'
            OnClick = OpenTexture6BtnClick
          end
          object Label64: TLabel
            Left = 392
            Top = 2
            Width = 54
            Height = 13
            Caption = 'Target Size'
          end
          object Unit3MEdit: TEdit
            Left = 216
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 0
            Text = '5'
          end
          object Unit3TargetXEdit: TEdit
            Left = 0
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 1
            Text = '450'
          end
          object Unit3TargetYEdit: TEdit
            Left = 128
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 2
            Text = '300'
          end
          object Unit3StartYEdit: TEdit
            Left = 128
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 3
            Text = '100'
          end
          object Unit3StartXEdit: TEdit
            Left = 0
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 4
            Text = '10'
          end
          object Unit3SpeedEdit: TEdit
            Left = 0
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 5
            Text = '5'
          end
          object Unit3ArmorEdit: TEdit
            Left = 128
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 6
            Text = '5'
          end
          object Unit3ManEdit: TEdit
            Left = 88
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 7
            Text = '5'
          end
          object Unit3OperatingOrdersRG: TRadioGroup
            Left = 256
            Top = 2
            Width = 121
            Height = 95
            Caption = 'Operating Orders'
            ItemIndex = 2
            Items.Strings = (
              'Search to Target'
              'Right Flank Unit 1'
              'Left Flank Unit 1'
              'Pete and Repeat'
              'Random Walk')
            TabOrder = 8
          end
          object Unit3M1XEdit: TEdit
            Left = 0
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 9
            Text = '2'
          end
          object Unit3M1YEdit: TEdit
            Left = 128
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 10
            Text = '2'
          end
          object Unit3M2XEdit: TEdit
            Left = 0
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 11
            Text = '2'
          end
          object Unit3M2YEdit: TEdit
            Left = 128
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 12
            Text = '0'
          end
          object Unit3M3XEdit: TEdit
            Left = 0
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 13
            Text = '-2'
          end
          object Unit3M3YEdit: TEdit
            Left = 128
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 14
            Text = '0'
          end
          object Unit3M4XEdit: TEdit
            Left = 0
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 15
            Text = '2'
          end
          object Unit3M4YEdit: TEdit
            Left = 128
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 16
            Text = '-2'
          end
          object Unit3M5XEdit: TEdit
            Left = 0
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 17
            Text = '-2'
          end
          object Unit3M5YEdit: TEdit
            Left = 128
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 18
            Text = '-2'
          end
          object FileAff3Edit: TEdit
            Tag = 3
            Left = 392
            Top = 52
            Width = 122
            Height = 21
            TabOrder = 19
          end
          object Unit3SearchRG: TRadioGroup
            Left = 256
            Top = 100
            Width = 89
            Height = 89
            Caption = 'Search'
            ItemIndex = 2
            Items.Strings = (
              'Dijkstra'
              'Diagonal'
              'Manhattan'
              'BestFirst'
              'Euclidean')
            TabOrder = 20
          end
          object Unit3TieBreakerRG: TRadioGroup
            Left = 256
            Top = 190
            Width = 65
            Height = 73
            Caption = 'TieBreaker'
            ItemIndex = 0
            Items.Strings = (
              'None'
              'Straight'
              'Close'
              'Far')
            TabOrder = 21
          end
          object FileAjp3Edit: TEdit
            Tag = 1
            Left = 390
            Top = 180
            Width = 122
            Height = 21
            TabOrder = 22
          end
          object FileAmd3Edit: TEdit
            Tag = 1
            Left = 390
            Top = 132
            Width = 122
            Height = 21
            TabOrder = 23
          end
          object Unit3ActorScaleEdit: TEdit
            Left = 464
            Top = 82
            Width = 49
            Height = 21
            TabOrder = 24
            Text = '0.04'
          end
          object WpnTextureEdit3: TEdit
            Tag = 1
            Left = 438
            Top = 236
            Width = 75
            Height = 21
            TabOrder = 25
          end
          object WpnEdit3: TEdit
            Tag = 1
            Left = 438
            Top = 206
            Width = 75
            Height = 21
            TabOrder = 26
          end
          object TargetSizeEdit3: TEdit
            Left = 463
            Top = 2
            Width = 49
            Height = 21
            TabOrder = 27
            Text = '2'
          end
        end
        object TabSheet12: TTabSheet
          Caption = 'Unit 4'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 3
          ParentFont = False
          object Label18: TLabel
            Left = 72
            Top = 128
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label19: TLabel
            Left = 184
            Top = 88
            Width = 68
            Height = 13
            Caption = '# of Members:'
          end
          object Label20: TLabel
            Left = 80
            Top = 48
            Width = 54
            Height = 13
            Caption = 'Target: X,Y'
          end
          object Label21: TLabel
            Left = 72
            Top = 8
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label36: TLabel
            Left = 0
            Top = 88
            Width = 31
            Height = 13
            Caption = 'Speed'
          end
          object Label37: TLabel
            Left = 128
            Top = 88
            Width = 27
            Height = 13
            Caption = 'Armor'
          end
          object Label55: TLabel
            Left = 48
            Top = 88
            Width = 74
            Height = 13
            Caption = 'Manueverability'
          end
          object Openaff4Btn: TSpeedButton
            Tag = 4
            Left = 392
            Top = 28
            Width = 122
            Height = 22
            Caption = 'Target Texture .bmp'
            OnClick = Openaff1BtnClick
          end
          object Openajp4Btn: TSpeedButton
            Tag = 4
            Left = 390
            Top = 156
            Width = 122
            Height = 22
            Caption = 'Actor Texture'
            OnClick = Openajp1BtnClick
          end
          object Openamd4Btn: TSpeedButton
            Tag = 4
            Left = 390
            Top = 108
            Width = 122
            Height = 22
            Caption = 'Open Actor.md2'
            OnClick = Openamd1BtnClick
          end
          object Label93: TLabel
            Left = 392
            Top = 82
            Width = 55
            Height = 13
            Caption = 'Actor Scale'
          end
          object OpenWpn4Btn: TSpeedButton
            Tag = 4
            Left = 391
            Top = 206
            Width = 42
            Height = 22
            Caption = 'Wpn'
            OnClick = OpenWpn1BtnClick
          end
          object OpenTexture4Btn: TSpeedButton
            Tag = 4
            Left = 392
            Top = 236
            Width = 41
            Height = 22
            Caption = 'Texture'
            OnClick = OpenTexture6BtnClick
          end
          object Label65: TLabel
            Left = 392
            Top = 2
            Width = 54
            Height = 13
            Caption = 'Target Size'
          end
          object Unit4M5YEdit: TEdit
            Left = 128
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 0
            Text = '0'
          end
          object Unit4M4YEdit: TEdit
            Left = 128
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 1
            Text = '0'
          end
          object Unit4M3YEdit: TEdit
            Left = 128
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 2
            Text = '0'
          end
          object Unit4M2YEdit: TEdit
            Left = 128
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 3
            Text = '0'
          end
          object Unit4M1YEdit: TEdit
            Left = 128
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 4
            Text = '-2'
          end
          object Unit4M5XEdit: TEdit
            Left = 0
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 5
            Text = '-4'
          end
          object Unit4M4XEdit: TEdit
            Left = 0
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 6
            Text = '-2'
          end
          object Unit4M3XEdit: TEdit
            Left = 0
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 7
            Text = '2'
          end
          object Unit4M2XEdit: TEdit
            Left = 0
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 8
            Text = '4'
          end
          object Unit4M1XEdit: TEdit
            Left = 0
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 9
            Text = '-2'
          end
          object Unit4MEdit: TEdit
            Left = 216
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 10
            Text = '5'
          end
          object Unit4TargetXEdit: TEdit
            Left = 0
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 11
            Text = '450'
          end
          object Unit4TargetYEdit: TEdit
            Left = 128
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 12
            Text = '100'
          end
          object Unit4StartYEdit: TEdit
            Left = 128
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 13
            Text = '400'
          end
          object Unit4StartXEdit: TEdit
            Left = 0
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 14
            Text = '10'
          end
          object Unit4SpeedEdit: TEdit
            Left = 0
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 15
            Text = '3'
          end
          object Unit4ArmorEdit: TEdit
            Left = 128
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 16
            Text = '2'
          end
          object Unit4ManEdit: TEdit
            Left = 88
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 17
            Text = '7'
          end
          object Unit4OperatingOrdersRG: TRadioGroup
            Left = 256
            Top = 2
            Width = 121
            Height = 97
            Caption = 'Operating Orders'
            ItemIndex = 4
            Items.Strings = (
              'Search to Target'
              'Right Flank Unit 1'
              'Left Flank Unit 1'
              'Pete and Repeat'
              'Random Walk')
            TabOrder = 18
          end
          object FileAff4Edit: TEdit
            Tag = 4
            Left = 392
            Top = 52
            Width = 122
            Height = 21
            TabOrder = 19
          end
          object Unit4SearchRG: TRadioGroup
            Left = 256
            Top = 100
            Width = 89
            Height = 89
            Caption = 'Search'
            ItemIndex = 2
            Items.Strings = (
              'Dijkstra'
              'Diagonal'
              'Manhattan'
              'BestFirst'
              'Euclidean')
            TabOrder = 20
          end
          object Unit4TieBreakerRG: TRadioGroup
            Left = 256
            Top = 190
            Width = 65
            Height = 73
            Caption = 'TieBreaker'
            ItemIndex = 0
            Items.Strings = (
              'None'
              'Straight'
              'Close'
              'Far')
            TabOrder = 21
          end
          object FileAjp4Edit: TEdit
            Tag = 1
            Left = 390
            Top = 180
            Width = 122
            Height = 21
            TabOrder = 22
          end
          object FileAmd4Edit: TEdit
            Tag = 1
            Left = 390
            Top = 132
            Width = 122
            Height = 21
            TabOrder = 23
          end
          object Unit4ActorScaleEdit: TEdit
            Left = 464
            Top = 82
            Width = 49
            Height = 21
            TabOrder = 24
            Text = '0.04'
          end
          object WpnTextureEdit4: TEdit
            Tag = 1
            Left = 438
            Top = 236
            Width = 75
            Height = 21
            TabOrder = 25
          end
          object WpnEdit4: TEdit
            Tag = 1
            Left = 438
            Top = 206
            Width = 75
            Height = 21
            TabOrder = 26
          end
          object TargetSizeEdit4: TEdit
            Left = 463
            Top = 2
            Width = 49
            Height = 21
            TabOrder = 27
            Text = '2'
          end
        end
        object TabSheet13: TTabSheet
          Caption = 'Unit 5'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 4
          ParentFont = False
          object Label22: TLabel
            Left = 72
            Top = 128
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label23: TLabel
            Left = 184
            Top = 88
            Width = 68
            Height = 13
            Caption = '# of Members:'
          end
          object Label24: TLabel
            Left = 80
            Top = 48
            Width = 54
            Height = 13
            Caption = 'Target: X,Y'
            Color = clBtnFace
            ParentColor = False
          end
          object Label25: TLabel
            Left = 72
            Top = 8
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Label38: TLabel
            Left = 0
            Top = 88
            Width = 31
            Height = 13
            Caption = 'Speed'
          end
          object Label39: TLabel
            Left = 128
            Top = 88
            Width = 27
            Height = 13
            Caption = 'Armor'
          end
          object Label56: TLabel
            Left = 48
            Top = 88
            Width = 74
            Height = 13
            Caption = 'Manueverability'
          end
          object Openaff5Btn: TSpeedButton
            Tag = 5
            Left = 392
            Top = 28
            Width = 122
            Height = 22
            Caption = 'Target Texture .bmp'
            OnClick = Openaff1BtnClick
          end
          object Openajp5Btn: TSpeedButton
            Tag = 5
            Left = 390
            Top = 156
            Width = 122
            Height = 22
            Caption = 'Actor Texture'
            OnClick = Openajp1BtnClick
          end
          object Openamd5Btn: TSpeedButton
            Tag = 5
            Left = 390
            Top = 108
            Width = 122
            Height = 22
            Caption = 'Open Actor.md2'
            OnClick = Openamd1BtnClick
          end
          object Label94: TLabel
            Left = 392
            Top = 82
            Width = 55
            Height = 13
            Caption = 'Actor Scale'
          end
          object OpenWpn5Btn: TSpeedButton
            Tag = 5
            Left = 391
            Top = 206
            Width = 42
            Height = 22
            Caption = 'Wpn'
            OnClick = OpenWpn1BtnClick
          end
          object OpenTexture5Btn: TSpeedButton
            Tag = 5
            Left = 392
            Top = 236
            Width = 41
            Height = 22
            Caption = 'Texture'
            OnClick = OpenTexture6BtnClick
          end
          object Label66: TLabel
            Left = 392
            Top = 2
            Width = 54
            Height = 13
            Caption = 'Target Size'
          end
          object Unit5M5YEdit: TEdit
            Left = 128
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 0
            Text = '0'
          end
          object Unit5M4YEdit: TEdit
            Left = 128
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 1
            Text = '0'
          end
          object Unit5M3YEdit: TEdit
            Left = 128
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 2
            Text = '0'
          end
          object Unit5M2YEdit: TEdit
            Left = 128
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 3
            Text = '0'
          end
          object Unit5M1YEdit: TEdit
            Left = 128
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 4
            Text = '0'
          end
          object Unit5M5XEdit: TEdit
            Left = 0
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 5
            Text = '0'
          end
          object Unit5M4XEdit: TEdit
            Left = 0
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 6
            Text = '0'
          end
          object Unit5M3XEdit: TEdit
            Left = 0
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 7
            Text = '0'
          end
          object Unit5M2XEdit: TEdit
            Left = 0
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 8
            Text = '0'
          end
          object Unit5M1XEdit: TEdit
            Left = 0
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 9
            Text = '0'
          end
          object Unit5MEdit: TEdit
            Left = 216
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 10
            Text = '0'
          end
          object Unit5TargetXEdit: TEdit
            Left = 0
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 11
            Text = '450'
          end
          object Unit5TargetYEdit: TEdit
            Left = 128
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 12
            Text = '55'
          end
          object Unit5StartYEdit: TEdit
            Left = 128
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 13
            Text = '10'
          end
          object Unit5StartXEdit: TEdit
            Left = 0
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 14
            Text = '10'
          end
          object Unit5SpeedEdit: TEdit
            Left = 0
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 15
            Text = '7'
          end
          object Unit5ArmorEdit: TEdit
            Left = 128
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 16
            Text = '4'
          end
          object Unit5ManEdit: TEdit
            Left = 88
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 17
            Text = '6'
          end
          object Unit5OperatingOrdersRG: TRadioGroup
            Left = 256
            Top = 2
            Width = 121
            Height = 95
            Caption = 'Operating Orders'
            ItemIndex = 3
            Items.Strings = (
              'Search to Target'
              'Right Flank Unit 1'
              'Left Flank Unit 1'
              'Pete and Repeat'
              'Random Walk')
            TabOrder = 18
          end
          object FileAff5Edit: TEdit
            Tag = 5
            Left = 392
            Top = 52
            Width = 122
            Height = 21
            TabOrder = 19
          end
          object Unit5SearchRG: TRadioGroup
            Left = 256
            Top = 100
            Width = 89
            Height = 89
            Caption = 'Search'
            ItemIndex = 2
            Items.Strings = (
              'Dijkstra'
              'Diagonal'
              'Manhattan'
              'BestFirst'
              'Euclidean')
            TabOrder = 20
          end
          object Unit5TieBreakerRG: TRadioGroup
            Left = 256
            Top = 190
            Width = 65
            Height = 73
            Caption = 'TieBreaker'
            ItemIndex = 0
            Items.Strings = (
              'None'
              'Straight'
              'Close'
              'Far')
            TabOrder = 21
          end
          object FileAjp5Edit: TEdit
            Tag = 1
            Left = 390
            Top = 180
            Width = 122
            Height = 21
            TabOrder = 22
          end
          object FileAmd5Edit: TEdit
            Tag = 1
            Left = 390
            Top = 132
            Width = 122
            Height = 21
            TabOrder = 23
          end
          object Unit5ActorScaleEdit: TEdit
            Left = 464
            Top = 82
            Width = 49
            Height = 21
            TabOrder = 24
            Text = '0.04'
          end
          object WpnTextureEdit5: TEdit
            Tag = 1
            Left = 438
            Top = 236
            Width = 75
            Height = 21
            TabOrder = 25
          end
          object WpnEdit5: TEdit
            Tag = 1
            Left = 438
            Top = 206
            Width = 75
            Height = 21
            TabOrder = 26
          end
          object TargetSizeEdit5: TEdit
            Left = 463
            Top = 2
            Width = 49
            Height = 21
            TabOrder = 27
            Text = '2'
          end
        end
        object TabSheet24: TTabSheet
          Caption = 'Enemy'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 5
          ParentFont = False
          object Label27: TLabel
            Left = 8
            Top = 124
            Width = 108
            Height = 16
            Caption = 'Enemy Bunkers'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label29: TLabel
            Left = 128
            Top = 48
            Width = 67
            Height = 13
            Caption = '\/ Target: X,Y'
          end
          object Label28: TLabel
            Left = 0
            Top = 48
            Width = 73
            Height = 13
            Caption = '^ Location: X,Y'
          end
          object Label26: TLabel
            Left = 0
            Top = 4
            Width = 149
            Height = 16
            Caption = 'Enemy Patrolling Unit'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label61: TLabel
            Left = 0
            Top = 88
            Width = 31
            Height = 13
            Caption = 'Speed'
          end
          object Label62: TLabel
            Left = 48
            Top = 88
            Width = 74
            Height = 13
            Caption = 'Manueverability'
          end
          object Label63: TLabel
            Left = 128
            Top = 88
            Width = 27
            Height = 13
            Caption = 'Armor'
          end
          object Openaff6Btn: TSpeedButton
            Tag = 6
            Left = 392
            Top = 28
            Width = 122
            Height = 22
            Caption = 'Bunker Texture .bmp'
            OnClick = Openaff1BtnClick
          end
          object Label76: TLabel
            Left = 184
            Top = 88
            Width = 68
            Height = 13
            Caption = '# of Members:'
          end
          object Label30: TLabel
            Left = 120
            Top = 128
            Width = 64
            Height = 13
            Caption = 'Location: X,Y'
          end
          object Openajp6Btn: TSpeedButton
            Tag = 6
            Left = 390
            Top = 156
            Width = 122
            Height = 22
            Caption = 'Actor Texture'
            OnClick = Openajp1BtnClick
          end
          object Openamd6Btn: TSpeedButton
            Tag = 6
            Left = 390
            Top = 108
            Width = 122
            Height = 22
            Caption = 'Open Actor.md2'
            OnClick = Openamd1BtnClick
          end
          object Label95: TLabel
            Left = 392
            Top = 82
            Width = 55
            Height = 13
            Caption = 'Actor Scale'
          end
          object OpenWpn6Btn: TSpeedButton
            Left = 391
            Top = 206
            Width = 42
            Height = 22
            Caption = 'Wpn'
            OnClick = OpenWpn1BtnClick
          end
          object OpenTexture6Btn: TSpeedButton
            Tag = 6
            Left = 392
            Top = 236
            Width = 41
            Height = 22
            Caption = 'Texture'
            OnClick = OpenTexture6BtnClick
          end
          object Label96: TLabel
            Left = 392
            Top = 2
            Width = 57
            Height = 13
            Caption = 'Bunker Size'
          end
          object Unit6M5YEdit: TEdit
            Left = 128
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 0
            Text = '500'
          end
          object Unit6M5XEdit: TEdit
            Left = 0
            Top = 240
            Width = 121
            Height = 21
            TabOrder = 1
            Text = '500'
          end
          object Unit6M4XEdit: TEdit
            Left = 0
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 2
            Text = '500'
          end
          object Unit6M4YEdit: TEdit
            Left = 128
            Top = 216
            Width = 121
            Height = 21
            TabOrder = 3
            Text = '400'
          end
          object Unit6M3YEdit: TEdit
            Left = 128
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 4
            Text = '300'
          end
          object Unit6M3XEdit: TEdit
            Left = 0
            Top = 192
            Width = 121
            Height = 21
            TabOrder = 5
            Text = '500'
          end
          object Unit6M2XEdit: TEdit
            Left = 0
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 6
            Text = '500'
          end
          object Unit6M2YEdit: TEdit
            Left = 128
            Top = 168
            Width = 121
            Height = 21
            TabOrder = 7
            Text = '200'
          end
          object Unit6M1YEdit: TEdit
            Left = 128
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 8
            Text = '100'
          end
          object Unit6M1XEdit: TEdit
            Left = 0
            Top = 144
            Width = 121
            Height = 21
            TabOrder = 9
            Text = '500'
          end
          object Unit6TargetXEdit: TEdit
            Left = 0
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 10
            Text = '150'
          end
          object Unit6TargetYEdit: TEdit
            Left = 128
            Top = 64
            Width = 121
            Height = 21
            TabOrder = 11
            Text = '150'
          end
          object Unit6StartYEdit: TEdit
            Left = 128
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 12
            Text = '250'
          end
          object Unit6StartXEdit: TEdit
            Left = 0
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 13
            Text = '500'
          end
          object Unit6OperatingOrdersRG: TRadioGroup
            Left = 256
            Top = 2
            Width = 121
            Height = 97
            Caption = 'Operating Orders'
            ItemIndex = 0
            Items.Strings = (
              'Search to Target'
              'Search to Engage'
              'Nana Nanu Now'
              'Pete and Repeat'
              'Random Walk')
            TabOrder = 14
          end
          object Unit6SpeedEdit: TEdit
            Left = 0
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 15
            Text = '5'
          end
          object Unit6ManEdit: TEdit
            Left = 88
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 16
            Text = '5'
          end
          object Unit6ArmorEdit: TEdit
            Left = 128
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 17
            Text = '5'
          end
          object FileAff6Edit: TEdit
            Tag = 6
            Left = 392
            Top = 52
            Width = 122
            Height = 21
            TabOrder = 18
          end
          object Unit6SearchRG: TRadioGroup
            Left = 256
            Top = 100
            Width = 89
            Height = 89
            Caption = 'Search'
            ItemIndex = 2
            Items.Strings = (
              'Dijkstra'
              'Diagonal'
              'Manhattan'
              'BestFirst'
              'Euclidean')
            TabOrder = 19
          end
          object Unit6TieBreakerRG: TRadioGroup
            Left = 256
            Top = 190
            Width = 65
            Height = 73
            Caption = 'TieBreaker'
            ItemIndex = 0
            Items.Strings = (
              'None'
              'Straight'
              'Close'
              'Far')
            TabOrder = 20
          end
          object Unit6MEdit: TEdit
            Left = 216
            Top = 104
            Width = 32
            Height = 21
            TabOrder = 21
            Text = '5'
          end
          object Unit6CB: TCheckBox
            Left = 154
            Top = 2
            Width = 97
            Height = 17
            Caption = 'Enemy Active'
            Checked = True
            State = cbChecked
            TabOrder = 22
          end
          object FileAjp6Edit: TEdit
            Tag = 1
            Left = 390
            Top = 180
            Width = 122
            Height = 21
            TabOrder = 23
          end
          object FileAmd6Edit: TEdit
            Tag = 1
            Left = 390
            Top = 132
            Width = 122
            Height = 21
            TabOrder = 24
          end
          object Unit6ActorScaleEdit: TEdit
            Left = 464
            Top = 82
            Width = 49
            Height = 21
            TabOrder = 25
            Text = '0.04'
          end
          object WpnTextureEdit6: TEdit
            Tag = 1
            Left = 438
            Top = 236
            Width = 75
            Height = 21
            TabOrder = 26
          end
          object WpnEdit6: TEdit
            Tag = 1
            Left = 438
            Top = 206
            Width = 75
            Height = 21
            TabOrder = 27
          end
          object TargetSizeEdit6: TEdit
            Left = 463
            Top = 2
            Width = 49
            Height = 21
            TabOrder = 28
            Text = '2'
          end
        end
        object TabSheet16: TTabSheet
          Caption = 'AI'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 6
          ParentFont = False
          object Label89: TLabel
            Left = 344
            Top = 16
            Width = 147
            Height = 26
            Caption = 'acf: Microsoft Agent Character File (HTTP format)'
            Transparent = True
            WordWrap = True
          end
          object Label87: TLabel
            Left = 344
            Top = 48
            Width = 156
            Height = 26
            Caption = 'Quake2Animations.aaf Required to be in the Projects Directory'
            Transparent = True
            WordWrap = True
          end
          object UnitsAITaskListRG: TRadioGroup
            Left = 8
            Top = 2
            Width = 89
            Height = 255
            Caption = 'AI Task List'
            Items.Strings = (
              'Stop'
              'Stay'
              'Flee'
              'Retreat'
              'Explore'
              'Wander'
              'Search'
              'Spy'
              'Patrol'
              'Defend'
              'Guard'
              'Attack'
              'Surround'
              'Shun'
              'Avoid'
              'Follow'
              'Group'
              'Work')
            TabOrder = 0
          end
          object Panel1: TPanel
            Left = 128
            Top = 63
            Width = 161
            Height = 121
            TabOrder = 1
            object Label47: TLabel
              Left = 8
              Top = 104
              Width = 82
              Height = 13
              Caption = 'Group Movement'
            end
            object Label46: TLabel
              Left = 8
              Top = 88
              Width = 143
              Height = 13
              Caption = 'Collision Avoidance timebased'
            end
            object Label45: TLabel
              Left = 8
              Top = 72
              Width = 144
              Height = 13
              Caption = 'Collision Avoidance loopbased'
            end
            object Label44: TLabel
              Left = 8
              Top = 56
              Width = 115
              Height = 13
              Caption = 'Smiley and Chaser 4way'
            end
            object Label43: TLabel
              Left = 8
              Top = 40
              Width = 109
              Height = 13
              Caption = 'Star Smiley and Chaser'
            end
            object Label42: TLabel
              Left = 8
              Top = 24
              Width = 30
              Height = 13
              Caption = 'Smiley'
            end
            object Label41: TLabel
              Left = 8
              Top = 8
              Width = 98
              Height = 13
              Caption = 'AStar... Data Display'
            end
          end
        end
      end
      object FileAufEdit: TEdit
        Left = 90
        Top = 4
        Width = 200
        Height = 21
        TabOrder = 1
      end
      object NumberofActiveUnitsEdit: TEdit
        Left = 480
        Top = 18
        Width = 32
        Height = 21
        TabOrder = 2
        Text = '5'
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Terrain Data'
      ImageIndex = 2
      object OpenadfBtn: TSpeedButton
        Left = 8
        Top = 4
        Width = 84
        Height = 22
        Caption = 'Open File.adf'
        OnClick = OpenadfBtnClick
      end
      object SaveadfBtn: TSpeedButton
        Left = 304
        Top = 4
        Width = 84
        Height = 22
        Caption = 'Save File.adf'
        OnClick = SaveadfBtnClick
      end
      object FileAdfEdit: TEdit
        Left = 96
        Top = 4
        Width = 200
        Height = 21
        TabOrder = 0
      end
      object ATerrainPageControl: TPageControl
        Left = 0
        Top = 40
        Width = 528
        Height = 280
        ActivePage = TabSheet17
        Align = alBottom
        TabOrder = 1
        object TabSheet17: TTabSheet
          Caption = 'Height data'
          ImageIndex = 4
          object OpenHeightDataBtn: TSpeedButton
            Tag = 8
            Left = 8
            Top = 16
            Width = 161
            Height = 22
            Caption = 'Open Height data (.bmp)'
            OnClick = OpenHeightDataBtnClick
          end
          object Label71: TLabel
            Left = 88
            Top = 120
            Width = 230
            Height = 52
            Caption = 
              'Elevation is per data point, Set into Tiles: 32x32. The Texture ' +
              'for the data is normally 2,4,8 Times the size.. to make better i' +
              'mage overlay on the Terrain'
            WordWrap = True
          end
          object Label73: TLabel
            Left = 8
            Top = 48
            Width = 268
            Height = 13
            Caption = 'Each point of Height (Elevation) data is a Pixel or a Tile ?'
          end
          object Label84: TLabel
            Left = 8
            Top = 72
            Width = 304
            Height = 13
            Caption = 
              'That depends... 4 Demdata =1 PathTile, actually 4x4 (16) = 1 til' +
              'e'
          end
          object Label85: TLabel
            Left = 88
            Top = 96
            Width = 280
            Height = 13
            Caption = '1 Dem data = 4x4 (16) Pixels on the Texture (and Path tiles)'
          end
          object Label86: TLabel
            Left = 88
            Top = 184
            Width = 255
            Height = 13
            Caption = 'Pathfinder 4x4 area normally represent a Height datum'
          end
          object Label33: TLabel
            Left = 88
            Top = 208
            Width = 215
            Height = 13
            Caption = '256x256 Height .. 1024x1024 Texture,Map,...'
          end
          object OpenHeightDataEdit: TEdit
            Left = 176
            Top = 16
            Width = 200
            Height = 21
            TabOrder = 0
          end
        end
        object TabSheet7: TTabSheet
          Caption = 'Image (Texture)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 6
          ParentFont = False
          object OpenImageTextureBtn: TSpeedButton
            Tag = 8
            Left = 8
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Open File.bmp'
            OnClick = OpenImageTextureBtnClick
          end
          object Label68: TLabel
            Left = 32
            Top = 48
            Width = 406
            Height = 13
            Caption = 
              'All Terrain data are combined into an Image to be used as the '#39'T' +
              'exture'#39' .. Background.'
          end
          object Label72: TLabel
            Left = 32
            Top = 64
            Width = 223
            Height = 26
            Caption = 
              'Image size for the Project Viewers (2D and 3D)  would be the sam' +
              'e as per set:'
            WordWrap = True
          end
          object Label77: TLabel
            Left = 24
            Top = 152
            Width = 338
            Height = 13
            Caption = 
              'Terrain Data would be at Image / Tilesize = Map width X,  Map he' +
              'ight Y'
          end
          object Label78: TLabel
            Left = 24
            Top = 96
            Width = 193
            Height = 13
            Caption = ' 650x500..800x600... are for the Demos, '
          end
          object Label79: TLabel
            Left = 24
            Top = 128
            Width = 180
            Height = 13
            Caption = '256x256 .. 4096x4096 are for Projects'
          end
          object Label80: TLabel
            Left = 32
            Top = 112
            Width = 214
            Height = 13
            Caption = '640x640 .. 2560x2560 are ? maybe useless ?'
          end
          object FileImageTextureEdit: TEdit
            Left = 96
            Top = 16
            Width = 200
            Height = 21
            TabOrder = 0
          end
        end
        object TabSheet15: TTabSheet
          Caption = 'Map Attributes'
          ImageIndex = 3
          object Label83: TLabel
            Left = 40
            Top = 48
            Width = 424
            Height = 13
            Caption = 
              'All Terrain data are combined to be used as the Attribute data f' +
              'or Landform object creation'
          end
          object OpenamfBtn: TSpeedButton
            Tag = 8
            Left = 8
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Open File.amf'
            OnClick = OpenamfBtnClick
          end
          object SaveamfBtn: TSpeedButton
            Left = 304
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Save File.amf'
            OnClick = SaveamfBtnClick
          end
          object FileAmfEdit: TEdit
            Left = 96
            Top = 16
            Width = 200
            Height = 21
            TabOrder = 0
          end
          object PageControl1: TPageControl
            Left = 0
            Top = 80
            Width = 520
            Height = 172
            ActivePage = TabSheet18
            Align = alBottom
            TabOrder = 1
            object TabSheet18: TTabSheet
              Caption = 'Elevation'
              object Label50: TLabel
                Left = 8
                Top = 48
                Width = 100
                Height = 13
                Caption = 'Slopes : 2,7,15, 45 %'
              end
              object OpenaefBtn: TSpeedButton
                Tag = 1
                Left = 8
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Open File.aef'
                OnClick = OpenaefBtnClick
              end
              object SaveaefBtn: TSpeedButton
                Left = 304
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Save File.aef'
                OnClick = SaveaefBtnClick
              end
              object Label51: TLabel
                Left = 8
                Top = 72
                Width = 91
                Height = 13
                Caption = 'Line Of Sight (LOS)'
              end
              object FileAefEdit: TEdit
                Left = 97
                Top = 16
                Width = 200
                Height = 21
                TabOrder = 0
              end
            end
            object TabSheet19: TTabSheet
              Caption = 'Vegetation'
              ImageIndex = 1
              object OpenavfBtn: TSpeedButton
                Tag = 4
                Left = 8
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Open File.avf'
                OnClick = OpenavfBtnClick
              end
              object SaveavfBtn: TSpeedButton
                Left = 304
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Save File.avf'
                OnClick = SaveavfBtnClick
              end
              object Label58: TLabel
                Left = 24
                Top = 48
                Width = 129
                Height = 13
                Caption = 'Forests, Shrubs, grasslands'
              end
              object FileAvfEdit: TEdit
                Left = 97
                Top = 16
                Width = 200
                Height = 21
                TabOrder = 0
              end
            end
            object TabSheet20: TTabSheet
              Caption = 'Soils'
              ImageIndex = 2
              object OpenasfBtn: TSpeedButton
                Tag = 2
                Left = 8
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Open File.asf'
                OnClick = OpenasfBtnClick
              end
              object SaveasfBtn: TSpeedButton
                Left = 304
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Save File.asf'
                OnClick = SaveasfBtnClick
              end
              object Label60: TLabel
                Left = 16
                Top = 48
                Width = 142
                Height = 13
                Caption = 'Muck, swamps, Sand Dunes, '
              end
              object FileAsfEdit: TEdit
                Left = 97
                Top = 16
                Width = 200
                Height = 21
                TabOrder = 0
              end
            end
            object TabSheet21: TTabSheet
              Caption = 'Hydrology'
              ImageIndex = 3
              object OpenahfBtn: TSpeedButton
                Tag = 3
                Left = 8
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Open File.ahf'
                OnClick = OpenahfBtnClick
              end
              object SaveahfBtn: TSpeedButton
                Left = 304
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Save File.ahf'
                OnClick = SaveahfBtnClick
              end
              object Label59: TLabel
                Left = 24
                Top = 48
                Width = 145
                Height = 13
                Caption = 'Rivers, Lakes, Streams, Ponds'
              end
              object FileAhfEdit: TEdit
                Left = 97
                Top = 16
                Width = 200
                Height = 21
                TabOrder = 0
              end
            end
            object TabSheet6: TTabSheet
              Caption = 'Transport'
              ImageIndex = 4
              object OpenatfBtn: TSpeedButton
                Tag = 5
                Left = 8
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Open File.atf'
                OnClick = OpenatfBtnClick
              end
              object SaveatfBtn: TSpeedButton
                Left = 304
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Save File.atf'
                OnClick = SaveatfBtnClick
              end
              object Label75: TLabel
                Left = 8
                Top = 56
                Width = 358
                Height = 13
                Caption = 
                  'Transportation: Roads, Rails, Ports (Air, Sea), Waterways (Lakes' +
                  ', Rivers, ...)'
              end
              object FileAtfEdit: TEdit
                Left = 96
                Top = 16
                Width = 200
                Height = 21
                TabOrder = 0
              end
            end
            object TabSheet22: TTabSheet
              Caption = 'Urban'
              ImageIndex = 5
              object OpenaafBtn: TSpeedButton
                Tag = 5
                Left = 8
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Open File.aaf'
                OnClick = OpenaafBtnClick
              end
              object SaveaafBtn: TSpeedButton
                Left = 304
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Save File.aaf'
                OnClick = SaveaafBtnClick
              end
              object Label49: TLabel
                Left = 16
                Top = 56
                Width = 130
                Height = 13
                Caption = 'Urban Areas: City, Town, ...'
              end
              object FileAafEdit: TEdit
                Left = 96
                Top = 16
                Width = 200
                Height = 21
                TabOrder = 0
              end
            end
            object TabSheet23: TTabSheet
              Caption = 'Obstacles'
              ImageIndex = 6
              object OpenaofBtn: TSpeedButton
                Tag = 6
                Left = 8
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Open File.aof'
                OnClick = OpenaofBtnClick
              end
              object SaveaofBtn: TSpeedButton
                Left = 304
                Top = 16
                Width = 84
                Height = 22
                Caption = 'Save File.aof'
                OnClick = SaveaofBtnClick
              end
              object Label57: TLabel
                Left = 8
                Top = 56
                Width = 212
                Height = 13
                Caption = 'NoGo areas, Minefields, Contaminated zones'
              end
              object FileAofEdit: TEdit
                Left = 96
                Top = 16
                Width = 200
                Height = 21
                TabOrder = 0
              end
            end
          end
        end
        object TabSheet14: TTabSheet
          Caption = 'NOGO'
          ImageIndex = 7
          object OpenanfBtn: TSpeedButton
            Tag = 9
            Left = 8
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Open File.anf'
            OnClick = OpenanfBtnClick
          end
          object SaveanfBtn: TSpeedButton
            Left = 304
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Save File.anf'
            OnClick = SaveanfBtnClick
          end
          object Label69: TLabel
            Left = 8
            Top = 48
            Width = 377
            Height = 26
            Caption = 
              'All data (including the Obstacle file) is combined into a NOGO f' +
              'ile... similar to the Obstacles file, this is the Superset of it' +
              ', for the Project'
            WordWrap = True
          end
          object Label34: TLabel
            Left = 8
            Top = 88
            Width = 436
            Height = 13
            Caption = 
              'Created from GCost.agf and Map Attributes: Elevation Slope.aef  ' +
              'when loaded by 3D Viewer.'
          end
          object FileAnfEdit: TEdit
            Left = 96
            Top = 16
            Width = 200
            Height = 21
            TabOrder = 0
          end
        end
        object TabSheet25: TTabSheet
          Caption = 'Influence Map'
          ImageIndex = 5
          object Label70: TLabel
            Left = 24
            Top = 56
            Width = 237
            Height = 13
            Caption = 'Influence Map: Danger Zones (+) .. Waypoints (-) :'
          end
          object OpenaifBtn: TSpeedButton
            Tag = 7
            Left = 8
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Open File.aif'
            OnClick = OpenaifBtnClick
          end
          object SaveaifBtn: TSpeedButton
            Left = 304
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Save File.aif'
            OnClick = SaveaifBtnClick
          end
          object Label88: TLabel
            Left = 24
            Top = 80
            Width = 320
            Height = 13
            Caption = '??? Added to G Cost when Influence Map.aif  loaded by 3D Viewer.'
          end
          object Label100: TLabel
            Left = 24
            Top = 104
            Width = 263
            Height = 13
            Caption = 'Use Influence Map (NA)... If it is there.. else all 00000....'
          end
          object FileAifEdit: TEdit
            Left = 96
            Top = 16
            Width = 200
            Height = 21
            TabOrder = 0
          end
        end
        object TabSheet8: TTabSheet
          Caption = 'G Cost'
          object Label82: TLabel
            Left = 40
            Top = 48
            Width = 338
            Height = 13
            Caption = 
              'All Map Attributes (Terrain data) are combined to be used as the' +
              '  G-Cost'
          end
          object OpenagfBtn: TSpeedButton
            Tag = 8
            Left = 8
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Open File.agf'
            OnClick = OpenagfBtnClick
          end
          object SaveagfBtn: TSpeedButton
            Left = 304
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Save File.agf'
            OnClick = SaveagfBtnClick
          end
          object FileAgfEdit: TEdit
            Left = 96
            Top = 16
            Width = 200
            Height = 21
            TabOrder = 0
          end
        end
        object TabSheet5: TTabSheet
          Caption = 'MAG Values'
          ImageIndex = 6
          object Label31: TLabel
            Left = 56
            Top = 48
            Width = 230
            Height = 13
            Caption = 'MAG : (Map Attributes [RGB]:[G#] G Cost) B File '
          end
          object OpenabfBtn: TSpeedButton
            Tag = 8
            Left = 8
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Open File.abf'
            OnClick = OpenabfBtnClick
          end
          object SaveabfBtn: TSpeedButton
            Left = 304
            Top = 16
            Width = 84
            Height = 22
            Caption = 'Save File.abf'
            OnClick = SaveabfBtnClick
          end
          object MAGResettoDefaultsBtn: TSpeedButton
            Left = 408
            Top = 16
            Width = 105
            Height = 22
            Caption = 'Reset to Defaults'
            OnClick = MAGResettoDefaultsBtnClick
          end
          object PrintabfBtn: TSpeedButton
            Left = 408
            Top = 40
            Width = 105
            Height = 22
            Caption = 'Print'
            OnClick = PrintabfBtnClick
          end
          object FileAbfEdit: TEdit
            Left = 96
            Top = 16
            Width = 200
            Height = 21
            TabOrder = 0
          end
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 120
    Top = 22
  end
  object SaveDialog1: TSaveDialog
    Left = 240
    Top = 22
  end
end

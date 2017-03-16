object alife3dForm: Talife3dForm
  Left = 228
  Top = 120
  HelpContext = 1800
  Caption = 'Life 3D Cells'
  ClientHeight = 372
  ClientWidth = 417
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
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000999999999999999900333333000000099999999999999999900333330000
    0099444400000000AAAA99003333000009944440000000000AAAA99003330000
    994444000000000000AAAA9900330009944440000CCCCCC0000AAAA990030099
    44440000CCCCCCCC0000AAAA990009944440000CCCCCCCCCC0000AAAA9909944
    440000CCCCCCCCCCCC0000AAAA99944440000CCCCCCCCCCCCCC0000AAAA94444
    0000CCCCCCCCCCCCCCCC0000AAAA4440000CCCCCCCC99CCCCCCCC0000AAA4400
    00CCCCCCCC9999CCCCCCCC0000AA40000CCCCCCCC999999CCCCCCCC0000A0000
    0CCCCCCC99999999CCCCCCC0000000000CCCCCCC99999999CCCCCCC00000E000
    0CCCCCCCC999999CCCCCCCC0000BEE0000CCCCCCCC9999CCCCCCCC0000BBEEE0
    000CCCCCCCC99CCCCCCCC0000BBBEEEE0000CCCCCCCCCCCCCCCC0000BBBB9EEE
    E0000CCCCCCCCCCCCCC0000BBBB999EEEE0000CCCCCCCCCCCC0000BBBB99099E
    EEE0000CCCCCCCCCC0000BBBB9900099EEEE0000CCCCCCCC0000BBBB99000009
    9EEEE0000CCCCCC0000BBBB99000000099EEEE000000000000BBBB9900005000
    099EEEE0000000000BBBB990000855000099EEEE00000000BBBB990000885550
    00099EEEE000000BBBB9900008885555000099EEEE0000BBBB99000088885555
    5000099EEE0000BBB99000088888555555000099EE0000BB990000888888FF00
    00C0FE000060FC0FF030F81FF818F03FFC0CE0781E06C0F00F0381E0078103C0
    03C0078001E00F0000F01E0000783C00003C7800001EF800001FF800001F7800
    001E3C00003C1E0000780F0000F0078001E003C003C081E00781C0F00F03E078
    1E07F03FFC0F781FF81E3C0FF03C1E07E0780F03C0F00783C1E003C3C3C0}
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 25
    Width = 417
    Height = 325
    HelpContext = 1800
    Camera = MainCam
    PostRender = ViewerPostRender
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roTwoSideLighting]
    Buffer.FaceCulling = False
    FieldOfView = 145.794540405273400000
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
    OnMouseUp = ViewerMouseUp
    TabOrder = 0
  end
  object aLifePanel: TPanel
    Left = 0
    Top = 0
    Width = 417
    Height = 25
    HelpContext = 1800
    Align = alTop
    PopupMenu = PopupMenu1
    TabOrder = 1
    object HelpBtn: TSpeedButton
      Left = 3
      Top = 1
      Width = 24
      Height = 24
      Hint = 'Help or F1 something'
      Glyph.Data = {
        BE060000424DBE06000000000000360400002800000024000000120000000100
        0800000000008802000000000000000000000001000000000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000C0DCC000F0C8
        A400000000000000000000000000000000000000000000000000000000000000
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
        000000000000000000000000000000000000F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00030303030303
        03030606030303030303030303030303030303FFFF0303030303030303030303
        0303030303060404060303030303030303030303030303F8F8FF030303030303
        030303030303030303FE06060403030303030303030303030303F8FF03F8FF03
        0303030303030303030303030303FE060603030303030303030303030303F8FF
        FFF8FF0303030303030303030303030303030303030303030303030303030303
        030303F8F8030303030303030303030303030303030304040603030303030303
        0303030303030303FFFF03030303030303030303030303030306060604030303
        0303030303030303030303F8F8F8FF0303030303030303030303030303FE0606
        0403030303030303030303030303F8FF03F8FF03030303030303030303030303
        03FE06060604030303030303030303030303F8FF03F8FF030303030303030303
        030303030303FE060606040303030303030303030303F8FF0303F8FF03030303
        0303030303030303030303FE060606040303030303030303030303F8FF0303F8
        FF030303030303030303030404030303FE060606040303030303030303FF0303
        F8FF0303F8FF030303030303030306060604030303FE06060403030303030303
        F8F8FF0303F8FF0303F8FF03030303030303FE06060604040406060604030303
        030303F8FF03F8FFFFFFF80303F8FF0303030303030303FE0606060606060606
        06030303030303F8FF0303F8F8F8030303F8FF030303030303030303FEFE0606
        060606060303030303030303F8FFFF030303030303F803030303030303030303
        0303FEFEFEFEFE03030303030303030303F8F8FFFFFFFFFFF803030303030303
        0303030303030303030303030303030303030303030303F8F8F8F8F803030303
        0303}
      NumGlyphs = 2
      OnClick = HelpBtnClick
    end
    object ExitBtn: TSpeedButton
      Left = 28
      Top = 1
      Width = 24
      Height = 24
      Hint = 'Exit'
      Glyph.Data = {
        06020000424D0602000000000000760000002800000028000000140000000100
        0400000000009001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
        777777773FFFFFFFFFFFF333333F888888888888F7F7F7888888888888883333
        33888888888888877F7F788888888888888F333FF88844444400888FFF444444
        888888888883338888833333342A207FFF433333333338F888F3338F33333333
        34A2A0AFFF4333333333388788F3338F33333333342A20FAFF433333333338F8
        78F3338F3333333334A2A0AEFE4333333333388788F3338F33333333342A20FA
        FF433333333338F878F3338F3333333334A2A0AEFE4333333333388788F3338F
        33333333342A20FAFF433333333338F878F3338F3333333334A2A0FEFE433333
        3333388788F3338F33333333342A20EFEF433333333338F878F3338F33333333
        34A2A0AEFE4333333333388788F3338F33333333342A20EAEF433333333338F8
        F8FFFF8F33333333344444444443333333333888888888833333333333333333
        3333333333333333FFFFFF333333333333300000033333333333333888888F33
        33333333333099990333333333333338FFFF8F33333333333330000003333333
        33333338888883333333}
      NumGlyphs = 2
      OnClick = ExitBtnClick
    end
    object LifeFileBtn: TSpeedButton
      Left = 53
      Top = 1
      Width = 24
      Height = 24
      Hint = 'Open Life file'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555555555555555555555555555555555555555555555555555555555
        555555555555555555555555555555555555555FFFFFFFFFF555550000000000
        55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
        B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
        000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
        555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
        55555575FFF75555555555700007555555555557777555555555555555555555
        5555555555555555555555555555555555555555555555555555}
      NumGlyphs = 2
      OnClick = LifeFileBtnClick
    end
    object RunLifeBtn: TSpeedButton
      Left = 78
      Top = 1
      Width = 24
      Height = 24
      Hint = 'RUN'
      Caption = '*'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000009999900
        033300009440AA900033000944000AA900030094400000AA9000094400CCC00A
        A90094400CCCCC00AA904400CCCCCCC00AA9000CCCC9CCCC0000000CCC999CCC
        0000E00CCCC9CCCC00B0EE00CCCCCCC00BBDDEE00CCCCC00BBD00DEE00CCC00B
        BD0050DEE00000BBD008550DEE000BBD00885550DE000BD00888}
      OnClick = RunLifeBtnClick
    end
    object StopLifeBtn: TSpeedButton
      Left = 151
      Top = 1
      Width = 24
      Height = 24
      Hint = 'Freeze Frame'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      OnClick = StopLifeBtnClick
    end
    object RunBtn: TSpeedButton
      Left = 104
      Top = 1
      Width = 23
      Height = 24
      Hint = 'Run'
      Caption = '>'
      Enabled = False
      OnClick = RunBtnClick
    end
    object PauseStepBtn: TSpeedButton
      Left = 128
      Top = 1
      Width = 23
      Height = 24
      Hint = 'Pause Step'
      Caption = '||'
      Enabled = False
      OnClick = PauseStepBtnClick
    end
    object NumberEdit: TEdit
      Left = 220
      Top = 2
      Width = 50
      Height = 21
      Hint = 'Slow down speed to see Life reincarnations'
      HelpContext = 1800
      AutoSize = False
      TabOrder = 0
      Text = '427'
    end
    object SaveStartCB: TCheckBox
      Left = 177
      Top = 1
      Width = 17
      Height = 17
      Hint = 'Save the Beginning of Life values?'
      HelpContext = 1800
      TabOrder = 1
    end
    object SaveEndCB: TCheckBox
      Left = 193
      Top = 1
      Width = 17
      Height = 17
      Hint = 'Save the End of Life values into a file ?'
      HelpContext = 1800
      TabOrder = 2
    end
    object MaxRandomCellsEdit: TEdit
      Left = 290
      Top = 2
      Width = 50
      Height = 21
      Hint = 'Max Random Cells'
      HelpContext = 1800
      AutoSize = False
      TabOrder = 3
      Text = '1000'
    end
  end
  object StatusBarred: TStatusBar
    Left = 0
    Top = 350
    Width = 417
    Height = 22
    Hint = 'Displays Rule and Game Scores'
    HelpContext = 1800
    Panels = <
      item
        Text = '23/3'
        Width = 100
      end
      item
        Width = 50
      end>
    ExplicitTop = 360
  end
  object aMinMaxPanel: TPanel
    Left = 0
    Top = 25
    Width = 417
    Height = 36
    HelpContext = 1800
    TabOrder = 3
    Visible = False
    object Label1: TLabel
      Left = 112
      Top = 2
      Width = 6
      Height = 13
      Caption = '9'
    end
    object Label2: TLabel
      Left = 112
      Top = 18
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label3: TLabel
      Left = 232
      Top = 15
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label4: TLabel
      Left = 232
      Top = 2
      Width = 6
      Height = 13
      Caption = '9'
    end
    object Label5: TLabel
      Left = 352
      Top = 15
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label6: TLabel
      Left = 352
      Top = 2
      Width = 6
      Height = 13
      Caption = '9'
    end
    object SizeLabel: TLabel
      Left = 400
      Top = 10
      Width = 12
      Height = 13
      Caption = '10'
    end
    object RedMaxTrackBar: TTrackBar
      Left = 10
      Top = 2
      Width = 100
      Height = 17
      Hint = 'Red X Max'
      HelpContext = 1800
      Max = 9
      PageSize = 20
      Position = 9
      TabOrder = 0
      ThumbLength = 10
      TickStyle = tsNone
      OnChange = RedMaxTrackBarChange
    end
    object GreenMaxTrackBar: TTrackBar
      Left = 130
      Top = 2
      Width = 100
      Height = 17
      Hint = 'Green Y Max'
      HelpContext = 1800
      Max = 9
      PageSize = 20
      Position = 9
      TabOrder = 1
      ThumbLength = 10
      TickStyle = tsNone
      OnChange = GreenMaxTrackBarChange
    end
    object BlueMaxTrackBar: TTrackBar
      Left = 250
      Top = 2
      Width = 100
      Height = 17
      Hint = 'Blue Z Max'
      HelpContext = 1800
      Max = 9
      PageSize = 20
      Position = 9
      TabOrder = 2
      ThumbLength = 10
      TickStyle = tsNone
      OnChange = BlueMaxTrackBarChange
    end
    object RedMinTrackBar: TTrackBar
      Left = 10
      Top = 16
      Width = 100
      Height = 17
      Hint = 'Red Min'
      HelpContext = 1800
      PageSize = 20
      TabOrder = 3
      ThumbLength = 10
      TickStyle = tsNone
      OnChange = RedMinTrackBarChange
    end
    object GreenMinTrackBar: TTrackBar
      Left = 130
      Top = 16
      Width = 100
      Height = 17
      Hint = 'Green Min'
      HelpContext = 1800
      PageSize = 20
      TabOrder = 4
      ThumbLength = 10
      TickStyle = tsNone
      OnChange = GreenMinTrackBarChange
    end
    object BlueMinTrackBar: TTrackBar
      Left = 250
      Top = 16
      Width = 100
      Height = 17
      Hint = 'Blue Min'
      HelpContext = 1800
      PageSize = 20
      TabOrder = 5
      ThumbLength = 10
      TickStyle = tsNone
      OnChange = BlueMinTrackBarChange
    end
    object CellSizeTB: TTrackBar
      Left = 384
      Top = 1
      Width = 17
      Height = 34
      Hint = 'Cell Size'
      HelpContext = 1800
      Min = 1
      Orientation = trVertical
      Position = 1
      TabOrder = 6
      ThumbLength = 12
      TickStyle = tsNone
      OnChange = CellSizeTBChange
    end
  end
  object Scene: TGLScene
    Left = 16
    Top = 144
    object DummyCube: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object Cube1: TGLCube
        Material.FrontProperties.Ambient.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.Texture.TextureMode = tmModulate
        Visible = False
      end
      object MainCam: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = Cube1
        Position.Coordinates = {0000204100002041000020410000803F}
        Direction.Coordinates = {000000000000803F0000008000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        Left = 376
        Top = 240
      end
    end
    object Light1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1.000000000000000000
      SpotCutOff = 180.000000000000000000
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 48
    Top = 88
  end
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 88
    object Size1: TMenuItem
      Caption = 'Number of Cells ^3'
      object lifesize10: TMenuItem
        Caption = '10'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize10Click
      end
      object lifesize25: TMenuItem
        Caption = '25'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize25Click
      end
      object lifesize50: TMenuItem
        Caption = '50'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize50Click
      end
      object lifesize100: TMenuItem
        Caption = '100'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize100Click
      end
      object lifesize150: TMenuItem
        Caption = '150'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize150Click
      end
      object lifesize200: TMenuItem
        Caption = '200'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize200Click
      end
      object lifesize256: TMenuItem
        Caption = '256'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize256Click
      end
      object lifesize300: TMenuItem
        Caption = '300'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize300Click
      end
      object lifesize400: TMenuItem
        Caption = '400'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize400Click
      end
      object lifesize512: TMenuItem
        Caption = '512'
        GroupIndex = 1
        RadioItem = True
        OnClick = lifesize512Click
      end
    end
    object World1: TMenuItem
      Caption = 'Edge of the World'
      object Fallover1: TMenuItem
        Caption = 'Fall off'
        Checked = True
        GroupIndex = 2
        RadioItem = True
        OnClick = Fallover1Click
      end
      object Torus1: TMenuItem
        Caption = 'Warp Around'
        GroupIndex = 2
        RadioItem = True
        OnClick = Torus1Click
      end
      object Infinity1: TMenuItem
        Caption = 'Infinity'
        GroupIndex = 2
        OnClick = Infinity1Click
      end
    end
    object CellColorScheme1: TMenuItem
      Caption = 'Cell Color Scheme'
      object ColorTwoTone: TMenuItem
        Caption = 'Two Tone'
        Checked = True
        GroupIndex = 3
        RadioItem = True
        OnClick = ColorTwoToneClick
      end
      object ColorRGB1: TMenuItem
        Caption = 'Color RGB'
        GroupIndex = 3
        RadioItem = True
        OnClick = ColorRGB1Click
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 3
      end
      object ColorAll: TMenuItem
        Caption = 'Color All'
        GroupIndex = 3
        RadioItem = True
        OnClick = ColorAllClick
      end
    end
    object CellularRules1: TMenuItem
      Caption = 'Cellular Rules'
      object Conway1: TMenuItem
        Caption = 'Conway 23/3'
        Checked = True
        OnClick = Conway1Click
      end
      object Universes: TMenuItem
        Caption = 'Universes...'
        OnClick = UniversesClick
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object UseSlowSteps1: TMenuItem
      Caption = 'Use Slow down'
      OnClick = UseSlowSteps1Click
    end
    object DisplayMinMaxBar1: TMenuItem
      Caption = 'Display Min..Max Bar'
      OnClick = DisplayMinMaxBar1Click
    end
    object Viewliffiles1: TMenuItem
      Caption = 'View .lif files...'
      OnClick = Viewliffiles1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Run1: TMenuItem
      Caption = 'Start - Running'
      OnClick = RunLifeBtnClick
    end
    object StopLife1: TMenuItem
      Caption = 'Stop Life'
      OnClick = StopLifeBtnClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Exit1: TMenuItem
      Caption = 'Exit'
      OnClick = ExitBtnClick
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 88
    Top = 88
  end
end

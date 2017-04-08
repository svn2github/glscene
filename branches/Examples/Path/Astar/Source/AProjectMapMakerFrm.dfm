object AProjectMapMakerForm: TAProjectMapMakerForm
  Left = 18
  Top = 22
  BorderStyle = bsSingle
  Caption = '3D Terrain Painter Demo'
  ClientHeight = 489
  ClientWidth = 770
  Color = 15724527
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
    0000000000000000000000000000000000000000000000000000000000000500
    00000501030CE90D070501000000007FA088800EAEAEAFAEAFAEAE00000000EA
    EA08888005EAE5EAECFAE3E5000000AECFA088800003AEA1AEAEAEA00000002A
    EAE0000E00000CEAE3EAEBEAE6000006AED00000E0000009AEAEAEA40000000A
    E90000000E0000000AEAEAEAE0000007A000000000900000000EAEA1A000000B
    7000000000090000000005EAE50000077000000000009000000000010000000B
    F900000000000900000000000000000BAE000000000000900000000000000000
    EA000000000000090000000000000007C0000000000000009000000000000000
    0700000000000000090000000000000097000000000000000090000000000000
    0000000000000000000900000000000070000000000000000000900000000000
    0000888888888880000009000000000007007888888888880000009000000000
    0700EAEAEAEAEA7000000009000000000700A6AEAEA0AE800000000090000000
    0700EAEAEAEAEA8000000000090000000000AEAEAEAEAE800000000000900000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFA02A2ABFC000003FC008000FC00E001FC1078003E1E3E00FE3F1
    F807E7F8FE07E7FC7F83E7FE3FEFE3FF1FFFE3FF8FFFF3FFC7FFE7FFE3FFFBFF
    F1FFF3FFF8FFFFFFFC7FF700163FF600071FE000078FE00007C7E00007E3E000
    07F1F60007F9FF0017FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 2
    Top = 0
    Width = 297
    Height = 489
    Align = alLeft
    BevelOuter = bvNone
    Color = 15724527
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 49
      Height = 16
      Caption = 'Texture:'
      Color = 12615680
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label3: TLabel
      Left = 144
      Top = 0
      Width = 37
      Height = 16
      Caption = 'Brush:'
      Color = 12615680
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lbGridSize: TLabel
      Left = 0
      Top = 205
      Width = 101
      Height = 16
      Caption = 'Terrain Size = 10'
      Color = 12615680
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lbBrushSize: TLabel
      Left = 144
      Top = 175
      Width = 98
      Height = 16
      Caption = 'Brush Size = 128'
      Color = 12615680
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lbAlpha: TLabel
      Left = 0
      Top = 174
      Width = 70
      Height = 16
      Caption = 'Alpha = 255'
      Color = 12615680
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label44: TLabel
      Left = 144
      Top = 159
      Width = 129
      Height = 15
      Caption = '-------------------------------------------'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Impact'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 0
      Top = 160
      Width = 129
      Height = 15
      Caption = '-------------------------------------------'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Impact'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 144
      Top = 207
      Width = 129
      Height = 15
      Caption = '-------------------------------------------'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Impact'
      Font.Style = []
      ParentFont = False
    end
    object LoadTextureBtn: TSpeedButton
      Left = 8
      Top = 240
      Width = 129
      Height = 22
      Caption = 'Load Texture'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 16744448
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = LoadTextureBtnClick
    end
    object SaveTextureBtn: TSpeedButton
      Left = 8
      Top = 264
      Width = 129
      Height = 22
      Caption = 'Save Texture'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 16744448
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = SaveTextureBtnClick
    end
    object LoadHeightDataBtn: TSpeedButton
      Left = 8
      Top = 304
      Width = 129
      Height = 22
      Caption = 'Load Height Data'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 16744448
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = LoadHeightDataBtnClick
    end
    object Image1: TImage
      Left = 9
      Top = 354
      Width = 15
      Height = 15
      AutoSize = True
      Picture.Data = {
        07544269746D617026050000424D260500000000000036040000280000000F00
        00000F0000000100080000000000F0000000880B0000880B0000000100000000
        0000000000000101010002020200030303000404040005050500060606000707
        070008080800090909000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F
        0F00101010001111110012121200131313001414140015151500161616001717
        170018181800191919001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F
        1F00202020002121210022222200232323002424240025252500262626002727
        270028282800292929002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F
        2F00303030003131310032323200333333003434340035353500363636003737
        370038383800393939003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F
        3F00404040004141410042424200434343004444440045454500464646004747
        470048484800494949004A4A4A004B4B4B004C4C4C004D4D4D004E4E4E004F4F
        4F00505050005151510052525200535353005454540055555500565656005757
        570058585800595959005A5A5A005B5B5B005C5C5C005D5D5D005E5E5E005F5F
        5F00606060006161610062626200636363006464640065656500666666006767
        670068686800696969006A6A6A006B6B6B006C6C6C006D6D6D006E6E6E006F6F
        6F00707070007171710072727200737373007474740075757500767676007777
        770078787800797979007A7A7A007B7B7B007C7C7C007D7D7D007E7E7E007F7F
        7F00808080008181810082828200838383008484840085858500868686008787
        870088888800898989008A8A8A008B8B8B008C8C8C008D8D8D008E8E8E008F8F
        8F00909090009191910092929200939393009494940095959500969696009797
        970098989800999999009A9A9A009B9B9B009C9C9C009D9D9D009E9E9E009F9F
        9F00A0A0A000A1A1A100A2A2A200A3A3A300A4A4A400A5A5A500A6A6A600A7A7
        A700A8A8A800A9A9A900AAAAAA00ABABAB00ACACAC00ADADAD00AEAEAE00AFAF
        AF00B0B0B000B1B1B100B2B2B200B3B3B300B4B4B400B5B5B500B6B6B600B7B7
        B700B8B8B800B9B9B900BABABA00BBBBBB00BCBCBC00BDBDBD00BEBEBE00BFBF
        BF00C0C0C000C1C1C100C2C2C200C3C3C300C4C4C400C5C5C500C6C6C600C7C7
        C700C8C8C800C9C9C900CACACA00CBCBCB00CCCCCC00CDCDCD00CECECE00CFCF
        CF00D0D0D000D1D1D100D2D2D200D3D3D300D4D4D400D5D5D500D6D6D600D7D7
        D700D8D8D800D9D9D900DADADA00DBDBDB00DCDCDC00DDDDDD00DEDEDE00DFDF
        DF00E0E0E000E1E1E100E2E2E200E3E3E300E4E4E400E5E5E500E6E6E600E7E7
        E700E8E8E800E9E9E900EAEAEA00EBEBEB00ECECEC00EDEDED00EEEEEE00EFEF
        EF00F0F0F000F1F1F100F2F2F200F3F3F300F4F4F400F5F5F500F6F6F600F7F7
        F700F8F8F800F9F9F900FAFAFA00FBFBFB00FCFCFC00FDFDFD00FEFEFE00FFFF
        FF00FF3D3D3D3D3D3D3D3D3D3D3D3D3D3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D
        3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D
        3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D
        3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D
        3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D
        3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D
        3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D3D00FF3D3D3D3D3D3D3D3D3D3D3D3D3D
        3D00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00}
    end
    object SaTBLabel: TLabel
      Left = 144
      Top = 328
      Width = 21
      Height = 13
      Caption = '0.01'
    end
    object STBLabel: TLabel
      Left = 144
      Top = 352
      Width = 15
      Height = 13
      Caption = '1.0'
    end
    object HTBLabel: TLabel
      Left = 144
      Top = 372
      Width = 27
      Height = 13
      Caption = '0.005'
    end
    object ResetDefaultsBtn: TSpeedButton
      Left = 151
      Top = 310
      Width = 50
      Height = 12
      Hint = 'Reset to Defaults'
      OnClick = ResetDefaultsBtnClick
    end
    object pbTexture: TPaintBox32
      Left = 0
      Top = 16
      Width = 128
      Height = 128
      TabOrder = 0
    end
    object gbTexture: TGaugeBar
      Left = 0
      Top = 144
      Width = 137
      Height = 16
      Backgnd = bgPattern
      ShowHandleGrip = True
      Position = 0
      OnChange = gbTextureChange
    end
    object pbBrush: TPaintBox32
      Left = 144
      Top = 16
      Width = 128
      Height = 128
      TabOrder = 2
    end
    object gbBrush: TGaugeBar
      Left = 144
      Top = 144
      Width = 137
      Height = 16
      Backgnd = bgPattern
      ShowHandleGrip = True
      Position = 0
      OnChange = gbBrushChange
    end
    object gbGrid: TGaugeBar
      Left = 0
      Top = 221
      Width = 137
      Height = 16
      Backgnd = bgPattern
      LargeChange = 4
      Min = 1
      ShowHandleGrip = True
      SmallChange = 2
      Position = 10
      OnChange = gbGridChange
    end
    object gbBrushSize: TGaugeBar
      Left = 144
      Top = 191
      Width = 137
      Height = 16
      Backgnd = bgPattern
      Max = 1024
      Min = 1
      ShowHandleGrip = True
      Position = 128
      OnChange = gbBrushSizeChange
    end
    object gbAlpha: TGaugeBar
      Left = 0
      Top = 190
      Width = 137
      Height = 16
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Position = 255
      OnChange = gbAlphaChange
    end
    object Memo1: TMemo
      Left = 8
      Top = 398
      Width = 129
      Height = 43
      BorderStyle = bsNone
      Lines.Strings = (
        'Mouse Left = Paint'
        'Mouse Right = Rotate'
        'Mouse Wheel = Zoom')
      TabOrder = 7
    end
    object rgTexSize: TRadioGroup
      Left = 144
      Top = 221
      Width = 137
      Height = 68
      Caption = 'Texture Resolution (Reset)'
      ItemIndex = 1
      Items.Strings = (
        '2048'
        '1024'
        '512'
        '256')
      TabOrder = 8
      OnClick = rgTexSizeClick
    end
    object EnableHeightCB: TCheckBox
      Left = 8
      Top = 338
      Width = 73
      Height = 17
      Hint = 'Image or Heightfield'
      HelpContext = 4567
      Caption = 'Use Image'
      TabOrder = 9
      OnClick = EnableHeightCBClick
    end
    object ScaleSampleTrackBar: TTrackBar
      Left = 187
      Top = 324
      Width = 100
      Height = 16
      Hint = 'Scale Sample'
      HelpContext = 4567
      Max = 1000
      Min = 1
      PageSize = 20
      Frequency = 100
      Position = 100
      TabOrder = 10
      TabStop = False
      ThumbLength = 10
      OnChange = ScaleSampleTrackBarChange
    end
    object ScaleTrackBar: TTrackBar
      Left = 187
      Top = 349
      Width = 100
      Height = 16
      Hint = 'Scale'
      HelpContext = 4567
      Max = 200
      Min = 1
      Frequency = 10
      Position = 100
      TabOrder = 11
      TabStop = False
      ThumbLength = 10
      OnChange = ScaleTrackBarChange
    end
    object HeightTrackBar1: TTrackBar
      Left = 173
      Top = 383
      Width = 100
      Height = 16
      Hint = 'Height  ratio'
      HelpContext = 4567
      Max = 1000
      Min = 1
      PageSize = 10
      Frequency = 100
      Position = 50
      TabOrder = 12
      TabStop = False
      ThumbLength = 10
      OnChange = HeightTrackBar1Change
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 2
    Height = 489
    Align = alLeft
    Caption = 'Panel2'
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 299
    Top = 0
    Width = 471
    Height = 489
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    object Label2: TLabel
      Left = 0
      Top = 0
      Width = 471
      Height = 16
      Align = alTop
      Caption = 'Terrain:'
      Color = 12615680
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitWidth = 47
    end
    object TerrainViewer: TGLSceneViewer
      Left = 0
      Top = 16
      Width = 471
      Height = 473
      Cursor = crHandPoint
      Camera = GLCamera
      FieldOfView = 156.026565551757800000
      Align = alClient
      OnMouseDown = TerrainViewerMouseDown
      OnMouseMove = TerrainViewerMouseMove
      TabOrder = 0
    end
  end
  object GLScene1: TGLScene
    Left = 74
    Top = 24
    object GLCamTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object HeightField: TGLHeightField
      Material.Texture.TextureMode = tmModulate
      Material.Texture.TextureFormat = tfRGB
      Material.Texture.MappingMode = tmmEyeLinear
      Material.Texture.Disabled = False
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {0000A0C0000000000000A0400000803F}
      Up.Coordinates = {0000000000000000000080BF00000000}
      XSamplingScale.Max = 10.000000000000000000
      XSamplingScale.Step = 0.100000001490116100
      YSamplingScale.Max = 10.000000000000000000
      YSamplingScale.Step = 0.100000001490116100
      OnGetHeight = HeightFieldGetHeight
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000803F00002041000000400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCamTarget
      Position.Coordinates = {000040400000E040000040400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 10
    Top = 56
  end
  object dSave: TSavePictureDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 42
    Top = 56
  end
  object dLoad: TOpenPictureDialog
    Filter = 
      'All (*.tga;*.jpg;*.jpeg;*.bmp)|*.tga;*.jpg;*.jpeg;*.bmp|Targa (*' +
      '.tga)|*.tga|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpe' +
      'g)|*.jpeg|Bitmaps (*.bmp)|*.bmp'
    Left = 72
    Top = 56
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 40
    Top = 24
  end
end

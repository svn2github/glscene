object Form1: TForm1
  Left = 210
  Top = 147
  BorderStyle = bsSingle
  Caption = '3D Terrain Painter Demo'
  ClientHeight = 652
  ClientWidth = 770
  Color = 15724527
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 128
    Height = 652
    Align = alLeft
    BevelOuter = bvNone
    Color = 15724527
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 128
      Height = 16
      Align = alTop
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
      Left = 0
      Top = 160
      Width = 128
      Height = 16
      Align = alTop
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
      Top = 429
      Width = 128
      Height = 16
      Align = alTop
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
      Left = 0
      Top = 335
      Width = 128
      Height = 16
      Align = alTop
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
      Top = 382
      Width = 128
      Height = 16
      Align = alTop
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
    object Label4: TLabel
      Left = 0
      Top = 367
      Width = 128
      Height = 15
      Align = alTop
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
      Top = 320
      Width = 128
      Height = 15
      Align = alTop
      Caption = '-------------------------------------------'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Impact'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 0
      Top = 414
      Width = 128
      Height = 15
      Align = alTop
      Caption = '-------------------------------------------'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Impact'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton1: TSpeedButton
      Left = 0
      Top = 536
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
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 0
      Top = 560
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
      OnClick = SpeedButton2Click
    end
    object pbTexture: TPaintBox32
      Left = 0
      Top = 16
      Width = 128
      Height = 128
      Align = alTop
      TabOrder = 0
    end
    object gbTexture: TGaugeBar
      Left = 0
      Top = 144
      Width = 128
      Height = 16
      Align = alTop
      Backgnd = bgPattern
      ShowHandleGrip = True
      Position = 0
      OnChange = gbTextureChange
    end
    object pbBrush: TPaintBox32
      Left = 0
      Top = 176
      Width = 128
      Height = 128
      Align = alTop
      TabOrder = 2
    end
    object gbBrush: TGaugeBar
      Left = 0
      Top = 304
      Width = 128
      Height = 16
      Align = alTop
      Backgnd = bgPattern
      ShowHandleGrip = True
      Position = 0
      OnChange = gbBrushChange
    end
    object gbGrid: TGaugeBar
      Left = 0
      Top = 445
      Width = 128
      Height = 16
      Align = alTop
      Backgnd = bgPattern
      LargeChange = 4
      Min = 1
      ShowHandleGrip = True
      SmallChange = 2
      Position = 10
      OnChange = gbGridChange
    end
    object gbBrushSize: TGaugeBar
      Left = 0
      Top = 351
      Width = 128
      Height = 16
      Align = alTop
      Backgnd = bgPattern
      Max = 1024
      Min = 1
      ShowHandleGrip = True
      Position = 128
      OnChange = gbBrushSizeChange
    end
    object gbAlpha: TGaugeBar
      Left = 0
      Top = 398
      Width = 128
      Height = 16
      Align = alTop
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Position = 255
      OnChange = gbAlphaChange
    end
    object Memo1: TMemo
      Left = 0
      Top = 595
      Width = 128
      Height = 57
      Align = alBottom
      BorderStyle = bsNone
      Lines.Strings = (
        'Mouse Left = Paint'
        'Mouse Right = Rotate'
        'Mouse Wheel = Zoom')
      TabOrder = 7
    end
    object rgTexSize: TRadioGroup
      Left = 0
      Top = 461
      Width = 128
      Height = 76
      Align = alTop
      Caption = 'Texture Resolution'
      ItemIndex = 0
      Items.Strings = (
        '1024'
        '512'
        '256')
      TabOrder = 8
      OnClick = rgTexSizeClick
    end
  end
  object Panel2: TPanel
    Left = 128
    Top = 0
    Width = 2
    Height = 652
    Align = alLeft
    Caption = 'Panel2'
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 130
    Top = 0
    Width = 640
    Height = 652
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    object Label2: TLabel
      Left = 0
      Top = 0
      Width = 640
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
    end
    object TerrainViewer: TGLSceneViewer
      Left = 0
      Top = 16
      Width = 640
      Height = 636
      Cursor = crHandPoint
      Camera = GLCamera
      FieldOfView = 162.128768920898400000
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
      XSamplingScale.max = 10.000000000000000000
      XSamplingScale.step = 0.100000001490116100
      YSamplingScale.max = 10.000000000000000000
      YSamplingScale.step = 0.100000001490116100
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

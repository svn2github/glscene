object Form1: TForm1
  Left = 195
  Top = 103
  Width = 465
  Height = 335
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 344
    Height = 306
    Camera = GLCamera1
    Buffer.AntiAliasing = aaNone
    Align = alClient
  end
  object Panel1: TPanel
    Left = 344
    Top = 0
    Width = 113
    Height = 306
    Align = alRight
    TabOrder = 1
    object BUSnapShot: TButton
      Left = 8
      Top = 8
      Width = 97
      Height = 25
      Caption = 'SnapShot'
      TabOrder = 0
      OnClick = BUSnapShotClick
    end
    object BURenderToBitmap: TButton
      Left = 8
      Top = 48
      Width = 97
      Height = 25
      Caption = 'Render To Bitmap'
      TabOrder = 1
      OnClick = BURenderToBitmapClick
    end
    object BUBitmapx2: TButton
      Left = 8
      Top = 96
      Width = 97
      Height = 25
      Caption = 'Bitmap x2'
      TabOrder = 2
      OnClick = BUBitmapx2Click
    end
    object BUBitmap600: TButton
      Left = 8
      Top = 176
      Width = 97
      Height = 25
      Caption = 'Bitmap 600 dpi'
      TabOrder = 3
      OnClick = BUBitmap600Click
    end
    object BUBitmap300: TButton
      Left = 8
      Top = 136
      Width = 97
      Height = 25
      Caption = 'Bitmap 300 dpi'
      TabOrder = 4
      OnClick = BUBitmap300Click
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000B4420000B442000048420000803F}
      Specular.Color = {B072083FB072083FB072083F0000803F}
      SpotCutOff = 180
    end
    object HUDSprite1: TGLHUDSprite
      Material.Texture.Disabled = False
      Width = 16
      Height = 16
      NoZWrite = True
    end
    object Plane1: TGLPlane
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      Material.Texture.TextureMode = tmReplace
      Material.Texture.Disabled = False
      Height = 6
      Width = 6
      object SpaceText1: TGLSpaceText
        Position.Coordinates = {000020C000000000CDCC4C3D0000803F}
        Up.Coordinates = {0000803F000000800000000000000000}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Specular.Color = {FA7EAA3EFA7EAA3E000000000000803F}
        Extrusion = 0.0500000007450581
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        Text = 'Render to Bitmap'
        AllowedDeviation = 1
        CharacterRange = stcrAlphaNum
        AspectRatio = 1.29999995231628
        TextHeight = 0.400000005960464
        Adjust.Horz = haCenter
        Adjust.Vert = vaCenter
      end
    end
    object Sphere1: TGLSphere
      OnProgress = Sphere1Progress
      Material.FrontProperties.Diffuse.Color = {9A99393F9A99393F0000803F0000803F}
      Material.Texture.TextureMode = tmModulate
      Material.Texture.MappingMode = tmmSphere
      Material.Texture.Disabled = False
      Radius = 1
      Slices = 24
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000000040000000000000803F}
      CubeSize = 1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000E0400000C040000000410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 40
    Top = 8
  end
end

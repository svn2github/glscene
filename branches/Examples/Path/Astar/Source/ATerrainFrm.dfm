object ATerrainForm: TATerrainForm
  Left = 38
  Top = 25
  Align = alClient
  BorderStyle = bsNone
  Caption = 'GLScene'
  ClientHeight = 482
  ClientWidth = 694
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
    000000000000000000000000000000000000000000000000B000000000000000
    000000000000000BFB0000000000000000000000000000BFBFB0000000000000
    0000000000000BFBFBF00000000000000000000000000FBFBF00000000000000
    000000000000FBFBF009000000000000000000000000BFB00090BFB000000000
    00000000000BFB00990BFBFB0000000000000000000FB00990BFBFB0F0000000
    00000000000B00990BFBFB0FB00000000000000000000990BFBFB0FB00000000
    000000000000990BFBFB0FB00000000000000000000990BFBF00FB0000000000
    00000000009900FB00BF0000000000000000000009990000FB00000000000000
    0000000099900000000000000000000000000009990000000000000000000000
    0000009990000000000000000000000000000F99000000000000000000000000
    0000FFF00000000000000000000000000000FF00000000000000000000000000
    000F000000000000000000000000000000F00000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFF7FFFFFFE3FFFFFFC1FFFFFF80FFFFFF003FFFFF003FFFFE
    001FFFFE000FFFFC0007FFFC0003FFFC0003FFFC0007FFFC000FFFF8001FFFF0
    003FFFE000FFFFC063FFFF80FFFFFF01FFFFFE03FFFFFE07FFFFFC0FFFFFF81F
    FFFFF87FFFFFF0FFFFFFE3FFFFFFE7FFFFFFDFFFFFFFBFFFFFFFFFFFFFFF}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 694
    Height = 463
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 200.000000000000000000
    Buffer.FogEnvironment.FogEnd = 650.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clGray
    Buffer.FogEnable = True
    Buffer.Lighting = False
    FieldOfView = 155.624618530273400000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 463
    Width = 694
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Panels = <
      item
        Alignment = taCenter
        Text = 'Edit Mode'
        Width = 80
      end
      item
        Alignment = taCenter
        Text = '0.192 MS / 3 = 0.066 MS'
        Width = 150
      end
      item
        Alignment = taCenter
        Text = 'Unit # 1'
        Width = 80
      end
      item
        Alignment = taCenter
        Text = 'Scenario #1'
        Width = 80
      end
      item
        Alignment = taCenter
        Text = 'X: 1200 Y: 1200'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = 'X: 120 Y: 120'
        Width = 100
      end
      item
        Alignment = taCenter
        Text = 'Hints'
        Width = 50
      end>
    UseSystemFont = False
  end
  object GLBitmapHDS1: TGLBitmapHDS
    InfiniteWrap = False
    MaxPoolSize = 0
    Left = 56
    Top = 16
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 56
    Top = 56
    object SkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Bands = <
        item
          StartAngle = -5.000000000000000000
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 25.000000000000000000
          Slices = 9
        end
        item
          StartAngle = 25.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Slices = 9
          Stacks = 4
        end>
      Stars = <>
      Options = [sdoTwinkle]
    end
    object PathDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object CameraCube: TGLDummyCube
      Position.Coordinates = {0000000000000041000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 650.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = CameraCube
        Position.Coordinates = {0000A040000020410000C8410000803F}
        Left = 264
        Top = 160
      end
    end
    object TerrainRenderer1: TGLTerrainRenderer
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ground'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00008040000080400000803E00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLBitmapHDS1
      TileSize = 32
      TilesPerTexture = 1.000000000000000000
      QualityDistance = 150.000000000000000000
      ContourWidth = 0
    end
    object HUDText1: TGLHUDText
      Position.Coordinates = {000096420000C841000000000000803F}
      BitmapFont = BitmapFont1
      Rotation = 0.000000000000000000
    end
    object Marker: TGLDummyCube
      Visible = False
      CubeSize = 1.000000000000000000
      object Torus1: TGLTorus
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Shininess = 32
        Direction.Coordinates = {00000000000080BF0000000000000000}
        Position.Coordinates = {000000000000003F000000000000803F}
        Up.Coordinates = {00000000000000000000803F00000000}
        MajorRadius = 1.000000000000000000
        MinorRadius = 0.100000001490116100
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object ArrowLine1: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Shininess = 32
        Direction.Coordinates = {00000000000080BF0000000000000000}
        Position.Coordinates = {000000000000803F000000000000803F}
        Up.Coordinates = {00000000000000000000803F00000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
    end
    object ActorBase0: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Actor0: TGLActor
        Material.FrontProperties.Shininess = 32
        Material.Texture.Disabled = False
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000204100000000000020410000803F}
        Up.Coordinates = {0000803F000000800000000000000000}
        Interval = 100
      end
      object ActorsxDC: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object ActorBasex1: TGLDummyCube
          CubeSize = 1.000000000000000000
          object GLActor1: TGLActor
            Material.Texture.Disabled = False
            Direction.Coordinates = {000000800000803F0000000000000000}
            Up.Coordinates = {0000803F000000000000000000000000}
            Interval = 100
          end
          object Targetx1: TGLCube
            Direction.Coordinates = {0000000000000080FFFF7F3F00000000}
            Up.Coordinates = {0000803F000000000000000000000000}
          end
        end
        object ActorBasex2: TGLDummyCube
          CubeSize = 1.000000000000000000
          object GLActor2: TGLActor
            Material.Texture.Disabled = False
            Direction.Coordinates = {00000080FFFF7F3F0000000000000000}
            Up.Coordinates = {0000803F000000000000000000000000}
            Interval = 100
          end
          object Targetx2: TGLCube
            Direction.Coordinates = {0000000000000080FFFF7F3F00000000}
            Up.Coordinates = {0000803F000000000000000000000000}
          end
        end
        object ActorBasex3: TGLDummyCube
          CubeSize = 1.000000000000000000
          object GLActor3: TGLActor
            Material.Texture.Disabled = False
            Direction.Coordinates = {00000080FFFF7F3F0000000000000000}
            Up.Coordinates = {0000803F000000000000000000000000}
            Interval = 100
          end
          object Targetx3: TGLCube
            Direction.Coordinates = {0000000000000080FFFF7F3F00000000}
            Up.Coordinates = {0000803F000000000000000000000000}
          end
        end
        object ActorBasex4: TGLDummyCube
          CubeSize = 1.000000000000000000
          object GLActor4: TGLActor
            Material.Texture.Disabled = False
            Direction.Coordinates = {00000080FFFF7F3F0000000000000000}
            Up.Coordinates = {0000803F000000000000000000000000}
            Interval = 100
          end
          object Targetx4: TGLCube
            Direction.Coordinates = {0000000000000080FFFF7F3F00000000}
            Up.Coordinates = {0000803F000000000000000000000000}
          end
        end
        object ActorBasex5: TGLDummyCube
          CubeSize = 1.000000000000000000
          object GLActor5: TGLActor
            Material.Texture.Disabled = False
            Direction.Coordinates = {00000080FFFF7F3F0000000000000000}
            Up.Coordinates = {0000803F000000000000000000000000}
            Interval = 100
          end
          object Targetx5: TGLCube
            Direction.Coordinates = {0000000000000080FFFF7F3F00000000}
            Up.Coordinates = {0000803F000000000000000000000000}
          end
        end
        object ActorBasex6: TGLDummyCube
          CubeSize = 1.000000000000000000
          object GLActor6: TGLActor
            Material.Texture.Disabled = False
            Direction.Coordinates = {00000080FFFF7F3F0000000000000000}
            Up.Coordinates = {0000803F000000000000000000000000}
            Interval = 100
          end
          object Targetx6: TGLCube
            Direction.Coordinates = {0000000000000080FFFF7F3F00000000}
            Up.Coordinates = {0000803F000000000000000000000000}
          end
        end
      end
    end
    object ActorsDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object BaseDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object BunkerDC: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object PathLines: TGLLines
      Nodes = <>
      Options = []
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 56
    Top = 96
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ground'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Emission.Color = {9A99993E9A99993E9A99993E0000803F}
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        Texture2Name = 'details'
      end
      item
        Name = 'details'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {00000043000000430000004300000000}
      end>
    Left = 16
    Top = 56
  end
  object BitmapFont1: TGLBitmapFont
    GlyphsIntervalX = 1
    GlyphsIntervalY = 1
    Ranges = <
      item
        StartASCII = ' '
        StopASCII = 'Z'
        StartGlyphIdx = 0
      end>
    CharWidth = 30
    CharHeight = 30
    Left = 16
    Top = 96
  end
end

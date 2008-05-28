object Form1: TForm1
  Left = 192
  Top = 114
  Width = 678
  Height = 512
  Caption = 'GLScene Dynamic Collision Engine - Demonstration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 670
    Height = 485
    Camera = GLCamera1
    Buffer.FogEnvironment.FogStart = 50.000000000000000000
    Buffer.FogEnvironment.FogEnd = 250.000000000000000000
    Buffer.BackgroundColor = clBlack
    Buffer.FogEnable = True
    FieldOfView = 156.699401855468800000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00000000000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Terrain: TGLTerrainRenderer
      Material.MaterialLibrary = GLMatlLib
      HeightDataSource = GLBitmapHDS1
      TilesPerTexture = 1.000000000000000000
      BehavioursData = {
        0201060C54474C4443455374617469630200060D474C4443454D616E61676572
        310203020009090F0000A0410F00000000020008}
    end
    object Ground: TGLPlane
      Material.FrontProperties.Diffuse.Color = {D3D2D23EC7C6463FC7C6C63E0000803F}
      Material.MaterialLibrary = GLMatlLib
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {00000000000010C0000000000000803F}
      Scale.Coordinates = {0000FA430000FA430000803F00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Visible = False
      Height = 1.000000000000000000
      Width = 1.000000000000000000
      XTiles = 50
      YTiles = 50
      Style = []
      NoZWrite = False
      BehavioursData = {
        0201060C54474C4443455374617469630200060D474C4443454D616E61676572
        310201020009080F0000A0410F00000000020008}
    end
    object Balls: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object moMushroom: TGLFreeForm
      Material.FrontProperties.Diffuse.Color = {A9A8283F8B8A8A3E9190103E0000803F}
      Position.Coordinates = {00000000000000000000A0C00000803F}
      NormalsOrientation = mnoInvert
      BehavioursData = {
        0201060C54474C4443455374617469630200060D474C4443454D616E61676572
        310202020009090F0000803F0F00000000020008}
    end
    object Mushrooms: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCube1: TGLCube
      Material.FrontProperties.Diffuse.Color = {B1B0B03D9998183F8180003D0000803F}
      Direction.Coordinates = {00000000CAA8073FAE19593F00000000}
      PitchAngle = 32.000000000000000000
      Position.Coordinates = {0000804000000000000020410000803F}
      Scale.Coordinates = {0000A041000080400000204100000000}
      Up.Coordinates = {00000000AE19593FCAA807BF00000000}
      BehavioursData = {
        0201060C54474C4443455374617469630200060D474C4443454D616E61676572
        310201020009090F0000F0410F00000000020008}
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object Player: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
      BehavioursData = {
        0201060D54474C44434544796E616D69630200060D474C4443454D616E616765
        723102000909090F0000003F0F00000000020502000200090000803F0000A03F
        0000803F00000000}
      object GLCamera1: TGLCamera
        DepthOfView = 300.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = Player
        Position.Coordinates = {0000000000000040000040C00000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
      object GLActor1: TGLActor
        Material.MaterialLibrary = GLMatlLib
        Interval = 100
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Ambient.Color = {0000803F9190903D000000000000803F}
        Material.FrontProperties.Diffuse.Color = {BFBE3E3F00000000000000006DE77B3E}
        Material.FrontProperties.Emission.Color = {0000000000000000A1A0203D0000803F}
        Material.BlendingMode = bmAdditive
        Radius = 1.000000000000000000
      end
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000804000000000000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
    end
    object HelpShadow: TGLHUDText
      Position.Coordinates = {000030410000F841000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      ModulateColor.Color = {A19E9E3ECFBC3C3ECFBC3C3E0000803F}
    end
    object Help: TGLHUDText
      Position.Coordinates = {000020410000F041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object GLDCEManager1: TGLDCEManager
    Gravity = -30.000000000000000000
    WorldScale = 1.000000000000000000
    MovimentScale = 1.000000000000000000
    StandardiseLayers = ccsDCEStandard
    ManualStep = False
    Left = 8
    Top = 72
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 8
    Top = 104
  end
  object GLMatlLib: TGLMaterialLibrary
    Left = 8
    Top = 136
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Impact'
    Font.Style = []
    Left = 8
    Top = 168
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 8
    Top = 200
  end
end

object Form1: TForm1
  Left = 153
  Top = 28
  Width = 520
  Height = 541
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Motion Blur Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 512
    Height = 512
    Camera = Camera
    VSync = vsmSync
    PostRender = GLSceneViewerPostRender
    Buffer.BackgroundColor = clNavy
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object Light: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000000000000F0410000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object Cube: TGLCube
      RollAngle = -15.000000000000000000
      Up.Coordinates = {EF83843EE946773F0000008000000000}
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      object Torus: TGLTorus
        Scale.Coordinates = {0000A0400000A0400000A04000000000}
        MajorRadius = 0.400000005960464400
        MinorRadius = 0.100000001490116100
      end
    end
    object DummyCube: TGLDummyCube
      Direction.Coordinates = {00000000441DAFBEB28F703F00000000}
      PitchAngle = -10.000000000000000000
      Up.Coordinates = {00008031B28F703F441DAF3E00000000}
      CubeSize = 1.000000000000000000
      object Dodecahedron: TGLDodecahedron
        Direction.Coordinates = {000000B3000000B30000803F00000000}
        Position.Coordinates = {0000000000000000000040400000803F}
        RollAngle = 10.000000000000000000
        Up.Coordinates = {D3D031BE5D1C7C3F28A8CF3200000000}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      end
    end
    object HUD: TGLHUDSprite
      Position.Coordinates = {0000804300008043000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F9A99193F}
      Material.BlendingMode = bmTransparency
      Material.MaterialOptions = [moIgnoreFog, moNoLighting]
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.MagFilter = maNearest
      Material.Texture.MinFilter = miNearest
      Material.Texture.TextureMode = tmModulate
      Material.Texture.Compression = tcNone
      Material.Texture.Disabled = False
      Width = 512.000000000000000000
      Height = 512.000000000000000000
      NoZWrite = True
      MirrorU = False
      MirrorV = False
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Cube
      Position.Coordinates = {0000C040000000000000803F0000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 1.000000000000000000
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 8
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 16
    Top = 40
  end
end

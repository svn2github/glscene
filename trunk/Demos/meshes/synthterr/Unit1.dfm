object Form1: TForm1
  Left = 159
  Top = 120
  Width = 611
  Height = 429
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 603
    Height = 400
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 400
    Buffer.FogEnvironment.FogEnd = 1200
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clBlack
    Buffer.FogEnable = True
    Buffer.Lighting = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 56
    Top = 56
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000000041000000000000803F}
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 1200
        FocalLength = 50
        TargetObject = DummyCube1
        Position.Coordinates = {0000A040000020410000C8410000803F}
        Left = 264
        Top = 160
      end
    end
    object TerrainRenderer1: TGLTerrainRenderer
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00008040000080400000003F00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
      HeightDataSource = GLCustomHDS
      TileSize = 32
      TilesPerTexture = 1
      QualityDistance = 50
      CLODPrecision = 20
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 56
    Top = 96
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 16
    Top = 56
  end
  object GLCustomHDS: TGLCustomHDS
    MaxPoolSize = 0
    OnStartPreparingData = GLCustomHDSStartPreparingData
    Left = 56
    Top = 16
  end
end

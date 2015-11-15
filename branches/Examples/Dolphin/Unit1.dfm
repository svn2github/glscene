object Form1: TForm1
  Left = 212
  Top = 272
  Caption = 'Dolphin Animation'
  ClientHeight = 330
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 516
    Height = 330
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 200.000000000000000000
    Buffer.FogEnvironment.FogEnd = 1000.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyePlane
    Buffer.FogEnable = True
    Buffer.ShadeModel = smFlat
    FieldOfView = 146.283203125000000000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 0
    Top = 0
    Width = 121
    Height = 17
    Caption = 'Dynamic animation'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {000080BF000000002EBD3BB300000000}
      Position.Coordinates = {0000803F0000A0400000803F0000803F}
      Scale.Coordinates = {00004040000040400000404000000000}
      TurnAngle = -90.000000000000000000
      OnProgress = GLDummyCube1Progress
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {000040C0000080BF000080400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
      object GLFreeForm1: TGLFreeForm
        Material.FrontProperties.Ambient.Color = {D1D0D03DBFBE3E3FFAF9793F0000803F}
        Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
        Material.FrontProperties.Emission.Color = {9A99993E9A99993E9A99993E0000803F}
        Material.FrontProperties.Shininess = 110
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Material.FaceCulling = fcNoCull
        Direction.Coordinates = {000000000000803F0000000000000000}
        RollAngle = 90.000000000000000000
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
        Up.Coordinates = {F5787EBF000000007A64DFBD00000000}
        NormalsOrientation = mnoInvert
      end
    end
    object GLTerrainRenderer1: TGLTerrainRenderer
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00008040000080400000803E00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLBitmapHDS1
      TilesPerTexture = 1.000000000000000000
      QualityDistance = 150.000000000000000000
      ContourWidth = 0
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 16
  end
  object Timer1: TTimer
    Interval = 300
    OnTimer = Timer1Timer
    Left = 96
    Top = 16
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 56
    Top = 64
  end
end

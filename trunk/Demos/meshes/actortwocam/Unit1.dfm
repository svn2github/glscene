object Form1: TForm1
  Left = 114
  Top = 96
  Width = 659
  Height = 405
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 25
    Width = 651
    Height = 351
    Camera = GLCamera2
    Align = alClient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 651
    Height = 25
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object Label3: TLabel
      Left = 304
      Top = 6
      Width = 75
      Height = 13
      Caption = 'F7 Third Person'
    end
    object Label4: TLabel
      Left = 400
      Top = 6
      Width = 86
      Height = 13
      Caption = 'F8 First Person'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 8
      Top = 6
      Width = 265
      Height = 13
      Caption = 'Move with arrow keys, strafe with CTRL, run with SHIFT'
    end
    object CBMouseLook: TCheckBox
      Left = 512
      Top = 4
      Width = 97
      Height = 17
      Caption = 'Mouse Look'
      TabOrder = 0
      OnClick = CBMouseLookClick
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 40
    Top = 32
    object SkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15
        end
        item
          StartAngle = 15
          StopAngle = 90
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Stacks = 4
        end
        item
          StartAngle = -15
          StartColor.Color = {9D9C1C3EA4A3233F9D9C1C3E0000803F}
          StopColor.Color = {000000008180003F000000000000803F}
        end>
      Stars = <>
    end
    object Disk1: TGLDisk
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Material.Texture.MinFilter = miLinear
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Material.Texture.Disabled = False
      Loops = 1
      OuterRadius = 80
      Slices = 7
      SweepAngle = 360
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000704200003443000000000000803F}
      LightStyle = lsOmni
      SpotCutOff = 360
    end
    object DummyCube1: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      CubeSize = 1
      object FreeForm1: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000803F0000803F000000000000803F}
        Scale.Coordinates = {0AD7A33CCDCCCC3C4260E53C00000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        NormalsOrientation = mnoInvert
      end
    end
    object DummyCube2: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 0.100000001490116
      object GLCamera2: TGLCamera
        DepthOfView = 500
        FocalLength = 100
        Position.Coordinates = {000000000000003F000000000000803F}
        Direction.Coordinates = {00000080000000000000803F00000000}
        Left = 320
        Top = 192
      end
      object Actor1: TGLActor
        Direction.Coordinates = {000000800000803F0000000000000000}
        Up.Coordinates = {0000803F000000000000000000000000}
        Visible = False
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.MinFilter = miLinear
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Material.Texture.Disabled = False
        StartFrame = 0
        EndFrame = 0
        CurrentFrame = 0
        Interval = 100
        Animations = <>
        OverlaySkeleton = False
        object Actor2: TGLActor
          Direction.Coordinates = {00000080000000000000803F00000000}
          Material.Texture.MinFilter = miLinear
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
          Material.Texture.Disabled = False
          StartFrame = 0
          EndFrame = 0
          CurrentFrame = 0
          Interval = 100
          Animations = <>
          OverlaySkeleton = False
        end
      end
      object DummyCube3: TGLDummyCube
        CubeSize = 1
        object GLCamera1: TGLCamera
          DepthOfView = 1000
          FocalLength = 200
          TargetObject = DummyCube2
          Position.Coordinates = {00000000000040400000A0C10000803F}
          Direction.Coordinates = {00000000000000800000803F00000000}
        end
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 32
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 64
  end
  object GLNavigator1: TGLNavigator
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = DummyCube2
    UseVirtualUp = True
    AutoUpdateObject = False
    AngleLock = False
    Left = 40
    Top = 64
  end
  object GLUserInterface1: TGLUserInterface
    InvertMouse = False
    MouseSpeed = 20
    GLNavigator = GLNavigator1
    Left = 40
    Top = 96
  end
end

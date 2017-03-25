object Form1: TForm1
  Left = 232
  Top = 172
  Caption = 'Cam path demo'
  ClientHeight = 596
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 862
    Height = 596
    Camera = GLCamera2
    FieldOfView = 160.950668334960900000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 696
    Top = 136
    object GLCamera2: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCube1
      Position.Coordinates = {3333333F3333333F333333BF0000803F}
    end
    object GLEarthSkyDome1: TGLEarthSkyDome
      Direction.Coordinates = {00000000000080BF0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Bands = <>
      Stars = <>
      SunElevation = 75.000000000000000000
      Turbidity = 15.000000000000000000
      ExtendedOptions = []
      Slices = 48
      Stacks = 24
    end
    object GLCube1: TGLCube
      ShowAxes = True
      CubeSize = {CDCC4C3DCDCC4C3DCDCC4C3D}
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        NearPlaneBias = 0.100000001490116100
        TargetObject = GLDummyCube1
        Position.Coordinates = {00000000CDCCCC3DCDCC4CBE0000803F}
        Direction.Coordinates = {00000000ABD248BE56077BBF00000000}
        Up.Coordinates = {0000000056077B3FABD248BE00000000}
      end
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {8180003F8180003F8180003F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000002041000000000000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLPlane1: TGLPlane
      Material.FrontProperties.Diffuse.Color = {B81E053EA470BD3E52B89E3E0000803F}
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      PitchAngle = 90.000000000000000000
      Position.Coordinates = {00000000CDCCCCBD000000000000803F}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Height = 20.000000000000000000
      Width = 20.000000000000000000
    end
    object GLLines1: TGLLines
      LineColor.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
      LineWidth = 4.000000000000000000
      NodeColor.Color = {00000000000000000000803F0000003F}
      Nodes = <
        item
        end
        item
          X = 0.500000000000000000
        end
        item
          X = 0.500000000000000000
          Z = -0.500000000000000000
        end
        item
          X = -0.500000000000000000
          Y = 0.500000000000000000
          Z = -0.500000000000000000
        end
        item
          X = -0.500000000000000000
          Y = 0.500000000000000000
          Z = 0.500000000000000000
        end
        item
          X = 0.500000000000000000
          Z = 0.500000000000000000
        end
        item
          Y = 0.100000001490116100
        end
        item
          X = -0.500000000000000000
          Y = 0.100000001490116100
          Z = -0.500000000000000000
        end
        item
        end>
      NodesAspect = lnaInvisible
      NodeSize = 0.050000000745058060
      Division = 500
      SplineMode = lsmCubicSpline
      Options = []
    end
    object GLLines2: TGLLines
      Visible = False
      Nodes = <>
      NodesAspect = lnaInvisible
      Options = []
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 0.050000000745058060
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 104
    Top = 72
  end
end

object Form1: TForm1
  Left = 192
  Top = 119
  Width = 665
  Height = 446
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 110
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 657
    Height = 409
    Camera = GLCamera1
    FieldOfView = 2.66200399398804
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 32
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000803F00000000000000000000803F}
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        TargetObject = GLDummyCube1
        Position.Coordinates = {00000040000060400000C0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1
          SpotCutOff = 180
        end
      end
    end
    object Machine: TGLDummyCube
      CubeSize = 1
      object Arm: TGLCube
        Material.FrontProperties.Diffuse.Color = {CDCC0C3FEC51B83DEC51B83D0000803F}
        Position.Coordinates = {0000003F0000403F0000C0BF0000803F}
        CubeSize = {0000F0400000803E0000403F}
      end
      object Pin2: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Position.Coordinates = {000060400000003F0000C0BF0000803F}
        BottomRadius = 0.25
        Height = 1
        TopRadius = 0.25
      end
      object Slider: TGLCube
        Material.FrontProperties.Diffuse.Color = {1F856B3F14AE473F52B81E3F0000803F}
        Position.Coordinates = {00005040000000000000C0BF0000803F}
        CubeSize = {000080400000003F0000803F}
      end
      object Wheel: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Position.Coordinates = {000020C000000000000000000000803F}
        BottomRadius = 2
        Height = 0.5
        TopRadius = 2
        object Pin1: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          Position.Coordinates = {000000000000003F0000C0BF0000803F}
          BottomRadius = 0.25
          Height = 1
          TopRadius = 0.25
        end
        object Axle: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          Position.Coordinates = {00000000000000C0000000000000803F}
          BottomRadius = 0.5
          Height = 5
          TopRadius = 0.5
        end
        object GLSphere1: TGLSphere
          Position.Coordinates = {0000000000000000000000400000803F}
          Radius = 0.5
        end
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 32
  end
  object GLODEManager1: TGLODEManager
    Solver = osmDefault
    Iterations = 3
    MaxContacts = 8
    Visible = False
    VisibleAtRunTime = False
    Left = 104
    Top = 32
  end
  object GLODEJointList1: TGLODEJointList
    Left = 136
    Top = 32
    ODEJointsData = {
      0201060E544F44454A6F696E7448696E67650200060B576865656C2046697865
      640200060D474C4F44454D616E61676572310600060009020002000802000802
      0005000000000000000000000500000000000000000000050000000000000000
      0000050000000000000000000005000000000000000000000500000000000000
      0000000500000000000000000000050000000000000000000005000000000000
      0000000005000000000000000000000500000000000000000000}
  end
end

object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 480
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
    Top = 0
    Width = 688
    Height = 446
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
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
      Up.Coordinates = {000000000000803F0000008000000000}
      CubeSize = 1
      object Wheel: TGLCylinder
        Position.Coordinates = {000020C000000000000000000000803F}
        Up.Coordinates = {000000000000803F0000008000000000}
        Material.FrontProperties.Diffuse.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        BottomRadius = 2
        Height = 0.5
        Slices = 32
        TopRadius = 2
        object Axle: TGLCylinder
          Position.Coordinates = {00000000000000C0000000000000803F}
          Up.Coordinates = {000000000000803F0000008000000000}
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          BottomRadius = 0.5
          Height = 5
          TopRadius = 0.5
        end
        object Pin1: TGLCylinder
          Position.Coordinates = {000000000000003F0000C0BF0000803F}
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          BottomRadius = 0.25
          Height = 1
          TopRadius = 0.25
        end
      end
      object Arm: TGLCube
        Direction.Coordinates = {4B413AB4000000000000803F00000000}
        Position.Coordinates = {0000003F0000403F0000C0BF0000803F}
        Material.FrontProperties.Diffuse.Color = {CDCC0C3FEC51B83DEC51B83D0000803F}
        CubeSize = {0000F0400000803E0000403F}
      end
      object Pin2: TGLCylinder
        Position.Coordinates = {000060400000003F0000C0BF0000803F}
        Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        BottomRadius = 0.25
        Height = 1
        TopRadius = 0.25
      end
      object Slider: TGLCube
        Position.Coordinates = {00005040000000000000C0BF0000803F}
        Material.FrontProperties.Diffuse.Color = {1F856B3F14AE473F52B81E3F0000803F}
        CubeSize = {000080400000003F0000803F}
      end
    end
  end
  object GLODEManager1: TGLODEManager
    Left = 40
    Top = 8
  end
  object GLODEJointList1: TGLODEJointList
    Left = 72
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
end

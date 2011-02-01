object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 531
  ClientWidth = 735
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 735
    Height = 531
    Camera = GLCamera1
    FieldOfView = 158.669494628906300000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 176
    Top = 16
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000000000004040000020410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object Floor: TGLCube
      Position.Coordinates = {00000000000080BF000000000000803F}
      BehavioursData = {
        0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
        746174696302000200060D474C4E47444D616E61676572310800080F0AD7233C
        1200000000}
      CubeSize = {000020410000803F00002041}
      object GLCube2: TGLCube
        Position.Coordinates = {000000000000A0400000A0C00000803F}
        BehavioursData = {
          0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
          746174696302000200060D474C4E47444D616E61676572310800080F0AD7233C
          1200000000}
        CubeSize = {00002041000020410000803F}
      end
      object GLCube3: TGLCube
        Position.Coordinates = {0000C0400000A040000080BF0000803F}
        BehavioursData = {
          0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
          746174696302000200060D474C4E47444D616E61676572310800080F0AD7233C
          1200000000}
        CubeSize = {0000803F0000204100002041}
      end
      object GLCube4: TGLCube
        Position.Coordinates = {0000C0C00000A040000080BF0000803F}
        BehavioursData = {
          0458434F4C02010201060C54474C4E47445374617469630200060A4E47442053
          746174696302000200060D474C4E47444D616E61676572310800080F0AD7233C
          1200000000}
        CubeSize = {0000803F0000204100002041}
      end
    end
    object GLCube1: TGLCube
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000200060D474C4E47444D616E61676572310800080F0000
        000012000000000200090FCDCCCC3D0F0000803F090F00000000020008020008
        0200090000000000000000000000000000803F020008}
    end
    object GLSphere1: TGLSphere
      Position.Coordinates = {000000C000004040000000000000803F}
      Radius = 0.500000000000000000
      BehavioursData = {
        0458434F4C02010201060D54474C4E474444796E616D69630200060B4E474420
        44796E616D696302000200060D474C4E47444D616E61676572310800080F0000
        000012000000000200090FCDCCCC3D0F0000803F090F00000000020008020008
        0200090000000000000000000000000000803F020008}
    end
    object GLLines1: TGLLines
      Pickable = False
      Nodes = <
        item
          X = -0.625000000000000000
          Y = -0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = -0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = -0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = 0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = 0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = 0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = 0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = -0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = -0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = -0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = -0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = 0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = 0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = 0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = 0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = -0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = -0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = -0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = -0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = -0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = 0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.625000000000000000
          Y = 0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = 0.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -0.625000000000000000
          Y = 0.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 2.375000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 2.375000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 2.375000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 3.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 3.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 3.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 3.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 2.375000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 2.375000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 2.375000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 2.375000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 3.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 3.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 3.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 3.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 2.375000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 2.375000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 2.375000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 2.375000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 2.375000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 3.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -1.375000000000000000
          Y = 3.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 3.625000000000000000
          Z = 0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = -2.625000000000000000
          Y = 3.625000000000000000
          Z = -0.625000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end>
      NodesAspect = lnaInvisible
      SplineMode = lsmSegments
      Options = [loUseNodeColorForLines]
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 120
    Top = 16
  end
  object GLNGDManager1: TGLNGDManager
    VisibleAtRunTime = True
    NewtonSurfaceItem = <>
    NewtonSurfacePair = <>
    DebugOption.NGDManagerDebugs = [mdShowAABB, mdShowJoint]
    Line = GLLines1
    NewtonJoint = <
      item
        KinematicControllerOptions.PickModeLinear = True
        KinematicControllerOptions.AngularFriction = 500.000000000000000000
        JointType = nj_KinematicController
        CollisionState = True
      end>
    Left = 360
    Top = 24
  end
end

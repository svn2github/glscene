object Form1: TForm1
  Left = 83
  Top = 84
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
    Height = 453
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
        Material.FrontProperties.Diffuse.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Position.Coordinates = {000020C000000000000000000000803F}
        Up.Coordinates = {000000000000803F0000008000000000}
        BottomRadius = 2
        Height = 0.5
        Slices = 32
        TopRadius = 2
        BehavioursData = {
          0201061654474C4F444544796E616D69634265686176696F757202000200060D
          474C4F44454D616E6167657231020002020612544F4445456C656D656E744361
          7073756C650200060743617073756C6502000200080200080200080500000000
          00000080FF3F02000500000000000000800040050000000000000080FE3F0200
          0200060743617073756C65020002000900000000000000C0000000000000803F
          020008020008050000000000000080FF3F0200050000000000000080FE3F0500
          000000000000A00140}
        object Axle: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          Position.Coordinates = {00000000000000C0000000000000803F}
          Up.Coordinates = {000000000000803F0000008000000000}
          BottomRadius = 0.5
          Height = 5
          TopRadius = 0.5
        end
        object Pin1: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          Position.Coordinates = {000000000000003F0000C0BF0000803F}
          BottomRadius = 0.25
          Height = 1
          TopRadius = 0.25
        end
      end
      object Arm: TGLCube
        Material.FrontProperties.Diffuse.Color = {CDCC0C3FEC51B83DEC51B83D0000803F}
        Direction.Coordinates = {4B413AB4000000000000803F00000000}
        Position.Coordinates = {0000003F0000403F0000C0BF0000803F}
        BehavioursData = {
          0201061654474C4F444544796E616D69634265686176696F757202000200060D
          474C4F44454D616E616765723102000201060E544F4445456C656D656E74426F
          7802000603426F780200020008020008020008050000000000000080FF3F0200
          0500000000000000F00140050000000000000080FD3F0500000000000000C0FE
          3F}
        CubeSize = {0000F0400000803E0000403F}
      end
      object Pin2: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Position.Coordinates = {000060400000003F0000C0BF0000803F}
        BottomRadius = 0.25
        Height = 1
        TopRadius = 0.25
        BehavioursData = {
          0201061654474C4F444544796E616D69634265686176696F757202000200060D
          474C4F44454D616E6167657231020002010612544F4445456C656D656E744361
          7073756C650200060743617073756C6502000200080200080200080500000000
          00000080FF3F0200050000000000000080FD3F050000000000000080FF3F}
      end
      object Slider: TGLCube
        Material.FrontProperties.Diffuse.Color = {1F856B3F14AE473F52B81E3F0000803F}
        Position.Coordinates = {00005040000000000000C0BF0000803F}
        CubeSize = {000080400000003F0000803F}
      end
    end
  end
  object GLODEManager1: TGLODEManager
    Solver = osmQuickStep
    Iterations = 3
    Left = 40
    Top = 8
  end
  object GLODEJointList1: TGLODEJointList
    Left = 72
    Top = 8
    ODEJointsData = {
      0204060E544F44454A6F696E7448696E67650200060B576865656C2046697865
      640200060D474C4F44454D616E6167657231020009000020C000000000000000
      000000803F0200080200080605576865656C060002000200060B576865656C20
      2D2041726D0200060D474C4F44454D616E6167657231020009000020C0000000
      3F0000C0BF0000803F0200080200080605576865656C060341726D0200020006
      0A41726D202D2050696E320200060D474C4F44454D616E616765723102000900
      0060400000003F0000C0BF0000803F020008020008060341726D060450696E32
      060F544F44454A6F696E74536C696465720200060A50696E3220466978656402
      00060D474C4F44454D616E61676572310200080200090000803F000000000000
      000000000000020008060450696E320600}
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
end

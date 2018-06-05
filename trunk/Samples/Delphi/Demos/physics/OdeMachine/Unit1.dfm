object Form1: TForm1
  Left = 83
  Top = 84
  Caption = 'Ode Machine'
  ClientHeight = 436
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 688
    Height = 436
    Camera = GLCamera1
    FieldOfView = 154.164367675781300000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000803F00000000000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {00000040000060400000C0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object Machine: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Wheel: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Position.Coordinates = {000020C000000000000000000000803F}
        BottomRadius = 2.000000000000000000
        Height = 0.500000000000000000
        Slices = 32
        TopRadius = 2.000000000000000000
        BehavioursData = {
          0458434F4C02010202060D54474C4F444544796E616D69630200120000000002
          000200060D474C4F44454D616E61676572310200050000000000000080004008
          12000500000000000000FA084005000000000000000000000500000000000000
          0000000500000000000000000000050000000000000000000005000000000000
          0000000005000000000000000000000500000000000000000000050000000000
          0000000000050000000000000000000002010458434F4C020102030613544F44
          45456C656D656E7443796C696E6465720200060843796C696E64657202000200
          08020008020008050000000000000080FF3F0200050000000000000080004005
          0000000000000080FE3F02000200060843796C696E6465720200020009000000
          00000000C0000000000000803F020008020008050000000000000080FF3F0200
          050000000000000080FE3F0500000000000000A0014002000200060843796C69
          6E6465720200020009000000000000003F0000C0BF0000803F02000802000805
          0000000000000080FF3F0200050000000000000080FD3F050000000000000080
          FF3F09060C54474C4F44455374617469630200120000000002000200060D474C
          4F44454D616E616765723102000500000000006F1283F53F0800000500000000
          000000FA08400500000000000000000000050000000000000000000005000000
          0000000000000005000000000000000000000500000000000000000000050000
          0000000000000000050000000000000000000005000000000000000000000500
          00000000000000000002000458434F4C02010200}
        object Axle: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          Position.Coordinates = {00000000000000C0000000000000803F}
          BottomRadius = 0.500000000000000000
          Height = 5.000000000000000000
          TopRadius = 0.500000000000000000
        end
        object Pin1: TGLCylinder
          Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
          Position.Coordinates = {000000000000003F0000C0BF0000803F}
          BottomRadius = 0.250000000000000000
          Height = 1.000000000000000000
          TopRadius = 0.250000000000000000
        end
      end
      object Arm: TGLCube
        Material.FrontProperties.Diffuse.Color = {CDCC0C3FEC51B83DEC51B83D0000803F}
        Direction.Coordinates = {4B413AB4000000000000803F00000000}
        Position.Coordinates = {0000003F0000403F0000C0BF0000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C4F444544796E616D69630200120000000002
          000200060D474C4F44454D616E61676572310200050000000000000080FF3F08
          0E000500000000000000FA084005000000000000000000000500000000000000
          0000000500000000000000000000050000000000000000000005000000000000
          0000000005000000000000000000000500000000000000000000050000000000
          0000000000050000000000000000000002010458434F4C02010201060E544F44
          45456C656D656E74426F7802000603426F780200020008020008020008050000
          000000000080FF3F02000500000000000000F00140050000000000000080FD3F
          0500000000000000C0FE3F09}
        CubeSize = {0000F0400000803E0000403F}
      end
      object Pin2: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Position.Coordinates = {000060400000003F0000C0BF0000803F}
        BottomRadius = 0.250000000000000000
        Height = 1.000000000000000000
        TopRadius = 0.250000000000000000
        BehavioursData = {
          0458434F4C02010201060D54474C4F444544796E616D69630200120000000002
          000200060D474C4F44454D616E61676572310200050000000000000080FF3F08
          12000500000000000000FA084005000000000000000000000500000000000000
          0000000500000000000000000000050000000000000000000005000000000000
          0000000005000000000000000000000500000000000000000000050000000000
          0000000000050000000000000000000002010458434F4C020102010613544F44
          45456C656D656E7443796C696E6465720200060843796C696E64657202000200
          08020008020008050000000000000080FF3F0200050000000000000080FD3F05
          0000000000000080FF3F09}
      end
      object Slider: TGLCube
        Material.FrontProperties.Diffuse.Color = {1F856B3F14AE473F52B81E3F0000803F}
        Position.Coordinates = {00005040000000000000C0BF0000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C4F444544796E616D69630200060A4F444520
          536C6964657202000200060D474C4F44454D616E616765723102000500000000
          006F1283F53F0800000500000000000000FA0840050000000000000000000005
          0000000000000000000005000000000000000000000500000000000000000000
          0500000000000000000000050000000000000000000005000000000000000000
          000500000000000000000000050000000000000000000002010458434F4C0201
          020009}
        CubeSize = {000080400000003F0000803F}
      end
    end
    object ODERenderPoint: TGLRenderPoint
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000204100002041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000000000000000000000000000803F}
    end
  end
  object GLODEManager1: TGLODEManager
    Solver = osmQuickStep
    Iterations = 3
    MaxContacts = 8
    RenderPoint = ODERenderPoint
    Visible = True
    VisibleAtRunTime = False
    Left = 40
    Top = 8
  end
  object GLODEJointList1: TGLODEJointList
    Left = 88
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Left = 40
    Top = 40
  end
end

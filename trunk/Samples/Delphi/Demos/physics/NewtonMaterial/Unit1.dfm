object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 605
  ClientWidth = 732
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
    Width = 732
    Height = 605
    Camera = GLCamera1
    FieldOfView = 161.228958129882800000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 168
    Top = 24
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube3
      Position.Coordinates = {000000000000A040000070C10000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000C0C000000000000000000000803F}
      CubeSize = 1.000000000000000000
      object GLSphere2: TGLSphere
        Position.Coordinates = {0000000000002041000000000000803F}
        Radius = 0.500000000000000000
        BehavioursData = {
          0458434F4C02010201060D54474C4E474444796E616D69630200120000000002
          000200060D474C4E47444D616E61676572310800090F0AD7233C120000000002
          00090FCDCCCC3D0F0000803F090F00000000020008020008020008020008}
      end
      object GLSphere1: TGLSphere
        Position.Coordinates = {000000C000004040000000000000803F}
        Radius = 0.500000000000000000
        BehavioursData = {
          0458434F4C02010201060D54474C4E474444796E616D69630200120000000002
          000200060D474C4E47444D616E61676572310800090F0AD7233C120000000002
          00090FCDCCCC3D0F0000803F090F00000000020008020008020008020008}
      end
      object GLCube1: TGLCube
        Position.Coordinates = {0000004000000040000000000000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C4E474444796E616D69630200120000000002
          000200060D474C4E47444D616E61676572310800090F0AD7233C120000000002
          00090FCDCCCC3D0F0000803F090F00000000020008020008020008020008}
      end
      object Trampoline: TGLCube
        Position.Coordinates = {00000000000080BF000000000000803F}
        BehavioursData = {
          0458434F4C02010201060C54474C4E4744537461746963020012000000000200
          0200060D474C4E47444D616E61676572310800090F0AD7233C1200000000}
        CubeSize = {000020410000803F00002041}
      end
    end
    object GLDummyCube2: TGLDummyCube
      Direction.Coordinates = {00000000ED83843EEA46773F00000000}
      PitchAngle = 15.000000000000000000
      Position.Coordinates = {0000A04000000000000000000000803F}
      Up.Coordinates = {00000000EA46773FED8384BE00000000}
      CubeSize = 1.000000000000000000
      object GLCube2: TGLCube
        Position.Coordinates = {0000004000000040000080400000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C4E474444796E616D69630200120000000002
          000200060D474C4E47444D616E61676572310800090F0AD7233C120000000002
          00090FCDCCCC3D0F0000803F090F00000000020008020008020008020008}
      end
      object GLCube3: TGLCube
        Position.Coordinates = {0000000000000040000080400000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C4E474444796E616D69630200120000000002
          000200060D474C4E47444D616E61676572310800090F0AD7233C120000000002
          00090FCDCCCC3D0F0000803F090F00000000020008020008020008020008}
      end
      object GLCube4: TGLCube
        Position.Coordinates = {000000C000000040000080400000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C4E474444796E616D69630200120000000002
          000200060D474C4E47444D616E61676572310800090F0AD7233C120000000002
          00090FCDCCCC3D0F0000803F090F00000000020008020008020008020008}
      end
      object Friction: TGLCube
        Position.Coordinates = {00000000000080BF000000000000803F}
        BehavioursData = {
          0458434F4C02010201060C54474C4E4744537461746963020012000000000200
          0200060D474C4E47444D616E61676572310800090F0AD7233C1200000000}
        CubeSize = {000020410000803F00002041}
      end
    end
    object GLDummyCube3: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 280
    Top = 24
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Form1 - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 120
    Top = 24
  end
  object GLNGDManager1: TGLNGDManager
    NewtonSurfaceItem = <
      item
        DisplayName = 'Trampoline'
      end
      item
        DisplayName = 'Friction'
      end
      item
        DisplayName = 'cube2'
      end
      item
        DisplayName = 'cube3'
      end
      item
        DisplayName = 'cube4'
      end
      item
        DisplayName = 'Sphere1+Sphere2+cube1'
      end>
    NewtonSurfacePair = <
      item
        Elasticity = 2.000000000000000000
      end
      item
        StaticFriction = 1.000000000000000000
        KineticFriction = 1.000000000000000000
      end
      item
        StaticFriction = 0.009999999776482582
        KineticFriction = 0.001000000047497451
      end
      item
        StaticFriction = 0.300000011920929000
        KineticFriction = 0.200000002980232200
      end>
    NewtonJoint = <>
    Left = 224
    Top = 24
  end
end

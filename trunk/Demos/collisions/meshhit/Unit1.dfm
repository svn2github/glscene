object Form1: TForm1
  Left = 108
  Top = 101
  Width = 562
  Height = 284
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
    Left = 8
    Top = 8
    Width = 265
    Height = 241
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLSceneViewer2: TGLSceneViewer
    Left = 280
    Top = 8
    Width = 265
    Height = 241
    Camera = GLCamera2
    Buffer.BackgroundColor = clGray
    OnMouseDown = GLSceneViewer2MouseDown
    OnMouseMove = GLSceneViewer2MouseMove
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {000048420000704200008C420000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object FreeForm1: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
        ShowAxes = True
        Up.Coordinates = {00000000000000000000803F00000000}
        NormalsOrientation = mnoInvert
      end
    end
    object Sphere1: TGLSphere
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
      Radius = 0.300000011920929
      Slices = 6
      Stacks = 6
      object ArrowLine1: TGLArrowLine
        Position.Coordinates = {0000000000000000CDCCCC3D0000803F}
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F9A99193F}
        Material.FrontProperties.Emission.Color = {1283803E1283803E000000000000803F}
        Material.BlendingMode = bmTransparency
        BottomRadius = 0.0500000007450581
        Height = 1
        TopRadius = 0.100000001490116
        TopArrowHeadHeight = 0.5
        TopArrowHeadRadius = 0.200000002980232
        BottomArrowHeadHeight = 0.5
        BottomArrowHeadRadius = 0.200000002980232
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000504100004040000010410000803F}
    end
    object GLCamera2: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000204100002041000020410000803F}
    end
  end
end

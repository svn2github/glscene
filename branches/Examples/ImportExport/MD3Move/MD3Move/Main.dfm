object Form1: TForm1
  Left = 214
  Top = 248
  Caption = 'MD3Move'
  ClientHeight = 590
  ClientWidth = 792
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
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  DesignSize = (
    792
    590)
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 792
    Height = 590
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 160.760543823242200000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 464
    Top = 16
    Width = 322
    Height = 242
    Anchors = [akTop, akRight]
    TabOrder = 1
    object PaintBox1: TPaintBox
      Left = 1
      Top = 1
      Width = 320
      Height = 240
      Align = alClient
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object Actor: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
      CubeSize = 1.000000000000000000
    end
    object GLPlane1: TGLPlane
      Position.Coordinates = {0000000000000000000040C00000803F}
      Visible = False
      Height = 20.000000000000000000
      Width = 20.000000000000000000
    end
    object GLXYZGrid1: TGLXYZGrid
      Position.Coordinates = {0000000000000000CDCC0CC00000803F}
      XSamplingScale.Min = -10.000000000000000000
      XSamplingScale.Max = 10.000000000000000000
      XSamplingScale.Step = 1.000000000000000000
      YSamplingScale.Min = -10.000000000000000000
      YSamplingScale.Max = 10.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ZSamplingScale.Step = 0.100000001490116100
    end
    object Map: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube1: TGLCube
        Position.Coordinates = {00000000000020C19A99D9BF0000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102030200}
        CubeSize = {333397410000803F0000803F}
      end
      object GLCube2: TGLCube
        Position.Coordinates = {00000000000020419A99D9BF0000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102030200}
        CubeSize = {333397410000803F0000803F}
      end
      object GLCube3: TGLCube
        Position.Coordinates = {00002041000000009A99D9BF0000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102030200}
        CubeSize = {0000803F333397410000803F}
      end
      object GLCube4: TGLCube
        Position.Coordinates = {000020C1000000009A99D9BF0000803F}
        BehavioursData = {
          0458434F4C02010201060D54474C42436F6C6C6973696F6E0202020012000000
          0002000611436F6C6C6973696F6E4D616E616765723102030200}
        CubeSize = {0000803F333397410000803F}
      end
      object GLCube5: TGLCube
        Position.Coordinates = {00000000000020C1000020410000803F}
      end
    end
    object GLSphere1: TGLSphere
      Position.Coordinates = {0000000000002041000000000000803F}
      Radius = 0.500000000000000000
    end
    object Help: TGLHUDText
      Position.Coordinates = {0000A0410000A041000000000000803F}
      BitmapFont = Font
      Text = '1111'
      Rotation = 0.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Actor
      Position.Coordinates = {0000A0400000A0400000A0400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLCamera2: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 10.000000000000000000
      TargetObject = GLDummyCube2
      Position.Coordinates = {0000104100000000000000000000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      object GLDummyCube2: TGLDummyCube
        Position.Coordinates = {000020C100000000000000000000803F}
        ShowAxes = True
        CubeSize = 1.000000000000000000
        VisibleAtRunTime = True
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object CollisionManager1: TGLCollisionManager
    OnCollision = CollisionManager1Collision
    Left = 8
    Top = 104
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = GLCamera2
    Width = 320
    Height = 240
    BeforeRender = GLMemoryViewer1BeforeRender
    AfterRender = GLMemoryViewer1AfterRender
    Buffer.BackgroundColor = clBlack
    Buffer.ColorDepth = cd8bits
    Left = 8
    Top = 72
  end
  object Font: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Ranges = <
      item
        StartASCII = #0
        StopASCII = #1103
        StartGlyphIdx = 0
      end>
    Left = 8
    Top = 136
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 8
    Top = 168
  end
end

object Form1: TForm1
  Left = 197
  Top = 104
  Width = 458
  Height = 362
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 450
    Height = 288
    Camera = GLCamera1
    Align = alClient
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 288
    Width = 450
    Height = 45
    Align = alBottom
    Max = 20
    Min = -20
    Orientation = trHorizontal
    Frequency = 5
    Position = 20
    SelEnd = 0
    SelStart = 0
    TabOrder = 1
    TickMarks = tmBoth
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object Button1: TButton
    Left = 336
    Top = 16
    Width = 91
    Height = 25
    Caption = 'Test collisions'
    TabOrder = 2
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000A0410000A0410000A0410000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
    end
    object Sphere1: TGLSphere
      Position.Coordinates = {0000000000000000000000400000803F}
      Material.FrontProperties.Diffuse.Color = {8A8F0F3FBEBC3C3F8A8F0F3F0000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Radius = 0.5
      BehavioursData = {
        0201060D54474C42436F6C6C6973696F6E02010611436F6C6C6973696F6E4D61
        6E616765723102010200}
    end
    object Sphere2: TGLSphere
      Material.FrontProperties.Diffuse.Color = {00000000F8FEFE3E0000803F0000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Radius = 0.5
      BehavioursData = {
        0201060D54474C42436F6C6C6973696F6E02010611436F6C6C6973696F6E4D61
        6E616765723102010200}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000A04000004040000000400000803F}
      Left = 256
      Top = 160
    end
  end
  object CollisionManager1: TCollisionManager
    OnCollision = CollisionManager1Collision
    Left = 16
    Top = 48
  end
end

object Form1: TForm1
  Left = 165
  Top = 101
  Width = 563
  Height = 276
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 56
    Top = 8
    Width = 68
    Height = 14
    Caption = 'Centered X, Y'
  end
  object Label2: TLabel
    Left = 232
    Top = 8
    Width = 55
    Height = 14
    Caption = 'Centered Y'
  end
  object Label3: TLabel
    Left = 408
    Top = 8
    Width = 81
    Height = 14
    Caption = 'Centered X, Y, Z'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 32
    Width = 513
    Height = 209
    Camera = GLCamera1
    Buffer.BackgroundColor = 11447982
  end
  object TrackBar1: TTrackBar
    Left = 528
    Top = 32
    Width = 25
    Height = 209
    Max = 80
    Min = -80
    Orientation = trVertical
    Frequency = 10
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 1
    ThumbLength = 10
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 56
    object DummyCube3: TGLDummyCube
      Position.Coordinates = {000000C000000000000000000000803F}
      ShowAxes = True
      CubeSize = 1
      VisibleAtRunTime = True
      object FreeForm3: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {0000803F8180003F8180803E0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      end
    end
    object DummyCube2: TGLDummyCube
      ShowAxes = True
      CubeSize = 1
      VisibleAtRunTime = True
      object FreeForm2: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {0000803F8180003F8180803E0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      end
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000004000000000000000000000803F}
      ShowAxes = True
      CubeSize = 1
      VisibleAtRunTime = True
      object FreeForm1: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {0000803F8180003F8180803E0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {00004040000020410000A0400000803F}
      SpotCutOff = 180
    end
    object DCCamera: TGLDummyCube
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 180
        TargetObject = DummyCube2
        Position.Coordinates = {0000000000000040000070410000803F}
        Left = 256
        Top = 96
      end
    end
  end
end

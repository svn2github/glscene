object Form1: TForm1
  Left = 204
  Top = 101
  Width = 457
  Height = 343
  Caption = 'Form1'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 433
    Height = 273
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object TrackBar: TTrackBar
    Left = 56
    Top = 288
    Width = 385
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Max = 360
    Orientation = trHorizontal
    PageSize = 10
    Frequency = 10
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 1
    ThumbLength = 15
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBarChange
  end
  object CBPlay: TCheckBox
    Left = 8
    Top = 292
    Width = 41
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Play'
    TabOrder = 2
  end
  object StaticText1: TStaticText
    Left = 16
    Top = 16
    Width = 45
    Height = 17
    BorderStyle = sbsSingle
    Caption = '??? FPS'
    TabOrder = 3
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 48
    object Cube1: TGLCube
      Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 2
      object Cube2: TGLCube
        Position.Coordinates = {0000404000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {8786063F8786063F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000000000000000A1A0203F0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        CubeSize = {0000003F0000003F0000003F}
        object DummyCube2: TGLDummyCube
          Direction.Coordinates = {00000000F304353FF304353F00000000}
          Up.Coordinates = {00000000F304353FF30435BF00000000}
          CubeSize = 1
          object Cube3: TGLCube
            Position.Coordinates = {000000000000803F000000000000803F}
            Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
            CubeSize = {CDCC4C3ECDCC4C3ECDCC4C3E}
          end
        end
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 100
      TargetObject = Cube1
      Position.Coordinates = {000020410000A040000020410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 88
  end
end

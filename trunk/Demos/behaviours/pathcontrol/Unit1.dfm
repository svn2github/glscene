object Form1: TForm1
  Left = 83
  Top = 95
  Width = 513
  Height = 424
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
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 489
    Height = 333
    Camera = GLCamera1
    Buffer.BackgroundColor = clAppWorkSpace
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object CBPlay: TCheckBox
    Left = 64
    Top = 357
    Width = 41
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Play'
    TabOrder = 1
    OnClick = CBPlayClick
  end
  object MoveBtn: TBitBtn
    Left = 8
    Top = 351
    Width = 49
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Move'
    TabOrder = 2
    OnClick = MoveBtnClick
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 40
    object DummyCube1: TGLDummyCube
      CubeSize = 2
      VisibleAtRunTime = True
      object Cube2: TGLCube
        Position.Coordinates = {0000404000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {8786063F8786063F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000000000000000A1A0203F0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Diffuse.Color = {B8B7373FB8B7373FB8B7373F0000803F}
      Position.Coordinates = {0000204100000000000020410000803F}
      SpotCutOff = 180
    end
    object Sphere1: TGLSphere
      Position.Coordinates = {0000004500002042008009450000803F}
      Scale.Coordinates = {0000A0400000A0400000A04000000000}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Radius = 0.5
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {00000000000000000000A0410000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    TimeMultiplier = 2
    SleepLength = 10
    Left = 80
    Top = 40
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 120
    Top = 40
  end
end

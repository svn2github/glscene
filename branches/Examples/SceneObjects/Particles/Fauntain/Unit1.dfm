object Form1: TForm1
  Left = 192
  Top = 113
  ActiveControl = Button1
  Caption = 'Fountain / Particle System Demo'
  ClientHeight = 369
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    334
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 16
    Top = 8
    Width = 292
    Height = 275
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 140.033782958984400000
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 16
    Top = 347
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Reset'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object Fountain: TGLParticles
      ObjectsSorting = osNone
      Direction.Coordinates = {0000803F2EBDBB330000000000000000}
      Scale.Coordinates = {00000040000000400000004000000000}
      Up.Coordinates = {2EBDBBB30000803F0000000000000000}
      CubeSize = 3.000000000000000000
      object Sphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {BFBEBE3E0000803FEBEA6A3F0000803F}
        Material.FrontProperties.Shininess = 79
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmAdditive
        OnProgress = Sphere1Progress
        Normals = nsFlat
        Radius = 0.014999999664723870
        Slices = 3
        Stacks = 3
      end
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000000040000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object Plane1: TGLPlane
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 5.000000000000000000
      Width = 5.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000000000004040000000410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000803F00000040000040400000803F}
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 48
    Top = 56
  end
end

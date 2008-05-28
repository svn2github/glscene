object Form1: TForm1
  Left = 194
  Top = 119
  Width = 701
  Height = 579
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 693
    Height = 511
    Camera = GLCamera1
    FieldOfView = 157.854904174804700000
    Align = alClient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 693
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 32
      Top = 3
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 32
      Top = 22
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object Label3: TLabel
      Left = 232
      Top = 1
      Width = 389
      Height = 39
      Caption = 'Test objects are hidden!!!!!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 48
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube1: TGLCube
      end
      object GLCylinder1: TGLCylinder
        Position.Coordinates = {0000803F00000000000000000000803F}
        BottomRadius = 0.500000000000000000
        Height = 1.000000000000000000
        TopRadius = 0.500000000000000000
      end
    end
    object GLDummyCube2: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCube2: TGLCube
      end
    end
    object OGLBeginQueries: TGLDirectOpenGL
      UseBuildList = False
      OnRender = OGLBeginQueriesRender
      Blend = False
    end
    object dcTestObjects: TGLDummyCube
      Position.Coordinates = {0000003F3333333F0000803F0000803F}
      CubeSize = 1.000000000000000000
      object GLTorus1: TGLTorus
        Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
        MajorRadius = 0.400000005960464500
        MinorRadius = 0.100000001490116100
      end
      object GLCone1: TGLCone
        Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
        BottomRadius = 0.500000000000000000
        Height = 1.000000000000000000
      end
    end
    object OGLEndQueries: TGLDirectOpenGL
      UseBuildList = False
      OnRender = OGLEndQueriesRender
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {00000000000000000000A0400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 80
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 112
  end
end

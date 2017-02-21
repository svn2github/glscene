object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'Animated Sprites'
  ClientHeight = 436
  ClientWidth = 698
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
    Left = 0
    Top = 0
    Width = 560
    Height = 436
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.FaceCulling = False
    FieldOfView = 154.164367675781200000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 560
    Top = 0
    Width = 138
    Height = 436
    Align = alRight
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Top = 16
      Width = 113
      Height = 25
      Cursor = crHandPoint
      Caption = 'Sprite 1 punch'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 16
      Top = 48
      Width = 113
      Height = 25
      Cursor = crHandPoint
      Caption = 'Sprite 2 punch'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object GLScene1: TGLScene
    Left = 56
    Top = 24
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000000000000A040000000000000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
  end
  object matLib: TGLMaterialLibrary
    Left = 24
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 20
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 64
  end
end

object Form1: TForm1
  Left = 283
  Top = 110
  Caption = 'Form1'
  ClientHeight = 596
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 688
    Height = 596
    Camera = GLCamera1
    FieldOfView = 160.950668334960900000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 688
    Top = 0
    Width = 174
    Height = 596
    Align = alRight
    TabOrder = 1
    DesignSize = (
      174
      596)
    object flipH_CB: TCheckBox
      Left = 21
      Top = 40
      Width = 97
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'HorizontalFlip'
      TabOrder = 0
    end
    object flipV_CB: TCheckBox
      Left = 21
      Top = 17
      Width = 97
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'VerticalFlip'
      TabOrder = 1
    end
    object RadioGroup1: TRadioGroup
      Left = 16
      Top = 104
      Width = 121
      Height = 105
      Caption = 'AirCraft'
      ItemIndex = 0
      Items.Strings = (
        'Jet'
        'Observer')
      TabOrder = 2
    end
  end
  object GLScene1: TGLScene
    Left = 176
    Top = 16
    object GLActor1: TGLActor
      Material.BackProperties.Ambient.Color = {0000000000000000000000000000803F}
      Position.Coordinates = {00000000000020C1000000000000803F}
      AnimationMode = aamBounceForward
      Interval = 100
      MaterialLibrary = GLMaterialLibrary1
    end
    object GLFreeForm1: TGLFreeForm
      MaterialLibrary = GLMaterialLibrary1
    end
    object GLFreeForm2: TGLFreeForm
      Direction.Coordinates = {00000000F404B5330000803F00000000}
      Position.Coordinates = {000040400000A040000000000000803F}
      Scale.Coordinates = {6F12033A6F12033A6F12033A00000000}
      Up.Coordinates = {000000000000803FF404B5B300000000}
      MaterialLibrary = GLMaterialLibrary1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLActor1
      Position.Coordinates = {0000A0400000C040000090410000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object Render1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = Render1Render
      Blend = False
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 176
    Top = 80
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 304
    Top = 72
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 280
    Top = 16
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 408
    Top = 16
  end
end

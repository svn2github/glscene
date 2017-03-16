object Form1: TForm1
  Left = 275
  Top = 208
  Caption = 'GLSL Video Effects'
  ClientHeight = 473
  ClientWidth = 786
  Color = clGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 504
    Height = 473
    Camera = GLCamera1
    Buffer.BackgroundColor = 16492697
    Buffer.FaceCulling = False
    FieldOfView = 156.125030517578100000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 504
    Top = 0
    Width = 282
    Height = 473
    Align = alRight
    BevelOuter = bvNone
    Color = 6316128
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 9
      Top = 8
      Width = 265
      Height = 177
      Caption = 'Controls'
      TabOrder = 0
      object Label3: TLabel
        Left = 8
        Top = 16
        Width = 49
        Height = 13
        Caption = 'Brightness'
      end
      object Label1: TLabel
        Left = 8
        Top = 56
        Width = 39
        Height = 13
        Caption = 'Contrast'
      end
      object Label2: TLabel
        Left = 8
        Top = 96
        Width = 48
        Height = 13
        Caption = 'Saturation'
      end
      object BrightnessTB: TTrackBar
        Left = 32
        Top = 32
        Width = 209
        Height = 20
        Max = 200
        Frequency = 10
        Position = 100
        TabOrder = 0
        TabStop = False
        ThumbLength = 14
      end
      object ContrastTB: TTrackBar
        Left = 32
        Top = 72
        Width = 209
        Height = 20
        Max = 200
        Frequency = 10
        Position = 100
        TabOrder = 1
        TabStop = False
        ThumbLength = 14
      end
      object SaturationTB: TTrackBar
        Left = 32
        Top = 112
        Width = 209
        Height = 20
        Max = 200
        Frequency = 10
        Position = 100
        TabOrder = 2
        TabStop = False
        ThumbLength = 14
      end
      object EnabledCB: TCheckBox
        Left = 16
        Top = 144
        Width = 97
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = EnabledCBClick
      end
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLFreeForm1: TGLFreeForm
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      PitchAngle = 90.000000000000000000
      Scale.Coordinates = {0AD7233C0AD7233C0AD7233C00000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      MaterialLibrary = GLMaterialLibrary1
    end
    object Render1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = Render1Render
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLFreeForm1
      Position.Coordinates = {00004842000048420000C8420000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 8
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 8
    Top = 40
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 40
    Top = 40
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 8
    Top = 72
  end
end

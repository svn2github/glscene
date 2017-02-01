object Form1: TForm1
  Left = 193
  Top = 123
  Caption = 'Form1'
  ClientHeight = 454
  ClientWidth = 689
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
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 689
    Height = 423
    Camera = cam
    Buffer.BackgroundColor = 7748106
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aa6x
    FieldOfView = 153.398193359375000000
    Align = alClient
    OnMouseDown = vpMouseDown
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 423
    Width = 689
    Height = 31
    Align = alBottom
    Color = 7748106
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object RadioButton1: TRadioButton
      Left = 16
      Top = 8
      Width = 73
      Height = 17
      Caption = 'sample1'
      TabOrder = 0
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 80
      Top = 8
      Width = 73
      Height = 17
      Caption = 'sample2'
      TabOrder = 1
      OnClick = RadioButton1Click
    end
    object RadioButton3: TRadioButton
      Left = 144
      Top = 8
      Width = 73
      Height = 17
      Caption = 'sample3'
      TabOrder = 2
      OnClick = RadioButton1Click
    end
    object RadioButton4: TRadioButton
      Left = 208
      Top = 8
      Width = 73
      Height = 17
      Caption = 'sample4'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RadioButton1Click
    end
    object StaticText1: TStaticText
      Left = 400
      Top = 6
      Width = 58
      Height = 17
      Caption = 'StaticText1'
      TabOrder = 4
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc_cam: TGLDummyCube
      Position.Coordinates = {0000204200002042000020410000803F}
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = dc_cam
        Position.Coordinates = {0000A0410000F041000020420000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object dc_sample1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object dc_sample2: TGLDummyCube
      Visible = False
      CubeSize = 1.000000000000000000
      object line_sample2: TGLLines
        AntiAliased = True
        Nodes = <>
        NodesAspect = lnaInvisible
        SplineMode = lsmSegments
        Options = []
      end
      object dogl_sample2: TGLDirectOpenGL
        UseBuildList = False
        OnRender = dogl_sample2Render
        Blend = False
        object ff_sample2: TGLFreeForm
          Visible = False
        end
      end
    end
    object dogl_sample3: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = dogl_sample3Render
      Blend = False
      object ff_sample3: TGLFreeForm
        Visible = False
      end
    end
    object dogl_sample4: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = dogl_sample4Render
      Blend = False
      object ff_sample4: TGLFreeForm
        Visible = False
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
  object at: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = atTimer
    ThreadPriority = tpNormal
    Left = 72
    Top = 8
  end
end

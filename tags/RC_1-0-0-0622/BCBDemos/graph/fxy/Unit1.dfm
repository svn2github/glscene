object Form1: TForm1
  Left = 192
  Top = 121
  Width = 627
  Height = 430
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 110
  TextHeight = 16
  object Label1: TLabel
    Left = 502
    Top = 39
    Width = 93
    Height = 16
    Caption = 'XY grid position'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 10
    Top = 10
    Width = 464
    Height = 365
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 111.183052062988
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CheckBox1: TCheckBox
    Left = 492
    Top = 10
    Width = 120
    Height = 21
    Caption = 'Centered Grids'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object TrackBar1: TTrackBar
    Left = 522
    Top = 59
    Width = 55
    Height = 316
    Min = -10
    Orientation = trVertical
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 2
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 24
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180
    end
    object GLHeightField1: TGLHeightField
      Direction.Coordinates = {0044F4970000803F2EBD3BB300000000}
      ShowAxes = True
      Up.Coordinates = {0000803FEDAD09A72EBD3BB300000000}
      XSamplingScale.Min = -1
      XSamplingScale.Max = 1
      XSamplingScale.Step = 0.0199999995529652
      YSamplingScale.Min = -1
      YSamplingScale.Max = 1
      YSamplingScale.Step = 0.0199999995529652
      object XYGrid: TGLXYZGrid
        LineColor.Color = {00000000000000000000803F0000803F}
        XSamplingScale.Min = -1
        XSamplingScale.Max = 1
        XSamplingScale.Step = 0.100000001490116
        YSamplingScale.Min = -1
        YSamplingScale.Max = 1
        YSamplingScale.Step = 0.100000001490116
        ZSamplingScale.Min = -1
        ZSamplingScale.Max = 1
        ZSamplingScale.Step = 0.100000001490116
      end
      object XZGrid: TGLXYZGrid
        LineColor.Color = {000000000000803F000000000000803F}
        XSamplingScale.Min = -1
        XSamplingScale.Max = 1
        XSamplingScale.Step = 0.100000001490116
        YSamplingScale.Min = -1
        YSamplingScale.Max = 1
        YSamplingScale.Step = 0.100000001490116
        ZSamplingScale.Min = -1
        ZSamplingScale.Max = 1
        ZSamplingScale.Step = 0.100000001490116
        Parts = [gpX, gpZ]
      end
      object YZGrid: TGLXYZGrid
        LineColor.Color = {0000803F00000000000000000000803F}
        XSamplingScale.Min = -1
        XSamplingScale.Max = 1
        XSamplingScale.Step = 0.100000001490116
        YSamplingScale.Min = -1
        YSamplingScale.Max = 1
        YSamplingScale.Step = 0.100000001490116
        ZSamplingScale.Min = -1
        ZSamplingScale.Max = 1
        ZSamplingScale.Step = 0.100000001490116
        Parts = [gpY, gpZ]
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 125
      TargetObject = GLHeightField1
      Position.Coordinates = {0000404000008040000000410000803F}
    end
  end
end

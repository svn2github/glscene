object Form1: TForm1
  Left = 192
  Top = 121
  Width = 517
  Height = 345
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 408
    Top = 32
    Width = 73
    Height = 13
    Caption = 'XY grid position'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 377
    Height = 297
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = aa4xHQ
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CheckBox1: TCheckBox
    Left = 400
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Centered Grids'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object TrackBar1: TTrackBar
    Left = 424
    Top = 48
    Width = 45
    Height = 257
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

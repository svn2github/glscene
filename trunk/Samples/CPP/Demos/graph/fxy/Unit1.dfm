object Form1: TForm1
  Left = 192
  Top = 121
  Caption = 'Fxy '
  ClientHeight = 386
  ClientWidth = 619
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 392
    Height = 386
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 114.140289306640600000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 392
    Top = 0
    Width = 227
    Height = 386
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 58
      Top = 40
      Width = 80
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'XYZ grid position'
    end
    object TrackBar1: TTrackBar
      Left = 21
      Top = 63
      Width = 36
      Height = 257
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Min = -10
      Orientation = trVertical
      TabOrder = 0
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 77
      Top = 63
      Width = 36
      Height = 257
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Min = -10
      Orientation = trVertical
      TabOrder = 1
      OnChange = TrackBar2Change
    end
    object TrackBar3: TTrackBar
      Left = 133
      Top = 63
      Width = 36
      Height = 257
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Min = -10
      Orientation = trVertical
      TabOrder = 2
      OnChange = TrackBar3Change
    end
    object CheckBox1: TCheckBox
      Left = 56
      Top = 339
      Width = 97
      Height = 17
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Centered Grids'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
  object GLScene1: TGLScene
    Left = 80
    Top = 48
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLHeightField1: TGLHeightField
      Direction.Coordinates = {0044F4970000803F2EBD3BB300000000}
      ShowAxes = True
      Up.Coordinates = {0000803FEDAD09A72EBD3BB300000000}
      XSamplingScale.Min = -1.000000000000000000
      XSamplingScale.Max = 1.000000000000000000
      XSamplingScale.Step = 0.019999999552965160
      YSamplingScale.Min = -1.000000000000000000
      YSamplingScale.Max = 1.000000000000000000
      YSamplingScale.Step = 0.019999999552965160
      object XYGrid: TGLXYZGrid
        LineColor.Color = {00000000000000000000803F0000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.Max = 1.000000000000000000
        XSamplingScale.Step = 0.100000001490116100
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.Max = 1.000000000000000000
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.Max = 1.000000000000000000
        ZSamplingScale.Step = 0.100000001490116100
      end
      object XZGrid: TGLXYZGrid
        LineColor.Color = {000000000000803F000000000000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.Max = 1.000000000000000000
        XSamplingScale.Step = 0.100000001490116100
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.Max = 1.000000000000000000
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.Max = 1.000000000000000000
        ZSamplingScale.Step = 0.100000001490116100
        Parts = [gpX, gpZ]
      end
      object YZGrid: TGLXYZGrid
        LineColor.Color = {0000803F00000000000000000000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.Max = 1.000000000000000000
        XSamplingScale.Step = 0.100000001490116100
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.Max = 1.000000000000000000
        YSamplingScale.Step = 0.100000001490116100
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.Max = 1.000000000000000000
        ZSamplingScale.Step = 0.100000001490116100
        Parts = [gpY, gpZ]
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 125.000000000000000000
      TargetObject = GLHeightField1
      Position.Coordinates = {0000404000008040000000410000803F}
    end
  end
end

object Form1: TForm1
  Left = 179
  Top = 106
  BorderWidth = 4
  Caption = 'Fxy'
  ClientHeight = 257
  ClientWidth = 441
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
    Width = 272
    Height = 257
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 91.582031250000000000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 272
    Top = 0
    Width = 169
    Height = 257
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 62
      Top = 48
      Width = 82
      Height = 13
      Caption = 'XYZ Grid position'
    end
    object CBCentered: TCheckBox
      Left = 64
      Top = 9
      Width = 97
      Height = 17
      Caption = 'Centered grids'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBCenteredClick
    end
    object TBXYPosition: TTrackBar
      Left = 6
      Top = 88
      Width = 45
      Height = 249
      Min = -10
      Orientation = trVertical
      TabOrder = 1
      TickMarks = tmBoth
      OnChange = TBXYPositionChange
    end
    object TBYZPosition: TTrackBar
      Left = 68
      Top = 88
      Width = 45
      Height = 249
      Min = -10
      Orientation = trVertical
      TabOrder = 2
      TickMarks = tmBoth
      OnChange = TBYZPositionChange
    end
    object TBXZPosition: TTrackBar
      Left = 119
      Top = 88
      Width = 45
      Height = 249
      Min = -10
      Orientation = trVertical
      TabOrder = 3
      TickMarks = tmBoth
      OnChange = TBXZPositionChange
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 125.000000000000000000
      TargetObject = HeightField1
      Position.Coordinates = {0000404000008040000000410000803F}
      Left = 208
      Top = 168
    end
    object HeightField1: TGLHeightField
      Direction.Coordinates = {0044F4970000803F2EBD3BB300000000}
      ShowAxes = True
      Up.Coordinates = {0000803F583DAF262EBD3B3300000000}
      XSamplingScale.Min = -1.000000000000000000
      XSamplingScale.max = 1.000000000000000000
      XSamplingScale.step = 0.019999999552965160
      YSamplingScale.Min = -1.000000000000000000
      YSamplingScale.max = 1.000000000000000000
      YSamplingScale.step = 0.019999999552965160
      OnGetHeight = HeightField1GetHeight
      object YZGrid: TGLXYZGrid
        LineColor.Color = {0000803F00000000000000000000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.max = 1.000000000000000000
        XSamplingScale.step = 0.100000001490116100
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.max = 1.000000000000000000
        YSamplingScale.step = 0.100000001490116100
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.max = 1.000000000000000000
        ZSamplingScale.step = 0.100000001490116100
        Parts = [gpY, gpZ]
      end
      object XZGrid: TGLXYZGrid
        LineColor.Color = {000000000000803F000000000000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.max = 1.000000000000000000
        XSamplingScale.step = 0.100000001490116100
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.max = 1.000000000000000000
        YSamplingScale.step = 0.100000001490116100
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.max = 1.000000000000000000
        ZSamplingScale.step = 0.100000001490116100
        Parts = [gpX, gpZ]
      end
      object XYGrid: TGLXYZGrid
        LineColor.Color = {00000000000000000000803F0000803F}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.max = 1.000000000000000000
        XSamplingScale.step = 0.100000001490116100
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.max = 1.000000000000000000
        YSamplingScale.step = 0.100000001490116100
        ZSamplingScale.Min = -1.000000000000000000
        ZSamplingScale.max = 1.000000000000000000
        ZSamplingScale.step = 0.100000001490116100
      end
    end
  end
end

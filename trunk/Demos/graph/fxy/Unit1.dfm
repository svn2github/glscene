object Form1: TForm1
  Left = 179
  Top = 106
  Width = 497
  Height = 334
  AutoSize = True
  BorderWidth = 4
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
    Left = 392
    Top = 32
    Width = 75
    Height = 13
    Caption = 'XY Grid position'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 377
    Height = 297
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CBCentered: TCheckBox
    Left = 384
    Top = 0
    Width = 97
    Height = 17
    Caption = 'Centered grids'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CBCenteredClick
  end
  object TBXYPosition: TTrackBar
    Left = 408
    Top = 48
    Width = 45
    Height = 249
    Min = -10
    Orientation = trVertical
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 2
    TickMarks = tmBoth
    TickStyle = tsAuto
    OnChange = TBXYPositionChange
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object HeightField1: TGLHeightField
      Direction.Coordinates = {0044F4970000803F2EBD3BB300000000}
      ShowAxes = True
      Up.Coordinates = {0000803F583DAF262EBD3B3300000000}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      XSamplingScale.Min = -1
      XSamplingScale.Max = 1
      XSamplingScale.Step = 0.0199999995529652
      YSamplingScale.Min = -1
      YSamplingScale.Max = 1
      YSamplingScale.Step = 0.0199999995529652
      OnGetHeight = HeightField1GetHeight
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
        LinesSmoothing = False
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
        LinesSmoothing = False
      end
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
        LinesSmoothing = False
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000F041000048420000C8420000803F}
      SpotCutOff = 180
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 125
      TargetObject = HeightField1
      Position.Coordinates = {0000404000008040000000410000803F}
      Left = 208
      Top = 168
    end
  end
end

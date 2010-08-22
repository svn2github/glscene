object Form1: TForm1
  Left = 217
  Top = 127
  Width = 334
  Height = 364
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
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 326
    Height = 327
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 162.560501098633
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLines1: TGLLines
      LineWidth = 6
      Nodes = <
        item
          X = -1
          Y = -1
        end
        item
          Color.Color = {0000803F000000000000803F0000803F}
        end
        item
          X = 1
          Y = 1
          Color.Color = {0000803F0000803F000000000000803F}
        end>
      NodesAspect = lnaCube
      NodeSize = 0.5
      SplineMode = lsmCubicSpline
      Options = [loUseNodeColorForLines]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 25
      TargetObject = GLLines1
      CameraStyle = csOrthogonal
      Position.Coordinates = {00000000000000000000A0400000803F}
    end
  end
end

object Form1: TForm1
  Left = 198
  Top = 140
  Width = 353
  Height = 372
  BorderWidth = 5
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
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 335
    Height = 335
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object Lines1: TLines
      Direction.Coordinates = {00000000000000000000803F00000000}
      Position.Coordinates = {0000000000000000000000000000803F}
      Scale.Coordinates = {0000803F0000803F0000803F00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      AntiAliased = True
      LineColor.Color = {0000803F0000803F0000803F0000803F}
      LineWidth = 6
      Nodes = <
        item
          X = -1
          Y = -1
          Color.Color = {00000000000000000000803F0000803F}
        end
        item
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 1
          Y = 1
          Color.Color = {0000803F00000000000000000000803F}
        end>
      NodeColor.Color = {00000000000000000000803F0000803F}
      NodesAspect = lnaCube
      NodeSize = 0.5
      SplineMode = lsmCubicSpline
      Options = [loUseNodeColorForLines]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 25
      TargetObject = Lines1
      CameraStyle = csOrthogonal
      Position.Coordinates = {00000000000000000000A0400000803F}
      Direction.Coordinates = {0000000000000000000080BF00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      Left = 248
      Top = 152
    end
  end
end

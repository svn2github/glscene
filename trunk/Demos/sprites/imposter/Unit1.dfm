object Form1: TForm1
  Left = 193
  Top = 102
  Width = 395
  Height = 320
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 272
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 361
    Height = 256
    Camera = GLCamera1
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha]
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 24
    object GLSkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 15.000000000000000000
        end
        item
          StartAngle = 15.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Stacks = 4
        end
        item
          StartAngle = -90.000000000000000000
          StartColor.Color = {0000000000000000000000000000803F}
          StopColor.Color = {0000803F0000803F0000803F0000803F}
        end>
      Stars = <>
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
    end
    object GLTeapot1: TGLTeapot
      Visible = False
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004842000034420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLDodecahedron1: TGLDodecahedron
      Visible = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDirectOpenGL1
      Position.Coordinates = {000040400000A0400000E0400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 72
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 64
  end
end

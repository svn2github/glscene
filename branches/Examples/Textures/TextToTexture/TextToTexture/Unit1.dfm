object Form1: TForm1
  Left = 550
  Top = 290
  Caption = 'TextToTexture'
  ClientHeight = 496
  ClientWidth = 635
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 635
    Height = 455
    Camera = GLCamera1
    FieldOfView = 141.251724243164100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 455
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 1
    object Edit1: TEdit
      Left = 16
      Top = 12
      Width = 557
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
  end
  object GLScene1: TGLScene
    Left = 4
    Top = 4
    object SkyDome1: TGLSkyDome
      Direction.Coordinates = {A77EB2BD000000009E067FBF00000000}
      Bands = <
        item
          StartColor.Color = {0000803F0000803F000000000000803F}
          StopAngle = 35.000000000000000000
          StopColor.Color = {0000803F0000803F0000803F0000803F}
        end
        item
          StartAngle = 35.000000000000000000
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 90.000000000000000000
          StopColor.Color = {0000803F000000000000803F0000803F}
          Stacks = 4
        end>
      Stars = <>
    end
    object Cube1: TGLCube
      CubeSize = {00008042000040420000003F}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 80.000000000000000000
      TargetObject = Cube1
      Position.Coordinates = {000020420000A0C00000A0420000803F}
      Left = 372
      Top = 244
    end
  end
  object Materials1: TGLMaterialLibrary
    Left = 68
    Top = 4
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 36
    Top = 4
  end
end

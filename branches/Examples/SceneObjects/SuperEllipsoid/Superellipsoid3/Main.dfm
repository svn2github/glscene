object Form1: TForm1
  Left = 137
  Top = 77
  Caption = '  SuperGems'
  ClientHeight = 700
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 700
    Height = 700
    Camera = GLCamera1
    FieldOfView = 120.510238647460900000
    Align = alClient
    TabOrder = 0
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    ZoomSpeed = 1.009999990463257000
    FormCaption = 'Superellipsoids Demo - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 16
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 48
    Top = 16
  end
  object GLScene1: TGLScene
    Left = 80
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000020410000A0410000C8420000803F}
      Specular.Color = {9A99593F9A99593FCDCCCC3D0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 200.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000204200004842000070420000803F}
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {EC51B83ECDCC4C3EEC51B83D0000803F}
    end
  end
end

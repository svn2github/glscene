object Form1: TForm1
  Left = 194
  Top = 130
  Align = alClient
  BorderStyle = bsSingle
  Caption = 'Cubemap'
  ClientHeight = 412
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 634
    Height = 412
    Camera = cam
    FieldOfView = 152.714172363281300000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc_cam: TGLDummyCube
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
      end
    end
    object GLSphere1: TGLSphere
      Material.MaterialLibrary = matlib
      Material.LibMaterialName = 'cubemap'
      NormalDirection = ndInside
      Radius = 40.000000000000000000
    end
  end
  object matlib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'cubemap'
        Tag = 0
      end>
    Left = 8
    Top = 40
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Enabled = False
    SleepLength = 1
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
end

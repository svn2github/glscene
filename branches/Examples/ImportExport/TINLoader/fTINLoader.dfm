object MainForm: TMainForm
  Left = 218
  Top = 161
  Caption = 'TIN Loader'
  ClientHeight = 501
  ClientWidth = 767
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object MapImage: TImage
    Left = 600
    Top = 112
    Width = 105
    Height = 105
    Visible = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 767
    Height = 501
    Camera = GLCamera1
    FieldOfView = 157.424118041992200000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 56
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000C842000000000000A0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object World: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      object DummyTerrain: TGLDummyCube
        CubeSize = 1.000000000000000000
        object DGLContourLines: TGLDirectOpenGL
          UseBuildList = False
          Blend = False
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = World
      Position.Coordinates = {000000000000A0C00000A0400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ImageLib'
        Tag = 0
      end>
    Left = 128
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 208
    Top = 8
  end
end

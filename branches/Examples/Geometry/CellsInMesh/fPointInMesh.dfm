object frmCellsInMesh: TfrmCellsInMesh
  Left = 282
  Top = 114
  Caption = 'Cells In Mesh'
  ClientHeight = 402
  ClientWidth = 765
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 82
    Width = 765
    Height = 301
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    FieldOfView = 143.244354248046900000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
    ExplicitTop = 88
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 65
    Width = 765
    Height = 17
    Align = alTop
    TabOrder = 1
    ExplicitTop = 59
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 765
    Height = 65
    Align = alTop
    TabOrder = 2
    object cbShowHull: TCheckBox
      Left = 303
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Show Hull'
      TabOrder = 0
      OnClick = cbShowHullClick
    end
    object rgHull: TRadioGroup
      Left = 15
      Top = 4
      Width = 266
      Height = 49
      Caption = 'Hull'
      Columns = 4
      ItemIndex = 0
      Items.Strings = (
        'Bunny'
        'Box'
        'Sphere'
        'Cone')
      TabOrder = 1
      OnClick = rgHullClick
    end
    object rgCells: TRadioGroup
      Left = 448
      Top = 6
      Width = 273
      Height = 47
      Caption = 'Cells'
      Columns = 4
      ItemIndex = 0
      Items.Strings = (
        'Cube'
        'Sphere'
        'Sprite'
        'None')
      TabOrder = 2
      OnClick = rgCellsClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 383
    Width = 765
    Height = 19
    Panels = <>
  end
  object GLScene1: TGLScene
    Left = 56
    Top = 112
    object GLFreeForm1: TGLFreeForm
      Material.BlendingMode = bmTransparency
      AutoCentering = [macCenterX, macCenterY, macCenterZ]
    end
    object GLLines1: TGLLines
      LineColor.Color = {0000000000000000000000000000803F}
      LineWidth = 3.000000000000000000
      Nodes = <>
      NodesAspect = lnaCube
      Options = []
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLFreeForm1
      Position.Coordinates = {0000B442000000000000B4420000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLCone1: TGLCone
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 144
    Top = 112
  end
end

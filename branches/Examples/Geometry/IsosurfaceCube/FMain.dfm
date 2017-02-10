object FrmMain: TFrmMain
  Left = 192
  Top = 107
  Caption = 'IsoSurface Cube'
  ClientHeight = 506
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 153
    Top = 0
    Width = 527
    Height = 506
    Camera = glcMainCamera
    Buffer.BackgroundColor = clGray
    FieldOfView = 136.866500854492200000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    TabOrder = 0
  end
  object PUSerInterface: TPanel
    Left = 0
    Top = 0
    Width = 153
    Height = 506
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 200
      Width = 38
      Height = 13
      Caption = 'Vertices'
    end
    object Label2: TLabel
      Left = 8
      Top = 216
      Width = 43
      Height = 13
      Caption = 'Triangles'
    end
    object lblVertices: TLabel
      Left = 88
      Top = 200
      Width = 57
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object lblTriangles: TLabel
      Left = 88
      Top = 216
      Width = 57
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0'
    end
    object LEXDim: TLabeledEdit
      Left = 10
      Top = 24
      Width = 137
      Height = 21
      EditLabel.Width = 59
      EditLabel.Height = 13
      EditLabel.Caption = 'X Dimension'
      TabOrder = 0
      Text = '27'
    end
    object LEYDim: TLabeledEdit
      Left = 8
      Top = 72
      Width = 137
      Height = 21
      EditLabel.Width = 59
      EditLabel.Height = 13
      EditLabel.Caption = 'Y Dimension'
      TabOrder = 1
      Text = '27'
    end
    object LEZDim: TLabeledEdit
      Left = 8
      Top = 120
      Width = 137
      Height = 21
      EditLabel.Width = 59
      EditLabel.Height = 13
      EditLabel.Caption = 'Z Dimension'
      TabOrder = 2
      Text = '27'
    end
    object LEIsoVal: TLabeledEdit
      Left = 8
      Top = 168
      Width = 137
      Height = 21
      EditLabel.Width = 40
      EditLabel.Height = 13
      EditLabel.Caption = 'Isovalue'
      TabOrder = 3
      Text = '128'
    end
    object RGAlgorithm: TRadioGroup
      Left = 2
      Top = 248
      Width = 145
      Height = 73
      Caption = 'Algorithm'
      ItemIndex = 0
      Items.Strings = (
        'March. Tetrahedra'
        'March. Cubes')
      TabOrder = 4
      OnClick = RGAlgorithmClick
    end
  end
  object GLScene: TGLScene
    Left = 232
    Top = 120
    object DCDummyCube: TGLDummyCube
      Direction.Coordinates = {F304353F86768AB2F304353F00000000}
      Position.Coordinates = {000000BF000000BF000000BF0000803F}
      TurnAngle = 45.000000000000000000
      Up.Coordinates = {F40435320000803FF404353200000000}
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object ffFreeForm: TGLFreeForm
        Material.PolygonMode = pmLines
      end
    end
    object glcMainCamera: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DCDummyCube
      Position.Coordinates = {0000000000000000000048420000803F}
    end
  end
  object OpenDialog: TOpenDialog
    Left = 232
    Top = 56
  end
  object MainMenu: TMainMenu
    Left = 344
    Top = 56
    object File1: TMenuItem
      Caption = '&File'
      object miFileOpen: TMenuItem
        Caption = '&Open...'
        OnClick = miFileOpenClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miFileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = miFileExitClick
      end
    end
  end
end

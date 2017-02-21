object frmMain: TfrmMain
  Left = 192
  Top = 107
  Caption = 'Mesh Source'
  ClientHeight = 478
  ClientWidth = 680
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
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 495
    Top = 0
    Width = 185
    Height = 459
    Align = alRight
    BevelInner = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 68
      Height = 13
      Caption = 'Polygon Mode'
    end
    object Label6: TLabel
      Left = 8
      Top = 192
      Width = 86
      Height = 13
      Caption = 'Camera Z Position'
    end
    object cbPolygonMode: TComboBox
      Left = 8
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbPolygonModeChange
      Items.Strings = (
        'Fill'
        'Lines / mesh'
        'Points')
    end
    object chbViewPoints: TCheckBox
      Left = 8
      Top = 56
      Width = 121
      Height = 17
      Caption = 'View vertex points'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chbViewPointsClick
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 80
      Width = 169
      Height = 105
      Caption = 'Axis'
      TabOrder = 2
      object Bevel1: TBevel
        Left = 8
        Top = 40
        Width = 153
        Height = 2
        Shape = bsBottomLine
      end
      object Label2: TLabel
        Left = 8
        Top = 48
        Width = 58
        Height = 13
        Caption = 'Axis Legend'
      end
      object Label3: TLabel
        Left = 8
        Top = 72
        Width = 36
        Height = 13
        Caption = 'X Axis'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Label4: TLabel
        Left = 58
        Top = 72
        Width = 36
        Height = 13
        Caption = 'Y Axis'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label5: TLabel
        Left = 112
        Top = 72
        Width = 36
        Height = 13
        Caption = 'Z Axis'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object chbShowAxis: TCheckBox
        Left = 8
        Top = 16
        Width = 153
        Height = 17
        Caption = 'Show central axis'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chbShowAxisClick
      end
    end
    object tbPos: TTrackBar
      Left = 8
      Top = 208
      Width = 169
      Height = 45
      Max = 20
      Position = 5
      TabOrder = 3
      OnChange = tbPosChange
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 248
      Width = 169
      Height = 65
      Caption = 'Movement Direction'
      TabOrder = 4
      object rbXY: TRadioButton
        Left = 8
        Top = 24
        Width = 113
        Height = 17
        Caption = 'Move on X, Y axis'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbZY: TRadioButton
        Left = 8
        Top = 40
        Width = 113
        Height = 17
        Caption = 'Move on Z, Y axis'
        TabOrder = 1
      end
    end
    object GroupBox3: TGroupBox
      Left = 8
      Top = 320
      Width = 169
      Height = 81
      Caption = 'Mesh Data'
      TabOrder = 5
      object btnVertex: TBitBtn
        Left = 4
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Vertex'
        TabOrder = 0
        OnClick = btnVertexClick
      end
      object btnNormals: TBitBtn
        Left = 4
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Normals'
        TabOrder = 1
        OnClick = btnNormalsClick
      end
      object btnTextcoords: TBitBtn
        Left = 79
        Top = 16
        Width = 86
        Height = 25
        Caption = 'Tex-Coords'
        TabOrder = 2
        OnClick = btnTextcoordsClick
      end
      object btnGroups: TBitBtn
        Left = 79
        Top = 48
        Width = 86
        Height = 25
        Caption = 'Triangle Count'
        TabOrder = 3
        OnClick = btnGroupsClick
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 495
    Height = 459
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object GLSceneViewer: TGLSceneViewer
      Left = 2
      Top = 2
      Width = 491
      Height = 455
      Camera = GLCamera
      BeforeRender = GLSceneViewerBeforeRender
      Buffer.BackgroundColor = clBlack
      Buffer.FaceCulling = False
      Buffer.ShadeModel = smSmooth
      FieldOfView = 155.209182739257800000
      Align = alClient
      OnMouseDown = GLSceneViewerMouseDown
      OnMouseMove = GLSceneViewerMouseMove
      OnMouseUp = GLSceneViewerMouseUp
      TabOrder = 0
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 459
    Width = 680
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object GLScene: TGLScene
    Left = 40
    Top = 32
    object GLFreeForm: TGLFreeForm
      Material.FrontProperties.Diffuse.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      Material.FrontProperties.Emission.Color = {8180003F8180003F8180003F0000803F}
      Material.FrontProperties.Specular.Color = {6ABC543F6ABC543F6ABC543F0000803F}
      Material.PolygonMode = pmLines
      Scale.Coordinates = {0AD7233D0AD7233D0AD7233D00000000}
      MaterialLibrary = GLMaterialLibrary1
    end
    object dcModifiers: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLFreeForm
      Position.Coordinates = {0000000000000040000080400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 128
    Top = 32
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene
    Left = 40
    Top = 104
  end
end

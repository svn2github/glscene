object Form1: TForm1
  Left = 193
  Top = 130
  Caption = 'Block Editor'
  ClientHeight = 711
  ClientWidth = 939
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
  OnMouseDown = FormMouseDown
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  DesignSize = (
    939
    711)
  PixelsPerInch = 96
  TextHeight = 13
  object Label8: TLabel
    Left = 655
    Top = 633
    Width = 7
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'X'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object XLabel: TLabel
    Left = 689
    Top = 633
    Width = 6
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label10: TLabel
    Left = 655
    Top = 652
    Width = 7
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Y'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object YLabel: TLabel
    Left = 689
    Top = 652
    Width = 6
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label12: TLabel
    Left = 655
    Top = 668
    Width = 7
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Z'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object ZLabel: TLabel
    Left = 689
    Top = 671
    Width = 6
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 16
    Top = 11
    Width = 9
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
    OnKeyPress = Edit1KeyPress
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 771
    Height = 695
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnMouseDown = FormMouseDown
    object glsViewer: TGLSceneViewer
      Left = 1
      Top = 1
      Width = 769
      Height = 693
      Camera = Camera
      Buffer.BackgroundColor = clBlack
      FieldOfView = 163.577774047851600000
      Align = alClient
      OnClick = glsViewerClick
      OnMouseDown = glsViewerMouseDown
      OnMouseUp = glsViewerMouseUp
      TabOrder = 0
    end
  end
  object pnRight: TPanel
    Left = 724
    Top = 0
    Width = 215
    Height = 711
    Align = alRight
    Caption = ' '
    TabOrder = 2
    DesignSize = (
      215
      711)
    object LabelParts: TLabel
      Left = 31
      Top = 229
      Width = 24
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Parts'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton10: TSpeedButton
      Tag = 3
      Left = 174
      Top = 173
      Width = 17
      Height = 10
      Anchors = [akTop, akRight]
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      Layout = blGlyphBottom
      ParentFont = False
      OnClick = SpeedButton2Click
    end
    object SpeedButton9: TSpeedButton
      Tag = 3
      Left = 174
      Top = 157
      Width = 17
      Height = 10
      Anchors = [akTop, akRight]
      Caption = '+'
      Layout = blGlyphBottom
      OnClick = SpeedButton1Click
    end
    object SpeedButton6: TSpeedButton
      Tag = 2
      Left = 174
      Top = 141
      Width = 17
      Height = 10
      Anchors = [akTop, akRight]
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      Layout = blGlyphBottom
      ParentFont = False
      OnClick = SpeedButton2Click
    end
    object SpeedButton5: TSpeedButton
      Tag = 2
      Left = 174
      Top = 125
      Width = 17
      Height = 10
      Anchors = [akTop, akRight]
      Caption = '+'
      Layout = blGlyphBottom
      OnClick = SpeedButton1Click
    end
    object SpeedButton4: TSpeedButton
      Tag = 1
      Left = 174
      Top = 109
      Width = 17
      Height = 10
      Anchors = [akTop, akRight]
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      Layout = blGlyphBottom
      ParentFont = False
      OnClick = SpeedButton2Click
    end
    object SpeedButton3: TSpeedButton
      Tag = 1
      Left = 174
      Top = 93
      Width = 17
      Height = 10
      Anchors = [akTop, akRight]
      Caption = '+'
      Layout = blGlyphBottom
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 171
      Top = 77
      Width = 17
      Height = 10
      Anchors = [akTop, akRight]
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      Layout = blGlyphBottom
      ParentFont = False
      OnClick = SpeedButton2Click
    end
    object SpeedButton1: TSpeedButton
      Left = 171
      Top = 61
      Width = 17
      Height = 10
      Anchors = [akTop, akRight]
      Caption = '+'
      Layout = blGlyphBottom
      OnClick = SpeedButton1Click
    end
    object LabelDepth: TLabel
      Left = 42
      Top = 171
      Width = 29
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Depth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LabelBHeight: TLabel
      Left = 12
      Top = 136
      Width = 59
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Back Height'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LabelWidth: TLabel
      Left = 27
      Top = 68
      Width = 28
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Width'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LabelFHeight: TLabel
      Left = 13
      Top = 100
      Width = 58
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Front Height'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Memo1: TMemo
      Left = 29
      Top = 440
      Width = 146
      Height = 97
      Anchors = [akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -8
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Lines.Strings = (
        'Movement=Arrows.PgUp,PgDown'
        ''
        'Look=Left button n mouse'
        ''
        'Add=Ins '
        ''
        'Remove=Del')
      ParentFont = False
      TabOrder = 0
      WordWrap = False
    end
    object cbBottom: TCheckBox
      Left = 46
      Top = 296
      Width = 65
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Bottom'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 1
      OnClick = PartsCBClick
    end
    object btAddCubes: TButton
      Left = 30
      Top = 375
      Width = 89
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Add cubes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btAddCubesClick
    end
    object cbBack: TCheckBox
      Left = 46
      Top = 264
      Width = 65
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Back'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 3
      OnClick = PartsCBClick
    end
    object cbFront: TCheckBox
      Left = 46
      Top = 248
      Width = 65
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Front'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 4
      OnClick = PartsCBClick
    end
    object cbRight: TCheckBox
      Left = 46
      Top = 328
      Width = 65
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Right'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 5
      OnClick = PartsCBClick
    end
    object cbLeft: TCheckBox
      Left = 46
      Top = 312
      Width = 65
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Left'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 6
      OnClick = PartsCBClick
    end
    object cbTop: TCheckBox
      Left = 46
      Top = 280
      Width = 65
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Top'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 7
      OnClick = PartsCBClick
    end
    object DepthEdit: TEdit
      Left = 94
      Top = 168
      Width = 49
      Height = 21
      Anchors = [akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      Text = '1.00'
    end
    object BackHeightEdit: TEdit
      Left = 94
      Top = 133
      Width = 49
      Height = 21
      Anchors = [akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      Text = '1.00'
    end
    object FrontHeightedit: TEdit
      Left = 94
      Top = 96
      Width = 49
      Height = 21
      Anchors = [akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      Text = '1.00'
    end
    object WidthEdit: TEdit
      Left = 92
      Top = 65
      Width = 49
      Height = 21
      Anchors = [akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      Text = '1.00'
    end
  end
  object GLScene1: TGLScene
    Left = 72
    Top = 24
    object grdField: TGLXYZGrid
      Position.Coordinates = {00000000B81E05BF000000000000803F}
      LineColor.Color = {CFBC3C3EA19E9E3EA19E9E3E0000803F}
      LineWidth = 0.100000001490116100
      XSamplingScale.Min = -100.000000000000000000
      XSamplingScale.Max = 99.000000000000000000
      XSamplingScale.Step = 3.000000000000000000
      YSamplingScale.Min = -10.000000000000000000
      YSamplingScale.Max = 10.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ZSamplingScale.Min = -100.000000000000000000
      ZSamplingScale.Max = 99.000000000000000000
      ZSamplingScale.Step = 3.000000000000000000
      Parts = [gpX, gpZ]
    end
    object GLDummyCube2: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Camera: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = dcBlock
        Position.Coordinates = {00000000000080400000C0C00000803F}
      end
    end
    object GLFreeForm1: TGLFreeForm
      Material.FrontProperties.Diffuse.Color = {0000803FF8FEFE3E000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
      Material.FrontProperties.Specular.Color = {938C0C3E938E0E3FDCD6D63E0000803F}
      Material.Texture.Disabled = False
      Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
      ShowAxes = True
      MaterialLibrary = GLMaterialLibrary1
    end
    object dcBlock: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      object Block: TGLCube
        Material.FrontProperties.Ambient.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000F8FEFE3E0000803F0000803F}
        Material.FrontProperties.Specular.Color = {00000000000000000000803F0000803F}
        Material.BlendingMode = bmAdditive
        CubeSize = {6666863F6666863F6666863F}
      end
      object grdExtruder: TGLXYZGrid
        Position.Coordinates = {0000003F0000003F0000003F0000803F}
        AntiAliased = True
        LineColor.Color = {0000803F000000000000803F0000803F}
        XSamplingScale.Min = -2.019999980926514000
        XSamplingScale.Max = 1.009999990463257000
        XSamplingScale.Step = 1.009999990463257000
        YSamplingScale.Min = -1.019999980926514000
        YSamplingScale.Step = 1.009999990463257000
        ZSamplingScale.Min = -1.019999980926514000
        ZSamplingScale.Step = 1.009999990463257000
        Parts = [gpX, gpY, gpZ]
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 1
    OnProgress = GLCadencer1Progress
    Left = 72
    Top = 80
  end
  object GLUserInterface1: TGLUserInterface
    MouseSpeed = 20.000000000000000000
    GLNavigator = GLNavigator2
    GLVertNavigator = GLNavigator1
    Left = 424
    Top = 24
  end
  object GLNavigator1: TGLNavigator
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = GLDummyCube2
    UseVirtualUp = True
    AutoUpdateObject = True
    MaxAngle = 35.000000000000000000
    MinAngle = -80.000000000000000000
    AngleLock = True
    Left = 424
    Top = 80
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end
      item
        Name = 'LibMaterial1'
        Tag = 0
      end
      item
        Name = 'LibMaterial2'
        Tag = 0
      end
      item
        Name = 'LibMaterial3'
        Tag = 0
      end
      item
        Name = 'LibMaterial4'
        Tag = 0
      end
      item
        Name = 'LibMaterial5'
        Tag = 0
      end>
    Left = 72
    Top = 133
  end
  object GLNavigator2: TGLNavigator
    VirtualUp.Coordinates = {000000000000803F000000000000803F}
    MovingObject = GLDummyCube2
    UseVirtualUp = True
    AutoUpdateObject = True
    Left = 424
    Top = 136
  end
end

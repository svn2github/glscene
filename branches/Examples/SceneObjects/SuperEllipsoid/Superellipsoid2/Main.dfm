object MainForm: TMainForm
  Left = 70
  Top = 80
  Caption = 'Superellipsoid Mesh'
  ClientHeight = 833
  ClientWidth = 1098
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1098
    833)
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 814
    Width = 1098
    Height = 19
    Cursor = crHandPoint
    Color = clCream
    Panels = <
      item
        Alignment = taCenter
        Width = 176
      end
      item
        Alignment = taCenter
        Width = 176
      end
      item
        Alignment = taCenter
        Width = 176
      end
      item
        Alignment = taCenter
        Width = 176
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 722
    Width = 1098
    Height = 92
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      1098
      92)
    object Label1: TLabel
      Left = 13
      Top = 3
      Width = 44
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'xRadius'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 13
      Top = 31
      Width = 44
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'yRadius'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 13
      Top = 59
      Width = 44
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'zRadius'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 201
      Top = 3
      Width = 33
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Slices'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 197
      Top = 31
      Width = 37
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Stacks'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 545
      Top = 3
      Width = 40
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Bottom'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 726
      Top = 31
      Width = 26
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Stop'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 726
      Top = 3
      Width = 28
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Start'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 376
      Top = 3
      Width = 22
      Height = 16
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Top'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 898
      Top = 3
      Width = 45
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'xyCurve'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 898
      Top = 31
      Width = 39
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'zCurve'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object zRadiusTrackBar: TTrackBar
      Left = 58
      Top = 57
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 100
      Min = 1
      PageSize = 1
      Position = 20
      TabOrder = 2
      TickStyle = tsNone
      OnChange = AnyChange
    end
    object yRadiusTrackBar: TTrackBar
      Left = 58
      Top = 29
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 100
      Min = 1
      PageSize = 1
      Position = 20
      TabOrder = 1
      TickStyle = tsNone
      OnChange = AnyChange
    end
    object xRadiusTrackBar: TTrackBar
      Left = 58
      Top = 1
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 100
      Min = 1
      PageSize = 1
      Position = 20
      TabOrder = 0
      TickStyle = tsNone
      OnChange = AnyChange
    end
    object GridCheckBox: TCheckBox
      Left = 202
      Top = 60
      Width = 71
      Height = 17
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Hide Grid'
      TabOrder = 5
      OnClick = GridCheckBoxClick
    end
    object ArrowsCheckBox: TCheckBox
      Left = 276
      Top = 60
      Width = 88
      Height = 17
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Hide Arrows'
      TabOrder = 6
      OnClick = ArrowsCheckBoxClick
    end
    object StacksTrackBar: TTrackBar
      Left = 236
      Top = 29
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 128
      Min = 2
      PageSize = 4
      Position = 16
      TabOrder = 4
      TickStyle = tsNone
      OnChange = AnyChange
    end
    object SlicesTrackBar: TTrackBar
      Left = 236
      Top = 3
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 128
      Min = 2
      PageSize = 4
      Position = 16
      TabOrder = 3
      TickStyle = tsNone
      OnChange = AnyChange
    end
    object TopCapRadioGroup: TRadioGroup
      Left = 377
      Top = 47
      Width = 165
      Height = 30
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Top Cap'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Center'
        'Flat')
      TabOrder = 8
      OnClick = AnyChange
    end
    object BottomTrackBar: TTrackBar
      Left = 584
      Top = 1
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 90
      PageSize = 5
      Position = 90
      TabOrder = 9
      TickStyle = tsNone
      OnChange = AnyChange
    end
    object TopTrackBar: TTrackBar
      Left = 402
      Top = 3
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 90
      PageSize = 5
      Position = 90
      TabOrder = 7
      TickStyle = tsNone
      OnChange = AnyChange
    end
    object BottomCapRadioGroup: TRadioGroup
      Left = 546
      Top = 47
      Width = 178
      Height = 30
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Bottom Cap'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Center'
        'Flat')
      TabOrder = 10
      OnClick = AnyChange
    end
    object Button1: TButton
      Left = 755
      Top = 56
      Width = 136
      Height = 25
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Cube Map'
      TabOrder = 13
      OnClick = Button1Click
    end
    object StopTrackBar: TTrackBar
      Left = 755
      Top = 30
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 360
      PageSize = 5
      Position = 360
      TabOrder = 12
      TickStyle = tsNone
      OnChange = StopTrackBarChange
    end
    object StartTrackBar: TTrackBar
      Left = 755
      Top = 3
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akBottom]
      Max = 360
      PageSize = 5
      TabOrder = 11
      TickStyle = tsNone
      OnChange = StartTrackBarChange
    end
    object xyCurveTrackBar: TTrackBar
      Left = 944
      Top = 1
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 100
      Min = 2
      PageSize = 5
      Position = 10
      TabOrder = 14
      TickStyle = tsNone
      OnChange = AnyChange
    end
    object zCurveTrackBar: TTrackBar
      Left = 944
      Top = 29
      Width = 140
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 100
      Min = 2
      PageSize = 5
      Position = 10
      TabOrder = 15
      TickStyle = tsNone
      OnChange = AnyChange
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1098
    Height = 718
    Cursor = crHandPoint
    Camera = GLCamera1
    FieldOfView = 71.356697082519530000
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 40
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 500.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000F0410000A0410000F0410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {0000000000000000000080BF00000000}
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {EC51B83ECDCC4C3EEC51B83D0000803F}
      object GLMesh1: TGLMesh
        Material.FrontProperties.Diffuse.Color = {B1A8A83EB1A8A83EB1A8A83E0000803F}
        Direction.Coordinates = {0000000000000000000080BF00000000}
        Up.Coordinates = {000000800000803F0000000000000000}
        Mode = mmTriangles
      end
    end
    object ObjectsCube: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {0000803F00000000000000000000803F}
      object ArrowZ: TGLArrowLine
        Material.FrontProperties.Emission.Color = {00000000000000000000803F0000803F}
        Direction.Coordinates = {00000000000000800000803F00000000}
        Position.Coordinates = {0000000000000000000000400000803F}
        BottomRadius = 0.050000000745058060
        Height = 4.000000000000000000
        TopRadius = 0.050000000745058060
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.100000001490116100
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object ArrowY: TGLArrowLine
        Material.FrontProperties.Emission.Color = {000000000000003F000000000000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000000000000040000000000000803F}
        Up.Coordinates = {00000000000000000000803F00000000}
        BottomRadius = 0.050000000745058060
        Height = 4.000000000000000000
        TopRadius = 0.050000000745058060
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.100000001490116100
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object ArrowX: TGLArrowLine
        Material.FrontProperties.Emission.Color = {0000803F00000000000000000000803F}
        Direction.Coordinates = {0000803F000000000000000000000000}
        Position.Coordinates = {0000004000000000000000000000803F}
        Up.Coordinates = {000000800000803F0000000000000000}
        BottomRadius = 0.050000000745058060
        Height = 4.000000000000000000
        TopRadius = 0.050000000745058060
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.100000001490116100
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLXYZGridXZ: TGLXYZGrid
        Direction.Coordinates = {00000000000000800000803F00000000}
        LineColor.Color = {B1A8A83EB1A8A83EB1A8A83E0000803F}
        XSamplingScale.Min = -10.000000000000000000
        XSamplingScale.Max = 10.000000000000000000
        XSamplingScale.Step = 1.000000000000000000
        YSamplingScale.Step = 1.000000000000000000
        ZSamplingScale.Min = -10.000000000000000000
        ZSamplingScale.Max = 10.000000000000000000
        ZSamplingScale.Step = 1.000000000000000000
        Parts = [gpX, gpZ]
      end
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000804000000000000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
      ModulateColor.Color = {EC51B83ECDCC4C3EEC51B83D0000803F}
    end
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    Ranges = <
      item
        StartASCII = ' '
        StopASCII = #176
        StartGlyphIdx = 0
      end>
    Left = 127
    Top = 40
  end
end

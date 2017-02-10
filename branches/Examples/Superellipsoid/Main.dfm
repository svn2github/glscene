object MainForm: TMainForm
  Left = 69
  Top = 77
  Caption = 'Super Ellipsoid'
  ClientHeight = 830
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
    830)
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1098
    Height = 811
    Cursor = crHandPoint
    Camera = Camera
    Buffer.BackgroundColor = clActiveCaption
    FieldOfView = 90.782424926757810000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 811
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
    Top = 718
    Width = 1098
    Height = 92
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
    DesignSize = (
      1098
      92)
    object Label1: TLabel
      Left = 13
      Top = 3
      Width = 44
      Height = 16
      Anchors = [akLeft, akBottom]
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
      Anchors = [akLeft, akBottom]
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
      Anchors = [akLeft, akBottom]
      Caption = 'zRadius'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 207
      Top = 3
      Width = 45
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'V Curve'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 207
      Top = 31
      Width = 45
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'H Curve'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 438
      Top = 3
      Width = 33
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Slices'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 434
      Top = 31
      Width = 37
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Stacks'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 658
      Top = 3
      Width = 22
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Top'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 640
      Top = 31
      Width = 40
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Bottom'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 846
      Top = 4
      Width = 28
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Start'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 849
      Top = 32
      Width = 26
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Stop'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object xRadiusTrackBar: TTrackBar
      Left = 58
      Top = 1
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 15
      Min = 1
      PageSize = 1
      Position = 10
      TabOrder = 0
      TickStyle = tsNone
      OnChange = RadiusTrackBarChange
    end
    object yRadiusTrackBar: TTrackBar
      Left = 58
      Top = 29
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 15
      Min = 1
      PageSize = 1
      Position = 10
      TabOrder = 1
      TickStyle = tsNone
      OnChange = RadiusTrackBarChange
    end
    object zRadiusTrackBar: TTrackBar
      Left = 58
      Top = 57
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 15
      Min = 1
      PageSize = 1
      Position = 10
      TabOrder = 2
      TickStyle = tsNone
      OnChange = RadiusTrackBarChange
    end
    object VCurveTrackBar: TTrackBar
      Left = 253
      Top = 1
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 100
      Min = 1
      Position = 10
      TabOrder = 3
      TickStyle = tsNone
      OnChange = VCurveTrackBarChange
    end
    object HCurveTrackBar: TTrackBar
      Left = 253
      Top = 29
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 100
      Min = 1
      Position = 10
      TabOrder = 4
      TickStyle = tsNone
      OnChange = HCurveTrackBarChange
    end
    object GridCheckBox: TCheckBox
      Left = 1031
      Top = 4
      Width = 65
      Height = 20
      Anchors = [akLeft, akBottom]
      Caption = 'Hide Grid'
      TabOrder = 5
      OnClick = GridCheckBoxClick
    end
    object ArrowsCheckBox: TCheckBox
      Left = 1031
      Top = 30
      Width = 81
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Hide Arrows'
      TabOrder = 6
      OnClick = ArrowsCheckBoxClick
    end
    object SlicesTrackBar: TTrackBar
      Left = 471
      Top = 3
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 128
      Min = 2
      PageSize = 4
      Position = 16
      TabOrder = 7
      TickStyle = tsNone
      OnChange = SlicesTrackBarChange
    end
    object StacksTrackBar: TTrackBar
      Left = 471
      Top = 29
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 128
      Min = 2
      PageSize = 4
      Position = 16
      TabOrder = 8
      TickStyle = tsNone
      OnChange = StacksTrackBarChange
    end
    object BottomTrackBar: TTrackBar
      Left = 679
      Top = 29
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 90
      PageSize = 5
      Position = 90
      TabOrder = 10
      TickStyle = tsNone
      OnChange = BottomTrackBarChange
    end
    object TopTrackBar: TTrackBar
      Left = 679
      Top = 3
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 90
      PageSize = 5
      Position = 90
      TabOrder = 9
      TickStyle = tsNone
      OnChange = TopTrackBarChange
    end
    object TopCapRadioGroup: TRadioGroup
      Left = 300
      Top = 53
      Width = 225
      Height = 30
      Caption = 'Top Cap'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Center'
        'Flat')
      TabOrder = 11
      OnClick = TopCapRadioGroupClick
    end
    object BottomCapRadioGroup: TRadioGroup
      Left = 547
      Top = 53
      Width = 225
      Height = 30
      Caption = 'Bottom Cap'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Center'
        'Flat')
      TabOrder = 12
      OnClick = BottomCapRadioGroupClick
    end
    object StartTrackBar: TTrackBar
      Left = 875
      Top = 4
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 360
      PageSize = 5
      TabOrder = 13
      TickStyle = tsNone
      OnChange = StartTrackBarChange
    end
    object StopTrackBar: TTrackBar
      Left = 875
      Top = 30
      Width = 150
      Height = 30
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Max = 360
      PageSize = 5
      Position = 360
      TabOrder = 14
      TickStyle = tsNone
      OnChange = StopTrackBarChange
    end
    object Button1: TButton
      Left = 846
      Top = 56
      Width = 81
      Height = 25
      Caption = 'Cube Map'
      TabOrder = 15
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 943
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Gems'
      TabOrder = 16
      OnClick = Button2Click
    end
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 82
    object GLLightSource: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000020410000A0410000C8420000803F}
      Specular.Color = {9A99593F9A99593FCDCCCC3D0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object CameraCube: TGLDummyCube
      ShowAxes = True
      Visible = False
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {CDCC0C3FD7A3F03E295C0F3E0000803F}
      object Camera: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 400.000000000000000000
        NearPlaneBias = 0.009999999776482582
        TargetObject = CameraCube
        Position.Coordinates = {00004842000048420000F0410000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          LightStyle = lsOmni
          SpotCutOff = 180.000000000000000000
        end
      end
      object GLSuperellipsoid: TGLSuperellipsoid
        Radius = 0.500000000000000000
        VCurve = 1.000000000000000000
        HCurve = 1.000000000000000000
      end
      object GLMesh: TGLMesh
        Mode = mmTriangles
      end
    end
    object ObjectsCube: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {0000803F00000000000000000000803F}
      object ArrowZ: TGLArrowLine
        Material.FrontProperties.Emission.Color = {00000000000000000000803F0000803F}
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
        BottomRadius = 0.050000000745058060
        Height = 4.000000000000000000
        TopRadius = 0.050000000745058060
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.100000001490116100
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
      object GLXYZGridXZ: TGLXYZGrid
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
    object GLHUDText: TGLHUDText
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
    Left = 168
    Top = 82
  end
  object GLCadencer1: TGLCadencer
    Left = 64
    Top = 136
  end
end

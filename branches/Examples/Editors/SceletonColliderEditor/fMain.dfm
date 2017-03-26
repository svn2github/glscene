object MainForm: TMainForm
  Left = 126
  Top = 134
  BorderStyle = bsSingle
  Caption = 'Skeleton Collider Editor'
  ClientHeight = 480
  ClientWidth = 880
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 240
    Height = 480
    Align = alLeft
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 224
      Width = 81
      Height = 13
      Caption = 'Choose a collider'
    end
    object Label2: TLabel
      Left = 8
      Top = 304
      Width = 57
      Height = 13
      Caption = 'Collider type'
    end
    object Label3: TLabel
      Left = 8
      Top = 328
      Width = 72
      Height = 13
      Caption = 'Radius / Width'
    end
    object ObjectName: TLabel
      Left = 8
      Top = 280
      Width = 26
      Height = 13
      Caption = 'None'
    end
    object Label4: TLabel
      Left = 8
      Top = 264
      Width = 78
      Height = 13
      Caption = 'Selected collider'
    end
    object Label5: TLabel
      Left = 8
      Top = 352
      Width = 31
      Height = 13
      Caption = 'Height'
    end
    object Label9: TLabel
      Left = 8
      Top = 376
      Width = 29
      Height = 13
      Caption = 'Depth'
    end
    object BonesCombo: TComboBox
      Left = 8
      Top = 240
      Width = 225
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = BonesComboChange
    end
    object ColliderTypeCombo: TComboBox
      Left = 88
      Top = 304
      Width = 145
      Height = 21
      Style = csDropDownList
      Enabled = False
      TabOrder = 1
      Items.Strings = (
        'Sphere'
        'Capsule'
        'Box')
    end
    object RadiusEdit: TEdit
      Left = 88
      Top = 328
      Width = 145
      Height = 21
      TabOrder = 2
    end
    object Button1: TButton
      Left = 8
      Top = 192
      Width = 105
      Height = 25
      Caption = 'Load colliders'
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 128
      Top = 192
      Width = 105
      Height = 25
      Caption = 'Save colliders'
      TabOrder = 4
      OnClick = Button2Click
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 225
      Height = 145
      Caption = 'Animation controls'
      TabOrder = 5
      object FrameLabel: TLabel
        Left = 8
        Top = 16
        Width = 44
        Height = 13
        Caption = 'Frame : 0'
      end
      object Label6: TLabel
        Left = 8
        Top = 40
        Width = 78
        Height = 13
        Caption = 'Animation speed'
      end
      object Label7: TLabel
        Left = 8
        Top = 80
        Width = 29
        Height = 13
        Caption = 'Faster'
      end
      object Label8: TLabel
        Left = 184
        Top = 80
        Width = 32
        Height = 13
        Caption = 'Slower'
      end
      object Button3: TButton
        Left = 56
        Top = 104
        Width = 121
        Height = 25
        Caption = 'Play animation'
        TabOrder = 0
        OnClick = Button3Click
      end
      object TrackBar1: TTrackBar
        Left = 8
        Top = 56
        Width = 209
        Height = 17
        Max = 190
        Min = 10
        Position = 100
        TabOrder = 1
        ThumbLength = 10
        TickStyle = tsManual
        OnChange = TrackBar1Change
      end
    end
    object HeightEdit: TEdit
      Left = 88
      Top = 352
      Width = 145
      Height = 21
      TabOrder = 6
    end
    object Button4: TButton
      Left = 8
      Top = 408
      Width = 105
      Height = 25
      Caption = 'Apply changes'
      TabOrder = 7
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 128
      Top = 408
      Width = 105
      Height = 25
      Caption = 'Reset collider'
      TabOrder = 8
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 8
      Top = 160
      Width = 105
      Height = 25
      Caption = 'Generate colliders'
      TabOrder = 9
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 128
      Top = 160
      Width = 105
      Height = 25
      Caption = 'Delete colliders'
      Enabled = False
      TabOrder = 10
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 128
      Top = 440
      Width = 105
      Height = 25
      Caption = 'Delete collider'
      TabOrder = 11
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 8
      Top = 440
      Width = 105
      Height = 25
      Caption = 'Add collider'
      Enabled = False
      TabOrder = 12
    end
    object DepthEdit: TEdit
      Left = 88
      Top = 376
      Width = 145
      Height = 21
      TabOrder = 13
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 240
    Top = 0
    Width = 640
    Height = 480
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    FieldOfView = 156.463424682617200000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 1
    ExplicitLeft = 336
    ExplicitTop = 8
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 272
    Top = 16
    object GLHeightField1: TGLHeightField
      XSamplingScale.Min = -5.000000000000000000
      XSamplingScale.Max = 5.000000000000000000
      XSamplingScale.Step = 1.000000000000000000
      YSamplingScale.Min = -5.000000000000000000
      YSamplingScale.Max = 5.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ColorMode = hfcmAmbient
    end
    object GLActor1: TGLActor
      Interval = 100
      OnFrameChanged = GLActor1FrameChanged
    end
    object CameraDummy: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 2000.000000000000000000
        FocalLength = 50.000000000000000000
        SceneScale = 1.500000000000000000
        TargetObject = CameraDummy
        Position.Coordinates = {000048420000A0C2000000000000803F}
        Direction.Coordinates = {000000000000803F0000008000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 360
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    Filter = 'SMD files|*.smd|All files|*.*'
    Left = 456
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    Filter = 'SMD files|*.smd|All files|*.*'
    Left = 552
    Top = 16
  end
  object MainMenu1: TMainMenu
    Left = 624
    Top = 16
    object File1: TMenuItem
      Caption = '&Actor'
      object LoadModel1: TMenuItem
        Caption = '&Load Mesh'
        OnClick = LoadModel1Click
      end
      object AddAnimation1: TMenuItem
        Caption = '&Add Animation'
        OnClick = AddAnimation1Click
      end
    end
    object Colliders1: TMenuItem
      Caption = '&Colliders'
      object Generate1: TMenuItem
        Caption = '&Generate'
        OnClick = Generate1Click
      end
      object DeleteAll1: TMenuItem
        Caption = 'Delete &All'
        Enabled = False
        OnClick = DeleteAll1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Load1: TMenuItem
        Caption = '&Load'
        OnClick = Load1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object ReadMe1: TMenuItem
        Caption = '&Read Me'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About'
        OnClick = About1Click
      end
    end
    object Exit1: TMenuItem
      Caption = 'E&xit'
      OnClick = Exit1Click
    end
  end
end

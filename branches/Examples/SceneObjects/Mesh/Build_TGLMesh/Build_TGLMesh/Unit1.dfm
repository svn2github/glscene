object Form1: TForm1
  Left = 247
  Top = 194
  Caption = 'Building Objects'
  ClientHeight = 593
  ClientWidth = 726
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
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 129
    Top = 0
    Width = 597
    Height = 593
    Camera = GLCamera1
    FieldOfView = 160.856079101562500000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 129
    Height = 593
    Align = alLeft
    TabOrder = 1
    object Label3: TLabel
      Left = 7
      Top = 84
      Width = 28
      Height = 13
      Caption = 'Slices'
    end
    object Label4: TLabel
      Left = 48
      Top = 84
      Width = 33
      Height = 13
      Caption = 'Radius'
    end
    object Label5: TLabel
      Left = 88
      Top = 84
      Width = 31
      Height = 13
      Caption = 'Height'
    end
    object Label1: TLabel
      Left = 26
      Top = 12
      Width = 21
      Height = 13
      Caption = #1052'/'#1057
    end
    object Label2: TLabel
      Left = 65
      Top = 12
      Width = 27
      Height = 13
      Caption = 'Steps'
    end
    object Button5: TButton
      Left = 24
      Top = 241
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 0
      OnClick = Button5Click
    end
    object Button8: TButton
      Left = 24
      Top = 196
      Width = 75
      Height = 25
      Caption = '|_'
      TabOrder = 1
      OnClick = Button8Click
    end
    object Button7: TButton
      Left = 24
      Top = 171
      Width = 75
      Height = 25
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = Button7Click
    end
    object Button4: TButton
      Left = 24
      Top = 148
      Width = 75
      Height = 25
      Caption = 'T'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button2: TButton
      Left = 24
      Top = 123
      Width = 75
      Height = 26
      Caption = '|'
      TabOrder = 4
      OnClick = Button2Click
    end
    object Edit3: TEdit
      Left = 88
      Top = 100
      Width = 33
      Height = 21
      TabOrder = 5
      Text = '6'
    end
    object Edit2: TEdit
      Left = 48
      Top = 100
      Width = 33
      Height = 21
      TabOrder = 6
      Text = '1'
    end
    object Edit1: TEdit
      Left = 8
      Top = 100
      Width = 33
      Height = 21
      TabOrder = 7
      Text = '8'
    end
    object Button6: TButton
      Left = 24
      Top = 52
      Width = 75
      Height = 24
      Caption = 'Paint'
      TabOrder = 8
      OnClick = Button6Click
    end
    object Edit5: TEdit
      Left = 64
      Top = 27
      Width = 33
      Height = 21
      TabOrder = 9
      Text = '20'
    end
    object Edit4: TEdit
      Left = 24
      Top = 27
      Width = 33
      Height = 21
      TabOrder = 10
      Text = '50'
    end
  end
  object GLScene1: TGLScene
    Left = 240
    Top = 56
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLMesh1: TGLMesh
        Mode = mmTriangles
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 2500.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {00000000000000400000A0400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object AsyncTimer1: TGLAsyncTimer
    Interval = 50
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpLower
    Left = 296
    Top = 80
  end
end

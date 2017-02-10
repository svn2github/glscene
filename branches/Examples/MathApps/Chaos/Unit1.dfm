object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 595
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 622
    Height = 448
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 154.834075927734400000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 448
    Width = 622
    Height = 147
    Align = alBottom
    TabOrder = 1
    object Label1: TLabel
      Left = 176
      Top = 14
      Width = 33
      Height = 13
      Caption = 'Points:'
    end
    object Label2: TLabel
      Left = 176
      Top = 65
      Width = 61
      Height = 13
      Caption = 'Color Points:'
    end
    object Label3: TLabel
      Left = 176
      Top = 41
      Width = 46
      Height = 13
      Caption = 'Iteration:'
    end
    object Button1: TButton
      Left = 8
      Top = 114
      Width = 57
      Height = 25
      Caption = 'Generate'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Edit2: TEdit
      Left = 8
      Top = 33
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '2.791139'
    end
    object Edit3: TEdit
      Left = 8
      Top = 60
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '1.85185185'
    end
    object Edit4: TEdit
      Left = 8
      Top = 87
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '1.5'
    end
    object Edit1: TEdit
      Left = 8
      Top = 6
      Width = 121
      Height = 21
      TabOrder = 4
      Text = '-0.9629629'
    end
    object Button2: TButton
      Left = 71
      Top = 114
      Width = 58
      Height = 25
      Caption = 'Random'
      TabOrder = 5
      OnClick = Button2Click
    end
    object Edit5: TEdit
      Left = 256
      Top = 6
      Width = 97
      Height = 21
      TabOrder = 6
      Text = '100000'
    end
    object Edit6: TEdit
      Left = 256
      Top = 65
      Width = 97
      Height = 21
      TabOrder = 7
      Text = '4'
    end
    object Edit7: TEdit
      Left = 256
      Top = 33
      Width = 97
      Height = 21
      TabOrder = 8
      Text = '100'
    end
    object CheckBox1: TCheckBox
      Left = 256
      Top = 104
      Width = 97
      Height = 17
      Caption = 'NoZWrite'
      TabOrder = 9
      OnClick = CheckBox1Click
    end
  end
  object GLScene1: TGLScene
    Left = 56
    Top = 48
    object GLDummyCube1: TGLDummyCube
      CubeSize = 10.000000000000000000
    end
    object GLDummyCube2: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLDummyCube4: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 180.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.200000002980232200
      TargetObject = GLDummyCube2
      Position.Coordinates = {0000204100002041000000000000803F}
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 56
    Top = 112
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 176
  end
end

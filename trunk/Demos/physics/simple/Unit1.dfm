object Form1: TForm1
  Left = 251
  Top = 117
  Width = 728
  Height = 452
  Caption = 'Simple ODE Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 209
    Top = 0
    Width = 511
    Height = 425
    Camera = GLCamera1
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 209
    Height = 425
    Align = alLeft
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 193
      Height = 16
      Alignment = taCenter
      Caption = 'Add ODE objects'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Button1: TButton
      Left = 48
      Top = 176
      Width = 115
      Height = 25
      Caption = 'Add composite'
      TabOrder = 0
      OnClick = Button1Click
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 216
      Width = 193
      Height = 81
      Caption = 'GLODEPlane1 Collision Surface'
      TabOrder = 1
      object CheckBoxBounce: TCheckBox
        Left = 8
        Top = 24
        Width = 65
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Bounce'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CheckBoxClick
      end
      object TrackBarBounce: TTrackBar
        Left = 72
        Top = 24
        Width = 110
        Height = 17
        Max = 100
        Position = 100
        TabOrder = 1
        ThumbLength = 10
        TickStyle = tsNone
        OnChange = TrackBarBounceChange
      end
      object CheckBoxSoftCFM: TCheckBox
        Left = 8
        Top = 48
        Width = 65
        Height = 17
        Alignment = taLeftJustify
        Caption = 'SoftCFM'
        TabOrder = 2
        OnClick = CheckBoxClick
      end
      object TrackBarSoftCFM: TTrackBar
        Left = 72
        Top = 48
        Width = 110
        Height = 17
        Max = 100
        Position = 50
        TabOrder = 3
        ThumbLength = 10
        TickStyle = tsNone
        OnChange = TrackBarSoftCFMChange
      end
    end
    object Button2: TButton
      Left = 48
      Top = 112
      Width = 113
      Height = 25
      Caption = 'Add capsule'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 48
      Top = 80
      Width = 113
      Height = 25
      Caption = 'Add sphere'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 48
      Top = 48
      Width = 113
      Height = 25
      Caption = 'Add box'
      TabOrder = 4
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 48
      Top = 144
      Width = 113
      Height = 25
      Caption = 'Add cylinder'
      TabOrder = 5
      OnClick = Button5Click
    end
  end
  object GLScene1: TGLScene
    Left = 216
    Top = 8
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F000040400000A0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          LightStyle = lsOmni
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object ODEObjects: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 248
    Top = 8
  end
  object GLODEManager1: TGLODEManager
    Left = 216
    Top = 40
  end
end

object Form1: TForm1
  Left = 192
  Top = 119
  Width = 672
  Height = 505
  Caption = 'Simple ODE Demo - BCB6'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 475
    Align = alLeft
    TabOrder = 0
    object Label1: TLabel
      Left = 40
      Top = 16
      Width = 120
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
      Left = 56
      Top = 56
      Width = 97
      Height = 25
      Caption = 'Add Box'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 56
      Top = 88
      Width = 97
      Height = 25
      Caption = 'Add Sphere'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 56
      Top = 120
      Width = 97
      Height = 25
      Caption = 'Add Capsule'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 56
      Top = 152
      Width = 97
      Height = 25
      Caption = 'Add Composite'
      TabOrder = 3
      OnClick = Button4Click
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 200
      Width = 185
      Height = 97
      Caption = 'GLODEPlane1 Collision Surface'
      TabOrder = 4
      object CheckBox1: TCheckBox
        Left = 8
        Top = 32
        Width = 65
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Bounce'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CheckBoxClick
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 64
        Width = 65
        Height = 17
        Alignment = taLeftJustify
        Caption = 'SoftCFM'
        TabOrder = 1
        OnClick = CheckBoxClick
      end
      object TrackBar1: TTrackBar
        Left = 80
        Top = 32
        Width = 94
        Height = 17
        Max = 100
        Orientation = trHorizontal
        Frequency = 1
        Position = 100
        SelEnd = 0
        SelStart = 0
        TabOrder = 2
        ThumbLength = 10
        TickMarks = tmBottomRight
        TickStyle = tsNone
        OnChange = TrackBar1Change
      end
      object TrackBar2: TTrackBar
        Left = 80
        Top = 64
        Width = 94
        Height = 9
        Max = 100
        Orientation = trHorizontal
        Frequency = 1
        Position = 50
        SelEnd = 0
        SelStart = 0
        TabOrder = 3
        ThumbLength = 10
        TickMarks = tmBottomRight
        TickStyle = tsNone
        OnChange = TrackBar2Change
      end
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 201
    Top = 0
    Width = 463
    Height = 475
    Camera = GLCamera1
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 256
    Top = 24
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F000040400000A0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1
          SpotCutOff = 180
        end
      end
    end
    object ODEObjects: TGLDummyCube
      CubeSize = 1
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 288
    Top = 24
  end
  object GLODEManager1: TGLODEManager
    Solver = osmDefault
    Iterations = 3
    Left = 256
    Top = 56
  end
end

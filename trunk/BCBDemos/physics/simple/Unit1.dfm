object Form1: TForm1
  Left = 192
  Top = 105
  Width = 694
  Height = 510
  Caption = 'Simple ODE Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 110
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 201
    Top = 0
    Width = 485
    Height = 473
    Camera = GLCamera1
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 156.125030517578
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 473
    Align = alLeft
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 79
      Width = 105
      Height = 16
      Caption = 'Choose an object'
    end
    object Label2: TLabel
      Left = 10
      Top = 256
      Width = 72
      Height = 48
      Caption = 'HeightField Contact Resolution'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 10
      Top = 10
      Width = 123
      Height = 16
      Caption = 'Choose surface type'
    end
    object Spawn: TButton
      Left = 49
      Top = 138
      Width = 93
      Height = 31
      Caption = 'Spawn'
      TabOrder = 0
      OnClick = SpawnClick
    end
    object ComboBox1: TComboBox
      Left = 10
      Top = 98
      Width = 168
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 1
      Items.Strings = (
        'Sphere'
        'Box'
        'Capsule (CCylinder)'
        'Cylinder'
        'Cone')
    end
    object CheckBox1: TCheckBox
      Left = 10
      Top = 177
      Width = 159
      Height = 21
      Caption = 'Show ODE Elements'
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 10
      Top = 207
      Width = 183
      Height = 40
      Caption = 'Show HeightField Contacts'
      TabOrder = 3
      OnClick = CheckBox2Click
    end
    object TrackBar1: TTrackBar
      Left = 10
      Top = 319
      Width = 168
      Height = 42
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 4
      ThumbLength = 10
      TickMarks = tmBoth
      TickStyle = tsAuto
      OnChange = TrackBar1Change
    end
    object ComboBox2: TComboBox
      Left = 10
      Top = 30
      Width = 168
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 5
      OnChange = ComboBox2Change
      Items.Strings = (
        'Plane'
        'HeightField')
    end
  end
  object GLScene1: TGLScene
    Left = 168
    Top = 8
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
          LightStyle = lsOmni
          SpotCutOff = 180
        end
      end
    end
    object GLHeightField1: TGLHeightField
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Visible = False
      XSamplingScale.Min = -10
      XSamplingScale.Max = 10
      XSamplingScale.Step = 0.5
      YSamplingScale.Min = -10
      YSamplingScale.Max = 10
      YSamplingScale.Step = 0.5
      Options = []
      OnGetHeight = GLHeightField1GetHeight
      BehavioursData = {
        0201061154474C4F44454865696768744669656C640200020006000200050000
        0000006F1283F53F0800000500000000000000FA084005000000000000000000
        0005000000000000000000000500000000000000000000050000000000000000
        0000050000000000000000000005000000000000000000000500000000000000
        0000000500000000000000000000050000000000000000000002000500000000
        00000080FF3F080500000000000000C000400000803F0200}
    end
    object GLPlane1: TGLPlane
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Height = 10
      Width = 10
      NoZWrite = False
      BehavioursData = {
        0201060C54474C4F444553746174696302000200060D474C4F44454D616E6167
        65723102000500000000006F1283F53F0800000500000000000000FA08400500
        0000000000000000000500000000000000000000050000000000000000000005
        0000000000000000000005000000000000000000000500000000000000000000
        0500000000000000000000050000000000000000000005000000000000000000
        00020002010610544F4445456C656D656E74506C616E650200}
    end
    object ODEObjects: TGLDummyCube
      CubeSize = 1
    end
    object GLRenderPoint1: TGLRenderPoint
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.02
    OnProgress = GLCadencer1Progress
    Left = 200
    Top = 8
  end
  object GLODEManager1: TGLODEManager
    Gravity.Coordinates = {00000000C3F51CC1000000000000803F}
    Solver = osmQuickStep
    Iterations = 3
    MaxContacts = 8
    RenderPoint = GLRenderPoint1
    Visible = False
    VisibleAtRunTime = True
    Left = 168
    Top = 40
  end
end

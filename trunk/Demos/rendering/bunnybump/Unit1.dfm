object Form1: TForm1
  Left = 192
  Top = 107
  Width = 400
  Height = 400
  Caption = 'GLBumpShader Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 57
    Width = 392
    Height = 309
    Camera = Camera
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 392
    Height = 57
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 70
      Height = 13
      Caption = 'Shade Method'
    end
    object ComboBox1: TComboBox
      Left = 8
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 0
      Text = 'Basic Fragment Program'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Per-Vertex'
        'Dot3 Texture Combiner'
        'Basic Fragment Program')
    end
    object GroupBox1: TGroupBox
      Left = 160
      Top = 8
      Width = 169
      Height = 41
      Caption = 'Lights'
      TabOrder = 1
      object Shape1: TShape
        Left = 32
        Top = 16
        Width = 17
        Height = 17
        OnMouseDown = ShapeMouseDown
      end
      object Shape2: TShape
        Left = 88
        Top = 16
        Width = 17
        Height = 17
        Brush.Color = clRed
        OnMouseDown = ShapeMouseDown
      end
      object Shape3: TShape
        Left = 144
        Top = 16
        Width = 17
        Height = 17
        Brush.Color = clBlue
        OnMouseDown = ShapeMouseDown
      end
      object CheckBox1: TCheckBox
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CheckBoxClick
      end
      object CheckBox2: TCheckBox
        Left = 64
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 1
        OnClick = CheckBoxClick
      end
      object CheckBox3: TCheckBox
        Left = 120
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 2
        OnClick = CheckBoxClick
      end
    end
    object CheckBox4: TCheckBox
      Left = 336
      Top = 24
      Width = 49
      Height = 17
      Caption = 'Spin'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 64
    object DCLights: TGLDummyCube
      CubeSize = 1.000000000000000000
      object WhiteLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000404000000040000000000000803F}
        LightStyle = lsOmni
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object RedLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {0000803F00000000000000000000803F}
        Position.Coordinates = {0000C0BF00000040666626C00000803F}
        LightStyle = lsOmni
        Shining = False
        Specular.Color = {0000803F0000003F0000003F0000803F}
        SpotCutOff = 180.000000000000000000
      end
      object BlueLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {00000000000000000000803F0000803F}
        Position.Coordinates = {0000C0BF00000040666626400000803F}
        LightStyle = lsOmni
        Shining = False
        Specular.Color = {0000003F0000003F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object Bunny: TGLFreeForm
      Material.FrontProperties.Shininess = 64
      Material.FrontProperties.Specular.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      Material.MaterialLibrary = GLMaterialLibrary1
      AutoCentering = [macCenterX, macCenterY, macCenterZ]
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Bunny
      Position.Coordinates = {000080400000803F000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 96
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Bump'
        Material.FrontProperties.Shininess = 64
        Material.FrontProperties.Specular.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Material.Texture.Disabled = False
        Tag = 0
        Shader = GLBumpShader1
      end>
    Left = 40
    Top = 64
  end
  object GLBumpShader1: TGLBumpShader
    BumpMethod = bmDot3TexCombiner
    BumpSpace = bsObject
    BumpOptions = []
    DesignTimeEnabled = False
    Left = 40
    Top = 96
  end
  object ColorDialog1: TColorDialog
    Left = 216
    Top = 16
  end
  object AsyncTimer1: TAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 8
    Top = 128
  end
end

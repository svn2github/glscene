object Form1: TForm1
  Left = 198
  Top = 140
  BorderWidth = 5
  Caption = 'Form1'
  ClientHeight = 324
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 324
    Height = 324
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.Lighting = False
    Buffer.AntiAliasing = aa4x
    FieldOfView = 162.454528808593800000
    Align = alLeft
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 324
    Top = 0
    Width = 146
    Height = 324
    Align = alClient
    TabOrder = 1
    object Label1: TLabel
      Left = 6
      Top = 8
      Width = 43
      Height = 13
      Caption = 'Line type'
    end
    object Label2: TLabel
      Left = 6
      Top = 128
      Width = 67
      Height = 13
      Caption = 'Spline division'
    end
    object CheckBox1: TCheckBox
      Left = 6
      Top = 64
      Width = 81
      Height = 17
      Caption = 'Textured'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object ComboBox1: TComboBox
      Left = 6
      Top = 27
      Width = 107
      Height = 21
      ItemIndex = 3
      TabOrder = 1
      Text = 'Bezier spline'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Lines'
        'Segments'
        'Lines loop'
        'Bezier spline'
        'Cubic spline'
        'Nurbs curve')
    end
    object CheckBox2: TCheckBox
      Left = 6
      Top = 96
      Width = 139
      Height = 17
      Caption = 'Use node color for lines'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox2Click
    end
    object UpDown1: TUpDown
      Left = 48
      Top = 150
      Width = 13
      Height = 19
      Min = 3
      Position = 10
      TabOrder = 3
      OnChanging = UpDown1Changing
    end
    object Edit1: TEdit
      Left = 6
      Top = 150
      Width = 43
      Height = 21
      ReadOnly = True
      TabOrder = 4
      Text = '10'
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object Lines1: TGLLines
      AntiAliased = True
      LineColor.Color = {6666663FC3F5683F48E17A3F0000803F}
      LineWidth = 5.000000000000000000
      LibMaterialName = 'LibMaterial1'
      Nodes = <
        item
          X = -1.500000000000000000
          Y = -1.500000000000000000
        end
        item
          X = -0.750000000000000000
          Y = 0.250000000000000000
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 0.750000000000000000
          Y = -0.250000000000000000
          Color.Color = {0000803F00000000000000000000803F}
        end
        item
          X = 1.500000000000000000
          Y = 1.500000000000000000
          Color.Color = {ACC8483ECDCC4C3FACC8483E0000803F}
        end>
      NodesAspect = lnaCube
      NodeSize = 0.500000000000000000
      Division = 20
      SplineMode = lsmBezierSpline
      Options = [loUseNodeColorForLines]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 25.000000000000000000
      TargetObject = Lines1
      CameraStyle = csOrthogonal
      Position.Coordinates = {00000000000000000000A0400000803F}
      Left = 248
      Top = 152
    end
  end
  object GLMaterialLibraryEx1: TGLMaterialLibraryEx
    Materials = <
      item
        Name = 'LineMaterial'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.DepthProperties.DepthWrite = False
        FixedFunction.BlendingMode = bmAlphaTest100
        FixedFunction.LineProperties.Enabled = False
        FixedFunction.Texture.Enabled = False
        FixedFunction.Texture.LibTextureName = 'TexturePattern'
        FixedFunction.Texture.LibSamplerName = 'Sampler'
        FixedFunction.Texture.TextureScale.Coordinates = {0000A0400000803F0000803F0000803F}
        Multitexturing.Enabled = False
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
        ShaderModel3.Enabled = False
        ShaderModel4.Enabled = False
        ShaderModel5.Enabled = False
      end>
    ComponentsData = {
      0458434F4C02010202061154474C5465787475726553616D706C657202000607
      53616D706C657208020002000200020002000200020000000000000000000000
      0000000000000200020309061154474C54657874757265496D61676545780201
      060E546578747572655061747465726E08021F02000202050000000000000080
      FF3F050000000000000080FF3F050000000000000080FF3F0500000000000000
      80FF3F060F4C696E655061747465726E2E626D700802020808}
    Left = 16
    Top = 72
  end
end

object Form1: TForm1
  Left = 192
  Top = 119
  Width = 769
  Height = 441
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 110
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 415
    Top = 0
    Width = 3
    Height = 404
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 415
    Height = 404
    Align = alLeft
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 413
      Height = 402
      ActivePage = TabSheet2
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Vertex Program'
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 402
          Height = 41
          Align = alTop
          TabOrder = 0
          object VPName: TLabel
            Left = 176
            Top = 10
            Width = 55
            Height = 16
            Caption = 'VPName'
          end
          object CBVP: TCheckBox
            Left = 20
            Top = 10
            Width = 119
            Height = 21
            Caption = 'Enabled'
            TabOrder = 0
            OnClick = CBVPClick
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 41
          Width = 402
          Height = 29
          Align = alTop
          Caption = 'Vertex Shader Code'
          TabOrder = 1
        end
        object Panel6: TPanel
          Left = 0
          Top = 345
          Width = 402
          Height = 124
          Align = alBottom
          TabOrder = 2
          object Label1: TLabel
            Left = 325
            Top = 20
            Width = 33
            Height = 16
            Caption = 'Show'
          end
          object Memo1: TMemo
            Left = 1
            Top = 1
            Width = 315
            Height = 122
            Align = alLeft
            ScrollBars = ssBoth
            TabOrder = 0
          end
          object Button2: TButton
            Left = 325
            Top = 39
            Width = 73
            Height = 31
            Caption = 'param'
            TabOrder = 1
            OnClick = Button2Click
          end
          object Button3: TButton
            Left = 325
            Top = 79
            Width = 73
            Height = 31
            Caption = 'asm'
            TabOrder = 2
            OnClick = Button3Click
          end
        end
        object Panel7: TPanel
          Left = 0
          Top = 305
          Width = 402
          Height = 40
          Align = alBottom
          TabOrder = 3
          object ApplyVP: TButton
            Left = 266
            Top = 5
            Width = 92
            Height = 31
            Caption = 'Apply'
            TabOrder = 0
            OnClick = ApplyVPClick
          end
        end
        object VertexCode: TMemo
          Left = 0
          Top = 70
          Width = 402
          Height = 235
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 4
          WantTabs = True
          OnChange = VertexCodeChange
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Fragment Program'
        ImageIndex = 1
        object Panel8: TPanel
          Left = 0
          Top = 0
          Width = 405
          Height = 41
          Align = alTop
          TabOrder = 0
          object FPName: TLabel
            Left = 176
            Top = 10
            Width = 54
            Height = 16
            Caption = 'FPName'
          end
          object CBFP: TCheckBox
            Left = 20
            Top = 10
            Width = 119
            Height = 21
            Caption = 'Enabled'
            TabOrder = 0
            OnClick = CBFPClick
          end
        end
        object Panel9: TPanel
          Left = 0
          Top = 41
          Width = 405
          Height = 29
          Align = alTop
          Caption = 'Fragment Shader Code'
          TabOrder = 1
        end
        object Panel10: TPanel
          Left = 0
          Top = 247
          Width = 405
          Height = 124
          Align = alBottom
          TabOrder = 2
          object Label3: TLabel
            Left = 325
            Top = 20
            Width = 33
            Height = 16
            Caption = 'Show'
          end
          object Memo2: TMemo
            Left = 1
            Top = 1
            Width = 315
            Height = 122
            Align = alLeft
            ScrollBars = ssBoth
            TabOrder = 0
          end
          object Button4: TButton
            Left = 325
            Top = 39
            Width = 73
            Height = 31
            Caption = 'param'
            TabOrder = 1
            OnClick = Button4Click
          end
          object Button5: TButton
            Left = 325
            Top = 79
            Width = 73
            Height = 31
            Caption = 'asm'
            TabOrder = 2
            OnClick = Button5Click
          end
        end
        object Panel11: TPanel
          Left = 0
          Top = 208
          Width = 405
          Height = 39
          Align = alBottom
          TabOrder = 3
          object ApplyFP: TButton
            Left = 266
            Top = 5
            Width = 92
            Height = 31
            Caption = 'Apply'
            TabOrder = 0
            OnClick = ApplyFPClick
          end
        end
        object FragmentCode: TMemo
          Left = 0
          Top = 70
          Width = 405
          Height = 138
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 4
          WantTabs = True
          OnChange = FragmentCodeChange
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 418
    Top = 0
    Width = 343
    Height = 404
    Align = alClient
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 341
      Height = 51
      Align = alTop
      Caption = 'Cg Simple'
      TabOrder = 0
    end
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 52
      Width = 341
      Height = 351
      Camera = GLCamera1
      FieldOfView = 147.311904907227
      Align = alClient
      OnMouseDown = GLSceneViewer1MouseDown
      OnMouseMove = GLSceneViewer1MouseMove
    end
  end
  object GLScene1: TGLScene
    Left = 368
    Top = 64
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1
      Position.Coordinates = {0000000000002041000000000000803F}
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180
    end
    object GLFreeForm1: TGLFreeForm
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {8FC2F53C8FC2F53C8FC2F53C00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      AutoCentering = [macCenterX, macCenterY]
      Rendered = True
    end
    object GLXYZGrid1: TGLXYZGrid
      XSamplingScale.Min = -2
      XSamplingScale.Max = 2
      XSamplingScale.Step = 0.100000001490116
      YSamplingScale.Step = 0.100000001490116
      ZSamplingScale.Min = -2
      ZSamplingScale.Max = 2
      ZSamplingScale.Step = 0.100000001490116
      Parts = [gpX, gpZ]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = GLFreeForm1
      Position.Coordinates = {0000004000004040000080400000803F}
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 432
    Top = 64
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Material.BackProperties.Shininess = 128
        Material.FrontProperties.Ambient.Color = {8988083E00000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {DBDADA3ED5D4543EA1A0A03D0000803F}
        Material.FrontProperties.Shininess = 127
        Material.FrontProperties.Specular.Color = {EDEC6C3EDDDC5C3ED5D4543E0000803F}
        Material.BlendingMode = bmTransparency
        Tag = 0
        Shader = CgShader1
      end>
    Left = 368
    Top = 96
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 400
    Top = 64
  end
  object CgShader1: TCgShader
    VertexProgram.OnApply = CgShader1ApplyVP
    OnApplyVP = CgShader1ApplyVP
    OnInitialize = CgShader1Initialize
    Left = 400
    Top = 96
  end
end

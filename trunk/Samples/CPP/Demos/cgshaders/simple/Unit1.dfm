object Form1: TForm1
  Left = 192
  Top = 119
  Caption = 'Cg Simple'
  ClientHeight = 397
  ClientWidth = 761
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 337
    Top = 0
    Height = 397
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ExplicitHeight = 328
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 337
    Height = 397
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alLeft
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 335
      Height = 395
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      ActivePage = TabSheet2
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Vertex Program'
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 327
          Height = 33
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          TabOrder = 0
          object VPName: TLabel
            Left = 143
            Top = 8
            Width = 42
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'VPName'
          end
          object CBVP: TCheckBox
            Left = 16
            Top = 8
            Width = 97
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Enabled'
            TabOrder = 0
            OnClick = CBVPClick
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 33
          Width = 327
          Height = 24
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Vertex Shader Code'
          TabOrder = 1
        end
        object Panel6: TPanel
          Left = 0
          Top = 266
          Width = 327
          Height = 101
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alBottom
          TabOrder = 2
          object Label1: TLabel
            Left = 264
            Top = 16
            Width = 27
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Show'
          end
          object Memo1: TMemo
            Left = 1
            Top = 1
            Width = 256
            Height = 99
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alLeft
            ScrollBars = ssBoth
            TabOrder = 0
          end
          object Button2: TButton
            Left = 264
            Top = 32
            Width = 59
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'param'
            TabOrder = 1
            OnClick = Button2Click
          end
          object Button3: TButton
            Left = 264
            Top = 64
            Width = 59
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'asm'
            TabOrder = 2
            OnClick = Button3Click
          end
        end
        object Panel7: TPanel
          Left = 0
          Top = 234
          Width = 327
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alBottom
          TabOrder = 3
          object ApplyVP: TButton
            Left = 216
            Top = 4
            Width = 75
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Apply'
            TabOrder = 0
            OnClick = ApplyVPClick
          end
        end
        object VertexCode: TMemo
          Left = 0
          Top = 57
          Width = 327
          Height = 177
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 4
          WantTabs = True
          OnChange = VertexCodeChange
        end
      end
      object TabSheet2: TTabSheet
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Fragment Program'
        ImageIndex = 1
        object Panel8: TPanel
          Left = 0
          Top = 0
          Width = 327
          Height = 33
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          TabOrder = 0
          object FPName: TLabel
            Left = 143
            Top = 8
            Width = 41
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'FPName'
          end
          object CBFP: TCheckBox
            Left = 16
            Top = 8
            Width = 97
            Height = 17
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Enabled'
            TabOrder = 0
            OnClick = CBFPClick
          end
        end
        object Panel9: TPanel
          Left = 0
          Top = 33
          Width = 327
          Height = 24
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Fragment Shader Code'
          TabOrder = 1
        end
        object Panel10: TPanel
          Left = 0
          Top = 267
          Width = 327
          Height = 100
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alBottom
          TabOrder = 2
          object Label3: TLabel
            Left = 264
            Top = 16
            Width = 27
            Height = 13
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Show'
          end
          object Memo2: TMemo
            Left = 1
            Top = 1
            Width = 256
            Height = 98
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alLeft
            ScrollBars = ssBoth
            TabOrder = 0
          end
          object Button4: TButton
            Left = 264
            Top = 32
            Width = 59
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'param'
            TabOrder = 1
            OnClick = Button4Click
          end
          object Button5: TButton
            Left = 264
            Top = 64
            Width = 59
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'asm'
            TabOrder = 2
            OnClick = Button5Click
          end
        end
        object Panel11: TPanel
          Left = 0
          Top = 235
          Width = 327
          Height = 32
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alBottom
          TabOrder = 3
          object ApplyFP: TButton
            Left = 216
            Top = 4
            Width = 75
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Caption = 'Apply'
            TabOrder = 0
            OnClick = ApplyFPClick
          end
        end
        object FragmentCode: TMemo
          Left = 0
          Top = 57
          Width = 327
          Height = 178
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
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
    Left = 340
    Top = 0
    Width = 421
    Height = 397
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    TabOrder = 1
    object PanelFPS: TPanel
      Left = 1
      Top = 1
      Width = 419
      Height = 41
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Caption = 'FPS'
      TabOrder = 0
    end
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 42
      Width = 419
      Height = 354
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Camera = GLCamera1
      FieldOfView = 148.451522827148400000
      Align = alClient
      OnMouseDown = GLSceneViewer1MouseDown
      OnMouseMove = GLSceneViewer1MouseMove
      TabOrder = 1
    end
  end
  object GLScene1: TGLScene
    OnProgress = GLCadencer1Progress
    Left = 392
    Top = 64
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLFreeForm1
      Position.Coordinates = {0000004000004040000080400000803F}
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000002041000000000000803F}
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLFreeForm1: TGLFreeForm
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {8FC2F53C8FC2F53C8FC2F53C00000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      OnProgress = GLCadencer1Progress
      AutoCentering = [macCenterX, macCenterY]
      MaterialLibrary = GLMaterialLibrary1
    end
    object GLXYZGrid1: TGLXYZGrid
      OnProgress = GLCadencer1Progress
      XSamplingScale.Min = -2.000000000000000000
      XSamplingScale.max = 2.000000000000000000
      XSamplingScale.step = 0.100000001490116100
      YSamplingScale.step = 0.100000001490116100
      ZSamplingScale.Min = -2.000000000000000000
      ZSamplingScale.max = 2.000000000000000000
      ZSamplingScale.step = 0.100000001490116100
      Parts = [gpX, gpZ]
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 544
    Top = 72
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.BackProperties.Shininess = 128
        Material.FrontProperties.Ambient.Color = {8988083E00000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {DBDADA3ED5D4543EA1A0A03D0000803F}
        Material.FrontProperties.Shininess = 127
        Material.FrontProperties.Specular.Color = {EDEC6C3EDDDC5C3ED5D4543E0000803F}
        Material.BlendingMode = bmTransparency
      end>
    Left = 392
    Top = 128
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 464
    Top = 64
  end
  object CgShader1: TCgShader
    Left = 472
    Top = 128
  end
end

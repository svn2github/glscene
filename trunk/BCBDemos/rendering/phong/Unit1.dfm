object Form1: TForm1
  Left = 264
  Top = 142
  Width = 487
  Height = 447
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 110
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 479
    Height = 410
    Camera = GLCamera1
    FieldOfView = 152.586074829102
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CheckBox1: TCheckBox
    Left = 378
    Top = 10
    Width = 80
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Shader'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F00000040000040400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1
          Specular.Color = {0000803F0000803F0000803F0000803F}
          SpotCutOff = 180
        end
      end
    end
    object GLTeapot1: TGLTeapot
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'phong'
      Scale.Coordinates = {00004040000040400000404000000000}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'phong'
        Material.FrontProperties.Shininess = 16
        Material.FrontProperties.Specular.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Tag = 0
        Shader = GLPhongShader1
      end>
    Left = 40
    Top = 8
  end
  object GLPhongShader1: TGLPhongShader
    Enabled = False
    DesignTimeEnabled = False
    Left = 72
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object AsyncTimer1: TAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpHigher
    Left = 40
    Top = 40
  end
end

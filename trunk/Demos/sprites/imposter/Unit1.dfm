object Form1: TForm1
  Left = 154
  Top = 111
  Width = 321
  Height = 346
  Caption = 'Form1'
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
  object Label1: TLabel
    Left = 16
    Top = 272
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 256
    Height = 256
    Camera = GLCamera1
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha]
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 24
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
    end
    object GLSprite1: TGLSprite
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.TextureMode = tmReplace
      Material.Texture.TextureWrap = twNone
      Material.Texture.Disabled = False
      Visible = False
      Width = 5
      Height = 5
      NoZWrite = False
      MirrorU = False
      MirrorV = False
    end
    object GLTeapot1: TGLTeapot
      Visible = False
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {00004842000034420000F0410000803F}
      SpotCutOff = 180
    end
    object GLDodecahedron1: TGLDodecahedron
      Visible = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = GLSprite1
      Position.Coordinates = {000040400000A0400000E0400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 72
    Top = 24
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 64
  end
end

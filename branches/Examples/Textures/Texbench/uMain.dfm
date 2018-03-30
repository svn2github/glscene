object Form1: TForm1
  Left = 192
  Top = 124
  BorderStyle = bsToolWindow
  Caption = 'Form1'
  ClientHeight = 488
  ClientWidth = 732
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
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 732
    Height = 488
    Camera = cam
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    FieldOfView = 156.838760375976600000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object cam: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
    object dogl: TGLDirectOpenGL
      UseBuildList = False
      OnRender = doglRender
      Blend = False
      object hud: TGLHUDSprite
        Material.BlendingMode = bmTransparency
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLCompositeImage'
        Material.Texture.Image.Width = 256
        Material.Texture.Image.Height = 256
        Material.Texture.Image.Depth = 0
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Position.Coordinates = {0000004400000044000000000000803F}
        Visible = False
        Width = 1024.000000000000000000
        Height = 1024.000000000000000000
        Rotation = 0.000000000000000000
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
  object at: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = atTimer
    ThreadPriority = tpNormal
    Left = 8
    Top = 40
  end
end

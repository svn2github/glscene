object Form1: TForm1
  Left = 193
  Top = 108
  Caption = 'Actor and sword collider'
  ClientHeight = 452
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 74
    Width = 556
    Height = 378
    Camera = GLCamera1
    Buffer.AmbientColor.Color = {CDCCCC3ECDCCCC3ECDCCCC3E0000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow, roTwoSideLighting]
    FieldOfView = 150.363708496093800000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 556
    Height = 33
    Align = alTop
    Caption = 
      'ASWDZX -- move sword    IKJL -- rotate sword    Left button -- r' +
      'otate camera'
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 33
    Width = 556
    Height = 41
    Align = alTop
    Caption = 'Panel2'
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 40
    object GLLightSource1: TGLLightSource
      Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000048420000C842000048C20000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Actor1
      Position.Coordinates = {00008C420000F041000020C20000803F}
      Direction.Coordinates = {00000000000000800000803F00000000}
    end
    object Actor1: TGLActor
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000800000803F00000000}
      Reference = aarSkeleton
      AnimationMode = aamLoop
      Interval = 75
      object colliders_cube: TGLDummyCube
        CubeSize = 1.000000000000000000
      end
    end
    object sword_cube: TGLDummyCube
      Position.Coordinates = {00000000000000000000A0C10000803F}
      CubeSize = 1.000000000000000000
      object sword: TGLFreeForm
        Material.BackProperties.Diffuse.Color = {C9C8483EC9C8483EC9C8483E0000803F}
        Material.FrontProperties.Diffuse.Color = {C9C8483EC9C8483EC9C8483E0000803F}
        Material.FaceCulling = fcNoCull
      end
    end
    object GLPlane1: TGLPlane
      Material.FrontProperties.Ambient.Color = {0000003F0000003F0000003F0000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Material.Texture.TextureMode = tmModulate
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {0000000000000CC2000000000000803F}
      Up.Coordinates = {00000000000000000000803F00000000}
      Height = 500.000000000000000000
      Width = 500.000000000000000000
      XTiles = 4
      YTiles = 4
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 48
    Top = 40
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 88
    Top = 40
  end
end

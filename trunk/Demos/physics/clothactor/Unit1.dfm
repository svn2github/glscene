object Form1: TForm1
  Left = 69
  Top = 116
  Width = 634
  Height = 476
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    618
    438)
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 24
    Width = 626
    Height = 423
    Camera = GLCamera1
    Buffer.AmbientColor.Color = {CDCCCC3ECDCCCC3ECDCCCC3E0000803F}
    Buffer.ContextOptions = [roHardwareAcceleration, roDoubleBuffer, roStencilBuffer, roRenderToWindow, roTwoSideLighting]
    FieldOfView = 153.398193359375000000
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object CheckBox_ShowOctree: TCheckBox
    Left = 0
    Top = 0
    Width = 97
    Height = 17
    Caption = 'Show Octree'
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000048420000C842000048C20000803F}
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLShadowVolume1: TGLShadowVolume
      Lights = <
        item
          CastingMode = scmAlways
          LightSource = GLLightSource1
        end>
      Occluders = <
        item
          Caster = GLActor1
        end
        item
          Caster = Cape
        end>
      Options = [svoScissorClips]
      object ActorDummy: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLActor1: TGLActor
          Direction.Coordinates = {000000000000803F0000000000000000}
          Up.Coordinates = {00000000000000000000803F00000000}
          Reference = aarSkeleton
          AnimationMode = aamLoop
          Interval = 75
          MaterialLibrary = GLMaterialLibrary1
        end
      end
      object Cape: TGLActor
        Material.BackProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.BackProperties.Shininess = 48
        Material.BackProperties.Specular.Color = {0000003F0000003F0000003F0000803F}
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Shininess = 48
        Material.FrontProperties.Specular.Color = {0000003F0000003F0000003F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.FaceCulling = fcNoCull
        Interval = 100
      end
      object GLPlane1: TGLPlane
        Material.FrontProperties.Ambient.Color = {0000003F0000003F0000003F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        Height = 500.000000000000000000
        Width = 500.000000000000000000
        XTiles = 4
        YTiles = 4
      end
    end
    object OctreeRenderer: TGLDirectOpenGL
      UseBuildList = False
      OnRender = OctreeRendererRender
      Blend = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLActor1
      Position.Coordinates = {00004842000048420000A0C20000803F}
      Direction.Coordinates = {00000000000000800000803F00000000}
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {9A99193F9A99193F9A99193F0000803F}
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 40
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 96
    Top = 56
  end
end

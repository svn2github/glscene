object Form1: TForm1
  Left = 220
  Top = 314
  BorderStyle = bsSingle
  Caption = 'MeshMorph'
  ClientHeight = 604
  ClientWidth = 854
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 854
    Height = 604
    Camera = cam
    Buffer.BackgroundColor = clWhite
    Buffer.AmbientColor.Color = {0000003F0000003F0000003F0000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roTwoSideLighting]
    Buffer.FaceCulling = False
    FieldOfView = 161.198440551757800000
    Align = alClient
    OnMouseDown = vpMouseDown
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object cam: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLCube1
      Position.Coordinates = {0000E0C00000A0400000A0C00000803F}
      object light: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object dogl: TGLDirectOpenGL
      UseBuildList = False
      OnRender = doglRender
      Blend = False
      object mesh: TGLMesh
        Material.BackProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Ambient.Color = {0000803E0000803E0000803E0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Shininess = 17
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.TextureEx = <
          item
            Texture.ImageClassName = 'TGLPicFileImage'
            Texture.Image.PictureFileName = 'texture.bmp'
            Texture.TextureMode = tmModulate
            Texture.FilteringQuality = tfAnisotropic
            Texture.Disabled = False
            TextureIndex = 0
            TextureScale.Coordinates = {0000C0400000C0400000803F00000000}
          end>
        Position.Coordinates = {0000A0C000000000000000000000803F}
        Up.Coordinates = {000000A8FFFF7F3F0000000000000000}
        Visible = False
        Pickable = False
        Mode = mmTriangleStrip
        VertexMode = vmVNT
      end
    end
    object dc_world: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLPlane1: TGLPlane
        Material.FrontProperties.Diffuse.Color = {00000000F3F2F23E0000803F0000003F}
        Material.BlendingMode = bmTransparency
        Material.MaterialOptions = [moNoLighting]
        Direction.Coordinates = {000000000000803F7AAC6AB400000000}
        PitchAngle = 90.000000000000000000
        Position.Coordinates = {000000000000A0C0000000000000803F}
        Up.Coordinates = {0000802779AC6AB4FFFF7FBF00000000}
        Height = 100.000000000000000000
        Width = 100.000000000000000000
      end
      object GLCube1: TGLCube
        Material.FrontProperties.Diffuse.Color = {A9A5253FB1A8283EB1A8283E0000803F}
        Material.FrontProperties.Emission.Color = {EC51B83ECDCC4C3EEC51B83D0000803F}
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = cadProgress
    Left = 72
    Top = 8
  end
  object at: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = atTimer
    Left = 40
    Top = 8
  end
end

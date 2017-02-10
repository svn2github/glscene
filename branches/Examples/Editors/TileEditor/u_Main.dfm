object Form1: TForm1
  Left = 193
  Top = 132
  ClientHeight = 424
  ClientWidth = 659
  Color = 1776411
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 64
    Top = 88
    Width = 513
    Height = 289
    Camera = cam
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roNoColorBufferClear]
    FieldOfView = 179.008743286132800000
    Align = alCustom
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc_cam: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {0000803F000000000000803F0000803F}
      VisibleAtRunTime = True
      object cam: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 1.250000000000000000
        TargetObject = dc_cam
        CameraStyle = csOrthogonal
        Position.Coordinates = {00000000000000000000A0400000803F}
      end
      object cam_fbo: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 1.000000000000000000
        TargetObject = dc_cam
        CameraStyle = csOrthogonal
        Position.Coordinates = {00000000000000000000A0400000803F}
      end
    end
    object GLCube1: TGLCube
      Visible = False
    end
    object dc_world: TGLDummyCube
      CubeSize = 1.000000000000000000
      object back: TGLPlane
        Material.MaterialLibrary = matlib
        Material.LibMaterialName = 'back'
        Height = 50.000000000000000000
        Width = 100.000000000000000000
        Style = [psSingleQuad]
      end
    end
    object dogl: TGLDirectOpenGL
      UseBuildList = False
      OnRender = doglRender
      Blend = False
      object fbo: TGLFBORenderer
        Active = False
        Width = 1
        Height = 1
        ColorTextureName = 'back'
        MaterialLibrary = matlib
        ClearOptions = [coColorBufferClear, coUseBufferBackground]
        Camera = cam_fbo
        RootObject = dc_map
        EnabledRenderBuffers = []
        AfterRender = fboAfterRender
        PostGenerateMipmap = False
      end
      object dc_map: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object mesh: TGLMesh
          Material.Texture.ImageClassName = 'TGLPicFileImage'
          Material.Texture.Image.PictureFileName = 'data/map_tex.jpg'
          Material.Texture.TextureWrap = twNone
          Material.Texture.FilteringQuality = tfAnisotropic
          Material.Texture.Disabled = False
          Mode = mmQuads
          VertexMode = vmVNT
        end
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Enabled = False
    Mode = cmApplicationIdle
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
  object at: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = atTimer
    Left = 8
    Top = 40
  end
  object matlib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'back'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {C1C0403FC1C0403FC1C0403F0000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureFormat = tfRGB16
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
      end>
    Left = 40
    Top = 40
  end
end

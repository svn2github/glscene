object Form1: TForm1
  Left = 184
  Top = 89
  Width = 414
  Height = 284
  BorderWidth = 3
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 400
    Height = 249
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {CECD4D3FCECD4D3FCECD4D3F0000803F}
    Buffer.FogEnvironment.FogStart = 30
    Buffer.FogEnvironment.FogEnd = 90
    Buffer.BackgroundColor = 13487565
    Buffer.FogEnable = True
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Button1: TButton
    Left = 144
    Top = 232
    Width = 137
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'I want more mushrooms !'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    VisibilityCulling = vcObjectBased
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000FA4400409C4500007A450000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object Disk1: TGLDisk
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Emission.Color = {0000803E0000803E0000803E0000803F}
        Material.Texture.Image.Picture.Data = {07544269746D617000000000}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmReplace
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Material.Texture.Disabled = False
        Loops = 3
        OuterRadius = 75
        Slices = 9
        SweepAngle = 360
      end
      object FreeForm1: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000000000004040000000000000803F}
        Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
        Up.Coordinates = {0000803F000000000000008000000000}
        Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
        UseMeshMaterials = False
        NormalsOrientation = mnoInvert
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000A041000040410000F0410000803F}
      Left = 200
      Top = 104
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 48
  end
end

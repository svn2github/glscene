object DataModule1: TDataModule1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 300
  Width = 400
  object GLScene1: TGLScene
    Left = 28
    Top = 13
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLTeapot1: TGLTeapot
    end
    object DCBlueLight: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {00000000000000000000803F0000803F}
        Position.Coordinates = {0000A04100000000000000000000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLTeapot1
      Position.Coordinates = {0000404000000040000000400000803F}
    end
  end
  object GLFullScreenViewer1: TGLFullScreenViewer
    Camera = GLCamera1
    Width = 800
    Height = 600
    PostRender = GLFullScreenViewer1PostRender
    Buffer.BackgroundColor = clBlack
    ManualRendering = False
    StayOnTop = True
    RefreshRate = 100
    OnKeyPress = GLFullScreenViewer1KeyPress
    Left = 96
    Top = 13
  end
end

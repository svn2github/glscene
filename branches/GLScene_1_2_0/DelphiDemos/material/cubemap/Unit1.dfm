object Form1: TForm1
  Left = 135
  Top = 85
  Caption = 'CubeMap mapping'
  ClientHeight = 296
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 300
    Height = 296
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.BackgroundColor = clBlack
    FieldOfView = 52.517318725585940000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 136
    Top = 8
    Width = 163
    Height = 25
    Caption = 'Apply Cube Environment Map'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 0.800000011920929000
      VisibleAtRunTime = True
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000E0400000A040000040400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Teapot1: TGLTeapot
      Material.MaterialLibrary = GLMaterialLibraryEx1
      Material.LibMaterialName = 'CubeMapMaterial'
      MaterialLibrary = GLMaterialLibraryEx1
      LibMaterialName = 'CubeMapMaterial'
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 300.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {000040400000A0400000E0400000803F}
    end
  end
  object GLMaterialLibraryEx1: TGLMaterialLibraryEx
    Materials = <
      item
        Name = 'CubeMapMaterial'
        Tag = 0
        FixedFunction.Enabled = True
        FixedFunction.FrontProperties.Ambient.Color = {B3B2B23EB3B2B23E000000000000803F}
        FixedFunction.FrontProperties.Diffuse.Color = {9594143F9C9B1B3F8180003C0000803F}
        FixedFunction.FrontProperties.Shininess = 32
        FixedFunction.FrontProperties.Specular.Color = {DDDCDC3EDDDCDC3EDDDCDC3E0000803F}
        FixedFunction.LineProperties.Enabled = False
        FixedFunction.Texture.Enabled = False
        FixedFunction.Texture.LibTextureName = 'CubeTexture'
        FixedFunction.Texture.LibSamplerName = 'DefaultSampler'
        FixedFunction.Texture.EnvMode = tmModulate
        FixedFunction.Texture.MappingMode = tmmCubeMapReflection
        Multitexturing.Enabled = False
        Multitexturing.Texture0.Enabled = False
        Multitexturing.Texture1.Enabled = False
        Multitexturing.Texture2.Enabled = False
        Multitexturing.Texture3.Enabled = False
        ShaderModel3.Enabled = False
        ShaderModel4.Enabled = False
        ShaderModel5.Enabled = False
        ShaderESSL1.Enabled = False
      end>
    OnMatLibComponentFail = GLMaterialLibraryEx1MatLibComponentFail
    ComponentsData = {
      0458434F4C02010202061154474C5465787475726553616D706C65720200060E
      44656661756C7453616D706C6572080205020102010200020002000200000000
      000000000000000000000000000200020309061154474C54657874757265496D
      61676545780201060B4375626554657874757265080224020002000500000000
      00000080FF3F050000000000000080FF3F050000000000000080FF3F05000000
      0000000080FF3F06262E2E5C2E2E5C2E2E5C6D656469615C646561646D656174
      5F736B796D6F726E696E672E6464730802020808}
    Left = 8
    Top = 56
  end
end

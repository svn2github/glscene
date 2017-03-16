object Form1: TForm1
  Left = 195
  Top = 129
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 508
  ClientWidth = 824
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 824
    Height = 472
    Camera = cam
    Buffer.BackgroundColor = clGray
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roNoColorBufferClear]
    Buffer.AntiAliasing = aaNone
    FieldOfView = 156.075897216796900000
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 472
    Width = 824
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    Color = clGray
    TabOrder = 1
    object tb: TTrackBar
      Left = 4
      Top = 4
      Width = 816
      Height = 28
      Align = alClient
      Max = 40
      Min = 1
      Position = 4
      TabOrder = 0
      ThumbLength = 23
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = tbChange
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc_cam: TGLDummyCube
      CubeSize = 1.000000000000000000
      object cam: TGLCamera
        DepthOfView = 10.000000000000000000
        FocalLength = 50.000000000000000000
        NearPlaneBias = 0.100000001490116100
        TargetObject = dc_cam
        CameraStyle = csInfinitePerspective
        Position.Coordinates = {0000C03F0000C03F0000C03F0000803F}
        object light: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          Specular.Color = {0000803F0000803F0000803F0000803F}
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object dc_pass1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object hud_back: TGLHUDSprite
        Position.Coordinates = {0000204400008043000000000000803F}
        Width = 1280.000000000000000000
        Height = 512.000000000000000000
        Rotation = 0.000000000000000000
      end
      object GLDodecahedron1: TGLDodecahedron
        Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
        Position.Coordinates = {0000000000000000000080BF0000803F}
      end
      object GLSpaceText1: TGLSpaceText
        Material.FrontProperties.Diffuse.Color = {8195233FA01AEF3E8716593E0000803F}
        Material.FrontProperties.Specular.Color = {BE9F9A3E60E5503D60E5503D0000803F}
        Extrusion = 0.100000001490116100
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Trebuchet MS'
        Font.Style = []
        Lines.Strings = (
          'GLS')
        TextHeight = 1.000000000000000000
        Adjust.Horz = haCenter
        Adjust.Vert = vaCenter
      end
    end
    object fbo1: TGLFBORenderer
      ForceTextureDimensions = False
      Height = 512
      ColorTextureName = 'pass1'
      MaterialLibrary = matlib
      BackgroundColor.Color = {EBE0E03EE4DB5B3F9A93133F0000003F}
      ClearOptions = [coDepthBufferClear]
      Camera = cam
      RootObject = dc_pass1
      EnabledRenderBuffers = [erbDepth]
      PostGenerateMipmap = False
    end
    object hud_res1: TGLHUDSprite
      Material.MaterialLibrary = matlib
      Material.LibMaterialName = 'pass1'
      Position.Coordinates = {0000604400008043000000000000803F}
      Width = 256.000000000000000000
      Height = 512.000000000000000000
      Rotation = 0.000000000000000000
    end
    object fbo2: TGLFBORenderer
      ForceTextureDimensions = False
      Height = 512
      ColorTextureName = 'pass2'
      MaterialLibrary = matlib
      BackgroundColor.Color = {9CC4E03E6DE75B3FBC74133F0000003F}
      ClearOptions = []
      Camera = cam
      RootObject = hud_res1
      EnabledRenderBuffers = []
      BeforeRender = fbo2BeforeRender
      AfterRender = fbo2AfterRender
      PostGenerateMipmap = False
    end
    object hud_res2: TGLHUDSprite
      Material.MaterialLibrary = matlib
      Material.LibMaterialName = 'pass2'
      Position.Coordinates = {0000904400008043000000000000803F}
      Width = 256.000000000000000000
      Height = 512.000000000000000000
      Rotation = 0.000000000000000000
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Enabled = False
    Mode = cmApplicationIdle
    OnProgress = cadProgress
    Left = 8
    Top = 40
  end
  object matlib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'pass1'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.Height = 512
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.TextureWrap = twNone
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
        Shader = glsl_pass1
      end
      item
        Name = 'pass2'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.Height = 512
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.TextureWrap = twNone
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
        Shader = glsl_pass2
      end>
    Left = 40
    Top = 8
  end
  object glsl_pass1: TGLSLShader
    FragmentProgram.Code.Strings = (
      'uniform float bsize;'
      'uniform sampler2D RT;'
      ''
      'void main(void)'
      '{'
      '   vec4 sum = vec4(0.0);'
      ''
      '  float tx=gl_TexCoord[0].x;'
      '  float ty=gl_TexCoord[0].y;'
      ''
      '   sum += texture2D(RT, vec2(tx-4.0*bsize, ty)) * 0.06;'
      '   sum += texture2D(RT, vec2(tx-3.0*bsize, ty)) * 0.09;'
      '   sum += texture2D(RT, vec2(tx-2.0*bsize, ty)) * 0.12;'
      '   sum += texture2D(RT, vec2(tx-bsize, ty)) * 0.15;'
      '   sum += texture2D(RT, vec2(tx, ty)) * 0.16;'
      '   sum += texture2D(RT, vec2(tx+bsize, ty)) * 0.15;'
      '   sum += texture2D(RT, vec2(tx+2.0*bsize, ty)) * 0.12;'
      '   sum += texture2D(RT, vec2(tx+3.0*bsize, ty)) * 0.09;'
      '   sum += texture2D(RT, vec2(tx+4.0*bsize, ty)) * 0.06;'
      ' '
      '   gl_FragColor = sum;'
      ''
      '}')
    FragmentProgram.Enabled = True
    OnApply = glsl_pass1Apply
    FailedInitAction = fiaReRaiseException
    Left = 40
    Top = 40
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = vp
    FormCaption = 'GLSL GaussianBlur: %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 8
    Top = 72
  end
  object glsl_pass2: TGLSLShader
    FragmentProgram.Code.Strings = (
      '#version 120'
      'uniform float bsize;'
      'uniform sampler2D RT;'
      ''
      'void main(void)'
      '{'
      '   vec4 sum = vec4(0.0);'
      ''
      '  float tx=gl_TexCoord[0].x;'
      '  float ty=gl_TexCoord[0].y;'
      ' '
      '   sum += texture2D(RT, vec2(tx, ty - 4.0*bsize)) * 0.06;'
      '   sum += texture2D(RT, vec2(tx, ty - 3.0*bsize)) * 0.09;'
      '   sum += texture2D(RT, vec2(tx, ty - 2.0*bsize)) * 0.12;'
      '   sum += texture2D(RT, vec2(tx, ty - bsize)) * 0.15;'
      '   sum += texture2D(RT, vec2(tx, ty)) * 0.16;'
      '   sum += texture2D(RT, vec2(tx, ty + bsize)) * 0.15;'
      '   sum += texture2D(RT, vec2(tx, ty + 2.0*bsize)) * 0.12;'
      '   sum += texture2D(RT, vec2(tx, ty + 3.0*bsize)) * 0.09;'
      '   sum += texture2D(RT, vec2(tx, ty + 4.0*bsize)) * 0.06;'
      ' '
      '   gl_FragColor = sum;'
      '}')
    FragmentProgram.Enabled = True
    OnApply = glsl_pass2Apply
    FailedInitAction = fiaReRaiseException
    Left = 40
    Top = 72
  end
end

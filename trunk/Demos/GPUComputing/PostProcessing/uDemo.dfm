object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'GLScene CUDA Post Processing'
  ClientHeight = 538
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 512
    Height = 512
    Camera = GLCamera1
    Buffer.AntiAliasing = aaNone
    FieldOfView = 157.897079467773400000
    Align = alClient
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 512
    Width = 512
    Height = 26
    Align = alBottom
    Max = 16
    Min = 1
    Position = 8
    ShowSelRange = False
    TabOrder = 1
    OnChange = TrackBar1Change
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 32
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLTeapot1
      Position.Coordinates = {0000803F0000803F000000400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object InfoCapture: TGLDirectOpenGL
      UseBuildList = False
      OnRender = InfoCaptureRender
      Blend = False
    end
    object GLFBORenderer1: TGLFBORenderer
      ForceTextureDimensions = False
      Width = 512
      Height = 512
      ColorTextureName = 'processedTexture'
      MaterialLibrary = GLMaterialLibrary1
      BackgroundColor.Color = {00000000000000000000000000000000}
      ClearOptions = [coColorBufferClear, coDepthBufferClear]
      RootObject = RenderRoot
      TargetVisibility = tvFBOOnly
      EnabledRenderBuffers = [erbDepth]
      BeforeRender = GLFBORenderer1BeforeRender
      AfterRender = GLFBORenderer1AfterRender
      PostGenerateMipmap = False
    end
    object RenderRoot: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        NormalDirection = ndInside
        Radius = 20.000000000000000000
        Slices = 128
        Stacks = 128
      end
      object GLTeapot1: TGLTeapot
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Shininess = 16
        Material.FrontProperties.Specular.Color = {3333733F3333733F3333733F0000803F}
        Material.DepthProperties.DepthClamp = True
      end
      object GLCylinder1: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {9A99193FCDCC4C3FACC8483E0000803F}
        Material.FrontProperties.Shininess = 64
        Material.FrontProperties.Specular.Color = {3333333F3333333F3333333F0000803F}
        Material.DepthProperties.DepthClamp = True
        Position.Coordinates = {0000003F00000000000040BF0000803F}
        BottomRadius = 0.250000000000000000
        Height = 1.000000000000000000
        Slices = 32
        Loops = 10
        TopRadius = 0.250000000000000000
      end
      object GLCapsule1: TGLCapsule
        Material.FrontProperties.Diffuse.Color = {EBE0E03E9A93133FE4DB5B3F0000803F}
        Material.FrontProperties.Shininess = 16
        Material.FrontProperties.Specular.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.DepthProperties.DepthClamp = True
        Direction.Coordinates = {00000000010000330000803F00000000}
        Position.Coordinates = {000080BF00000000000000000000803F}
        Up.Coordinates = {000000000000803F010000B300000000}
        Height = 0.750000000000000000
        Slices = 16
        Stacks = 4
        Radius = 0.250000000000000000
      end
    end
    object CallPostProcess: TGLDirectOpenGL
      UseBuildList = False
      OnRender = CallPostProcessRender
      Blend = False
    end
    object GL3xSprite1: TGL3xSprite
      Material.Shader = ResultShader
      Width = 1.000000000000000000
      Height = 1.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 80
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'GLScene CUDA Post Processing - %FPS'
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
    Left = 32
    Top = 176
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'processedTexture'
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.Width = 512
        Material.Texture.Image.Height = 512
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureFormat = tfExtended
        Material.Texture.TextureFormatEx = tfRGBA8UI
        Tag = 0
        Shader = ResultShader
      end>
    Left = 32
    Top = 128
  end
  object GLSCUDADevice1: TGLSCUDADevice
    Left = 448
    Top = 24
  end
  object GLSCUDA1: TGLSCUDA
    ComputingDevice = GLSCUDADevice1
    OnOpenGLContextNeeded = GLSCUDA1OpenGLContextNeeded
    Left = 448
    Top = 72
    object MainModule: TCUDAModule
      Code.Strings = (
        #9'.version 1.4'
        #9'.target sm_10, map_f64_to_f32'
        #9'// compiled with C:\CUDA\bin/../open64/lib//be.exe'
        #9'// nvopencc 3.0 built on 2009-10-29'
        ''
        #9'//-----------------------------------------------------------'
        
          #9'// Compiling C:/Users/YARUND~1/AppData/Local/Temp/tmpxft_00000e' +
          'd0_00000001-9_temp.cpp3.i (C:/Users/YARUND~1/AppData/Local/Temp/' +
          'ccBI#.a03732)'
        #9'//-----------------------------------------------------------'
        ''
        #9'//-----------------------------------------------------------'
        #9'// Options:'
        #9'//-----------------------------------------------------------'
        #9'//  Target:ptx, ISA:sm_10, Endian:little, Pointer Size:32'
        #9'//  -O3'#9'(Optimization level)'
        #9'//  -g0'#9'(Debug level)'
        #9'//  -m2'#9'(Report advisories)'
        #9'//-----------------------------------------------------------'
        ''
        
          #9'.file'#9'1'#9'"C:/Users/YARUND~1/AppData/Local/Temp/tmpxft_00000ed0_0' +
          '0000001-8_temp.cudafe2.gpu"'
        
          #9'.file'#9'2'#9'"C:\Program Files\Microsoft Visual Studio 9.0\VC\INCLUD' +
          'E\crtdefs.h"'
        #9'.file'#9'3'#9'"C:\CUDA\include\crt/device_runtime.h"'
        #9'.file'#9'4'#9'"C:\CUDA\include\host_defines.h"'
        #9'.file'#9'5'#9'"C:\CUDA\include\builtin_types.h"'
        #9'.file'#9'6'#9'"c:\cuda\include\device_types.h"'
        #9'.file'#9'7'#9'"c:\cuda\include\driver_types.h"'
        #9'.file'#9'8'#9'"c:\cuda\include\surface_types.h"'
        #9'.file'#9'9'#9'"c:\cuda\include\texture_types.h"'
        #9'.file'#9'10'#9'"c:\cuda\include\vector_types.h"'
        #9'.file'#9'11'#9'"c:\cuda\include\host_defines.h"'
        #9'.file'#9'12'#9'"C:\CUDA\include\device_launch_parameters.h"'
        #9'.file'#9'13'#9'"c:\cuda\include\crt\storage_class.h"'
        
          #9'.file'#9'14'#9'"C:\Program Files\Microsoft Visual Studio 9.0\VC\INCLU' +
          'DE\time.h"'
        #9'.file'#9'15'#9'"C:/Users/YARUND~1/AppData/Local/Temp/temp.cu"'
        #9'.file'#9'16'#9'"C:\CUDA\include\common_functions.h"'
        #9'.file'#9'17'#9'"c:\cuda\include\crt/func_macro.h"'
        #9'.file'#9'18'#9'"c:\cuda\include\math_functions.h"'
        #9'.file'#9'19'#9'"c:\cuda\include\device_functions.h"'
        #9'.file'#9'20'#9'"c:\cuda\include\math_constants.h"'
        #9'.file'#9'21'#9'"c:\cuda\include\sm_11_atomic_functions.h"'
        #9'.file'#9'22'#9'"c:\cuda\include\sm_12_atomic_functions.h"'
        #9'.file'#9'23'#9'"c:\cuda\include\sm_13_double_functions.h"'
        #9'.file'#9'24'#9'"c:\cuda\include\common_types.h"'
        #9'.file'#9'25'#9'"c:\cuda\include\sm_20_atomic_functions.h"'
        #9'.file'#9'26'#9'"c:\cuda\include\sm_20_intrinsics.h"'
        #9'.file'#9'27'#9'"c:\cuda\include\surface_functions.h"'
        #9'.file'#9'28'#9'"c:\cuda\include\texture_fetch_functions.h"'
        #9'.file'#9'29'#9'"c:\cuda\include\math_functions_dbl_ptx1.h"'
        ''
        #9'.extern'#9'.shared .align 4 .b8 sdata[];'
        ''
        #9'.entry cudaProcess_k ('
        #9#9'.param .u32 __cudaparm_cudaProcess_k_g_data,'
        #9#9'.param .u32 __cudaparm_cudaProcess_k_g_odata,'
        #9#9'.param .s32 __cudaparm_cudaProcess_k_imgw,'
        #9#9'.param .s32 __cudaparm_cudaProcess_k_imgh,'
        #9#9'.param .s32 __cudaparm_cudaProcess_k_tilew,'
        #9#9'.param .s32 __cudaparm_cudaProcess_k_r,'
        #9#9'.param .f32 __cudaparm_cudaProcess_k_threshold,'
        #9#9'.param .f32 __cudaparm_cudaProcess_k_highlight)'
        #9'{'
        #9'.reg .u32 %r<205>;'
        #9'.reg .f32 %f<17>;'
        #9'.reg .f64 %fd<5>;'
        #9'.reg .pred %p<9>;'
        #9'.loc'#9'15'#9'62'#9'0'
        '$LBB1_cudaProcess_k:'
        #9'mov.u32 '#9'%r1, sdata;'
        #9'ld.param.s32 '#9'%r2, [__cudaparm_cudaProcess_k_imgw];'
        #9'sub.s32 '#9'%r3, %r2, 1;'
        #9'cvt.s32.u16 '#9'%r4, %ntid.x;'
        #9'cvt.u32.u16 '#9'%r5, %ctaid.x;'
        #9'mul.lo.u32 '#9'%r6, %r4, %r5;'
        #9'ld.param.s32 '#9'%r7, [__cudaparm_cudaProcess_k_imgh];'
        #9'sub.s32 '#9'%r8, %r7, 1;'
        #9'cvt.s32.u16 '#9'%r9, %ntid.y;'
        #9'cvt.u32.u16 '#9'%r10, %ctaid.y;'
        #9'mul.lo.u32 '#9'%r11, %r9, %r10;'
        #9'cvt.s32.u16 '#9'%r12, %tid.x;'
        #9'add.u32 '#9'%r13, %r12, %r6;'
        #9'cvt.s32.u16 '#9'%r14, %tid.y;'
        #9'add.u32 '#9'%r15, %r14, %r11;'
        #9'min.s32 '#9'%r16, %r3, %r13;'
        #9'min.s32 '#9'%r17, %r8, %r15;'
        #9'mov.s32 '#9'%r18, 0;'
        #9'max.s32 '#9'%r19, %r16, %r18;'
        #9'mov.s32 '#9'%r20, 0;'
        #9'max.s32 '#9'%r21, %r17, %r20;'
        #9'mul.lo.s32 '#9'%r22, %r21, %r2;'
        #9'ld.param.u32 '#9'%r23, [__cudaparm_cudaProcess_k_g_data];'
        #9'add.s32 '#9'%r24, %r19, %r22;'
        #9'mul.lo.u32 '#9'%r25, %r24, 4;'
        #9'add.u32 '#9'%r26, %r23, %r25;'
        #9'ld.global.s32 '#9'%r27, [%r26+0];'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r28, [__cudaparm_cudaProcess_k_r];'
        #9'add.s32 '#9'%r29, %r12, %r28;'
        #9'add.s32 '#9'%r30, %r28, %r14;'
        #9'ld.param.s32 '#9'%r31, [__cudaparm_cudaProcess_k_tilew];'
        #9'mul.lo.s32 '#9'%r32, %r30, %r31;'
        #9'add.s32 '#9'%r33, %r29, %r32;'
        #9'mul.lo.u32 '#9'%r34, %r33, 4;'
        #9'add.u32 '#9'%r35, %r1, %r34;'
        #9'st.shared.s32 '#9'[%r35+0], %r27;'
        #9'setp.lt.u32 '#9'%p1, %r12, %r28;'
        #9'@!%p1 bra '#9'$Lt_0_5634;'
        #9'ld.param.s32 '#9'%r28, [__cudaparm_cudaProcess_k_r];'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'sub.s32 '#9'%r36, %r13, %r28;'
        #9'min.s32 '#9'%r37, %r3, %r36;'
        #9'mov.s32 '#9'%r38, 0;'
        #9'max.s32 '#9'%r39, %r37, %r38;'
        #9'add.s32 '#9'%r40, %r22, %r39;'
        #9'mul.lo.u32 '#9'%r41, %r40, 4;'
        #9'.loc'#9'15'#9'62'#9'0'
        #9'ld.param.u32 '#9'%r23, [__cudaparm_cudaProcess_k_g_data];'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'add.u32 '#9'%r42, %r23, %r41;'
        #9'ld.global.s32 '#9'%r43, [%r42+0];'
        #9'.loc'#9'15'#9'80'#9'0'
        #9'add.s32 '#9'%r44, %r32, %r12;'
        #9'mul.lo.u32 '#9'%r45, %r44, 4;'
        #9'add.u32 '#9'%r46, %r1, %r45;'
        #9'st.shared.s32 '#9'[%r46+0], %r43;'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'add.s32 '#9'%r47, %r4, %r13;'
        #9'min.s32 '#9'%r48, %r3, %r47;'
        #9'mov.s32 '#9'%r49, 0;'
        #9'max.s32 '#9'%r50, %r48, %r49;'
        #9'add.s32 '#9'%r51, %r22, %r50;'
        #9'mul.lo.u32 '#9'%r52, %r51, 4;'
        #9'add.u32 '#9'%r53, %r23, %r52;'
        #9'ld.global.s32 '#9'%r54, [%r53+0];'
        #9'.loc'#9'15'#9'82'#9'0'
        #9'add.s32 '#9'%r55, %r32, %r12;'
        #9'add.s32 '#9'%r56, %r28, %r4;'
        #9'add.s32 '#9'%r57, %r55, %r56;'
        #9'mul.lo.u32 '#9'%r58, %r57, 4;'
        #9'add.u32 '#9'%r59, %r1, %r58;'
        #9'st.shared.s32 '#9'[%r59+0], %r54;'
        '$Lt_0_5634:'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r28, [__cudaparm_cudaProcess_k_r];'
        #9'.loc'#9'15'#9'82'#9'0'
        #9'setp.gt.u32 '#9'%p2, %r28, %r14;'
        #9'@!%p2 bra '#9'$Lt_0_6146;'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r28, [__cudaparm_cudaProcess_k_r];'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'sub.s32 '#9'%r60, %r15, %r28;'
        #9'min.s32 '#9'%r61, %r8, %r60;'
        #9'mov.s32 '#9'%r62, 0;'
        #9'max.s32 '#9'%r63, %r61, %r62;'
        #9'.loc'#9'15'#9'62'#9'0'
        #9'ld.param.s32 '#9'%r2, [__cudaparm_cudaProcess_k_imgw];'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'mul.lo.s32 '#9'%r64, %r2, %r63;'
        #9'add.s32 '#9'%r65, %r19, %r64;'
        #9'mul.lo.u32 '#9'%r66, %r65, 4;'
        #9'.loc'#9'15'#9'62'#9'0'
        #9'ld.param.u32 '#9'%r23, [__cudaparm_cudaProcess_k_g_data];'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'add.u32 '#9'%r67, %r23, %r66;'
        #9'ld.global.s32 '#9'%r68, [%r67+0];'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r31, [__cudaparm_cudaProcess_k_tilew];'
        #9'.loc'#9'15'#9'86'#9'0'
        #9'mul.lo.s32 '#9'%r69, %r31, %r14;'
        #9'add.s32 '#9'%r70, %r29, %r69;'
        #9'mul.lo.u32 '#9'%r71, %r70, 4;'
        #9'add.u32 '#9'%r72, %r1, %r71;'
        #9'st.shared.s32 '#9'[%r72+0], %r68;'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'add.s32 '#9'%r73, %r9, %r15;'
        #9'min.s32 '#9'%r74, %r8, %r73;'
        #9'mov.s32 '#9'%r75, 0;'
        #9'max.s32 '#9'%r76, %r74, %r75;'
        #9'mul.lo.s32 '#9'%r77, %r2, %r76;'
        #9'add.s32 '#9'%r78, %r19, %r77;'
        #9'mul.lo.u32 '#9'%r79, %r78, 4;'
        #9'add.u32 '#9'%r80, %r23, %r79;'
        #9'ld.global.s32 '#9'%r81, [%r80+0];'
        #9'.loc'#9'15'#9'88'#9'0'
        #9'add.s32 '#9'%r82, %r28, %r9;'
        #9'add.s32 '#9'%r83, %r14, %r82;'
        #9'mul.lo.s32 '#9'%r84, %r31, %r83;'
        #9'add.s32 '#9'%r85, %r29, %r84;'
        #9'mul.lo.u32 '#9'%r86, %r85, 4;'
        #9'add.u32 '#9'%r87, %r1, %r86;'
        #9'st.shared.s32 '#9'[%r87+0], %r81;'
        '$Lt_0_6146:'
        #9'selp.s32 '#9'%r88, 1, 0, %p1;'
        #9'selp.s32 '#9'%r89, 1, 0, %p2;'
        #9'and.b32 '#9'%r90, %r88, %r89;'
        #9'mov.u32 '#9'%r91, 0;'
        #9'setp.eq.s32 '#9'%p3, %r90, %r91;'
        #9'@%p3 bra '#9'$Lt_0_6658;'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r28, [__cudaparm_cudaProcess_k_r];'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'sub.s32 '#9'%r92, %r13, %r28;'
        #9'sub.s32 '#9'%r93, %r15, %r28;'
        #9'min.s32 '#9'%r94, %r3, %r92;'
        #9'min.s32 '#9'%r95, %r8, %r93;'
        #9'mov.s32 '#9'%r96, 0;'
        #9'max.s32 '#9'%r97, %r94, %r96;'
        #9'mov.s32 '#9'%r98, 0;'
        #9'max.s32 '#9'%r99, %r95, %r98;'
        #9'.loc'#9'15'#9'62'#9'0'
        #9'ld.param.s32 '#9'%r2, [__cudaparm_cudaProcess_k_imgw];'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'mul.lo.s32 '#9'%r100, %r99, %r2;'
        #9'add.s32 '#9'%r101, %r100, %r97;'
        #9'mul.lo.u32 '#9'%r102, %r101, 4;'
        #9'.loc'#9'15'#9'62'#9'0'
        #9'ld.param.u32 '#9'%r23, [__cudaparm_cudaProcess_k_g_data];'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'add.u32 '#9'%r103, %r23, %r102;'
        #9'ld.global.s32 '#9'%r104, [%r103+0];'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r31, [__cudaparm_cudaProcess_k_tilew];'
        #9'.loc'#9'15'#9'94'#9'0'
        #9'mul.lo.s32 '#9'%r105, %r31, %r14;'
        #9'add.s32 '#9'%r106, %r105, %r12;'
        #9'mul.lo.u32 '#9'%r107, %r106, 4;'
        #9'add.u32 '#9'%r108, %r1, %r107;'
        #9'st.shared.s32 '#9'[%r108+0], %r104;'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'add.s32 '#9'%r109, %r9, %r15;'
        #9'min.s32 '#9'%r110, %r8, %r109;'
        #9'mov.s32 '#9'%r111, 0;'
        #9'max.s32 '#9'%r112, %r110, %r111;'
        #9'mul.lo.s32 '#9'%r113, %r112, %r2;'
        #9'add.s32 '#9'%r114, %r113, %r97;'
        #9'mul.lo.u32 '#9'%r115, %r114, 4;'
        #9'add.u32 '#9'%r116, %r23, %r115;'
        #9'ld.global.s32 '#9'%r117, [%r116+0];'
        #9'.loc'#9'15'#9'96'#9'0'
        #9'add.s32 '#9'%r118, %r28, %r9;'
        #9'add.s32 '#9'%r119, %r118, %r14;'
        #9'mul.lo.s32 '#9'%r120, %r119, %r31;'
        #9'add.s32 '#9'%r121, %r120, %r12;'
        #9'mul.lo.u32 '#9'%r122, %r121, 4;'
        #9'add.u32 '#9'%r123, %r1, %r122;'
        #9'st.shared.s32 '#9'[%r123+0], %r117;'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'add.s32 '#9'%r124, %r9, %r13;'
        #9'min.s32 '#9'%r125, %r3, %r124;'
        #9'mov.s32 '#9'%r126, 0;'
        #9'max.s32 '#9'%r127, %r125, %r126;'
        #9'add.s32 '#9'%r128, %r100, %r127;'
        #9'mul.lo.u32 '#9'%r129, %r128, 4;'
        #9'add.u32 '#9'%r130, %r23, %r129;'
        #9'ld.global.s32 '#9'%r131, [%r130+0];'
        #9'.loc'#9'15'#9'98'#9'0'
        #9'add.s32 '#9'%r132, %r28, %r4;'
        #9'add.s32 '#9'%r133, %r132, %r12;'
        #9'add.s32 '#9'%r134, %r105, %r133;'
        #9'mul.lo.u32 '#9'%r135, %r134, 4;'
        #9'add.u32 '#9'%r136, %r1, %r135;'
        #9'st.shared.s32 '#9'[%r136+0], %r131;'
        #9'.loc'#9'15'#9'26'#9'0'
        #9'add.s32 '#9'%r137, %r4, %r13;'
        #9'min.s32 '#9'%r138, %r3, %r137;'
        #9'mov.s32 '#9'%r139, 0;'
        #9'max.s32 '#9'%r140, %r138, %r139;'
        #9'add.s32 '#9'%r141, %r113, %r140;'
        #9'mul.lo.u32 '#9'%r142, %r141, 4;'
        #9'add.u32 '#9'%r143, %r23, %r142;'
        #9'ld.global.s32 '#9'%r144, [%r143+0];'
        #9'.loc'#9'15'#9'100'#9'0'
        #9'add.s32 '#9'%r145, %r120, %r133;'
        #9'mul.lo.u32 '#9'%r146, %r145, 4;'
        #9'add.u32 '#9'%r147, %r1, %r146;'
        #9'st.shared.s32 '#9'[%r147+0], %r144;'
        '$Lt_0_6658:'
        #9'.loc'#9'15'#9'104'#9'0'
        #9'bar.sync '#9'0;'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r28, [__cudaparm_cudaProcess_k_r];'
        #9'.loc'#9'15'#9'112'#9'0'
        #9'neg.s32 '#9'%r148, %r28;'
        #9'mov.s32 '#9'%r149, %r148;'
        #9'setp.gt.s32 '#9'%p4, %r148, %r28;'
        #9'mov.f32 '#9'%f1, 0f00000000;     '#9'// 0'
        #9'mov.f32 '#9'%f2, 0f00000000;     '#9'// 0'
        #9'mov.f32 '#9'%f3, 0f00000000;     '#9'// 0'
        #9'mov.f32 '#9'%f4, 0f00000000;     '#9'// 0'
        #9'@%p4 bra '#9'$Lt_0_9730;'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r28, [__cudaparm_cudaProcess_k_r];'
        #9'.loc'#9'15'#9'112'#9'0'
        #9'add.s32 '#9'%r150, %r28, %r28;'
        #9'add.s32 '#9'%r151, %r150, 1;'
        #9'mov.s32 '#9'%r152, %r151;'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r31, [__cudaparm_cudaProcess_k_tilew];'
        #9'.loc'#9'15'#9'112'#9'0'
        #9'mul.lo.s32 '#9'%r153, %r148, %r31;'
        #9'mul.lo.s32 '#9'%r154, %r28, %r28;'
        #9'add.s32 '#9'%r155, %r28, 1;'
        #9'add.s32 '#9'%r156, %r29, %r155;'
        #9'cvt.rn.f32.s32 '#9'%f5, %r154;'
        #9'add.s32 '#9'%r157, %r153, %r32;'
        #9'mov.s32 '#9'%r158, %r152;'
        '$Lt_0_7682:'
        
          ' //<loop> Loop body line 112, nesting depth: 1, estimated iterat' +
          'ions: unknown'
        #9'.loc'#9'15'#9'113'#9'0'
        #9'mov.s32 '#9'%r159, %r148;'
        #9'mov.s32 '#9'%r160, %r151;'
        #9'mul.lo.s32 '#9'%r161, %r149, %r149;'
        #9'add.s32 '#9'%r162, %r157, %r12;'
        #9'add.s32 '#9'%r163, %r156, %r157;'
        #9'mul.lo.u32 '#9'%r164, %r162, 4;'
        #9'mul.lo.u32 '#9'%r165, %r163, 4;'
        #9'add.u32 '#9'%r166, %r164, %r1;'
        #9'add.u32 '#9'%r167, %r165, %r1;'
        ' //<loop> Part of loop body line 112, head labeled $Lt_0_7682'
        #9'mov.s32 '#9'%r168, %r160;'
        '$Lt_0_8450:'
        
          ' //<loop> Loop body line 113, nesting depth: 2, estimated iterat' +
          'ions: unknown'
        #9'.loc'#9'15'#9'114'#9'0'
        #9'ld.shared.s32 '#9'%r169, [%r166+0];'
        #9'mul.lo.s32 '#9'%r170, %r159, %r159;'
        #9'add.s32 '#9'%r171, %r161, %r170;'
        #9'cvt.rn.f32.s32 '#9'%f6, %r171;'
        #9'setp.le.f32 '#9'%p5, %f6, %f5;'
        #9'@!%p5 bra '#9'$Lt_0_8706;'
        ' //<loop> Part of loop body line 113, head labeled $Lt_0_8450'
        #9'.loc'#9'15'#9'133'#9'0'
        #9'and.b32 '#9'%r172, %r169, 255;'
        #9'cvt.rn.f32.s32 '#9'%f7, %r172;'
        #9'add.f32 '#9'%f4, %f7, %f4;'
        #9'.loc'#9'15'#9'134'#9'0'
        #9'shr.s32 '#9'%r173, %r169, 8;'
        #9'and.b32 '#9'%r174, %r173, 255;'
        #9'cvt.rn.f32.s32 '#9'%f8, %r174;'
        #9'add.f32 '#9'%f3, %f8, %f3;'
        #9'.loc'#9'15'#9'135'#9'0'
        #9'shr.s32 '#9'%r175, %r169, 16;'
        #9'and.b32 '#9'%r176, %r175, 255;'
        #9'cvt.rn.f32.s32 '#9'%f9, %r176;'
        #9'add.f32 '#9'%f2, %f9, %f2;'
        #9'.loc'#9'15'#9'136'#9'0'
        #9'cvt.f64.f32 '#9'%fd1, %f1;'
        #9'mov.f64 '#9'%fd2, 0d3ff0000000000000;'#9'// 1'
        #9'add.f64 '#9'%fd3, %fd1, %fd2;'
        #9'cvt.rn.f32.f64 '#9'%f1, %fd3;'
        '$Lt_0_8706:'
        ' //<loop> Part of loop body line 113, head labeled $Lt_0_8450'
        #9'add.s32 '#9'%r159, %r159, 1;'
        #9'add.u32 '#9'%r166, %r166, 4;'
        #9'setp.ne.u32 '#9'%p6, %r166, %r167;'
        #9'@%p6 bra '#9'$Lt_0_8450;'
        ' //<loop> Part of loop body line 112, head labeled $Lt_0_7682'
        #9'add.s32 '#9'%r149, %r149, 1;'
        #9'.loc'#9'15'#9'75'#9'0'
        #9'ld.param.s32 '#9'%r31, [__cudaparm_cudaProcess_k_tilew];'
        #9'.loc'#9'15'#9'136'#9'0'
        #9'add.s32 '#9'%r157, %r157, %r31;'
        #9'setp.ne.s32 '#9'%p7, %r155, %r149;'
        #9'@%p7 bra '#9'$Lt_0_7682;'
        #9'bra.uni '#9'$Lt_0_7170;'
        '$Lt_0_9730:'
        '$Lt_0_7170:'
        #9'.loc'#9'15'#9'144'#9'0'
        #9'div.full.f32 '#9'%f10, %f2, %f1;'
        #9'cvt.rzi.s32.f32 '#9'%r177, %f10;'
        #9'mov.s32 '#9'%r178, 255;'
        #9'min.s32 '#9'%r179, %r177, %r178;'
        #9'mov.s32 '#9'%r180, 0;'
        #9'max.s32 '#9'%r181, %r179, %r180;'
        #9'cvt.rn.f32.s32 '#9'%f11, %r181;'
        #9'cvt.rzi.s32.f32 '#9'%r182, %f11;'
        #9'shl.b32 '#9'%r183, %r182, 16;'
        #9'div.full.f32 '#9'%f12, %f3, %f1;'
        #9'cvt.rzi.s32.f32 '#9'%r184, %f12;'
        #9'mov.s32 '#9'%r185, 255;'
        #9'min.s32 '#9'%r186, %r184, %r185;'
        #9'mov.s32 '#9'%r187, 0;'
        #9'max.s32 '#9'%r188, %r186, %r187;'
        #9'cvt.rn.f32.s32 '#9'%f13, %r188;'
        #9'cvt.rzi.s32.f32 '#9'%r189, %f13;'
        #9'shl.b32 '#9'%r190, %r189, 8;'
        #9'or.b32 '#9'%r191, %r183, %r190;'
        #9'div.full.f32 '#9'%f14, %f4, %f1;'
        #9'cvt.rzi.s32.f32 '#9'%r192, %f14;'
        #9'mov.s32 '#9'%r193, 255;'
        #9'min.s32 '#9'%r194, %r192, %r193;'
        #9'mov.s32 '#9'%r195, 0;'
        #9'max.s32 '#9'%r196, %r194, %r195;'
        #9'cvt.rn.f32.s32 '#9'%f15, %r196;'
        #9'cvt.rzi.s32.f32 '#9'%r197, %f15;'
        #9'or.b32 '#9'%r198, %r191, %r197;'
        #9'ld.param.u32 '#9'%r199, [__cudaparm_cudaProcess_k_g_odata];'
        #9'.loc'#9'15'#9'62'#9'0'
        #9'ld.param.s32 '#9'%r2, [__cudaparm_cudaProcess_k_imgw];'
        #9'.loc'#9'15'#9'144'#9'0'
        #9'mul.lo.s32 '#9'%r200, %r2, %r15;'
        #9'add.s32 '#9'%r201, %r13, %r200;'
        #9'mul.lo.u32 '#9'%r202, %r201, 4;'
        #9'add.u32 '#9'%r203, %r199, %r202;'
        #9'st.global.s32 '#9'[%r203+0], %r198;'
        #9'.loc'#9'15'#9'145'#9'0'
        #9'exit;'
        '$LDWend_cudaProcess_k:'
        #9'} // cudaProcess_k'
        '')
      Compiler = GLSCUDACompiler1
      object cudaProcess: TCUDAFunction
        KernelName = 'cudaProcess_k'
        BlockShape.SizeX = 16
        BlockShape.SizeY = 16
        Grid.SizeX = 32
        Grid.SizeY = 32
        OnParameterSetup = cudaProcessParameterSetup
      end
    end
    object processedTextureMapper: TCUDAGLImageResource
      TextureName = 'processedTexture'
      MaterialLibrary = GLMaterialLibrary1
    end
    object processedTextureArray: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtArray
      ChannelsNum = cnFour
    end
    object outputBuffer: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtDevice
      ChannelsNum = cnFour
    end
    object inputBuffer: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtDevice
      ChannelsNum = cnFour
    end
  end
  object GLSCUDACompiler1: TGLSCUDACompiler
    NVCCPath = 'C:\CUDA\bin\'
    CppCompilerPath = 'C:\Program Files\Microsoft Visual Studio 9.0\VC\bin\'
    ProjectModule = 'postProcessGL_kernel.cu'
    Left = 448
    Top = 120
  end
  object ResultShader: TGLSLShader
    FragmentProgram.Code.Strings = (
      '#version 150'
      'uniform usampler2D TexUnit0; '
      'out vec4 FragColor;'
      'void main(void)'
      '{'
      
        '   uvec3 intColor= texelFetch(TexUnit0, ivec2(gl_FragCoord.xy), ' +
        '0).rgb;'
      '   FragColor = vec4(vec3(intColor)/ 255.0, 1.0);'
      '}')
    FragmentProgram.Enabled = True
    VertexProgram.Code.Strings = (
      '#version 150'
      'in vec3 Position;'
      'void main(void)'
      '{'
      '   gl_Position = vec4(sign(Position.xy), 0.0, 1.0);'
      '}')
    VertexProgram.Enabled = True
    OnApply = ResultShaderApply
    Left = 32
    Top = 280
  end
  object CommonShader: TGLSLShader
    FragmentProgram.Code.Strings = (
      '#version 130'
      'const float LightIntensity = 1.0;'
      'const float SpecPower = 32.0; '
      'in vec3 Normal; '
      'in vec3 LightVector; '
      'in vec3 CameraVector; '
      'in vec4 Color;'
      'out uvec4 FragColor;'
      ''
      'void main(void) '
      '{'
      
        '  vec4 DiffuseContrib = clamp(gl_LightSource[0].diffuse * dot(Li' +
        'ghtVector, Normal), 0.0, 1.0); '
      '  vec3 reflect_vec = reflect(CameraVector, -Normal); '
      '  float Temp = max(dot(reflect_vec, LightVector), 0.0); '
      
        '  vec4 SpecContrib = gl_LightSource[0].specular * clamp(pow(Temp' +
        ', SpecPower), 0.0, 0.95); '
      
        '  vec4 fFragColor = LightIntensity * (Color* (gl_LightSource[0].' +
        'ambient + DiffuseContrib) + SpecContrib);'
      '  FragColor = uvec4(255.0 * fFragColor.rgb, 255u); '
      '}')
    FragmentProgram.Enabled = True
    VertexProgram.Code.Strings = (
      '#version 120'
      'varying vec3 Normal; '
      'varying vec3 LightVector; '
      'varying vec3 CameraVector; '
      'varying vec4 Color; '
      ''
      'void main(void) '
      '{'
      '  gl_Position = ftransform(); '
      '  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0; '
      '  Normal = normalize(gl_NormalMatrix * gl_Normal); '
      '  vec3 p = (gl_ModelViewMatrix * gl_Vertex).xyz; '
      '  LightVector = normalize(gl_LightSource[0].position.xyz - p); '
      '  CameraVector = normalize(p); '
      '  Color = gl_FrontMaterial.diffuse;'
      '}')
    VertexProgram.Enabled = True
    Left = 32
    Top = 232
  end
end

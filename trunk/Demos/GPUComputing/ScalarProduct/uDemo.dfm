object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 
    'Calculation scalar products of a  given set of input vector pair' +
    's'
  ClientHeight = 292
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 450
    Height = 233
    TabOrder = 0
  end
  object Button1: TButton
    Left = 383
    Top = 259
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLSCUDA1: TGLSCUDA
    ComputingDevice = GLSCUDADevice1
    Left = 104
    Top = 248
    object MainModule: TCUDAModule
      Code.Strings = (
        #9'.version 1.4'
        #9'.target sm_10, map_f64_to_f32'
        #9'// compiled with C:\CUDA\bin/../open64/lib//be.exe'
        #9'// nvopencc 3.0 built on 2009-10-29'
        ''
        #9'//-----------------------------------------------------------'
        
          #9'// Compiling C:/Users/YARUND~1/AppData/Local/Temp/tmpxft_00000c' +
          '0c_00000000-9_temp.cpp3.i (C:/Users/YARUND~1/AppData/Local/Temp/' +
          'ccBI#.a02136)'
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
        
          #9'.file'#9'1'#9'"C:/Users/YARUND~1/AppData/Local/Temp/tmpxft_00000c0c_0' +
          '0000000-8_temp.cudafe2.gpu"'
        
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
        #9'.file'#9'15'#9'"c:\cuda\include\texture_fetch_functions.h"'
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
        #9'.file'#9'28'#9'"c:\cuda\include\math_functions_dbl_ptx1.h"'
        #9'.file'#9'29'#9'"C:/Users/YARUND~1/AppData/Local/Temp/temp.cu"'
        ''
        ''
        #9'.entry _Z13scalarProdGPUPfS_S_ii ('
        #9#9'.param .u32 __cudaparm__Z13scalarProdGPUPfS_S_ii_d_C,'
        #9#9'.param .u32 __cudaparm__Z13scalarProdGPUPfS_S_ii_d_A,'
        #9#9'.param .u32 __cudaparm__Z13scalarProdGPUPfS_S_ii_d_B,'
        #9#9'.param .s32 __cudaparm__Z13scalarProdGPUPfS_S_ii_vectorN,'
        #9#9'.param .s32 __cudaparm__Z13scalarProdGPUPfS_S_ii_elementN)'
        #9'{'
        #9'.reg .u16 %rh<3>;'
        #9'.reg .u32 %r<46>;'
        #9'.reg .f32 %f<9>;'
        #9'.reg .pred %p<12>;'
        #9'.shared .align 4 .b8 __cuda_accumResult20[4096];'
        #9'.loc'#9'29'#9'41'#9'0'
        '$LBB1__Z13scalarProdGPUPfS_S_ii:'
        #9'.loc'#9'29'#9'51'#9'0'
        #9'cvt.s32.u16 '#9'%r1, %ctaid.x;'
        #9'mov.s32 '#9'%r2, %r1;'
        
          #9'ld.param.s32 '#9'%r3, [__cudaparm__Z13scalarProdGPUPfS_S_ii_vector' +
          'N];'
        #9'setp.le.s32 '#9'%p1, %r3, %r1;'
        #9'@%p1 bra '#9'$Lt_0_6146;'
        #9'cvt.s32.u16 '#9'%r4, %tid.x;'
        #9'mov.s32 '#9'%r5, 1023;'
        #9'setp.le.s32 '#9'%p2, %r4, %r5;'
        #9'mov.u32 '#9'%r6, 0;'
        #9'setp.eq.u32 '#9'%p3, %r4, %r6;'
        #9'cvt.u32.u16 '#9'%r7, %nctaid.x;'
        #9'mov.u32 '#9'%r8, __cuda_accumResult20;'
        '$Lt_0_6658:'
        
          ' //<loop> Loop body line 51, nesting depth: 1, estimated iterati' +
          'ons: unknown'
        #9'@!%p2 bra '#9'$Lt_0_6914;'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_6658'
        
          #9'ld.param.s32 '#9'%r9, [__cudaparm__Z13scalarProdGPUPfS_S_ii_elemen' +
          'tN];'
        #9'mul24.lo.s32 '#9'%r10, %r9, %r2;'
        #9'add.s32 '#9'%r11, %r10, %r9;'
        #9'mul24.lo.u32 '#9'%r12, %r4, 4;'
        #9'cvt.u32.u16 '#9'%r13, %ntid.x;'
        #9'mul24.lo.u32 '#9'%r14, %r13, 4;'
        #9'add.s32 '#9'%r15, %r10, %r4;'
        #9'add.u32 '#9'%r16, %r12, %r8;'
        #9'add.u32 '#9'%r17, %r8, 4092;'
        '$Lt_0_7426:'
        
          ' //<loop> Loop body line 51, nesting depth: 2, estimated iterati' +
          'ons: unknown'
        #9'.loc'#9'29'#9'64'#9'0'
        #9'mov.s32 '#9'%r18, %r15;'
        #9'setp.le.s32 '#9'%p4, %r11, %r18;'
        #9'mov.f32 '#9'%f1, 0f00000000;     '#9'// 0'
        #9'@%p4 bra '#9'$Lt_0_12290;'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_7426'
        #9'sub.s32 '#9'%r19, %r11, %r18;'
        #9'add.s32 '#9'%r20, %r19, 1023;'
        #9'shr.s32 '#9'%r21, %r20, 31;'
        #9'mov.s32 '#9'%r22, 1023;'
        #9'and.b32 '#9'%r23, %r21, %r22;'
        #9'add.s32 '#9'%r24, %r23, %r20;'
        #9'shr.s32 '#9'%r25, %r24, 10;'
        #9'mul.lo.u32 '#9'%r26, %r18, 4;'
        #9'ld.param.u32 '#9'%r27, [__cudaparm__Z13scalarProdGPUPfS_S_ii_d_A];'
        #9'add.u32 '#9'%r28, %r26, %r27;'
        #9'ld.param.u32 '#9'%r29, [__cudaparm__Z13scalarProdGPUPfS_S_ii_d_B];'
        #9'add.u32 '#9'%r30, %r29, %r26;'
        #9'mul.lo.u32 '#9'%r31, %r11, 4;'
        #9'add.u32 '#9'%r32, %r31, %r27;'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_7426'
        #9'mov.s32 '#9'%r33, %r25;'
        '$Lt_0_8194:'
        
          ' //<loop> Loop body line 64, nesting depth: 3, estimated iterati' +
          'ons: unknown'
        #9'.loc'#9'29'#9'65'#9'0'
        #9'ld.global.f32 '#9'%f2, [%r28+0];'
        #9'ld.global.f32 '#9'%f3, [%r30+0];'
        #9'mad.f32 '#9'%f1, %f2, %f3, %f1;'
        #9'add.u32 '#9'%r30, %r30, 4096;'
        #9'add.u32 '#9'%r28, %r28, 4096;'
        #9'setp.lt.u32 '#9'%p5, %r28, %r32;'
        #9'@%p5 bra '#9'$Lt_0_8194;'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_7426'
        #9'bra.uni '#9'$Lt_0_7682;'
        '$Lt_0_12290:'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_7426'
        '$Lt_0_7682:'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_7426'
        #9'.loc'#9'29'#9'67'#9'0'
        #9'st.shared.f32 '#9'[%r16+0], %f1;'
        #9'add.s32 '#9'%r15, %r18, %r13;'
        #9'add.u32 '#9'%r16, %r16, %r14;'
        #9'setp.le.u32 '#9'%p6, %r16, %r17;'
        #9'@%p6 bra '#9'$Lt_0_7426;'
        '$Lt_0_6914:'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_6658'
        #9'mov.s32 '#9'%r34, 512;'
        '$Lt_0_9474:'
        
          ' //<loop> Loop body line 67, nesting depth: 2, estimated iterati' +
          'ons: unknown'
        #9'.loc'#9'29'#9'75'#9'0'
        #9'bar.sync '#9'0;'
        #9'setp.ge.s32 '#9'%p7, %r4, %r34;'
        #9'@%p7 bra '#9'$Lt_0_9730;'
        ' //<loop> Part of loop body line 67, head labeled $Lt_0_9474'
        #9'mul24.lo.u32 '#9'%r35, %r4, 4;'
        #9'mov.u16 '#9'%rh1, %ntid.x;'
        #9'mul.wide.u16 '#9'%r14, %rh1, 4;'
        #9'mul.lo.u32 '#9'%r36, %r34, 4;'
        #9'add.u32 '#9'%r37, %r35, %r8;'
        #9'add.u32 '#9'%r38, %r36, %r8;'
        #9'add.s32 '#9'%r39, %r36, %r35;'
        #9'add.u32 '#9'%r40, %r39, %r8;'
        '$Lt_0_10242:'
        
          ' //<loop> Loop body line 75, nesting depth: 2, estimated iterati' +
          'ons: unknown'
        #9'.loc'#9'29'#9'77'#9'0'
        #9'ld.shared.f32 '#9'%f4, [%r37+0];'
        #9'ld.shared.f32 '#9'%f5, [%r40+0];'
        #9'add.f32 '#9'%f6, %f4, %f5;'
        #9'st.shared.f32 '#9'[%r37+0], %f6;'
        #9'add.u32 '#9'%r40, %r40, %r14;'
        #9'add.u32 '#9'%r37, %r37, %r14;'
        #9'setp.lt.u32 '#9'%p8, %r37, %r38;'
        #9'@%p8 bra '#9'$Lt_0_10242;'
        '$Lt_0_9730:'
        ' //<loop> Part of loop body line 67, head labeled $Lt_0_9474'
        #9'.loc'#9'29'#9'74'#9'0'
        #9'shr.s32 '#9'%r34, %r34, 1;'
        #9'mov.u32 '#9'%r41, 0;'
        #9'setp.gt.s32 '#9'%p9, %r34, %r41;'
        #9'@%p9 bra '#9'$Lt_0_9474;'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_6658'
        #9'@!%p3 bra '#9'$Lt_0_11010;'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_6658'
        #9'.loc'#9'29'#9'80'#9'0'
        #9'ld.shared.f32 '#9'%f7, [__cuda_accumResult20+0];'
        #9'ld.param.u32 '#9'%r42, [__cudaparm__Z13scalarProdGPUPfS_S_ii_d_C];'
        #9'mul.lo.u32 '#9'%r43, %r2, 4;'
        #9'add.u32 '#9'%r44, %r42, %r43;'
        #9'st.global.f32 '#9'[%r44+0], %f7;'
        '$Lt_0_11010:'
        ' //<loop> Part of loop body line 51, head labeled $Lt_0_6658'
        #9'add.u32 '#9'%r2, %r2, %r7;'
        #9'.loc'#9'29'#9'51'#9'0'
        
          #9'ld.param.s32 '#9'%r3, [__cudaparm__Z13scalarProdGPUPfS_S_ii_vector' +
          'N];'
        #9'.loc'#9'29'#9'80'#9'0'
        #9'setp.lt.s32 '#9'%p10, %r2, %r3;'
        #9'@%p10 bra '#9'$Lt_0_6658;'
        '$Lt_0_6146:'
        #9'.loc'#9'29'#9'82'#9'0'
        #9'exit;'
        '$LDWend__Z13scalarProdGPUPfS_S_ii:'
        #9'} // _Z13scalarProdGPUPfS_S_ii'
        '')
      Compiler = GLSCUDACompiler1
      object scalarProdGPU: TCUDAFunction
        KernelName = '_Z13scalarProdGPUPfS_S_ii'
        BlockShape.SizeX = 128
        Grid.SizeX = 256
        OnParameterSetup = scalarProdGPUParameterSetup
      end
    end
    object deviceA: TCUDAMemData
      MemoryType = mtDevice
      ChannelsType = ctFloat
    end
    object deviceB: TCUDAMemData
      MemoryType = mtDevice
      ChannelsType = ctFloat
    end
    object deviceC: TCUDAMemData
      MemoryType = mtDevice
      ChannelsType = ctFloat
    end
    object hostC_GPU: TCUDAMemData
      ChannelsType = ctFloat
    end
    object hostB: TCUDAMemData
      ChannelsType = ctFloat
    end
    object hostC_CPU: TCUDAMemData
      ChannelsType = ctFloat
    end
    object hostA: TCUDAMemData
      ChannelsType = ctFloat
    end
  end
  object GLSCUDADevice1: TGLSCUDADevice
    Left = 16
    Top = 248
  end
  object GLSCUDACompiler1: TGLSCUDACompiler
    NVCCPath = 'C:\CUDA\bin\'
    CppCompilerPath = 'C:\Program Files\Microsoft Visual Studio 9.0\VC\bin\'
    CodeSourceFile = 'G:\Doc\Projects\CUDA\scalarProd\scalarProd_kernel.c'
    Left = 200
    Top = 248
  end
end

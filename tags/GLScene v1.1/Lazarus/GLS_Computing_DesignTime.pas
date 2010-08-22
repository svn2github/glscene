{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLS_Computing_DesignTime; 

interface

uses
    GLS_CL, GLS_CL_GL, GLS_CL_Platform, GLS_CUDA_API, 
  GLS_CUDA_FastFourierTransformation, GLS_CUDA_ParallelPrimitives, 
  GLS_CUDA_Runtime, GLS_CUDA_Utility, GLSComputingRegister, GLSCUDAEditor, 
  GLSCUDA, GLSCUDACompiler, GLSCUDAContext, GLSCUDAFFTPlan, GLSCUDAGraphics, 
  GLSCUDAParser, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSComputingRegister', @GLSComputingRegister.Register); 
end; 

initialization
  RegisterPackage('GLS_Computing_DesignTime', @Register); 
end.

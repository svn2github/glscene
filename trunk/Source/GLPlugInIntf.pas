//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  An interface unit to GLScene plug-ins. 
  For more information see help file for writing plug-ins. 

   History :  
   28/07/01 - EG - Creation
   The whole history is logged in previous version of the unit
   
}

unit GLPlugInIntf;

interface

{$I GLScene.inc}

type
  TPIServiceType = (stRaw, stObject, stBitmap, stTexture, stImport, stExport);
  TPIServices = set of TPIServiceType;

  TEnumCallBack = procedure(Name: PAnsiChar); stdcall;

  TEnumResourceNames = procedure(Service: TPIServiceType;
    Callback: TEnumCallBack); stdcall;
  TGetServices = function: TPIServices; stdcall;
  TGetVendor = function: PAnsiChar; stdcall;
  TGetDescription = function: PAnsiChar; stdcall;
  TGetVersion = function: PAnsiChar; stdcall;

implementation

end.

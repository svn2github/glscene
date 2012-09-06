{
 *****************************************************************************
 *                         CustomDrawnLazDeviceAPIS.pas                      *
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CustomDrawnWSLazDeviceAPIS;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

uses
  // RTL
  Types, Math,
  {$ifdef CD_Android}
  jni,
  {$endif}
  // LCL
  LazDeviceAPIs, LCLProc,
  // Widgetset
  customdrawnint, WSLCLClasses, WSLazDeviceAPIs;

type
  { TWSLazDeviceAPIS }
  
  { TCDWSLazDeviceAPIs }

  TCDWSLazDeviceAPIs = class(TWSLazDeviceAPIs)
  public
    //
    class procedure RequestPositionInfo(AMethod: TLazPositionMethod); override;
    //
    class procedure SendMessage(AMsg: TLazDeviceMessage); override;
    //
    class procedure StartReadingAccelerometerData(); override;
    class procedure StopReadingAccelerometerData(); override;
    // TLazDevice
    class function GetDeviceManufacturer: string; override;
    class function GetDeviceModel: string; override;
    class procedure Vibrate(ADurationMS: Cardinal); override;
  end;

implementation

{ TCDWSLazDeviceAPIs }

{$if defined(CD_Windows) or defined(CD_Cocoa) or defined(CD_X11)}
class procedure TCDWSLazDeviceAPIs.RequestPositionInfo(
  AMethod: TLazPositionMethod);
begin

end;

class procedure TCDWSLazDeviceAPIs.SendMessage(AMsg: TLazDeviceMessage);
begin

end;

class procedure TCDWSLazDeviceAPIs.StartReadingAccelerometerData;
begin

end;

class procedure TCDWSLazDeviceAPIs.StopReadingAccelerometerData;
begin

end;

class function TCDWSLazDeviceAPIs.GetDeviceManufacturer: string;
begin
  Result := '';
end;

class function TCDWSLazDeviceAPIs.GetDeviceModel: string;
begin
  Result := '';
end;

class procedure TCDWSLazDeviceAPIs.Vibrate(ADurationMS: Cardinal);
begin

end;

{$endif}

{$ifdef CD_Android}
class procedure TCDWSLazDeviceAPIs.RequestPositionInfo(
  AMethod: TLazPositionMethod);
var
  lPositionMethod: jint;
begin
{  // Prepare the input
  case AMethod of
    pmGPS: lPositionMethod := 1;
    pmNetwork: lPositionMethod := 2;
  else
    Exit;
  end;
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclkind, lPositionMethod);
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoRequestPositionInfo);}
end;

class procedure TCDWSLazDeviceAPIs.SendMessage(AMsg: TLazDeviceMessage);
var
  lJavaString: jstring;
  lStr: String;
begin
  // Prepare the input
  // String fields
{  lStr := AMsg.Body;
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  lStr := AMsg.destinationAddress.Text;
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcldestination, lJavaString);
  // Message type
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclkind, 1);
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoSendMessage);}
end;

class procedure TCDWSLazDeviceAPIs.StartReadingAccelerometerData;
begin
  // Call the method
  javaVMRef.Env.CallVoidMethod(javaActivityObject,javaActivityClass, 'LCLDoStartReadingAccelerometer', '()V');
end;

class procedure TCDWSLazDeviceAPIs.StopReadingAccelerometerData;
begin
  // Call the method
  javaVMRef.Env.CallVoidMethod(javaActivityObject,javaActivityClass, 'LCLDoStopReadingAccelerometer', '()V');
end;

class function TCDWSLazDeviceAPIs.GetDeviceManufacturer: string;
var
  lFieldID: JFieldID;
  lJavaString: JString;
  lNativeString: PChar;
  iscopy: JBoolean;
begin
  with javaVMRef.Env do
  begin
  lFieldID := GetStaticFieldID( javaAndroidOSBuildClass, 'MANUFACTURER', 'Ljava/lang/String;');
  lJavaString := JString(GetStaticObjectField(javaAndroidOSBuildClass, lFieldID));
  lNativeString := GetStringUTFChars(lJavaString, iscopy);
  Result := lNativeString;
  ReleaseStringUTFChars(lJavaString, lNativeString);
  end;
end;

class function TCDWSLazDeviceAPIs.GetDeviceModel: string;
var
  lFieldID: JFieldID;
  lJavaString: JString;
  lNativeString: PChar;
  iscopy: JBoolean;
begin
  with javaVMRef.Env do
  begin
  lFieldID := GetStaticFieldID(javaAndroidOSBuildClass, 'MODEL', 'Ljava/lang/String;');
  lJavaString := JString(GetStaticObjectField(javaAndroidOSBuildClass, lFieldID));
  lNativeString := GetStringUTFChars(lJavaString, iscopy);
  Result := lNativeString;
  ReleaseStringUTFChars(lJavaString, lNativeString);
  end;
end;

class procedure TCDWSLazDeviceAPIs.Vibrate(ADurationMS: Cardinal);
var
  lVibratorObject: JObject;
  javaMethod_vibrate: JMethodID;
  javaString_VIBRATOR_SERVICE: JString;
  // array for the parameters
  lParams: array[0..0] of JValue;
const
  javaConstant_VIBRATOR_SERVICE = 'vibrator';
begin
  with javaVMRef.Env do
  begin
  // First IDs
  javaMethod_vibrate := GetMethodID(javaAndroidOSVibratorClass, 'vibrate', '(J)V');

  // get the string Context.VIBRATOR_SERVICE remember that NewStringUTF does not require ReleaseStringUTFChars
  javaString_VIBRATOR_SERVICE := NewStringUTF( pchar(javaConstant_VIBRATOR_SERVICE));

  // Get the vibrator object
  // Vibrator v = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);
  lParams[0].l := javaString_VIBRATOR_SERVICE;
  lVibratorObject := CallObjectMethodA(javaActivityObject, javaMethod_getSystemService, @lParams[0]);

  // Now call the method from the vibrator object
  lParams[0].j := ADurationMS;
  CallVoidMethodA(lVibratorObject, javaMethod_Vibrate, @lParams[0]);
  end;
end;
{$endif}

end.

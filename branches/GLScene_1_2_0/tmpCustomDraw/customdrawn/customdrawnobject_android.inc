{%MainUnit customdrawnint.pas}

{******************************************************************************
                                   customdrawnobject_win.inc
 ******************************************************************************

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

const
  // Shared Mouse / Key constants
  ACTION_DOWN = 0;
  ACTION_UP = 1;

  // Touch constants
  ACTION_MOVE = 2;

  // Constants from android/view/KeyEvent up to level 8
  ACTION_MULTIPLE = 2;
  // Keys are in KeyCodes.pas

  // from android.view.KeyCharacterMap
  COMBINING_ACCENT = $80000000;

function Java_com_pascal_lclproject_LCLActivity_LCLOnTouch(env:PJNIEnv;this:jobject; x, y: single; action: jint): jint; cdecl;
var
  lCurForm: TCDNonNativeForm;
  lTarget: TWinControl;
  lEventPos: TPoint;
begin
  {$ifdef VerboseCDEvents}
  __android_log_write(ANDROID_LOG_INFO,'lclapp',PChar(Format('LCLOnTouch called x=%f y=%f action=%d', [x, y, action])));
  {$endif}
  eventResult := 0;

  lCurForm := GetCurrentForm();

  case action of
    ACTION_DOWN:
    begin
      CallbackMouseDown(lCurForm, Round(X), Round(Y), mbLeft, []);
    end;
    ACTION_UP:
    begin
      CallbackMouseUp(lCurForm, Round(X), Round(Y), mbLeft, []);
    end;
    ACTION_MOVE: CallbackMouseMove(lCurForm, Round(X), Round(Y), []);
  end;

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnDraw(
    env:PJNIEnv;this:jobject; width, height: jint): jint; cdecl;
var
  pixels: PCardinal;
  lCurForm: TCDNonNativeForm;

  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart: TDateTime;
  {$ENDIF}
begin
  Result := 0;
 // AndroidBitmap_lockPixels(env, abitmap, @pixels);
  lCurForm := GetCurrentForm();
  if lCurForm <> nil then
  begin
    {$IFDEF VerboseCDPaintProfiler}
    //lTimeStart := NowUTC();
    {$ENDIF}
    {$IFDEF VerboseCDPaintEvent}
    DebugLn(Format('[Java_com_pascal_lclproject_LCLActivity_LCLOnDraw] lCurForm:TCDNonNativeForm=%x', [PtrInt(lCurForm)]));
    {$ENDIF}
    CallbackDraw(lCurForm);
    // Prepare the non-native image and canvas
  //  UpdateControlLazImageAndCanvas(lCurForm.Image, lCurForm.Canvas, Width, Height, clfRGBA32, pixels, True, False);

   // RenderForm(lCurForm.Image, lCurForm.Canvas, lCurForm.LCLForm);
  end;

  // Now returns the bitmap buffer to LCLActivity so that it can render it
//  AndroidBitmap_unlockPixels(env, abitmap);
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnCreate(
    env:PJNIEnv; this:jobject; alclactivity: jobject): jint; cdecl;
begin
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', 'LCLOnCreate called by LCLActivity.onCreate');
  Result := 0;

  with javaVMRef.Env do
  begin
  //Кэшируем указатель на класс Activity что бы можно было его
  //использовать в паралельном потоке
  //http://stackoverflow.com/questions/6838397/how-do-i-load-my-own-java-class-in-c-on-android
  if javaActivityObject<>nil then
  DeleteGlobalRef(javaActivityObject);
  javaActivityObject := alclactivity;
  NewGlobalRef(javaActivityObject);
  end;

  Screen.UpdateScreen(); // Any values read before LCLOnCreate are wrong
  // Update the font size
  CDWidgetset.DefaultFontAndroidSize := Round(16 * (Screen.PixelsPerInch / 125));
  // Now inform the application
  if Assigned(CDWidgetset.ActivityOnCreate) then CDWidgetset.ActivityOnCreate()
  else Application.Run; // <- Support for older code up to 21 dezember
end;

// This one is for all simple dialogs: MessageBox, PromptUser (MessageDlg) and AskUser
function Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished(
    env:PJNIEnv; this:jobject; AResult: jint): jint; cdecl;
begin
  //__android_log_write(ANDROID_LOG_INFO, 'lclapp', 'LCLOnCreate called by LCLActivity.onCreate');
  Result := 0;
  if Assigned(Application.OnMessageDialogFinished) then
    Application.OnMessageDialogFinished(Application, AResult);
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnKey(
    env:PJNIEnv; this:jobject; AKind: jint; AKeyCode: jint;
    AEvent: jobject; AChar: jint): jint; cdecl;
var
  lCurForm: TCDNonNativeForm;
  lTarget, lFocusedControl: TWinControl;
  lKey: Word;
  lCombinedAChar: jint;
  lChar: Cardinal;
  AUTF8Text: string;
  AUTF8Char: TUTF8Char;
  lForm: TCDNonNativeForm;
begin
  lChar := Cardinal(AChar);
  {$ifdef VerboseCDEvents}
  __android_log_write(ANDROID_LOG_INFO,'lclapp',PChar(
    Format('[LCLOnKey] called AKind=%d AKeyCode=%x AChar=%s', [AKind, AKeyCode, UnicodeToUTF8(lChar)])));
  {$endif}
  eventResult := 0;

  lCurForm := GetCurrentForm();
  lKey := CDWidgetset.AndroidKeyCodeToLCLKeyCode(AKeyCode);

  case AKind of
    ACTION_DOWN: CallbackKeyDown(lCurForm, lKey);
    -1: // This indicates a key char event
    begin
      AUTF8Text := UnicodeToUTF8(lChar);
      AUTF8Char := AUTF8Text;
      CallbackKeyChar(lCurForm, lKey, AUTF8Char);
    end;
    ACTION_UP:
    begin
      CallbackKeyUp(lCurForm, lKey);
(*      if (lChar <> 0) and ((COMBINING_ACCENT and lChar) = 0) then
      begin
        if CDWidgetset.CombiningAccent <> 0 then
        begin
          // Prepare the input
          lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, Str);
          javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
          javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltextsize, lFontSize);

          // Call the method
          javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoGetTextBounds);

          // Read the output
          Size.cx := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclwidth);
          Size.cy := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclheight);
          //
          lCombinedAChar := getDeadChar();
        end;
        AUTF8Text := UnicodeToUTF8(lChar);
        AUTF8Char := AUTF8Text;
        CallbackKeyChar(lCurForm, lKey, AUTF8Char);
        CDWidgetset.CombiningAccent := 0;
      end
      else if (lChar <> 0) and ((COMBINING_ACCENT and lChar) <> 0) then
      begin
        CDWidgetset.CombiningAccent := lChar;
      end;*)

      // Handle the Back hardware key
      if AKeyCode = AKEYCODE_BACK then
      begin
        //DebugLn(Format('CallbackKeyUp D lForm=%x', [PtrInt(lForm)]));
        // The back hardware key hides the current form and shows the one bellow it
        // except if the currently focused control is a text editor, in which case
        // it will take focus out of the text editor
        lForm := lCurForm;
        lFocusedControl := lForm.GetFocusedControl();
        if (lFocusedControl <> nil) and (csRequiresKeyboardInput in lFocusedControl.ControlStyle) then
        begin
          //DebugLn('[LCLOnKey] Sending focus to the form');
          CDWidgetset.CDSetFocusToControl(lForm.LCLForm, nil);
        end
        // If this is the main form, then go back to desktop
        else if (Application.MainForm <> nil) and (lForm = TCDForm(Application.MainForm.Handle)) then
        begin
          //DebugLn('[LCLOnKey] Back key is going to hide the application');
          eventResult := eventResult or 2;
        end
        // for other forms, hide them
        else
        begin
          //DebugLn('[LCLOnKey] Hiding the form');
          HideForm(lForm);
        end;
      end;
    end;
    //ACTION_MULTIPLE:
  end;

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnTimer(
    env:PJNIEnv; this:jobject; ATimer: jobject): jint; cdecl;
var
  lTimer: TCDTimer;
begin
  {$ifdef VerboseCDEvents}
  __android_log_write(ANDROID_LOG_INFO,'lclapp',PChar(
    Format('LCLOnTimer called ATimer=%x', [PtrInt(ATimer)])));
  {$endif}
  eventResult := 0;

  lTimer := FindTimerWithNativeHandle(PtrInt(ATimer));

  if lTimer <> nil then lTimer.TimerFunc()
  else DebugLn('[LCLOnTimer] OnTimer message sent to unknown timer!');

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged(
  env:PJNIEnv; this:jobject; ANewxDPI,ANewyDPI, ANewWidth,ANewHeight: jint): jint; cdecl;
var
  lForm: TCDNonNativeForm;
  lOldDPI, lNewDPI, lOldFormWidth, lNewFormWidth,lOldFormHeight, lNewFormHeight: Integer;
  i: Integer;
begin
  for i := 0 to GetFormCount()-1 do
  begin
    lForm := GetForm(i);

    if lForm.LayoutAutoAdjusted then lOldDPI := Screen.PixelsPerInch
    else lOldDPI := lForm.LCLForm.DesignTimeDPI;
    lNewDPI := ANewxDPI;
    lOldFormWidth := lForm.LCLForm.Width;
    lNewFormWidth := ANewWidth;
    lOldFormHeight := lForm.LCLForm.Height;
    lNewFormHeight := ANewHeight;
    DebugLn(Format('[LCLOnConfigurationChanged] i=%d lOldDPI=%d lNewDPI=%d lOldFormWidth=%d lNewFormWidth=%d  lOldFormHeight=%d lNewFormHeight=%d',
      [i, lOldDPI, lNewDPI, lOldFormWidth, lNewFormWidth,lOldFormHeight, lNewFormHeight]));
   // lForm.LCLForm.AutoAdjustLayout(lapAutoAdjustWithoutHorizontalScrolling,
    //  lOldDPI, lNewDPI, lOldFormWidth, lNewFormWidth);
    lForm.LCLForm.DesignTimeDPI := ANewxDPI;
    lForm.LCLForm.SetBounds(0,0,lNewFormWidth,lNewFormHeight);
  end;

  // Update the font size
  CDWidgetset.DefaultFontAndroidSize := Round(16 * (ANewxDPI / 125));
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged(
  env:PJNIEnv; this:jobject; ASensorKind: jint; AValues: JDoubleArray): jint; cdecl;
var
  arraydata: PDouble;
  arraylen: jsize;
  lIsCopy: jboolean;
  lSensorDataInt: Integer;
  lMessagingStatus: TLazMessagingStatus;
  lUnixTimeStamp: Int64;
begin
  Result := 0;
  with javaVMRef.Env do
  begin

  // Get the elements and length
  lIsCopy := 0;
  arraylen := GetArrayLength( AValues);
  arraydata := GetDoubleArrayElements(AValues, lIsCopy);

  // Send the data to the LCL
  case ASensorKind of
    -11: // Defined by ourselves for Messaging Status
    begin
      lSensorDataInt := Round(arraydata[0]);
      case lSensorDataInt of
        1: lMessagingStatus := mssSentSuccessfully;
        2: lMessagingStatus := mssSendingGeneralError;
        3: lMessagingStatus := mssNoService;
        5: lMessagingStatus := mssRadioOff;
        10:lMessagingStatus := mssReceivedSuccessfully;
        11:lMessagingStatus := mssReceivingGeneralError;
      else
        lMessagingStatus := mssSendingGeneralError;
      end;

      if Assigned(Messaging.OnMessagingStatus) then
        Messaging.OnMessagingStatus(nil, lMessagingStatus);
    end;
    -10: // Defined by ourselves for PositionInfo
    begin
      PositionInfo.latitude := arraydata[0];
      PositionInfo.longitude := arraydata[1];
      PositionInfo.altitude := arraydata[2];
      PositionInfo.accuracy := arraydata[3];
      PositionInfo.altitudeAccuracy := PositionInfo.accuracy;
      PositionInfo.speed := arraydata[4];
      lUnixTimeStamp := Round(arraydata[5]);
      PositionInfo.timeStamp :=  UnixToDateTime(lUnixTimeStamp);

      if Assigned(PositionInfo.OnPositionRetrieved) then
        PositionInfo.OnPositionRetrieved(PositionInfo);
    end;
    1: // ACCELEROMETER
    begin
      Accelerometer.xaxis := -1*arraydata[0];
      Accelerometer.yaxis := -1*arraydata[1];
      Accelerometer.zaxis := -1*arraydata[2];
      if Assigned(Accelerometer.OnSensorChanged) then
        Accelerometer.OnSensorChanged(Accelerometer);
    end;
  end;

  // Don't forget to release it
  ReleaseDoubleArrayElements( AValues, arraydata, 0);

  // This sends messages like Invalidate requests
  Result := eventResult;
  end;
end;

procedure Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceCreated(); cdecl;
begin
 // DebugLn('LCLOnSurfaceCreated()');
  if Assigned(CDWidgetset.OnSurfaceCreated) then
    CDWidgetset.OnSurfaceCreated(nil);
end;

procedure Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceDestroyed(); cdecl;
begin
 // DebugLn('LCLOnSurfaceDestroyed()');
  if Assigned(CDWidgetset.OnSurfaceDestroyed) then
    CDWidgetset.OnSurfaceDestroyed(nil);
end;

procedure Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceChanged(); cdecl;
begin
 // DebugLn('LCLOnSurfaceChanged()');
  if Assigned(CDWidgetset.OnSurfaceChanged) then
    CDWidgetset.OnSurfaceChanged(nil);
end;

const NativeMethods: array[0..10] of JNINativeMethod=
  ((name:'LCLOnDraw';
    signature:'(II)I';//(IILandroid/graphics/Bitmap;)I
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnDraw;),
   (name:'LCLOnTouch';
    signature:'(FFI)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnTouch;),
   (name:'LCLOnCreate';
    signature:'(Lcom/pascal/lcltest/LCLActivity;)I'; //android/app/Activity;
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnCreate;),
   (name:'LCLOnMessageBoxFinished';
    signature:'(I)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished;),
   (name:'LCLOnKey';
    signature:'(IILandroid/view/KeyEvent;I)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnKey;),
   (name:'LCLOnTimer';
    signature:'(Ljava/lang/Runnable;)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnTimer;),
   (name:'LCLOnConfigurationChanged';
    signature:'(IIII)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged;),
   (name:'LCLOnSensorChanged';
    signature:'(I[D)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged;),

   (name:'LCLOnSurfaceCreated';
     signature:'()V';
     fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceCreated;),
   (name:'LCLOnSurfaceDestroyed';
     signature:'()V';
     fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceDestroyed;),
   (name:'LCLOnSurfaceChanged';
     signature:'()V';
     fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnSurfaceChanged;)
  );

{function javaEnvRef():PJNIEnv; //{$IFDEF GLS_INLINE} inline; {$ENDIF}
begin
 // DebugLn('GetCurrentThreadID='+ inttostr(GetCurrentThreadId));
     Result := fArrayEnvRefLayerThread[GetEnvIDForCurrentThread].EnvID;
end;        }

function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; cdecl;
begin
  javaVMRef:= TThreadJavaVM.Create(vm);
  //javaVMRef := vm;


  __android_log_write(ANDROID_LOG_INFO, 'lclapp', 'JNI_OnLoad called');
{  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(Format('vm=%x', [PtrInt(vm)])));
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(Format('vm^=%x', [PtrInt(vm^)])));
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(Format('vm^^.reserved0=%x', [PtrInt(vm^^.reserved0)])));
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(Format('vm^^.GetEnv=%x', [PtrInt(Pointer(@vm^^.GetEnv))])));  }
  {
    vm^^.GetEnv crashes HTC Wildfire, Alcatel and the Emulator if you don't build your project with -CpARMV6
    see: http://groups.google.com/group/android-ndk/browse_thread/thread/ba542483f062a828/ef9077617794e0f5
  }
  if javaVMRef.GetEnv(javaEnv,JNI_VERSION_1_4)<>JNI_OK then
  begin
    __android_log_write(ANDROID_LOG_FATAL, 'lclapp', 'curVM^.GetEnv failed');
    Exit(JNI_ERR);
  end;

 // javaVMRef.SetEnv(defaultjavaEnvRef);
{
    //задаем длинну массива для текущего потока
  setlength(fArrayEnvRefLayerThread,1);
  //задаем длинну для переменных текущего потока
  setlength(fArrayEnvRefLayerThread[0].variablesid, Ord(High(javavariablesid)) + 1);
  fArrayEnvRefLayerThread[0].EnvID := defaultjavaEnvRef;
  fArrayEnvRefLayerThread[0].ThreadID := GetCurrentThreadId;
  IdCurrentEnvInArray := -1;    }
  with javaEnv do
  begin
  // Find our activity class
  __android_log_write(ANDROID_LOG_INFO,'lclapp','Reading our Activity Class');

  javaActivityClass := NewGlobalRef((FindClass('com/pascal/lcltest/LCLActivity')));

  if not assigned(javaActivityClass) then
  begin
    __android_log_write(ANDROID_LOG_FATAL, 'lclapp', 'javaEnvRef^.FindClass failed');
    Exit(JNI_ERR);
  end;
  // Now other classes
  javaAndroidAppActivityClass := FindClass('android/app/Activity');
  javaJavaLandSystemClass := FindClass('java/lang/System');
  javaAndroidOSBuildClass := FindClass('android/os/Build');
  javaAndroidOSVibratorClass := FindClass('android/os/Vibrator');
  javaAndroidContentContextClass := FindClass('android/content/Context');


  // Register Pascal exported calls
  if RegisterNatives(javaActivityClass, @NativeMethods[0],length(NativeMethods))<0 then
  begin
    __android_log_write(ANDROID_LOG_FATAL, 'lclapp', 'javaEnvRef^.RegisterNatives failed');
    // Exit(JNI_ERR); Don't exit if exporting the native method fails because it works without this too
  end;

    //GPS and SMS не нужны сцене
//  javaMethod_LCLDoSendMessage := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoSendMessage', '()V');
//  javaMethod_LCLDoRequestPositionInfo := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoRequestPositionInfo', '()V');
  // Methods from android.app.Activity
  javaMethod_Activity_finish := GetMethodID(javaAndroidAppActivityClass, 'finish', '()V');
  // Methods from java.lang.System
  javaMethod_System_exit := GetStaticMethodID( javaJavaLandSystemClass, 'exit', '(I)V');
  // Generic methods from Context
  javaMethod_getSystemService := GetMethodID(javaAndroidContentContextClass, 'getSystemService', '(Ljava/lang/String;)Ljava/lang/Object;');

  end;

//  FillArrayJavaIDPointers(javaEnvRef);

  __android_log_write(ANDROID_LOG_INFO, 'lclapp', 'JNI_OnLoad finished');
  result:=JNI_VERSION_1_4;// 1_6 is another option
end;

procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;
begin
  javaEnv.DeleteGlobalRef(javaActivityClass);
  freeandnil(javaEnv);
  freeandnil(javaVMRef);
end;

procedure TCDWidgetSet.AttachCurrentThread();
begin
  javaVMRef.AttachCurrentThread(nil);
end;

procedure TCDWidgetSet.DetachCurrentThread();
begin
  javaVMRef.DetachCurrentThread;
end;



function TCDWidgetSet.CreateContext(aconfig : EGLConfig; shareRC: EGLContext):EGLContext;
var
  context: EGLContext ;
begin
  with javaVMRef.Env do
  begin
  context  := EGLContext(CallObjectMethodA(javaActivityObject,
  javaActivityClass,
  'LCLDoCreateContext',
  '(Ljavax/microedition/khronos/egl/EGLConfig;Ljavax/microedition/khronos/egl/EGLContext;)Ljavax/microedition/khronos/egl/EGLContext;',
  [aconfig,shareRC]));
   //Делаем указатель глобальным
   // JNI WARNING: 0x40522160 is not a valid JNI reference
   //http://java.sun.com/docs/books/jni/html/refs.html#27423
   //иначе ява его забудет
  result := NewGlobalRef(context);
  end;
end;

function TCDWidgetSet.DestroyContext(aContext:EGLContext): EGLint;
begin
  with javaVMRef.Env do
  begin
    // Call the method
  result := EGLint(CallIntMethodA(javaActivityObject,
                       javaActivityClass,
                       'LCLDoDestroyContext',
                       '(Ljavax/microedition/khronos/egl/EGLContext;)I',
                       [aContext]));
  DeleteGlobalRef(acontext);
  end;
end;

function TCDWidgetSet.CreateSurface(aconfig : EGLConfig): EGLSurface;
  var
  Surface: EGLSurface ;
begin
    // Call the method
  with javaVMRef.Env do
  begin
  Surface  := EGLSurface(CallObjectMethodA( javaActivityObject,
                         javaActivityClass,
                         'LCLDoCreateSurface',
                         '(Ljavax/microedition/khronos/egl/EGLConfig;)Ljavax/microedition/khronos/egl/EGLSurface;'
                         ,[aconfig]));
  result := NewGlobalRef(Surface);
  end;
end;

procedure TCDWidgetSet.DestroySurface(aSurface : EGLSurface);
begin
  with javaVMRef.Env do
  begin
  // Call the method
   CallVoidMethodA(javaActivityObject,javaActivityClass, 'LCLDoDestroySurface',
                   '(Ljavax/microedition/khronos/egl/EGLSurface;)V',[aSurface]);
   DeleteGlobalRef(aSurface);
  end;
end;

procedure TCDWidgetSet.PurgeBuffers(asurface : EGLSurface; acontext : EGLContext);
var
Surface: EGLSurface ;
begin
  // Call the method
  with javaVMRef.Env do
  begin
   CallVoidMethodA( javaActivityObject,javaActivityClass,
                    'LCLDoPurgeBuffers', '(Ljavax/microedition/khronos/egl/EGLSurface;Ljavax/microedition/khronos/egl/EGLContext;)V',
                    [asurface,acontext]);
  end;
end;

procedure TCDWidgetSet.ClearBuffers;
begin
  with javaVMRef.Env do
  begin
  CallVoidMethod(javaActivityObject, javaActivityClass,
  'LCLDoClearBuffers', '()V');
  end;
end;

function TCDWidgetSet.SwapBuffers(asurface : EGLSurface): boolean;
begin
  // Call the method
   with javaVMRef.Env do
   begin
     result := Boolean(CallBooleanMethodA( javaActivityObject, javaActivityClass,
               'LCLDoSwapBuffers', '(Ljavax/microedition/khronos/egl/EGLSurface;)Z',
               [aSurface]));
   end;
end;

function TCDWidgetSet.eglGetError: EGLint;
begin
  with javaVMRef.Env do
  begin
  result := EGLint(CallIntMethod(javaActivityObject, javaActivityClass, 'LCLeglGetError', '()I'));
  end;
end;

procedure TCDWidgetSet.SetContextClientVersion2;
var javaField_mEGLContextClientVersion: JfieldID=nil;
begin
  with javaVMRef.Env do
  begin
  javaField_mEGLContextClientVersion := GetFieldID( javaActivityClass,  'mEGLContextClientVersion', 'I');
  SetIntField( javaActivityObject, javaField_mEGLContextClientVersion, 2);
  end;
end;

procedure TCDWidgetSet.SetFullScreenMode(fs: EGLBoolean);
begin
  // Call the method
  with javaVMRef.Env do
  begin
   CallVoidMethodA( javaActivityObject,javaActivityClass, 'SetFullScreen', '(Z)V',[fs]);
  end;
end;

function TCDWidgetSet.SetScreenOrientation(so: EGLint): boolean;
begin
  // Call the method
  with javaVMRef.Env do
  begin
   Result:=Boolean(CallBooleanMethodA(javaActivityObject,javaActivityClass, 'SetScreenOrientation', '(I)Z',[so]));
  end;
end;


function TCDWidgetSet.eglGetConfigs(): EGLint;
begin
  with javaVMRef.Env do
  begin
    Result := EGLint(CallIntMethod(javaActivityObject,javaActivityClass,'LCLGetConfigs', '()I'));
  end;
end;

function TCDWidgetSet.eglGetFixedAttribute(attribute: EGLint; Param: EGLint): EGLint;
begin
    // Call the method
  with javaVMRef.Env do
  begin
    Result := EGLint(CallIntMethodA(javaActivityObject,javaActivityClass, 'LCLGetFixedAttribute', '(II)I',[attribute,Param]));
  end;
end;

procedure TCDWidgetSet.eglAddIAttrib(attribute: EGLint; value: EGLint);
begin
  // Call the method
  with javaVMRef.Env do
  begin
    CallVoidMethodA(javaActivityObject, javaActivityClass, 'LCLAddIAttrib', '(II)V',[attribute,value]);
  end;
end;

function TCDWidgetSet.eglChooseConfig: EGLConfig;
begin
  with javaVMRef.Env do
  begin
    result := EGLConfig(CallObjectMethod(javaActivityObject,javaActivityClass,'LCLChooseConfig', '()Ljavax/microedition/khronos/egl/EGLConfig;'));
  end;
end;

procedure TCDWidgetSet.GeteglVersion;
var
  javaField_lclmajorversion: JfieldID=nil;
  javaField_lclminorversion: JfieldID=nil;
begin
  with javaVMRef.Env do
  begin
  javaField_lclmajorversion:= GetFieldID( javaActivityClass,   'lclmajorversion', 'I');
  javaField_lclminorversion:= GetFieldID( javaActivityClass,   'lclminorversion', 'I');
  self.MajorVersion := Integer(GetIntField( javaActivityObject, javaField_lclmajorversion));
  self.MinorVersion := Integer(GetIntField( javaActivityObject, javaField_lclminorversion));
  end;
end;

procedure TCDWidgetSet.GetPlatformInfo();
var
  lJavaString : Jstring;
  javaField_lclplatformversion: JfieldID=nil;
  javaField_lclplatformapi: JfieldID=nil;
  javaField_lclplatformdevice: JfieldID=nil;
begin
  with javaVMRef.Env do
  begin
    javaField_lclplatformversion:= GetFieldID( javaActivityClass,   'lclplatformversion', 'Ljava/lang/String;');
    lJavaString :=GetObjectField(javaActivityObject, javaField_lclplatformversion);
    PlatformInfo[0] :=JStringToString(lJavaString);

    javaField_lclplatformapi:= GetFieldID( javaActivityClass,   'lclplatformapi', 'Ljava/lang/String;');
    lJavaString :=GetObjectField( javaActivityObject, javaField_lclplatformapi);
    PlatformInfo[1] :=JStringToString(lJavaString);

    javaField_lclplatformdevice:= GetFieldID( javaActivityClass,   'lclplatformdevice', 'Ljava/lang/String;');
    lJavaString :=GetObjectField(javaActivityObject, javaField_lclplatformdevice);
    PlatformInfo[2] :=JStringToString(lJavaString);
  end;

end;

procedure TCDWidgetSet.LCLDoInvalidate;
var  lCurForm: TCDNonNativeForm;
begin
  with javaVMRef.Env do
  begin
    CallVoidMethod(javaActivityObject,javaActivityClass, 'LCLDoInvalidate', '()V');
  end;
end;

procedure TCDWidgetSet.LCLDoCreateHolder;
begin
  with javaVMRef.Env do
  begin
    CallVoidMethod(javaActivityObject,javaActivityClass, 'LCLDoCreateHolder', '()V');
  end;
end;

procedure TCDWidgetSet.LCLDoDestroyHolder;
begin
  with javaVMRef.Env do
  begin
    CallVoidMethod(javaActivityObject,javaActivityClass, 'LCLDoDestroyHolder', '()V');
  end;
end;

procedure TCDWidgetSet.LCLDoRecreateHolder;
begin
  with javaVMRef.Env do
  begin
    CallVoidMethod(javaActivityObject,javaActivityClass, 'LCLDoRecreateHolder', '()V');
  end;
end;

function TCDWidgetSet.LCLisHolderCreated: boolean;
begin
  with javaVMRef.Env do
  begin
    result := Boolean(CallBooleanMethod( javaActivityObject,javaActivityClass, 'LCLisHolderCreated', '()Z'));
  end;
end;

procedure TCDWidgetSet.SetTitleBar(aStr: string);
var
vstr: JString;
begin
  with javaVMRef.Env do
  begin
    vstr := NewStringUTF(pchar(aStr));
  // Call the method
    CallVoidMethodA( javaActivityObject,javaActivityClass, 'LCLSetTitleBar', '(Ljava/lang/String;)V',[vStr]);
   // ReleaseStringUTFChars(vstr,pchar(aStr));
  end;
end;

function TCDWidgetSet.JavaLoadlib(aLib: string) : boolean;
var
  vstr: JString;
begin
  with javaVMRef.Env do
  begin
    vstr := NewStringUTF(pchar(aLib));
    // Call the method
     Result := Boolean(CallBooleanMethodA( javaActivityObject,javaActivityClass, 'LCLLoadLibrary', '(Ljava/lang/String;)Z',[vstr]));
     ReleaseStringUTFChars(vstr,pchar(aLib));

  end;

end;

{$IFnDEF WithOldDebugln}

procedure TCDWidgetSet.AndroidDebugLn(ASender: TObject; AStr: string; var AHandled: Boolean);
begin
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(AccumulatedStr+AStr));
  AccumulatedStr := '';
  AHandled := True;
end;

{$ELSE}
procedure TCDWidgetSet.AndroidDebugLn(AStr: string);
begin
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(AccumulatedStr+AStr));
  AccumulatedStr := '';
end;
{$ENDIF}

function TCDWidgetSet.AndroidKeyCodeToLCLKeyCode(AAndroidKeyCode: Integer): Word;
var
  lExtendedKeysSupport: Boolean;
begin
  lExtendedKeysSupport := Application.ExtendedKeysSupport;

  case AAndroidKeyCode of
    // First keys which are defined in NDK too
{    KEYCODE_SOFT_LEFT = 1;
    KEYCODE_SOFT_RIGHT = 2;
    KEYCODE_HOME = 3;}
    KEYCODE_BACK: Result := VK_ESCAPE;
    KEYCODE_CALL: Result := VK_LCL_CALL;
    KEYCODE_ENDCALL: Result := VK_LCL_ENDCALL;
    KEYCODE_0: Result := VK_0;
    KEYCODE_1: Result := VK_1;
    KEYCODE_2: Result := VK_2;
    KEYCODE_3: Result := VK_3;
    KEYCODE_4: Result := VK_4;
    KEYCODE_5: Result := VK_5;
    KEYCODE_6: Result := VK_6;
    KEYCODE_7: Result := VK_7;
    KEYCODE_8: Result := VK_8;
    KEYCODE_9: Result := VK_9;
{   KEYCODE_STAR = 17;
    KEYCODE_POUND = 18;}
    KEYCODE_DPAD_UP:     Result := VK_UP;
    KEYCODE_DPAD_DOWN:   Result := VK_DOWN;
    KEYCODE_DPAD_LEFT:   Result := VK_LEFT;
    KEYCODE_DPAD_RIGHT:  Result := VK_RIGHT;
    KEYCODE_DPAD_CENTER: Result := VK_RETURN;
    KEYCODE_VOLUME_UP:   Result := VK_VOLUME_UP;
    KEYCODE_VOLUME_DOWN: Result := VK_VOLUME_DOWN;
    KEYCODE_POWER: Result := VK_LCL_POWER;
//    KEYCODE_CAMERA = 27;}
    KEYCODE_CLEAR: Result := VK_CLEAR;
    KEYCODE_A: Result := VK_A;
    KEYCODE_B: Result := VK_B;
    KEYCODE_C: Result := VK_C;
    KEYCODE_D: Result := VK_D;
    KEYCODE_E: Result := VK_E;
    KEYCODE_F: Result := VK_F;
    KEYCODE_G: Result := VK_G;
    KEYCODE_H: Result := VK_H;
    KEYCODE_I: Result := VK_I;
    KEYCODE_J: Result := VK_J;
    KEYCODE_K: Result := VK_K;
    KEYCODE_L: Result := VK_L;
    KEYCODE_M: Result := VK_M;
    KEYCODE_N: Result := VK_N;
    KEYCODE_O: Result := VK_O;
    KEYCODE_P: Result := VK_P;
    KEYCODE_Q: Result := VK_Q;
    KEYCODE_R: Result := VK_R;
    KEYCODE_S: Result := VK_S;
    KEYCODE_T: Result := VK_T;
    KEYCODE_U: Result := VK_U;
    KEYCODE_V: Result := VK_V;
    KEYCODE_W: Result := VK_W;
    KEYCODE_X: Result := VK_X;
    KEYCODE_Y: Result := VK_Y;
    KEYCODE_Z: Result := VK_Z;
    KEYCODE_COMMA:  Result := VK_LCL_COMMA;
    KEYCODE_PERIOD: Result := VK_LCL_POINT;
    KEYCODE_ALT_LEFT:   if lExtendedKeysSupport then Result := VK_LMENU else Result := VK_MENU;
    KEYCODE_ALT_RIGHT:  if lExtendedKeysSupport then Result := VK_RMENU else Result := VK_MENU;
    KEYCODE_SHIFT_LEFT: if lExtendedKeysSupport then Result := VK_LSHIFT else Result := VK_SHIFT;
    KEYCODE_SHIFT_RIGHT:if lExtendedKeysSupport then Result := VK_RSHIFT else Result := VK_SHIFT;
    KEYCODE_TAB: Result := VK_TAB;
    KEYCODE_SPACE: Result := VK_SPACE;
{    KEYCODE_SYM = 63;
    KEYCODE_EXPLORER = 64;
    KEYCODE_ENVELOPE = 65;}
    KEYCODE_ENTER: Result := VK_RETURN;
    KEYCODE_DEL:   Result := VK_BACK; // The "Backspace" key
    KEYCODE_GRAVE: Result := VK_LCL_TILDE;
    KEYCODE_MINUS: Result := VK_LCL_MINUS;
    KEYCODE_EQUALS:Result := VK_LCL_EQUAL;
    KEYCODE_LEFT_BRACKET:  Result := VK_LCL_OPEN_BRAKET;
    KEYCODE_RIGHT_BRACKET: Result := VK_LCL_CLOSE_BRAKET;
    KEYCODE_BACKSLASH:     Result := VK_LCL_BACKSLASH;
    KEYCODE_SEMICOLON:     Result := VK_LCL_SEMI_COMMA;
    KEYCODE_APOSTROPHE:    Result := VK_LCL_QUOTE;
    KEYCODE_SLASH: Result := VK_LCL_SLASH;
    KEYCODE_AT:    Result := VK_LCL_AT;
{    KEYCODE_NUM = 78;
    KEYCODE_HEADSETHOOK = 79; // Headset Hook key. Used to hang up calls and stop media.
    KEYCODE_FOCUS = 80;  // *Camera* focus
    KEYCODE_PLUS = 81;}
    KEYCODE_MENU: Result := VK_MENU;
{    KEYCODE_NOTIFICATION = 83;
    KEYCODE_SEARCH = 84;
    KEYCODE_MEDIA_PLAY_PAUSE = 85;
    KEYCODE_MEDIA_STOP = 86;
    KEYCODE_MEDIA_NEXT = 87;
    KEYCODE_MEDIA_PREVIOUS = 88;
    KEYCODE_MEDIA_REWIND = 89;
    KEYCODE_MEDIA_FAST_FORWARD = 90;
    KEYCODE_MUTE = 91;}
    KEYCODE_PAGE_UP: Result := VK_NEXT;
    KEYCODE_PAGE_DOWN: Result := VK_PRIOR;
{    KEYCODE_PICTSYMBOLS = 94;
    KEYCODE_SWITCH_CHARSET = 95;
    KEYCODE_BUTTON_A = 96;
    KEYCODE_BUTTON_B = 97;
    KEYCODE_BUTTON_C = 98;
    KEYCODE_BUTTON_X = 99;
    KEYCODE_BUTTON_Y = 100;
    KEYCODE_BUTTON_Z = 101;
    KEYCODE_BUTTON_L1 = 102;
    KEYCODE_BUTTON_R1 = 103;
    KEYCODE_BUTTON_L2 = 104;
    KEYCODE_BUTTON_R2 = 105;
    KEYCODE_BUTTON_THUMBL = 106;
    KEYCODE_BUTTON_THUMBR = 107;
    KEYCODE_BUTTON_START = 108;
    KEYCODE_BUTTON_SELECT = 109;
    KEYCODE_BUTTON_MODE = 110;}

    // Now keys from the SDK

    {KEYCODE_3D_MODE = $000000ce; // 3D Mode key. Toggles the display between 2D and 3D mode.
    KEYCODE_APP_SWITCH = $000000bb;
    KEYCODE_AVR_INPUT = $000000b6;
    KEYCODE_AVR_POWER = $000000b5;
    KEYCODE_BOOKMARK = $000000ae;}
    KEYCODE_BREAK: Result := VK_PAUSE;
{    KEYCODE_BUTTON_1 = $000000bc;
    KEYCODE_BUTTON_10 = $000000c5;
    KEYCODE_BUTTON_11 = $000000c6;
    KEYCODE_BUTTON_12 = $000000c7;
    KEYCODE_BUTTON_13 = $000000c8;
    KEYCODE_BUTTON_14 = $000000c9;
    KEYCODE_BUTTON_15 = $000000ca;
    KEYCODE_BUTTON_16 = $000000cb; // Generic Game Pad Button #16.
    KEYCODE_BUTTON_2 = $000000bd; // Generic Game Pad Button #2.
    KEYCODE_BUTTON_3 = $000000be;
    KEYCODE_BUTTON_4 = $000000bf;
    KEYCODE_BUTTON_5 = $000000c0;
    KEYCODE_BUTTON_6 = $000000c1;
    KEYCODE_BUTTON_7 = $000000c2;
    KEYCODE_BUTTON_8 = $000000c3;
    KEYCODE_BUTTON_9 = $000000c4; // Generic Game Pad Button #9.
    KEYCODE_CALCULATOR = $000000d2; // Calculator special function key. Used to launch a calculator application.
    KEYCODE_CALENDAR = $000000d0; // Calendar special function key. Used to launch a calendar application.}
    KEYCODE_CAPS_LOCK: Result := VK_CAPITAL;
{    KEYCODE_CAPTIONS = $000000af; // Toggle captions key. Switches the mode for closed-captioning text, for example during television shows.
    KEYCODE_CHANNEL_DOWN = $000000a7; // Channel down key. On TV remotes, decrements the television channel.
    KEYCODE_CHANNEL_UP = $000000a6; // Channel up key. On TV remotes, increments the television channel.
    KEYCODE_CONTACTS = $000000cf; // Contacts special function key. Used to launch an address book application.
    KEYCODE_CTRL_LEFT = $00000071; // Left Control modifier key.
    KEYCODE_CTRL_RIGHT = $00000072; // Right Control modifier key.
    KEYCODE_DVR = $000000ad; // DVR key. On some TV remotes, switches to a DVR mode for recorded shows.
    KEYCODE_ENTER = $00000042; // Enter key.
    KEYCODE_ENVELOPE = $00000041; // Envelope special function key. Used to launch a mail application.
    KEYCODE_EQUALS = $00000046; // '=' key.
    KEYCODE_ESCAPE = $0000006f; // Escape key.
    KEYCODE_EXPLORER = $00000040; // Explorer special function key. Used to launch a browser application.
    KEYCODE_F1 = $00000083;
    KEYCODE_F10 = $0000008c;
    KEYCODE_F11 = $0000008d;
    KEYCODE_F12 = $0000008e;
    KEYCODE_F2 = $00000084;
    KEYCODE_F3 = $00000085;
    KEYCODE_F4 = $00000086;
    KEYCODE_F5 = $00000087;
    KEYCODE_F6 = $00000088;
    KEYCODE_F7 = $00000089;
    KEYCODE_F8 = $0000008a;
    KEYCODE_F9 = $0000008b;
    KEYCODE_FORWARD = $0000007d; // Forward key. Navigates forward in the history stack. Complement of KEYCODE_BACK.}
    KEYCODE_FORWARD_DEL: Result := VK_DELETE;
{    KEYCODE_FUNCTION = $00000077; // Function modifier key.
    KEYCODE_GUIDE = $000000ac; // Guide key. On TV remotes, shows a programming guide.
    KEYCODE_INFO = $000000a5; // Info key. Common on TV remotes to show additional information related to what is currently being viewed.}
    KEYCODE_INSERT:      Result := VK_INSERT;
{    KEYCODE_LANGUAGE_SWITCH = $000000cc; // Language Switch key. Toggles the current input language such as switching between English and Japanese on a QWERTY keyboard. On some devices, the same function may be performed by pressing Shift+Spacebar.
    KEYCODE_MANNER_MODE = $000000cd; // Manner Mode key. Toggles silent or vibrate mode on and off to make the device behave more politely in certain settings such as on a crowded train. On some devices, the key may only operate when long-pressed.
    KEYCODE_MEDIA_CLOSE = $00000080; // Close media key. May be used to close a CD tray, for example.
    KEYCODE_MEDIA_EJECT = $00000081; // Eject media key. May be used to eject a CD tray, for example.
    KEYCODE_MEDIA_PAUSE = $0000007f; // Pause media key.
    KEYCODE_MEDIA_PLAY = $0000007e; // Play media key.
    KEYCODE_MEDIA_RECORD = $00000082; // Record media key.}
    KEYCODE_META_LEFT:  Result := VK_LWIN;
    KEYCODE_META_RIGHT: Result := VK_RWIN;
{    KEYCODE_MOVE_END  = $0000007b; // End Movement key. Used for scrolling or moving the cursor around to the end of a line or to the bottom of a list.
    KEYCODE_MOVE_HOME = $0000007a; // Home Movement key. Used for scrolling or moving the cursor around to the start of a line or to the top of a list.
    KEYCODE_MUSIC = $000000d1; // Music special function key. Used to launch a music player application.}
    KEYCODE_NUMPAD_0: Result := VK_NUMPAD0;
    KEYCODE_NUMPAD_1: Result := VK_NUMPAD1;
    KEYCODE_NUMPAD_2: Result := VK_NUMPAD2;
    KEYCODE_NUMPAD_3: Result := VK_NUMPAD3;
    KEYCODE_NUMPAD_4: Result := VK_NUMPAD4;
    KEYCODE_NUMPAD_5: Result := VK_NUMPAD5;
    KEYCODE_NUMPAD_6: Result := VK_NUMPAD6;
    KEYCODE_NUMPAD_7: Result := VK_NUMPAD7;
    KEYCODE_NUMPAD_8: Result := VK_NUMPAD8;
    KEYCODE_NUMPAD_9: Result := VK_NUMPAD9;
    KEYCODE_NUMPAD_ADD:        Result := VK_ADD;
    //KEYCODE_NUMPAD_COMMA:      Result := $0000009f;
    KEYCODE_NUMPAD_DIVIDE:     Result := VK_DIVIDE;
    KEYCODE_NUMPAD_DOT:        Result := VK_DECIMAL;
    KEYCODE_NUMPAD_ENTER:      Result := VK_RETURN;
    //KEYCODE_NUMPAD_EQUALS:     Result := VK_LCL_EQUAL;
    //KEYCODE_NUMPAD_LEFT_PAREN: Result := $000000a2;
    KEYCODE_NUMPAD_MULTIPLY:   Result := VK_MULTIPLY;
    //KEYCODE_NUMPAD_RIGHT_PAREN:Result := $000000a3;
    KEYCODE_NUMPAD_SUBTRACT:   Result := VK_LCL_MINUS;
    KEYCODE_NUM_LOCK:          Result := VK_NUMLOCK;
    {KEYCODE_PROG_BLUE = $000000ba;
    KEYCODE_PROG_GREEN = $000000b8;
    KEYCODE_PROG_RED = $000000b7;
    KEYCODE_PROG_YELLOW = $000000b9;}
    KEYCODE_SCROLL_LOCK: Result := VK_SCROLL;
{    KEYCODE_SETTINGS = $000000b0;
    KEYCODE_STB_INPUT = $000000b4;
    KEYCODE_STB_POWER = $000000b3;}
    KEYCODE_SYSRQ:       Result := VK_PRINT;
{    KEYCODE_TV = $000000aa;
    KEYCODE_TV_INPUT = $000000b2;
    KEYCODE_TV_POWER = $000000b1;}
    KEYCODE_VOLUME_MUTE: Result := VK_VOLUME_MUTE;
    {KEYCODE_WINDOW = $000000ab;
    KEYCODE_ZOOM_IN = $000000a8;
    KEYCODE_ZOOM_OUT = $000000a9; }
  else
    Result := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendCreate;
begin
  // Setup DebugLn
  {$IFnDEF WithOldDebugln}
  DebugLogger.OnDebugLn := @AndroidDebugLn;
  DebugLogger.OnDbgOut := @AccumulatingDebugOut;
  {$ELSE}
  DebugLnProc := @AndroidDebugLn;
  DebugOutProc := @AccumulatingDebugOut;
  {$ENDIF}

  {$ifdef CD_UseNativeText}
  // Create the dummy screen DC
 { ScreenBitmapRawImage.Init;
  ScreenBitmapHeight := 100;
  ScreenBitmapWidth := 100;
  ScreenBitmapRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(ScreenBitmapWidth, ScreenBitmapHeight);
  ScreenBitmapRawImage.CreateData(True);
  ScreenImage := TLazIntfImage.Create(0, 0);
  ScreenImage.SetRawImage(ScreenBitmapRawImage);
  ScreenDC := TLazCanvas.Create(ScreenImage);

  DefaultFontAndroidSize := 16;   }
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  destructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendDestroy;
begin
  {$ifdef CD_UseNativeText}
  // Free the dummy screen DC
//  ScreenImage.Free;
 // ScreenDC.Free;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  initialize Windows
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  {$ifdef VerboseCDApplication}
  DebugLn('TCDWidgetSet.AppInit');
  {$endif}

  Forms.MessageBoxFunction := @CDMessageBoxFunction;

  if Application.LayoutAdjustmentPolicy = lapDefault then
    Application.LayoutAdjustmentPolicy := lapAutoAdjustWithoutHorizontalScrolling;
end;

procedure TCDWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  {$ifdef VerboseCDApplication}
  DebugLn('TCDWidgetSet.AppRun');
  {$endif}
end;

(*
function TWinCEWidgetSet.GetAppHandle: THandle;
begin
  Result:= FAppHandle;
end;

procedure TWinCEWidgetSet.SetAppHandle(const AValue: THandle);
begin
  // Do it only if handle is not yet created (for example for DLL initialization)
  // if handle is already created we can't reassign it
  if AppHandle = 0 then
    FAppHandle := AValue;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppMinimize;
begin
  // calling Activity.Free doesnt close the app, only hides it, so it is good for AppMinimize
   javaEnv.CallVoidMethod(javaActivityObject, javaMethod_Activity_finish);
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppRestore
  Params:  None
  Returns: Nothing

  Restore minimized whole application from taskbar
 ------------------------------------------------------------------------------}

procedure TCDWidgetSet.AppRestore;
begin
//  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;


{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppBringToFront;
begin
end;

(*
procedure TWinCEWidgetSet.SetDesigning(AComponent: TComponent);
begin
  //if Data<>nil then EnableWindow((AComponent As TWinControl).Handle, boolean(Data^));
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
var
  Window: HWnd;
begin
  //DebugLn('Trace:TWinCEWidgetSet.SetCallback - Start');
  //DebugLn(Format('Trace:TWinCEWidgetSet.SetCallback - Class Name --> %S', [Sender.ClassName]));
  //DebugLn(Format('Trace:TWinCEWidgetSet.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
  if Sender Is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else if Sender Is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := TWinControl(Sender).Handle;
  if Window=0 then exit;

  //DebugLn('Trace:TWinCEWidgetSet.SetCallback - Exit');
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.RemoveCallbacks(Sender: TObject);
var
  Window: HWnd;
begin
  if Sender Is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else if Sender Is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := (Sender as TWinControl).Handle;
  if Window=0 then exit;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppProcessMessages;
begin
end;
(*
procedure TWinCEWidgetSet.CheckPipeEvents;
var
  lHandler: PPipeEventInfo;
//  lBytesAvail: dword;
//  SomethingChanged: Boolean;
  ChangedCount:integer;
begin
  lHandler := FWaitPipeHandlers;
  ChangedCount := 0;
  while (lHandler <> nil) and (ChangedCount < 10) do
  begin
    {
    roozbeh : ooops not supported
    SomethingChanged:=true;
    if Windows.PeekNamedPipe(lHandler^.Handle, nil, 0, nil, @lBytesAvail, nil) then
    begin
      if lBytesAvail <> 0 then
        lHandler^.OnEvent(lHandler^.UserData, [prDataAvailable])
      else
        SomethingChanged := false;
    end else
      lHandler^.OnEvent(lHandler^.UserData, [prBroken]);
    if SomethingChanged then
      lHandler := FWaitPipeHandlers
    else begin
      lHandler := lHandler^.Next;
      ChangedCount := 0;
    end;
    inc(ChangedCount);}
  end;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
//roozbeh:new update...whole procedure body is added.what is it?
procedure TCDWidgetSet.AppWaitMessage;
begin
end;

{------------------------------------------------------------------------------
  Method: TCDWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Terminates the application
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppTerminate;
var
  lParams: array[0..0] of JValue;
begin
  DebugLn('[TCDWidgetSet.AppTerminate] Start');

  // Call the method
  lParams[0].i := 0;
  javaEnv.CallStaticVoidMethodA(javaJavaLandSystemClass, javaMethod_System_exit, @lParams[0]);

  DebugLn('[TCDWidgetSet.AppTerminate] End');
end;

procedure TCDWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
end;

procedure TCDWidgetSet.AppSetTitle(const ATitle: string);
begin
    SetTitleBar(ATitle);
end;

procedure TCDWidgetSet.AppSetVisible(const AVisible: Boolean);
begin
end;

function TCDWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

function TCDWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

procedure TCDWidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
begin
end;

{------------------------------------------------------------------------------
  function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle;
var
  lTimer: TCDTimer;
  javaField_lcltimerinterval: JfieldID=nil;
  javaMethod_LCLDoCreateTimer: jmethodid = nil;
  javaField_lcltimerid: JfieldID=nil;
begin
  lTimer := TCDTimer.Create;

  Result := THandle(lTimer);

  lTimer.Interval := Interval;
  lTimer.TimerFunc := TimerFunc;
  with javaEnv do
  begin
    javaField_lcltimerinterval:= GetFieldID( javaActivityClass,   'lcltimerinterval', 'I');
    javaMethod_LCLDoCreateTimer:= GetMethodID( javaActivityClass,    'LCLDoCreateTimer', '()V');
    javaField_lcltimerid:= GetFieldID( javaActivityClass, 'lcltimerid', 'Ljava/lang/Runnable;');
    // Prepare the input
    SetIntField( javaActivityObject, javaField_lcltimerinterval, Interval);

    // Call the method
    CallVoidMethod( javaActivityObject, javaMethod_LCLDoCreateTimer);

    // Read the output
    lTimer.NativeHandle := PtrInt(GetObjectField( javaActivityObject, javaField_lcltimerid));

    // Add it to our list
    AddTimer(lTimer);
    DebugLn(Format('[TCDWidgetSet.CreateTimer] Result=%x', [PtrInt(Result)]));
  end;
end;

{------------------------------------------------------------------------------
  function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
var
  lTimer: TCDTimer;
  javaMethod_LCLDoDestroyTimer: jmethodid = nil;
  javaField_lcltimerid: JfieldID=nil;
begin
  DebugLn(Format('[TCDWidgetSet.DestroyTimer] TimerHandle=%x', [PtrInt(TimerHandle)]));
  Result := False;

  lTimer := TCDTimer(TimerHandle);

  if lTimer = nil then Exit;

  with javaEnv do
  begin
    javaMethod_LCLDoDestroyTimer:= GetMethodID( javaActivityClass,    'LCLDoDestroyTimer', '()V');
    javaField_lcltimerid:= GetFieldID( javaActivityClass, 'lcltimerid', 'Ljava/lang/Runnable;');
    // Prepare the input
    SetObjectField( javaActivityObject, javaField_lcltimerid, jobject(lTimer.NativeHandle));

    // Call the method
    CallVoidMethod(javaActivityObject, javaMethod_LCLDoDestroyTimer);
  end;

  // Remove from the list
  RemoveTimer(lTimer);
  lTimer.Free;

  Result := True;
end;
(*
procedure TWinCEWidgetSet.HandleWakeMainThread(Sender: TObject);
begin
  // wake up GUI thread by sending a message to it
  Windows.PostMessage(AppHandle, WM_NULL, 0, 0);
end;
*)

// This code is unnecessary in FPC 2.6+,
// it was required when the 2.5.1 snapshot was created
{$ifdef ver2_5}
procedure PASCALMAIN; external name 'PASCALMAIN';

procedure FPC_SHARED_LIB_START; [public, alias: 'FPC_SHARED_LIB_START'];
begin
   PASCALMAIN;
end;
{$endif}


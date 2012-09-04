unit jni;
{$ifdef fpc}
 {$mode delphi}
 {$packrecords c}
{$endif}
{.$DEFINE jnidebug}
interface

(*
 * Manifest constants.
 *)
const JNI_FALSE=0;
      JNI_TRUE=1;

      JNI_VERSION_1_1=$00010001;
      JNI_VERSION_1_2=$00010002;
      JNI_VERSION_1_4=$00010004;
      JNI_VERSION_1_6=$00010006;

      JNI_OK=0;         // no error
      JNI_ERR=-1;       // generic error
      JNI_EDETACHED=-2; // thread detached from the VM
      JNI_EVERSION=-3;  // JNI version error

      JNI_COMMIT=1;     // copy content, do not free buffer
      JNI_ABORT=2;      // free buffer w/o copying back

(*
 * Type definitions.
 *)
type va_list=pointer;

     jboolean=byte;        // unsigned 8 bits
     jbyte=shortint;       // signed 8 bits
     jchar=word;           // unsigned 16 bits
     jshort=smallint;      // signed 16 bits
     jint=longint;         // signed 32 bits
     jlong=int64;          // signed 64 bits
     jfloat=single;        // 32-bit IEEE 754
     jdouble=double;       // 64-bit IEEE 754

     jsize=jint;            // "cardinal indices and sizes"

     Pjboolean=^jboolean;
     Pjbyte=^jbyte;
     Pjchar=^jchar;
     Pjshort=^jshort;
     Pjint=^jint;
     Pjlong=^jlong;
     Pjfloat=^jfloat;
     Pjdouble=^jdouble;

     Pjsize=^jsize;

     // Reference type
     jobject=pointer;
     jclass=jobject;
     jstring=jobject;
     jarray=jobject;
     jobjectArray=jarray;
     jbooleanArray=jarray;
     jbyteArray=jarray;
     jcharArray=jarray;
     jshortArray=jarray;
     jintArray=jarray;
     jlongArray=jarray;
     jfloatArray=jarray;
     jdoubleArray=jarray;
     jthrowable=jobject;
     jweak=jobject;
     jref=jobject;

     PPointer=^pointer;
     Pjobject=^jobject;
     Pjclass=^jclass;
     Pjstring=^jstring;
     Pjarray=^jarray;
     PjobjectArray=^jobjectArray;
     PjbooleanArray=^jbooleanArray;
     PjbyteArray=^jbyteArray;
     PjcharArray=^jcharArray;
     PjshortArray=^jshortArray;
     PjintArray=^jintArray;
     PjlongArray=^jlongArray;
     PjfloatArray=^jfloatArray;
     PjdoubleArray=^jdoubleArray;
     Pjthrowable=^jthrowable;
     Pjweak=^jweak;
     Pjref=^jref;

     _jfieldID=record // opaque structure
     end;
     jfieldID=^_jfieldID;// field IDs
     PjfieldID=^jfieldID;

     _jmethodID=record // opaque structure
     end;
     jmethodID=^_jmethodID;// method IDs
     PjmethodID=^jmethodID;

     PJNIInvokeInterface=^JNIInvokeInterface;

     Pjvalue=^jvalue;
     jvalue={$ifdef packedrecords}packed{$endif} record
      case integer of
       0:(z:jboolean);
       1:(b:jbyte);
       2:(c:jchar);
       3:(s:jshort);
       4:(i:jint);
       5:(j:jlong);
       6:(f:jfloat);
       7:(d:jdouble);
       8:(l:jobject);
       9:(pl:Pjobject);
       10:(pi:Pjint);

     end;

     jobjectRefType=(
      JNIInvalidRefType=0,
      JNILocalRefType=1,
      JNIGlobalRefType=2,
      JNIWeakGlobalRefType=3);

     PJNINativeMethod=^JNINativeMethod;
     JNINativeMethod={$ifdef packedrecords}packed{$endif} record
      name:pchar;
      signature:pchar;
      fnPtr:pointer;
     end;

     PJNINativeInterface=^JNINativeInterface;

     _JNIEnv={$ifdef packedrecords}packed{$endif} record
      functions:PJNINativeInterface;
     end;

     _JavaVM= record
      functions:PJNIInvokeInterface;
     end;
     P_JavaVM = ^_JavaVM;

     C_JNIEnv=^JNINativeInterface;
     JNIEnv=^JNINativeInterface;
     JavaVM=^JNIInvokeInterface;

     PPJNIEnv=^PJNIEnv;
     PJNIEnv=^JNIEnv;

     PPJavaVM=^PJavaVM;
     PJavaVM=^JavaVM;

     JNINativeInterface= record
      reserved0:pointer;
      reserved1:pointer;
      reserved2:pointer;
      reserved3:pointer;

      GetVersion:function(Env:PJNIEnv):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      DefineClass:function(Env:PJNIEnv;const Name:pchar;Loader:JObject;const Buf:PJByte;Len:JSize):JClass;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      FindClass:function(Env:PJNIEnv;const Name:pchar):JClass;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // Reflection Support
      FromReflectedMethod:function(Env:PJNIEnv;Method:JObject):JMethodID;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      FromReflectedField:function(Env:PJNIEnv;Field:JObject):JFieldID;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ToReflectedMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;IsStatic:JBoolean):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetSuperclass:function(Env:PJNIEnv;Sub:JClass):JClass;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      IsAssignableFrom:function(Env:PJNIEnv;Sub:JClass;Sup:JClass):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // Reflection Support
      ToReflectedField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;IsStatic:JBoolean):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      Throw:function(Env:PJNIEnv;Obj:JThrowable):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ThrowNew:function(Env:PJNIEnv;AClass:JClass;const Msg:pchar):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ExceptionOccurred:function(Env:PJNIEnv):JThrowable;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ExceptionDescribe:procedure(Env:PJNIEnv);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ExceptionClear:procedure(Env:PJNIEnv);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      FatalError:procedure(Env:PJNIEnv;const Msg:pchar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // Local Reference Management
      PushLocalFrame:function(Env:PJNIEnv;Capacity:JInt):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      PopLocalFrame:function(Env:PJNIEnv;Result:JObject):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      NewGlobalRef:function(Env:PJNIEnv;LObj:JObject):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      DeleteGlobalRef:procedure(Env:PJNIEnv;GRef:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      DeleteLocalRef:procedure(Env:PJNIEnv;Obj:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      IsSameObject:function(Env:PJNIEnv;Obj1:JObject;Obj2:JObject):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // Local Reference Management
      NewLocalRef:function(Env:PJNIEnv;Ref:JObject):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      EnsureLocalCapacity:function(Env:PJNIEnv;Capacity:JInt):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      AllocObject:function(Env:PJNIEnv;AClass:JClass):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewObject:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewObjectV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewObjectA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetObjectClass:function(Env:PJNIEnv;Obj:JObject):JClass;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      IsInstanceOf:function(Env:PJNIEnv;Obj:JObject;AClass:JClass):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetMethodID:function(Env:PJNIEnv;AClass:JClass;const Name:pchar;const Sig:pchar):JMethodID;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallObjectMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallObjectMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallObjectMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallBooleanMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallBooleanMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallBooleanMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallByteMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallByteMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallByteMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallCharMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallCharMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallCharMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallShortMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallShortMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallShortMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallIntMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallIntMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallIntMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallLongMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallLongMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallLongMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallFloatMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallFloatMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallFloatMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallDoubleMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallDoubleMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallDoubleMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallVoidMethod:procedure(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallVoidMethodV:procedure(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallVoidMethodA:procedure(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualObjectMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualObjectMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualObjectMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualBooleanMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualBooleanMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualBooleanMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualByteMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualByteMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualByteMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualCharMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualCharMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualCharMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualShortMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualShortMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualShortMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualIntMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualIntMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualIntMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualLongMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualLongMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualLongMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualFloatMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualFloatMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualFloatMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualDoubleMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualDoubleMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualDoubleMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallNonvirtualVoidMethod:procedure(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualVoidMethodV:procedure(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallNonvirtualVoidMethodA:procedure(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetFieldID:function(Env:PJNIEnv;AClass:JClass;const Name:pchar;const Sig:pchar):JFieldID;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetObjectField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetBooleanField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetByteField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetCharField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetShortField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetIntField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetLongField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetFloatField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetDoubleField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      SetObjectField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetBooleanField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JBoolean);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetByteField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JByte);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetCharField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JChar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetShortField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JShort);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetIntField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetLongField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JLong);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetFloatField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JFloat);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetDoubleField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JDouble);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetStaticMethodID:function(Env:PJNIEnv;AClass:JClass;const Name:pchar;const Sig:pchar):JMethodID;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticObjectMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticObjectMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticObjectMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticBooleanMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticBooleanMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticBooleanMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticByteMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticByteMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticByteMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticCharMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticCharMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticCharMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticShortMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticShortMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticShortMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticIntMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticIntMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticIntMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticLongMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticLongMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticLongMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticFloatMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticFloatMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticFloatMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticDoubleMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticDoubleMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticDoubleMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      CallStaticVoidMethod:procedure(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticVoidMethodV:procedure(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      CallStaticVoidMethodA:procedure(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetStaticFieldID:function(Env:PJNIEnv;AClass:JClass;const Name:pchar;const Sig:pchar):JFieldID;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticObjectField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticBooleanField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticByteField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticCharField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticShortField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticIntField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticLongField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticFloatField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStaticDoubleField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      SetStaticObjectField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetStaticBooleanField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JBoolean);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetStaticByteField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JByte);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetStaticCharField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JChar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetStaticShortField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JShort);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetStaticIntField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetStaticLongField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JLong);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetStaticFloatField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JFloat);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetStaticDoubleField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JDouble);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      NewString:function(Env:PJNIEnv;const Unicode:PJChar;Len:JSize):JString;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStringLength:function(Env:PJNIEnv;Str:JString):JSize;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStringChars:function(Env:PJNIEnv;Str:JString;var IsCopy:JBoolean):PJChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseStringChars:procedure(Env:PJNIEnv;Str:JString;const Chars:PJChar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      NewStringUTF:function(Env:PJNIEnv;const UTF:pchar):JString;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStringUTFLength:function(Env:PJNIEnv;Str:JString):JSize;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStringUTFChars:function(Env:PJNIEnv;Str:JString; IsCopy: PJBoolean):pchar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseStringUTFChars:procedure(Env:PJNIEnv;Str:JString;const Chars:pchar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetArrayLength:function(Env:PJNIEnv;AArray:JArray):JSize;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      NewObjectArray:function(Env:PJNIEnv;Len:JSize;AClass:JClass;Init:JObject):JObjectArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetObjectArrayElement:function(Env:PJNIEnv;AArray:JObjectArray;Index:JSize):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetObjectArrayElement:procedure(Env:PJNIEnv;AArray:JObjectArray;Index:JSize;Val:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      NewBooleanArray:function(Env:PJNIEnv;Len:JSize):JBooleanArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewByteArray:function(Env:PJNIEnv;Len:JSize):JByteArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewCharArray:function(Env:PJNIEnv;Len:JSize):JCharArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewShortArray:function(Env:PJNIEnv;Len:JSize):JShortArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewIntArray:function(Env:PJNIEnv;Len:JSize):JIntArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewLongArray:function(Env:PJNIEnv;Len:JSize):JLongArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewFloatArray:function(Env:PJNIEnv;Len:JSize):JFloatArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      NewDoubleArray:function(Env:PJNIEnv;Len:JSize):JDoubleArray;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetBooleanArrayElements:function(Env:PJNIEnv;AArray:JBooleanArray;var IsCopy:JBoolean):PJBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetByteArrayElements:function(Env:PJNIEnv;AArray:JByteArray;var IsCopy:JBoolean):PJByte;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetCharArrayElements:function(Env:PJNIEnv;AArray:JCharArray;var IsCopy:JBoolean):PJChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetShortArrayElements:function(Env:PJNIEnv;AArray:JShortArray;var IsCopy:JBoolean):PJShort;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetIntArrayElements:function(Env:PJNIEnv;AArray:JIntArray;var IsCopy:JBoolean):PJInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetLongArrayElements:function(Env:PJNIEnv;AArray:JLongArray;var IsCopy:JBoolean):PJLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetFloatArrayElements:function(Env:PJNIEnv;AArray:JFloatArray;var IsCopy:JBoolean):PJFloat;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetDoubleArrayElements:function(Env:PJNIEnv;AArray:JDoubleArray;var IsCopy:JBoolean):PJDouble;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      ReleaseBooleanArrayElements:procedure(Env:PJNIEnv;AArray:JBooleanArray;Elems:PJBoolean;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseByteArrayElements:procedure(Env:PJNIEnv;AArray:JByteArray;Elems:PJByte;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseCharArrayElements:procedure(Env:PJNIEnv;AArray:JCharArray;Elems:PJChar;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseShortArrayElements:procedure(Env:PJNIEnv;AArray:JShortArray;Elems:PJShort;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseIntArrayElements:procedure(Env:PJNIEnv;AArray:JIntArray;Elems:PJInt;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseLongArrayElements:procedure(Env:PJNIEnv;AArray:JLongArray;Elems:PJLong;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseFloatArrayElements:procedure(Env:PJNIEnv;AArray:JFloatArray;Elems:PJFloat;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseDoubleArrayElements:procedure(Env:PJNIEnv;AArray:JDoubleArray;Elems:PJDouble;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetBooleanArrayRegion:procedure(Env:PJNIEnv;AArray:JBooleanArray;Start:JSize;Len:JSize;Buf:PJBoolean);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetByteArrayRegion:procedure(Env:PJNIEnv;AArray:JByteArray;Start:JSize;Len:JSize;Buf:PJByte);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetCharArrayRegion:procedure(Env:PJNIEnv;AArray:JCharArray;Start:JSize;Len:JSize;Buf:PJChar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetShortArrayRegion:procedure(Env:PJNIEnv;AArray:JShortArray;Start:JSize;Len:JSize;Buf:PJShort);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetIntArrayRegion:procedure(Env:PJNIEnv;AArray:JIntArray;Start:JSize;Len:JSize;Buf:PJInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetLongArrayRegion:procedure(Env:PJNIEnv;AArray:JLongArray;Start:JSize;Len:JSize;Buf:PJLong);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetFloatArrayRegion:procedure(Env:PJNIEnv;AArray:JFloatArray;Start:JSize;Len:JSize;Buf:PJFloat);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetDoubleArrayRegion:procedure(Env:PJNIEnv;AArray:JDoubleArray;Start:JSize;Len:JSize;Buf:PJDouble);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      SetBooleanArrayRegion:procedure(Env:PJNIEnv;AArray:JBooleanArray;Start:JSize;Len:JSize;Buf:PJBoolean);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetByteArrayRegion:procedure(Env:PJNIEnv;AArray:JByteArray;Start:JSize;Len:JSize;Buf:PJByte);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetCharArrayRegion:procedure(Env:PJNIEnv;AArray:JCharArray;Start:JSize;Len:JSize;Buf:PJChar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetShortArrayRegion:procedure(Env:PJNIEnv;AArray:JShortArray;Start:JSize;Len:JSize;Buf:PJShort);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetIntArrayRegion:procedure(Env:PJNIEnv;AArray:JIntArray;Start:JSize;Len:JSize;Buf:PJInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetLongArrayRegion:procedure(Env:PJNIEnv;AArray:JLongArray;Start:JSize;Len:JSize;Buf:PJLong);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetFloatArrayRegion:procedure(Env:PJNIEnv;AArray:JFloatArray;Start:JSize;Len:JSize;Buf:PJFloat);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      SetDoubleArrayRegion:procedure(Env:PJNIEnv;AArray:JDoubleArray;Start:JSize;Len:JSize;Buf:PJDouble);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      RegisterNatives:function(Env:PJNIEnv;AClass:JClass;const Methods:PJNINativeMethod;NMethods:JInt):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      UnregisterNatives:function(Env:PJNIEnv;AClass:JClass):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      MonitorEnter:function(Env:PJNIEnv;Obj:JObject):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      MonitorExit:function(Env:PJNIEnv;Obj:JObject):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      GetJavaVM:function(Env:PJNIEnv;var VM:JavaVM):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // String Operations
      GetStringRegion:procedure(Env:PJNIEnv;Str:JString;Start:JSize;Len:JSize;Buf:PJChar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetStringUTFRegion:procedure(Env:PJNIEnv;Str:JString;Start:JSize;Len:JSize;Buf:pchar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // Array Operations
      GetPrimitiveArrayCritical:function(Env:PJNIEnv;AArray:JArray;var IsCopy:JBoolean):pointer;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleasePrimitiveArrayCritical:procedure(Env:PJNIEnv;AArray:JArray;CArray:pointer;Mode:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // String Operations
      GetStringCritical:function(Env:PJNIEnv;Str:JString;var IsCopy:JBoolean):PJChar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      ReleaseStringCritical:procedure(Env:PJNIEnv;Str:JString;CString:PJChar);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // Weak Global References
      NewWeakGlobalRef:function(Env:PJNIEnv;Obj:JObject):JWeak;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      DeleteWeakGlobalRef:procedure(Env:PJNIEnv;Ref:JWeak);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // Exceptions
      ExceptionCheck:function(Env:PJNIEnv):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // J2SDK1_4
      NewDirectByteBuffer:function(Env:PJNIEnv;Address:pointer;Capacity:JLong):JObject;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetDirectBufferAddress:function(Env:PJNIEnv;Buf:JObject):pointer;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetDirectBufferCapacity:function(Env:PJNIEnv;Buf:JObject):JLong;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

      // added in JNI 1.6
      GetObjectRefType:function(Env:PJNIEnv;AObject:JObject):jobjectRefType;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
     end;

     JNIInvokeInterface= record
      reserved0:pointer;
      reserved1:pointer;
      reserved2:pointer;

      DestroyJavaVM:function(PVM:PJavaVM):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      AttachCurrentThread:function(PVM:PJavaVM;PEnv:PPJNIEnv;Args:pointer):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      DetachCurrentThread:function(PVM:PJavaVM):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetEnv:function(PVM:PJavaVM;PEnv:Ppointer;Version:JInt):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      AttachCurrentThreadAsDaemon:function(PVM:PJavaVM;PEnv:PPJNIEnv;Args:pointer):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
     end;

     JavaVMAttachArgs=packed record
      version:jint;  // must be >= JNI_VERSION_1_2
      name:pchar;    // NULL or name of thread as modified UTF-8 str
      group:jobject; // global ref of a ThreadGroup object, or NULL
     end;

(**
 * JNI 1.2+ initialization.  (As of 1.6, the pre-1.2 structures are no
 * longer supported.)
 *)

     PJavaVMOption=^JavaVMOption;
     JavaVMOption={$ifdef packedrecords}packed{$endif} record
      optionString:pchar;
      extraInfo:pointer;
     end;

     JavaVMInitArgs={$ifdef packedrecords}packed{$endif} record
      version:jint; // use JNI_VERSION_1_2 or later
      nOptions:jint;
      options:PJavaVMOption;
      ignoreUnrecognized:Pjboolean;
     end;

(*
 * VM initialization functions.
 *
 * Note these are the only symbols exported for JNI by the VM.
 *)
{$ifdef jniexternals}
function JNI_GetDefaultJavaVMInitArgs(p:pointer):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}external 'jni' name 'JNI_GetDefaultJavaVMInitArgs';
function JNI_CreateJavaVM(vm:PPJavaVM;AEnv:PPJNIEnv;p:pointer):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}external 'jni' name 'JNI_CreateJavaVM';
function JNI_GetCreatedJavaVMs(vm:PPJavaVM;ASize:jsize;p:Pjsize):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}external 'jni' name 'JNI_GetCreatedJavaVMs';


   EJVMError = class(Exception);
   EJNIError = class(Exception);
   EJNIUnsupportedMethodError = class(EJNIError);
{$endif}

   TJValueArray = array of JValue;

   { TJavaVM }

   TJavaVM = class(TObject)
   private
     FJavaVM: PJavaVM ;
     FEnv: PJNIEnv ;
     FVersion: JInt;

     {$ifdef jniexternals}
     FJavaVMInitArgs: JavaVMInitArgs;
     FJDK1_1InitArgs: JDK1_1InitArgs;
     FJVMDLLFile: string;

     {$IFDEF MSWINDOWS}
     FDLLHandle: THandle;
     {$ENDIF}
     {$IFDEF LINUX}
     FDLLHandle: Pointer;
     {$ENDIF}
     FIsInitialized: Boolean;

     // DLL functions
     FJNI_GetDefaultJavaVMInitArgs: TJNI_GetDefaultJavaVMInitArgs;
     FJNI_CreateJavaVM:             TJNI_CreateJavaVM;
     FJNI_GetCreatedJavaVMs:        TJNI_GetCreatedJavaVMs;
     {$endif}
     procedure SetVersion(const Value: JInt);
   public
     property JavaVM: PJavaVM read FJavaVM write FJavaVM;
     property Env: PJNIEnv read FEnv write FEnv;
     {$ifdef jniexternals}
     property JDK1_1InitArgs: JDK1_1InitArgs read FJDK1_1InitArgs;
     property JDK1_2InitArgs: JavaVMInitArgs read FJavaVMInitArgs;
     {$endif}
     property Version: JInt read FVersion write SetVersion;

     // Constructors
     constructor Create; overload;
     constructor Create(AJavaVM: PJavaVM); overload;
     {$ifdef jniexternals}

     constructor Create(JDKVersion: Integer); overload;
     constructor Create(JDKVersion: Integer; const JVMDLLFilename: string); overload;

     destructor Destroy; override;
     function LoadVM(const Options: JDK1_1InitArgs): JInt; overload;
     function LoadVM(const Options: JavaVMInitArgs): JInt; overload;
     function GetDefaultJavaVMInitArgs(Args: PJDK1_1InitArgs): JInt;
     function GetCreatedJavaVMs(PJVM: PJavaVM; JSize1: JSize; var JSize2: JSize): JInt;
     {$endif}
     function DestroyJavaVM():JInt;
     function AttachCurrentThread(var PEnv:PPJNIEnv;Args:pointer):JInt;
     function DetachCurrentThread():JInt;
     function GetEnv(PEnv:Ppointer;Version:JInt):JInt;
     function AttachCurrentThreadAsDaemon(var PEnv:PPJNIEnv;Args:pointer):JInt;
   end;

  { TJNIEnv }

  TJNIEnv = class(TObject)
  private
    FEnv: PJNIEnv ;
    FThreadID: Integer;
    FMajorVersion: JInt;
    FMinorVersion: JInt;
    FVersion: JInt;
    FConvertedArgs: TJValueArray;
    function GetMajorVersion: JInt;
    function GetMinorVersion: JInt;
    procedure VersionCheck(const FuncName: string; RequiredVersion: JInt);
  public

    // Properties
    //not used in the current class
    //не используеца в текущем классе
    property ThreadID: integer read FThreadID write FThreadID;
    property Env: PJNIEnv read FEnv;
    property MajorVersion: JInt read FMajorVersion;
    property MinorVersion: JInt read FMinorVersion;
    property Version: JInt read FVersion;

    // Constructors
    constructor Create(AEnv: PJNIEnv);

    // Support methods
    function ArgsToJValues(const Args: array of const): PJValue;
    function JStringToString(JStr: JString): string;
    function StringToJString(const AString: PAnsiChar): JString;
    function UnicodeJStringToString(JStr: JString): string;
    function StringToUnicodeJString(const AString: PAnsiChar): JString;

    // JNIEnv methods
    function GetVersion: JInt;
    function DefineClass(const Name: PAnsiChar; Loader: JObject; const Buf: PJByte; Len: JSize): JClass;
    function FindClass(const Name: PAnsiChar): JClass;

    // Reflection Support
    function FromReflectedMethod(Method: JObject): JMethodID;
    function FromReflectedField(Field: JObject): JFieldID;
    function ToReflectedMethod(AClass: JClass; MethodID: JMethodID; IsStatic: JBoolean): JObject;

    function GetSuperclass(Sub: JClass): JClass;
    function IsAssignableFrom(Sub: JClass; Sup: JClass): JBoolean;

    // Reflection Support
    function ToReflectedField(AClass: JClass; FieldID: JFieldID; IsStatic: JBoolean): JObject;

    function Throw(Obj: JThrowable): JInt;
    function ThrowNew(AClass: JClass; const Msg: PAnsiChar): JInt;
    function ExceptionOccurred: JThrowable;
    procedure ExceptionDescribe;
    procedure ExceptionClear;
    procedure FatalError(const Msg: PAnsiChar);

    // Local Reference Management
    function PushLocalFrame(Capacity: JInt): JInt;
    function PopLocalFrame(AResult: JObject): JObject;

    function NewGlobalRef(LObj: JObject): JObject;
    procedure DeleteGlobalRef(GRef: JObject);
    procedure DeleteLocalRef(Obj: JObject);
    function IsSameObject(Obj1: JObject; Obj2: JObject): JBoolean;

    // Local Reference Management
    function NewLocalRef(Ref: JObject): JObject;
    function EnsureLocalCapacity(Capacity: JInt): JObject;

    function AllocObject(AClass: JClass): JObject;
    function NewObject(AClass: JClass; MethodID: JMethodID ): JObject;  overload;
    function NewObject(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JObject;  overload;
    function NewObjectV(AClass: JClass; MethodID: JMethodID; Args: va_list): JObject;
    function NewObjectA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JObject; overload;
    function NewObjectA(AClass: JClass; MethodID: JMethodID;  const Args: array of const ): JObject; overload;
    function NewObjectA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ;  const Args: array of const ): JObject; overload;

    function GetObjectClass(Obj: JObject): JClass;
    function IsInstanceOf(Obj: JObject; AClass: JClass): JBoolean;

    function GetMethodID(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar): JMethodID;

    function CallObjectMethod(Obj: JObject; MethodID: JMethodID ): JObject; overload;
    function CallObjectMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JObject; overload;
    function CallObjectMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JObject;
    function CallObjectMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JObject;   overload;
    function CallObjectMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const ): JObject;  overload;
    function CallObjectMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const ): JObject;  overload;

    function CallBooleanMethod(Obj: JObject; MethodID: JMethodID ): JBoolean;  overload;
    function CallBooleanMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JBoolean; overload;
    function CallBooleanMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JBoolean;
    function CallBooleanMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JBoolean; overload;
    function CallBooleanMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const ): JBoolean; overload;
    function CallBooleanMethodA(Obj: JObject;AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const ): JBoolean; overload;

    function CallByteMethod(Obj: JObject; MethodID: JMethodID ): JByte; overload;
    function CallByteMethod(Obj: JObject ;AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JByte; overload;
    function CallByteMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JByte;
    function CallByteMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JByte; overload;
    function CallByteMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const): JByte; overload;
    function CallByteMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JByte; overload;

    function CallCharMethod(Obj: JObject; MethodID: JMethodID ): JChar; overload;
    function CallCharMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JChar; overload;
    function CallCharMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JChar;
    function CallCharMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JChar; overload;
    function CallCharMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const): JChar; overload;
    function CallCharMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JChar; overload;

    function CallShortMethod(Obj: JObject; MethodID: JMethodID ): JShort; overload;
    function CallShortMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JShort; overload;
    function CallShortMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JShort;
    function CallShortMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JShort; overload;
    function CallShortMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const): JShort; overload;
    function CallShortMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JShort; overload;

    function CallIntMethod(Obj: JObject; MethodID: JMethodID ): JInt; overload;
    function CallIntMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JInt; overload;
    function CallIntMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JInt;
    function CallIntMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JInt; overload;
    function CallIntMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const): JInt; overload;
    function CallIntMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JInt; overload;

    function CallLongMethod(Obj: JObject; MethodID: JMethodID ): JLong; overload;
    function CallLongMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JLong; overload;
    function CallLongMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JLong;
    function CallLongMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JLong; overload;
    function CallLongMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const): JLong; overload;
    function CallLongMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JLong; overload;

    function CallFloatMethod(Obj: JObject; MethodID: JMethodID ): JFloat; overload;
    function CallFloatMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JFloat; overload;
    function CallFloatMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JFloat;
    function CallFloatMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JFloat; overload;
    function CallFloatMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const): JFloat; overload;
    function CallFloatMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JFloat; overload;

    function CallDoubleMethod(Obj: JObject; MethodID: JMethodID ): JDouble; overload;
    function CallDoubleMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JDouble; overload;
    function CallDoubleMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JDouble;
    function CallDoubleMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JDouble; overload;
    function CallDoubleMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const): JDouble; overload;
    function CallDoubleMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JDouble; overload;

    procedure CallVoidMethod(Obj: JObject; MethodID: JMethodID ); overload;
    procedure CallVoidMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ); overload;
    procedure CallVoidMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list);
    procedure CallVoidMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue); overload;
    procedure CallVoidMethodA(Obj: JObject; MethodID: JMethodID; const Args: array of const); overload;
    procedure CallVoidMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const); overload;

    function CallNonvirtualObjectMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JObject; overload;
    function CallNonvirtualObjectMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JObject; overload;
    function CallNonvirtualObjectMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JObject;
    function CallNonvirtualObjectMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JObject; overload;
    function CallNonvirtualObjectMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JObject; overload;
    function CallNonvirtualObjectMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JObject; overload;

    function CallNonvirtualBooleanMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JBoolean; overload;
    function CallNonvirtualBooleanMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JBoolean; overload;
    function CallNonvirtualBooleanMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JBoolean;
    function CallNonvirtualBooleanMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JBoolean; overload;
    function CallNonvirtualBooleanMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JBoolean; overload;
    function CallNonvirtualBooleanMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JBoolean; overload;

    function CallNonvirtualByteMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JByte; overload;
    function CallNonvirtualByteMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ): JByte; overload;
    function CallNonvirtualByteMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JByte;
    function CallNonvirtualByteMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JByte; overload;
    function CallNonvirtualByteMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JByte; overload;
    function CallNonvirtualByteMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JByte; overload;

    function CallNonvirtualCharMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JChar;  overload;
    function CallNonvirtualCharMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JChar;  overload;
    function CallNonvirtualCharMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JChar;
    function CallNonvirtualCharMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JChar; overload;
    function CallNonvirtualCharMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JChar; overload;
    function CallNonvirtualCharMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JChar; overload;

    function CallNonvirtualShortMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JShort; overload;
    function CallNonvirtualShortMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JShort; overload;
    function CallNonvirtualShortMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JShort;
    function CallNonvirtualShortMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JShort; overload;
    function CallNonvirtualShortMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JShort; overload;
    function CallNonvirtualShortMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JShort; overload;

    function CallNonvirtualIntMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JInt; overload;
    function CallNonvirtualIntMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JInt; overload;
    function CallNonvirtualIntMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JInt;
    function CallNonvirtualIntMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JInt; overload;
    function CallNonvirtualIntMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JInt; overload;
    function CallNonvirtualIntMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JInt; overload;

    function CallNonvirtualLongMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JLong; overload;
    function CallNonvirtualLongMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JLong; overload;
    function CallNonvirtualLongMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JLong;
    function CallNonvirtualLongMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JLong; overload;
    function CallNonvirtualLongMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JLong; overload;
    function CallNonvirtualLongMethodA(Obj: JObject; AClass: JClass;const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JLong; overload;

    function CallNonvirtualFloatMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JFloat; overload;
    function CallNonvirtualFloatMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JFloat; overload;
    function CallNonvirtualFloatMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JFloat;
    function CallNonvirtualFloatMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JFloat; overload;
    function CallNonvirtualFloatMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JFloat; overload;
    function CallNonvirtualFloatMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JFloat; overload;

    function CallNonvirtualDoubleMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JDouble; overload;
    function CallNonvirtualDoubleMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JDouble; overload;
    function CallNonvirtualDoubleMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JDouble;
    function CallNonvirtualDoubleMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JDouble; overload;
    function CallNonvirtualDoubleMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const): JDouble; overload;
    function CallNonvirtualDoubleMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JDouble; overload;

    procedure CallNonvirtualVoidMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ); overload;
    procedure CallNonvirtualVoidMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ); overload;
    procedure CallNonvirtualVoidMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list);
    procedure CallNonvirtualVoidMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue); overload;
    procedure CallNonvirtualVoidMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; const Args: array of const); overload;
    procedure CallNonvirtualVoidMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ; const Args: array of const); overload;

    function GetFieldID(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar): JFieldID;

    function GetObjectField(Obj: JObject; FieldID: JFieldID): JObject;
    function GetBooleanField(Obj: JObject; FieldID: JFieldID): JBoolean;
    function GetByteField(Obj: JObject; FieldID: JFieldID): JByte;
    function GetCharField(Obj: JObject; FieldID: JFieldID): JChar;
    function GetShortField(Obj: JObject; FieldID: JFieldID): JShort;
    function GetIntField(Obj: JObject; FieldID: JFieldID): JInt;
    function GetLongField(Obj: JObject; FieldID: JFieldID): JLong;
    function GetFloatField(Obj: JObject; FieldID: JFieldID): JFloat;
    function GetDoubleField(Obj: JObject; FieldID: JFieldID): JDouble;

    procedure SetObjectField(Obj: JObject; FieldID: JFieldID; Val: JObject);
    procedure SetBooleanField(Obj: JObject; FieldID: JFieldID; Val: JBoolean);
    procedure SetByteField(Obj: JObject; FieldID: JFieldID; Val: JByte);
    procedure SetCharField(Obj: JObject; FieldID: JFieldID; Val: JChar);
    procedure SetShortField(Obj: JObject; FieldID: JFieldID; Val: JShort);
    procedure SetIntField(Obj: JObject; FieldID: JFieldID; Val: JInt);
    procedure SetLongField(Obj: JObject; FieldID: JFieldID; Val: JLong);
    procedure SetFloatField(Obj: JObject; FieldID: JFieldID; Val: JFloat);
    procedure SetDoubleField(Obj: JObject; FieldID: JFieldID; Val: JDouble);

    function GetStaticMethodID(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar): JMethodID;

    function CallStaticObjectMethod(AClass: JClass; MethodID: JMethodID ): JObject; overload;
    function CallStaticObjectMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JObject; overload;
    function CallStaticObjectMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JObject;
    function CallStaticObjectMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JObject; overload;
    function CallStaticObjectMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JObject; overload;
    function CallStaticObjectMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JObject; overload;

    function CallStaticBooleanMethod(AClass: JClass; MethodID: JMethodID ): JBoolean; overload;
    function CallStaticBooleanMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JBoolean; overload;
    function CallStaticBooleanMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JBoolean;
    function CallStaticBooleanMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JBoolean; overload;
    function CallStaticBooleanMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JBoolean; overload;
    function CallStaticBooleanMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JBoolean; overload;

    function CallStaticByteMethod(AClass: JClass; MethodID: JMethodID ): JByte; overload;
    function CallStaticByteMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JByte; overload;
    function CallStaticByteMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JByte;
    function CallStaticByteMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JByte; overload;
    function CallStaticByteMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JByte; overload;
    function CallStaticByteMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JByte; overload;

    function CallStaticCharMethod(AClass: JClass; MethodID: JMethodID ): JChar; overload;
    function CallStaticCharMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JChar; overload;
    function CallStaticCharMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JChar;
    function CallStaticCharMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JChar; overload;
    function CallStaticCharMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JChar; overload;
    function CallStaticCharMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JChar; overload;

    function CallStaticShortMethod(AClass: JClass; MethodID: JMethodID ): JShort; overload;
    function CallStaticShortMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JShort; overload;
    function CallStaticShortMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JShort;
    function CallStaticShortMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JShort;  overload;
    function CallStaticShortMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JShort;  overload;
    function CallStaticShortMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JShort;  overload;

    function CallStaticIntMethod(AClass: JClass; MethodID: JMethodID ): JInt; overload;
    function CallStaticIntMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JInt; overload;
    function CallStaticIntMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JInt;
    function CallStaticIntMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JInt;  overload;
    function CallStaticIntMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JInt;  overload;
    function CallStaticIntMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar ; const Args: array of const): JInt;  overload;

    function CallStaticLongMethod(AClass: JClass; MethodID: JMethodID ): JLong; overload;
    function CallStaticLongMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JLong; overload;
    function CallStaticLongMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JLong;
    function CallStaticLongMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JLong; overload;
    function CallStaticLongMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JLong; overload;
    function CallStaticLongMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JLong; overload;

    function CallStaticFloatMethod(AClass: JClass; MethodID: JMethodID ): JFloat; overload;
    function CallStaticFloatMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JFloat; overload;
    function CallStaticFloatMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JFloat;
    function CallStaticFloatMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JFloat; overload;
    function CallStaticFloatMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JFloat; overload;
    function CallStaticFloatMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JFloat; overload;

    function CallStaticDoubleMethod(AClass: JClass; MethodID: JMethodID ): JDouble; overload;
    function CallStaticDoubleMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ): JDouble; overload;
    function CallStaticDoubleMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JDouble;
    function CallStaticDoubleMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JDouble; overload;
    function CallStaticDoubleMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const): JDouble; overload;
    function CallStaticDoubleMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const): JDouble; overload;

    procedure CallStaticVoidMethod(AClass: JClass; MethodID: JMethodID ); overload;
    procedure CallStaticVoidMethod(AClass: JClass;  const Name: PAnsiChar; const Sig: PAnsiChar ); overload;
    procedure CallStaticVoidMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list);
    procedure CallStaticVoidMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue); overload;
    procedure CallStaticVoidMethodA(AClass: JClass; MethodID: JMethodID; const Args: array of const); overload;
    procedure CallStaticVoidMethodA(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const); overload;

    function GetStaticFieldID(AClass: JClass; const Name: PAnsiChar; const Sig: PAnsiChar): JFieldID;
    function GetStaticObjectField(AClass: JClass; FieldID: JFieldID): JObject;
    function GetStaticBooleanField(AClass: JClass; FieldID: JFieldID): JBoolean;
    function GetStaticByteField(AClass: JClass; FieldID: JFieldID): JByte;
    function GetStaticCharField(AClass: JClass; FieldID: JFieldID): JChar;
    function GetStaticShortField(AClass: JClass; FieldID: JFieldID): JShort;
    function GetStaticIntField(AClass: JClass; FieldID: JFieldID): JInt;
    function GetStaticLongField(AClass: JClass; FieldID: JFieldID): JLong;
    function GetStaticFloatField(AClass: JClass; FieldID: JFieldID): JFloat;
    function GetStaticDoubleField(AClass: JClass; FieldID: JFieldID): JDouble;

    procedure SetStaticObjectField(AClass: JClass; FieldID: JFieldID; Val: JObject);
    procedure SetStaticBooleanField(AClass: JClass; FieldID: JFieldID; Val: JBoolean);
    procedure SetStaticByteField(AClass: JClass; FieldID: JFieldID; Val: JByte);
    procedure SetStaticCharField(AClass: JClass; FieldID: JFieldID; Val: JChar);
    procedure SetStaticShortField(AClass: JClass; FieldID: JFieldID; Val: JShort);
    procedure SetStaticIntField(AClass: JClass; FieldID: JFieldID; Val: JInt);
    procedure SetStaticLongField(AClass: JClass; FieldID: JFieldID; Val: JLong);
    procedure SetStaticFloatField(AClass: JClass; FieldID: JFieldID; Val: JFloat);
    procedure SetStaticDoubleField(AClass: JClass; FieldID: JFieldID; Val: JDouble);

    function NewString(const Unicode: PJChar; Len: JSize): JString;
    function GetStringLength(Str: JString): JSize;
    function GetStringChars(Str: JString; var IsCopy: JBoolean): PJChar;
    procedure ReleaseStringChars(Str: JString; const Chars: PJChar);

    function NewStringUTF(const UTF: PAnsiChar): JString;
    function GetStringUTFLength(Str: JString): JSize;
    function GetStringUTFChars(Str: JString; var IsCopy: JBoolean): PAnsiChar;
    procedure ReleaseStringUTFChars(Str: JString; const Chars: PAnsiChar);

    function GetArrayLength(AArray: JArray): JSize;

    function NewObjectArray(Len: JSize; AClass: JClass; Init: JObject): JObjectArray;
    function GetObjectArrayElement(AArray: JObjectArray; Index: JSize): JObject;
    procedure SetObjectArrayElement(AArray: JObjectArray; Index: JSize; Val: JObject);

    function NewBooleanArray(Len: JSize): JBooleanArray;
    function NewByteArray(Len: JSize): JByteArray;
    function NewCharArray(Len: JSize): JCharArray;
    function NewShortArray(Len: JSize): JShortArray;
    function NewIntArray(Len: JSize): JIntArray;
    function NewLongArray(Len: JSize): JLongArray;
    function NewFloatArray(Len: JSize): JFloatArray;
    function NewDoubleArray(Len: JSize): JDoubleArray;

    function GetBooleanArrayElements(AArray: JBooleanArray; var IsCopy: JBoolean): PJBoolean;
    function GetByteArrayElements(AArray: JByteArray; var IsCopy: JBoolean): PJByte;
    function GetCharArrayElements(AArray: JCharArray; var IsCopy: JBoolean): PJChar;
    function GetShortArrayElements(AArray: JShortArray; var IsCopy: JBoolean): PJShort;
    function GetIntArrayElements(AArray: JIntArray; var IsCopy: JBoolean): PJInt;
    function GetLongArrayElements(AArray: JLongArray; var IsCopy: JBoolean): PJLong;
    function GetFloatArrayElements(AArray: JFloatArray; var IsCopy: JBoolean): PJFloat;
    function GetDoubleArrayElements(AArray: JDoubleArray; var IsCopy: JBoolean): PJDouble;

    procedure ReleaseBooleanArrayElements(AArray: JBooleanArray; Elems: PJBoolean; Mode: JInt);
    procedure ReleaseByteArrayElements(AArray: JByteArray; Elems: PJByte; Mode: JInt);
    procedure ReleaseCharArrayElements(AArray: JCharArray; Elems: PJChar; Mode: JInt);
    procedure ReleaseShortArrayElements(AArray: JShortArray; Elems: PJShort; Mode: JInt);
    procedure ReleaseIntArrayElements(AArray: JIntArray; Elems: PJInt; Mode: JInt);
    procedure ReleaseLongArrayElements(AArray: JLongArray; Elems: PJLong; Mode: JInt);
    procedure ReleaseFloatArrayElements(AArray: JFloatArray; Elems: PJFloat; Mode: JInt);
    procedure ReleaseDoubleArrayElements(AArray: JDoubleArray; Elems: PJDouble; Mode: JInt);

    procedure GetBooleanArrayRegion(AArray: JBooleanArray; Start: JSize; Len: JSize; Buf: PJBoolean);
    procedure GetByteArrayRegion(AArray: JByteArray; Start: JSize; Len: JSize; Buf: PJByte);
    procedure GetCharArrayRegion(AArray: JCharArray; Start: JSize; Len: JSize; Buf: PJChar);
    procedure GetShortArrayRegion(AArray: JShortArray; Start: JSize; Len: JSize; Buf: PJShort);
    procedure GetIntArrayRegion(AArray: JIntArray; Start: JSize; Len: JSize; Buf: PJInt);
    procedure GetLongArrayRegion(AArray: JLongArray; Start: JSize; Len: JSize; Buf: PJLong);
    procedure GetFloatArrayRegion(AArray: JFloatArray; Start: JSize; Len: JSize; Buf: PJFloat);
    procedure GetDoubleArrayRegion(AArray: JDoubleArray; Start: JSize; Len: JSize; Buf: PJDouble);

    procedure SetBooleanArrayRegion(AArray: JBooleanArray; Start: JSize; Len: JSize; Buf: PJBoolean);
    procedure SetByteArrayRegion(AArray: JByteArray; Start: JSize; Len: JSize; Buf: PJByte);
    procedure SetCharArrayRegion(AArray: JCharArray; Start: JSize; Len: JSize; Buf: PJChar);
    procedure SetShortArrayRegion(AArray: JShortArray; Start: JSize; Len: JSize; Buf: PJShort);
    procedure SetIntArrayRegion(AArray: JIntArray; Start: JSize; Len: JSize; Buf: PJInt);
    procedure SetLongArrayRegion(AArray: JLongArray; Start: JSize; Len: JSize; Buf: PJLong);
    procedure SetFloatArrayRegion(AArray: JFloatArray; Start: JSize; Len: JSize; Buf: PJFloat);
    procedure SetDoubleArrayRegion(AArray: JDoubleArray; Start: JSize; Len: JSize; Buf: PJDouble);

    function RegisterNatives(AClass: JClass; const Methods: PJNINativeMethod; NMethods: JInt): JInt;
    function UnregisterNatives(AClass: JClass): JInt;

    function MonitorEnter(Obj: JObject): JInt;
    function MonitorExit(Obj: JObject): JInt;

    function GetJavaVM(var VM: JavaVM): JInt;

    // String Operations
    procedure GetStringRegion(Str: JString; Start: JSize; Len: JSize; Buf: PJChar);
    procedure GetStringUTFRegion(Str: JString; Start: JSize; Len: JSize; Buf: PAnsiChar);

    // Array Operations
    function GetPrimitiveArrayCritical(AArray: JArray; var IsCopy: JBoolean): Pointer;
    procedure ReleasePrimitiveArrayCritical(AArray: JArray; CArray: Pointer; Mode: JInt);

    // String Operations
    function GetStringCritical(Str: JString; var IsCopy: JBoolean): PJChar;
    procedure ReleaseStringCritical(Str: JString; CString: PJChar);

    // Weak Global References
    function NewWeakGlobalRef(Obj: JObject): JWeak;
    procedure DeleteWeakGlobalRef(Ref: JWeak);

    // Exceptions
    function ExceptionCheck: JBoolean;

    // J2SDK1_4
    function NewDirectByteBuffer(Address: Pointer; Capacity: JLong): JObject;
    function GetDirectBufferAddress(Buf: JObject): Pointer;
    function GetDirectBufferCapacity(Buf: JObject): JLong;
  end;

(*
 * Prototypes for functions exported by loadable shared libs.  These are
 * called by JNI, not provided by JNI.
 *)

{var
  curVM:PJavaVM=nil;
  curEnv:PJNIEnv=nil;}
      
(*
function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
*)


implementation

{$IFDEF jnidebug}
uses log;
{$ENDIF}

(*function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 curVM:=vm;
 result:=JNI_VERSION_1_6;
end;

procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
end;*)

constructor TJNIEnv.Create(AEnv: PJNIEnv);
begin
  inherited Create;
  FConvertedArgs := nil;
  FEnv := AEnv;
  FMajorVersion := GetMajorVersion;
  FMinorVersion := GetMinorVersion;
  FVersion      := GetVersion;
end;

function TJNIEnv.ArgsToJValues(const Args: array of const): PJValue;
var
  I: Integer;
begin
  if Length(Args) <> Length(FConvertedArgs) then
    SetLength(FConvertedArgs, Length(Args));
  for I := 0 to High(Args) do
    case Args[I].VType of
      vtInteger:
        FConvertedArgs[I].i := Args[I].VInteger;
      vtBoolean:
        FConvertedArgs[I].z := ORD(Args[I].VBoolean);
      vtWideChar:
        FConvertedArgs[I].c := word(Args[I].VWideChar);
      vtInt64:
        FConvertedArgs[I].j := Args[I].VInt64^;
      vtPointer, vtObject:
        FConvertedArgs[I].l := JObject(Args[I].VObject);
      vtAnsiString:
        FConvertedArgs[I].l := StringToJString(Args[I].VAnsiString);
      vtExtended:
        FConvertedArgs[I].d := Args[I].VExtended^; // Extended to Double (we lose Floats here)
   { else
      raise EJNIError.Create('Unsupported variant argument');   }
    end;
  Result := PJValue(FConvertedArgs);
end;

function TJNIEnv.StringToJString(const AString: PAnsiChar): JString;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: StringToJString');
  {$ENDIF}
  Result := Env^^.NewStringUTF(Env, PChar(AString));
end;

function TJNIEnv.StringToUnicodeJString(const AString: PAnsiChar): JString;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: StringToUnicodeJString');
  {$ENDIF}
  Result := Env^^.NewString(Env, PJChar(AString), Length(AString));
end;

function TJNIEnv.JStringToString(JStr: JString): string;
var
  IsCopy: JBoolean;
  Chars: PChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: JStringToString');
  {$ENDIF}
  if JStr = nil then
  begin
    Result := '';
    Exit;
  end;

  Chars := Env^^.GetStringUTFChars(Env, JStr, @IsCopy);
  if Chars = nil then
    Result := ''
  else
  begin
    Result := string(Chars);
    Env^^.ReleaseStringUTFChars(Env, JStr, Chars);
  end;
end;

function TJNIEnv.UnicodeJStringToString(JStr: JString): string;
var
  IsCopy: JBoolean;
  Chars: PJChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: UnicodeJStringToString');
  {$ENDIF}
  if JStr = nil then
  begin
    Result := '';
    Exit;
  end;

  Chars := Env^^.GetStringChars(Env, JStr, IsCopy);
  if Chars = nil then
    Result := ''
  else
  begin
    Result := string(Chars);
    Env^^.ReleaseStringChars(Env, JStr, Chars);
  end;
end;

function TJNIEnv.GetMajorVersion: JInt;
begin
  Result := GetVersion shr 16;
end;

function TJNIEnv.GetMinorVersion: JInt;
begin
  Result := GetVersion mod 65536;
end;

function TJNIEnv.GetVersion: JInt;
begin
  Result := Env^^.GetVersion(Env);
end;

procedure TJNIEnv.VersionCheck(const FuncName: string; RequiredVersion: JInt);
begin
  {$ifdef jniexternals}
  if Version < RequiredVersion then
    raise EJNIUnsupportedMethodError.CreateFmt('Method "%s" not supported in JDK %d.%d', [FuncName, MajorVersion, MinorVersion]);
  {$endif}
end;

procedure TJNIEnv.CallVoidMethod(Obj: JObject; MethodID: JMethodID );
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallVoidMethod()');
  {$ENDIF}
  Env^^.CallVoidMethod(Env, Obj, MethodID);
end;

procedure TJNIEnv.CallVoidMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar);
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallVoidMethod(Name,Sig)'+
  'const Sig: PAnsiChar)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    CallVoidMethod(Obj, MethodID);
end;

function TJNIEnv.AllocObject(AClass: JClass): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: AllocObject');
  {$ENDIF}
  Result := Env^^.AllocObject(Env, AClass);
end;

function TJNIEnv.CallBooleanMethod(Obj: JObject; MethodID: JMethodID ): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallBooleanMethod');
  {$ENDIF}
  Result := Env^^.CallBooleanMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallBooleanMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JBoolean;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallBooleanMethod(Name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallBooleanMethod(Obj, MethodID);
end;

function TJNIEnv.CallBooleanMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallBooleanMethodA');
  {$ENDIF}
  Result := Env^^.CallBooleanMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallBooleanMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallBooleanMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallBooleanMethodA(Env, Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallBooleanMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JBoolean;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallBooleanMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallBooleanMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallBooleanMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallBooleanMethodV');
  {$ENDIF}
  Result := Env^^.CallBooleanMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallByteMethod(Obj: JObject; MethodID: JMethodID ): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallByteMethod');
  {$ENDIF}
  Result := Env^^.CallByteMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallByteMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar): JByte;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallByteMethod(Name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallByteMethod(Obj, MethodID);
end;

function TJNIEnv.CallByteMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallByteMethodA');
  {$ENDIF}
  Result := Env^^.CallByteMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallByteMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallByteMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallByteMethodA(Env, Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallByteMethodA(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JByte;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallByteMethodA(Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallByteMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallByteMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallByteMethodV');
  {$ENDIF}
  Result := Env^^.CallByteMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallCharMethod(Obj: JObject; MethodID: JMethodID ): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallCharMethod');
  {$ENDIF}
  Result := Env^^.CallCharMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallCharMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar): JChar;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallCharMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallCharMethod(Obj, MethodID);
end;

function TJNIEnv.CallCharMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallCharMethodA');
  {$ENDIF}
  Result := Env^^.CallCharMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallCharMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallCharMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallCharMethodA(Env, Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallCharMethodA(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JChar;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallCharMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallCharMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallCharMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallCharMethodV');
  {$ENDIF}
  Result := Env^^.CallCharMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallDoubleMethod(Obj: JObject; MethodID: JMethodID ): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallDoubleMethod');
  {$ENDIF}
  Result := Env^^.CallDoubleMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallDoubleMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar): JDouble;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallDoubleMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallDoubleMethod(Obj, MethodID);

end;

function TJNIEnv.CallDoubleMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallDoubleMethodA');
  {$ENDIF}
  Result := Env^^.CallDoubleMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallDoubleMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallDoubleMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallDoubleMethodA(Env, Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallDoubleMethodA(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JDouble;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallDoubleMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallDoubleMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallDoubleMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallDoubleMethodV');
  {$ENDIF}
  Result := Env^^.CallDoubleMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallFloatMethod(Obj: JObject; MethodID: JMethodID ): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallFloatMethod');
  {$ENDIF}
  Result := Env^^.CallFloatMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallFloatMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar): JFloat;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallFloatMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallFloatMethod(Obj, MethodID);

end;

function TJNIEnv.CallFloatMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallFloatMethodA');
  {$ENDIF}
  Result := Env^^.CallFloatMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallFloatMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallFloatMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallFloatMethodA(Env, Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallFloatMethodA(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JFloat;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallFloatMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallFloatMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallFloatMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallFloatMethodV');
  {$ENDIF}
  Result := Env^^.CallFloatMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallIntMethod(Obj: JObject; MethodID: JMethodID ): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallIntMethod()');
  {$ENDIF}
  Result := Env^^.CallIntMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallIntMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar): JInt;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallIntMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallIntMethod(Obj, MethodID);
end;

function TJNIEnv.CallIntMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallIntMethodA()');
  {$ENDIF}
  Result := Env^^.CallIntMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallIntMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallIntMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallIntMethodA(Env, Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallIntMethodA(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JInt;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallIntMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallIntMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallIntMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallIntMethodV');
  {$ENDIF}
  Result := Env^^.CallIntMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallLongMethod(Obj: JObject; MethodID: JMethodID ): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallLongMethod()');
  {$ENDIF}
    Result := Env^^.CallLongMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallLongMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar): JLong;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallLongMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallLongMethod(Obj, MethodID);

end;

function TJNIEnv.CallLongMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallLongMethodA()');
  {$ENDIF}
  Result := Env^^.CallLongMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallLongMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallLongMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallLongMethodA(Env, Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallLongMethodA(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JLong;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallLongMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallLongMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallLongMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallLongMethodV()');
  {$ENDIF}
  Result := Env^^.CallLongMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallObjectMethod(Obj: JObject; MethodID: JMethodID ): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallObjectMethod');
  {$ENDIF}
  Result := Env^^.CallObjectMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallObjectMethod(Obj: JObject; AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JObject;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallObjectMethod(Name, Sig)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallObjectMethod(Obj, MethodID);
end;

function TJNIEnv.CallObjectMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallObjectMethodA');
  {$ENDIF}
  Result := Env^^.CallObjectMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallObjectMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallObjectMethodA(Args: array of const)');
  {$ENDIF}
   CallObjectMethodA(Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallObjectMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JObject;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallObjectMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallObjectMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallObjectMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallObjectMethodV');
  {$ENDIF}
  Result := Env^^.CallObjectMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallShortMethod(Obj: JObject; MethodID: JMethodID ): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallShortMethod()');
  {$ENDIF}
  Result := Env^^.CallShortMethod(Env, Obj, MethodID );
end;

function TJNIEnv.CallShortMethod(Obj: JObject; AClass: JClass;  const Name: PAnsiChar;
  const Sig: PAnsiChar): JShort;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallShortMethod(Name, Sig)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallShortMethod(Obj, MethodID);
end;

function TJNIEnv.CallShortMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallShortMethodA');
  {$ENDIF}
  Result := Env^^.CallShortMethodA(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallShortMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallShortMethodA(Args: array of const)');
  {$ENDIF}
  CallShortMethodA(Obj, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallShortMethodA(Obj: JObject; AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JShort;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallShortMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := CallShortMethodA(Obj, MethodID, Args);
end;

function TJNIEnv.CallShortMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallShortMethodV');
  {$ENDIF}
  Result := Env^^.CallShortMethodV(Env, Obj, MethodID, Args);
end;

procedure TJNIEnv.CallVoidMethodA(Obj: JObject; MethodID: JMethodID; Args: PJValue);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallVoidMethodA');
  {$ENDIF}
  Env^^.CallVoidMethodA(Env, Obj, MethodID, Args);
end;

procedure TJNIEnv.CallVoidMethodA(Obj: JObject; MethodID: JMethodID;
  const Args: array of const);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallVoidMethodA(Args: array of const)');
  {$ENDIF}
  Env^^.CallVoidMethodA(Env, Obj, MethodID, ArgsToJValues(Args));
end;

procedure TJNIEnv.CallVoidMethodA(Obj: JObject;  AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const);
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallVoidMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    CallVoidMethodA(Obj, MethodID, Args);
end;

procedure TJNIEnv.CallVoidMethodV(Obj: JObject; MethodID: JMethodID; Args: va_list);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallVoidMethodV');
  {$ENDIF}
  Env^^.CallVoidMethodV(Env, Obj, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualBooleanMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualBooleanMethod()');
  {$ENDIF}
  Result := Env^^.CallNonvirtualBooleanMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualBooleanMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JBoolean;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualBooleanMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualBooleanMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualBooleanMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualBooleanMethodA');
  {$ENDIF}
  Result := Env^^.CallNonvirtualBooleanMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualBooleanMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualBooleanMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualBooleanMethodA(Env, Obj, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualBooleanMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JBoolean;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualBooleanMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualBooleanMethodA(Obj,AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualBooleanMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualBooleanMethodV');
  {$ENDIF}
  Result := Env^^.CallNonvirtualBooleanMethodV(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualByteMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualByteMethod()');
  {$ENDIF}
  Result := Env^^.CallNonvirtualByteMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualByteMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JByte;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualByteMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualByteMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualByteMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualByteMethodA()');
  {$ENDIF}
  Result := Env^^.CallNonvirtualByteMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualByteMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualByteMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualByteMethodA(Env, Obj, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualByteMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JByte;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualByteMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualByteMethodA(Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualByteMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualByteMethodV');
  {$ENDIF}
  Result := Env^^.CallNonvirtualByteMethodV(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualCharMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualCharMethod()');
  {$ENDIF}
  Result := Env^^.CallNonvirtualCharMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualCharMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JChar;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualCharMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualCharMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualCharMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualCharMethodA');
  {$ENDIF}
  Result := Env^^.CallNonvirtualCharMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualCharMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualCharMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualCharMethodA(Env, Obj, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualCharMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JChar;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualCharMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualCharMethodA(Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualCharMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualCharMethodV');
  {$ENDIF}
  Result := Env^^.CallNonvirtualCharMethodV(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualDoubleMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualDoubleMethod');
  {$ENDIF}
  Result := Env^^.CallNonvirtualDoubleMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualDoubleMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JDouble;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualDoubleMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualDoubleMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualDoubleMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualDoubleMethodA');
  {$ENDIF}
  Result := Env^^.CallNonvirtualDoubleMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualDoubleMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualDoubleMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualDoubleMethodA(Env, Obj, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualDoubleMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JDouble;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualDoubleMethodA(name, Sig, Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualDoubleMethodA(Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualDoubleMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualDoubleMethodV');
  {$ENDIF}
  Result := Env^^.CallNonvirtualDoubleMethodV(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualFloatMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualFloatMethod');
  {$ENDIF}
  Result := Env^^.CallNonvirtualFloatMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualFloatMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JFloat;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualFloatMethod(name, sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualFloatMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualFloatMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualFloatMethodA');
  {$ENDIF}
  Result := Env^^.CallNonvirtualFloatMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualFloatMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualFloatMethodA( Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualFloatMethodA(Env, Obj, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualFloatMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JFloat;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualFloatMethodA(name, Sig, Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualFloatMethodA(Obj, AClass, MethodID, Args);

end;

function TJNIEnv.CallNonvirtualFloatMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualFloatMethodV');
  {$ENDIF}
  Result := Env^^.CallNonvirtualFloatMethodV(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualIntMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualIntMethod');
  {$ENDIF}
  Result := Env^^.CallNonvirtualIntMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualIntMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JInt;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualIntMethod(name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualIntMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualIntMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualIntMethodA( Args: PJValue)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualIntMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualIntMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualIntMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualIntMethodA(Env, Obj, AClass, MethodID,  ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualIntMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JInt;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualIntMethodA(Name, Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualIntMethodA(Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualIntMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualIntMethodV()');
  {$ENDIF}
  Result := Env^^.CallNonvirtualIntMethodV(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualLongMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualLongMethod');
  {$ENDIF}
  Result := Env^^.CallNonvirtualLongMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualLongMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JLong;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualLongMethod(name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualLongMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualLongMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualLongMethodA');
  {$ENDIF}
  Result := Env^^.CallNonvirtualLongMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualLongMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualLongMethodA( Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualLongMethodA(Env, Obj, AClass, MethodID,  ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualLongMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JLong;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualLongMethodA(Name, Sig, Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualLongMethodA(Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualLongMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualLongMethodV');
  {$ENDIF}
  Result := Env^^.CallNonvirtualLongMethodV(Env, Obj, AClass, MethodID, Args);
end;
{-------------------------------------------------}
function TJNIEnv.CallNonvirtualObjectMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualObjectMethod');
  {$ENDIF}
  Result := Env^^.CallNonvirtualObjectMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualObjectMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JObject;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualObjectMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualObjectMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualObjectMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualObjectMethodA');
  {$ENDIF}
  Result := Env^^.CallNonvirtualObjectMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualObjectMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualObjectMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualObjectMethodA(Env, Obj, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualObjectMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JObject;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualObjectMethodA(name, Sig, Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualObjectMethodA(Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualObjectMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualObjectMethodV');
  {$ENDIF}
  Result := Env^^.CallNonvirtualObjectMethodV(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualShortMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID ): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualShortMethod');
  {$ENDIF}
  Result := Env^^.CallNonvirtualShortMethod(Env, Obj, AClass, MethodID );
end;

function TJNIEnv.CallNonvirtualShortMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar): JShort;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualShortMethod(Name, Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualShortMethod(Obj, AClass, MethodID);
end;

function TJNIEnv.CallNonvirtualShortMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualShortMethodA');
  {$ENDIF}
  Result := Env^^.CallNonvirtualShortMethodA(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualShortMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualShortMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallNonvirtualShortMethodA(Env, Obj, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallNonvirtualShortMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JShort;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualShortMethodA(name,Sig, Args: array of const');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    Result := CallNonvirtualShortMethodA(Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallNonvirtualShortMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualShortMethodV');
  {$ENDIF}
  Result := Env^^.CallNonvirtualShortMethodV(Env, Obj, AClass, MethodID, Args);
end;

procedure TJNIEnv.CallNonvirtualVoidMethod(Obj: JObject; AClass: JClass; MethodID: JMethodID );
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualVoidMethod');
  {$ENDIF}
  Env^^.CallNonvirtualVoidMethod(Env, Obj, AClass, MethodID );
end;

procedure TJNIEnv.CallNonvirtualVoidMethod(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar);
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualVoidMethod(name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    CallNonvirtualVoidMethod(Obj, AClass, MethodID);
end;

procedure TJNIEnv.CallNonvirtualVoidMethodA(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: PJValue);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualVoidMethodA');
  {$ENDIF}
  Env^^.CallNonvirtualDoubleMethodA(Env, Obj, AClass, MethodID, Args);
end;

procedure TJNIEnv.CallNonvirtualVoidMethodA(Obj: JObject; AClass: JClass;
  MethodID: JMethodID; const Args: array of const);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualVoidMethodA( Args: array of const)');
  {$ENDIF}
  Env^^.CallNonvirtualDoubleMethodA(Env, Obj, AClass, MethodID, ArgsToJValues(Args));
end;

procedure TJNIEnv.CallNonvirtualVoidMethodA(Obj: JObject; AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar; const Args: array of const);
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualVoidMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
    CallNonvirtualVoidMethodA(Obj, AClass, MethodID, Args);
end;

procedure TJNIEnv.CallNonvirtualVoidMethodV(Obj: JObject; AClass: JClass; MethodID: JMethodID; Args: va_list);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallNonvirtualVoidMethodV');
  {$ENDIF}
  Env^^.CallNonvirtualVoidMethodV(Env, Obj, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticBooleanMethod(AClass: JClass; MethodID: JMethodID ): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticBooleanMethod');
  {$ENDIF}
  Result := Env^^.CallStaticBooleanMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticBooleanMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JBoolean;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticBooleanMethod(Name,Sig)');
  {$ENDIF}
  MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
  Result := CallStaticBooleanMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticBooleanMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticBooleanMethodA()');
  {$ENDIF}
  Result := Env^^.CallStaticBooleanMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticBooleanMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticBooleanMethodA( Args: array of const)');
  {$ENDIF}
  CallStaticBooleanMethodA(AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticBooleanMethodA(AClass: JClass;
  const Name: PAnsiChar; const Sig: PAnsiChar;
  const Args: array of const): JBoolean;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticBooleanMethodA(Name,Sig,  Args: array of const)');
  {$ENDIF}
  MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
  Result := CallStaticBooleanMethodA(AClass, MethodID, Args );
end;

function TJNIEnv.CallStaticBooleanMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticBooleanMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticBooleanMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticByteMethod(AClass: JClass; MethodID: JMethodID ): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticByteMethod');
  {$ENDIF}
  Result := Env^^.CallStaticByteMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticByteMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JByte;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticByteMethod(name,Sig)');
  {$ENDIF}
  MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
  Result := CallStaticByteMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticByteMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticByteMethodA');
  {$ENDIF}
  Result := Env^^.CallStaticByteMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticByteMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticByteMethodA( Args: array of const)');
  {$ENDIF}
   CallStaticByteMethodA(AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticByteMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JByte;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticByteMethodA(Name,Sig, Args: array of const');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticByteMethodA(AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticByteMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticByteMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticByteMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticCharMethod(AClass: JClass; MethodID: JMethodID ): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticCharMethod');
  {$ENDIF}
  Result := Env^^.CallStaticCharMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticCharMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JChar;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticCharMethod(Name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticCharMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticCharMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticCharMethodA');
  {$ENDIF}
  Result := Env^^.CallStaticCharMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticCharMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticCharMethodA( Args: array of const)');
  {$ENDIF}
   CallStaticCharMethodA(AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticCharMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JChar;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticCharMethodA(name,Sig,Args: array of const');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticCharMethodA(AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticCharMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticCharMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticCharMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticDoubleMethod(AClass: JClass; MethodID: JMethodID ): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticDoubleMethod');
  {$ENDIF}
  Result := Env^^.CallStaticDoubleMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticDoubleMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JDouble;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticDoubleMethod(Name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticDoubleMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticDoubleMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticDoubleMethodA');
  {$ENDIF}
  Result := Env^^.CallStaticDoubleMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticDoubleMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticDoubleMethodA( Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallStaticDoubleMethodA(Env, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticDoubleMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JDouble;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticDoubleMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticDoubleMethodA(AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticDoubleMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticDoubleMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticDoubleMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticFloatMethod(AClass: JClass; MethodID: JMethodID ): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticFloatMethod');
  {$ENDIF}
  Result := Env^^.CallStaticFloatMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticFloatMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JFloat;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticFloatMethod(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticFloatMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticFloatMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticFloatMethodA');
  {$ENDIF}
  Result := Env^^.CallStaticFloatMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticFloatMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticFloatMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallStaticFloatMethodA(Env, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticFloatMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JFloat;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticFloatMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticFloatMethodA(AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticFloatMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticFloatMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticFloatMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticIntMethod(AClass: JClass; MethodID: JMethodID ): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticIntMethod');
  {$ENDIF}
  Result := Env^^.CallStaticIntMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticIntMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JInt;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticIntMethod(Name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticIntMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticIntMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticIntMethodA');
  {$ENDIF}
  Result := Env^^.CallStaticIntMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticIntMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticIntMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallStaticIntMethodA(Env, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticIntMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JInt;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticIntMethodA(name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticIntMethodA(AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticIntMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticIntMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticIntMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticLongMethod(AClass: JClass; MethodID: JMethodID ): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticLongMethod');
  {$ENDIF}
  Result := Env^^.CallStaticLongMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticLongMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JLong;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticLongMethod(Name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticLongMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticLongMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticLongMethodA');
  {$ENDIF}
  Result := Env^^.CallStaticLongMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticLongMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticLongMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallStaticLongMethodA(Env, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticLongMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JLong;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticLongMethodA(Name.Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticLongMethodA(AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticLongMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticLongMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticLongMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticObjectMethod(AClass: JClass; MethodID: JMethodID ): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticObjectMethod');
  {$ENDIF}
  Result := Env^^.CallStaticObjectMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticObjectMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JObject;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticObjectMethod(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticObjectMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticObjectMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticObjectMethodA');
  {$ENDIF}
  Result := Env^^.CallStaticObjectMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticObjectMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticObjectMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallStaticObjectMethodA(Env, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticObjectMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JObject;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticObjectMethodA(Name,Sig, Args: array of const');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticObjectMethodA(AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticObjectMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticObjectMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticObjectMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticShortMethod(AClass: JClass; MethodID: JMethodID ): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticShortMethod');
  {$ENDIF}
  Result := Env^^.CallStaticShortMethod(Env, AClass, MethodID );
end;

function TJNIEnv.CallStaticShortMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JShort;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticShortMethod(name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticShortMethod(AClass, MethodID);
end;

function TJNIEnv.CallStaticShortMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticShortMethodA');
  {$ENDIF}
  Result := Env^^.CallStaticShortMethodA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticShortMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticShortMethodA(Args: array of const)');
  {$ENDIF}
  Result := Env^^.CallStaticShortMethodA(Env, AClass, MethodID, ArgsToJValues(Args));
end;

function TJNIEnv.CallStaticShortMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JShort;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticShortMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    Result := CallStaticShortMethodA(AClass, MethodID, Args);
end;

function TJNIEnv.CallStaticShortMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticShortMethodV');
  {$ENDIF}
  Result := Env^^.CallStaticShortMethodV(Env, AClass, MethodID, Args);
end;

procedure TJNIEnv.CallStaticVoidMethod(AClass: JClass; MethodID: JMethodID );
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticVoidMethod');
  {$ENDIF}
  Env^^.CallStaticVoidMethod(Env, AClass, MethodID );
end;

procedure TJNIEnv.CallStaticVoidMethod(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar);
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticVoidMethod(name,Sig)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    CallStaticVoidMethod(AClass, MethodID);
end;

procedure TJNIEnv.CallStaticVoidMethodA(AClass: JClass; MethodID: JMethodID; Args: PJValue);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticVoidMethodA');
  {$ENDIF}
  Env^^.CallStaticVoidMethodA(Env, AClass, MethodID, Args);
end;

procedure TJNIEnv.CallStaticVoidMethodA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticVoidMethodA(Args: array of const)');
  {$ENDIF}
  Env^^.CallStaticVoidMethodA(Env, AClass, MethodID, ArgsToJValues(Args));
end;

procedure TJNIEnv.CallStaticVoidMethodA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const);
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticVoidMethodA(Name,Sig,Args: array of const)');
  {$ENDIF}
    MethodID := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
    CallStaticVoidMethodA(AClass, MethodID, Args);
end;

procedure TJNIEnv.CallStaticVoidMethodV(AClass: JClass; MethodID: JMethodID; Args: va_list);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: CallStaticVoidMethodV');
  {$ENDIF}
  Env^^.CallStaticVoidMethodV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.DefineClass(const Name: PAnsiChar; Loader: JObject; const Buf: PJByte; Len: JSize): JClass;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: DefineClass');
  {$ENDIF}
  Result := Env^^.DefineClass(Env, Name, Loader, Buf, Len);
end;

procedure TJNIEnv.DeleteGlobalRef(GRef: JObject);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: DeleteGlobalRef');
  {$ENDIF}
  Env^^.DeleteGlobalRef(Env, GRef);
end;

procedure TJNIEnv.DeleteLocalRef(Obj: JObject);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: DeleteLocalRef');
  {$ENDIF}
  Env^^.DeleteLocalRef(Env, Obj);
end;

procedure TJNIEnv.ExceptionClear;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ExceptionClear');
  {$ENDIF}
  Env^^.ExceptionClear(Env);
end;

procedure TJNIEnv.ExceptionDescribe;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ExceptionDescribe');
  {$ENDIF}
  Env^^.ExceptionDescribe(Env);
end;

function TJNIEnv.ExceptionOccurred: JThrowable;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ExceptionOccurred');
  {$ENDIF}
  Result := Env^^.ExceptionOccurred(Env);
end;

procedure TJNIEnv.FatalError(const Msg: PAnsiChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: FatalError');
  {$ENDIF}
  Env^^.FatalError(Env, Msg);
end;

function TJNIEnv.FindClass(const Name: PAnsiChar): JClass;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: FindClass');
  {$ENDIF}
  Result := Env^^.FindClass(Env, Name);
end;

function TJNIEnv.GetArrayLength(AArray: JArray): JSize;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetArrayLength');
  {$ENDIF}
  Result := Env^^.GetArrayLength(Env, AArray);
end;

function TJNIEnv.GetBooleanArrayElements(AArray: JBooleanArray; var IsCopy: JBoolean): PJBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetBooleanArrayElements');
  {$ENDIF}
  Result := Env^^.GetBooleanArrayElements(Env, AArray, IsCopy);
end;

procedure TJNIEnv.GetBooleanArrayRegion(AArray: JBooleanArray; Start: JSize;
  Len: JSize; Buf: PJBoolean);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetBooleanArrayRegion');
  {$ENDIF}
  Env^^.GetBooleanArrayRegion(Env, AArray, Start, Len, Buf);
end;

function TJNIEnv.GetBooleanField(Obj: JObject; FieldID: JFieldID): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetBooleanField');
  {$ENDIF}
  Result := Env^^.GetBooleanField(Env, Obj, FieldID);
end;

function TJNIEnv.GetByteArrayElements(AArray: JByteArray; var IsCopy: JBoolean): PJByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetByteArrayElements');
  {$ENDIF}
  Result := Env^^.GetByteArrayElements(Env, AArray, IsCopy);
end;

procedure TJNIEnv.GetByteArrayRegion(AArray: JByteArray; Start: JSize;
  Len: JSize; Buf: PJByte);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetByteArrayRegion');
  {$ENDIF}
  Env^^.GetByteArrayRegion(Env, AArray, Start, Len, Buf);
end;

function TJNIEnv.GetByteField(Obj: JObject; FieldID: JFieldID): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetByteField');
  {$ENDIF}
  Result := Env^^.GetByteField(Env, Obj, FieldID);
end;

function TJNIEnv.GetCharArrayElements(AArray: JCharArray; var IsCopy: JBoolean): PJChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetCharArrayElements');
  {$ENDIF}
  Result := Env^^.GetCharArrayElements(Env, AArray, IsCopy);
end;

procedure TJNIEnv.GetCharArrayRegion(AArray: JCharArray; Start: JSize;
  Len: JSize; Buf: PJChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetCharArrayRegion');
  {$ENDIF}
  Env^^.GetCharArrayRegion(Env, AArray, Start, Len, Buf);
end;

function TJNIEnv.GetCharField(Obj: JObject; FieldID: JFieldID): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetCharField');
  {$ENDIF}
  Result := Env^^.GetCharField(Env, Obj, FieldID);
end;

function TJNIEnv.GetDoubleArrayElements(AArray: JDoubleArray; var IsCopy: JBoolean): PJDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetDoubleArrayElements');
  {$ENDIF}
  Result := Env^^.GetDoubleArrayElements(Env, AArray, IsCopy);
end;

procedure TJNIEnv.GetDoubleArrayRegion(AArray: JDoubleArray; Start: JSize;
  Len: JSize; Buf: PJDouble);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetDoubleArrayRegion');
  {$ENDIF}
  Env^^.GetDoubleArrayRegion(Env, AArray, Start, Len, Buf);
end;

function TJNIEnv.GetDoubleField(Obj: JObject; FieldID: JFieldID): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetDoubleField');
  {$ENDIF}
  Result := Env^^.GetDoubleField(Env, Obj, FieldID);
end;

function TJNIEnv.GetFieldID(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JFieldID;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetFieldID');
  {$ENDIF}
  Result := Env^^.GetFieldID(Env, AClass, Name, Sig);
end;

function TJNIEnv.GetFloatArrayElements(AArray: JFloatArray; var IsCopy: JBoolean): PJFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetFloatArrayElements');
  {$ENDIF}
  Result := Env^^.GetFloatArrayElements(Env, AArray, IsCopy);
end;

procedure TJNIEnv.GetFloatArrayRegion(AArray: JFloatArray; Start: JSize;
  Len: JSize; Buf: PJFloat);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetFloatArrayRegion');
  {$ENDIF}
  Env^^.GetFloatArrayRegion(Env, AArray, Start, Len, Buf);
end;

function TJNIEnv.GetFloatField(Obj: JObject; FieldID: JFieldID): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetFloatField');
  {$ENDIF}
  Result := Env^^.GetFloatField(Env, Obj, FieldID);
end;

function TJNIEnv.GetIntArrayElements(AArray: JIntArray; var IsCopy: JBoolean): PJInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetIntArrayElements');
  {$ENDIF}
  Result := Env^^.GetIntArrayElements(Env, AArray, IsCopy);
end;

procedure TJNIEnv.GetIntArrayRegion(AArray: JIntArray; Start: JSize;
  Len: JSize; Buf: PJInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetIntArrayRegion');
  {$ENDIF}
  Env^^.GetIntArrayRegion(Env, AArray, Start, Len, Buf);
end;

function TJNIEnv.GetIntField(Obj: JObject; FieldID: JFieldID): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetIntField');
  {$ENDIF}
  Result := Env^^.GetIntField(Env, Obj, FieldID);
end;

function TJNIEnv.GetJavaVM(var VM: JavaVM): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetJavaVM');
  {$ENDIF}
  Result :=Env^^.GetJavaVM(Env, VM);
end;

function TJNIEnv.GetLongArrayElements(AArray: JLongArray; var IsCopy: JBoolean): PJLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetLongArrayElements');
  {$ENDIF}
  Result := Env^^.GetLongArrayElements(Env, AArray, IsCopy);
end;

procedure TJNIEnv.GetLongArrayRegion(AArray: JLongArray; Start: JSize;
  Len: JSize; Buf: PJLong);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetLongArrayRegion');
  {$ENDIF}
  Env^^.GetLongArrayRegion(Env, AArray, Start, Len, Buf);
end;

function TJNIEnv.GetLongField(Obj: JObject; FieldID: JFieldID): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetLongField');
  {$ENDIF}
  Result := Env^^.GetLongField(Env, Obj, FieldID);
end;

function TJNIEnv.GetMethodID(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JMethodID;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetMethodID');
  {$ENDIF}
  Result := Env^^.GetMethodID(Env, AClass, Name, Sig);
end;

function TJNIEnv.GetObjectArrayElement(AArray: JObjectArray; Index: JSize): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetObjectArrayElement');
  {$ENDIF}
  Result := Env^^.GetObjectArrayElement(Env, AArray, Index);
end;

function TJNIEnv.GetObjectClass(Obj: JObject): JClass;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetObjectClass');
  {$ENDIF}
  Result := Env^^.GetObjectClass(Env, Obj);
end;

function TJNIEnv.GetObjectField(Obj: JObject; FieldID: JFieldID): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetObjectField');
  {$ENDIF}
  Result := Env^^.GetObjectField(Env, Obj, FieldID);
end;

function TJNIEnv.GetShortArrayElements(AArray: JShortArray; var IsCopy: JBoolean): PJShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetShortArrayElements');
  {$ENDIF}
  Result := Env^^.GetShortArrayElements(Env, AArray, IsCopy);
end;

procedure TJNIEnv.GetShortArrayRegion(AArray: JShortArray; Start: JSize;
  Len: JSize; Buf: PJShort);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetShortArrayRegion');
  {$ENDIF}
  Env^^.GetShortArrayRegion(Env, AArray, Start, Len, Buf);
end;

function TJNIEnv.GetShortField(Obj: JObject; FieldID: JFieldID): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetShortField');
  {$ENDIF}
  Result := Env^^.GetShortField(Env, Obj, FieldID);
end;

function TJNIEnv.GetStaticBooleanField(AClass: JClass; FieldID: JFieldID): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticBooleanField');
  {$ENDIF}
  Result := Env^^.GetStaticBooleanField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStaticByteField(AClass: JClass; FieldID: JFieldID): JByte;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticByteField');
  {$ENDIF}
  Result := Env^^.GetStaticByteField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStaticCharField(AClass: JClass; FieldID: JFieldID): JChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticCharField');
  {$ENDIF}
  Result := Env^^.GetStaticCharField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStaticDoubleField(AClass: JClass; FieldID: JFieldID): JDouble;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticDoubleField');
  {$ENDIF}
  Result := Env^^.GetStaticDoubleField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStaticFieldID(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JFieldID;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticFieldID');
  {$ENDIF}
  Result := Env^^.GetStaticFieldID(Env, AClass, Name, Sig);
end;

function TJNIEnv.GetStaticFloatField(AClass: JClass; FieldID: JFieldID): JFloat;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticFloatField');
  {$ENDIF}
  Result := Env^^.GetStaticFloatField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStaticIntField(AClass: JClass; FieldID: JFieldID): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticIntField');
  {$ENDIF}
  Result := Env^^.GetStaticIntField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStaticLongField(AClass: JClass; FieldID: JFieldID): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticLongField');
  {$ENDIF}
  Result := Env^^.GetStaticLongField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStaticMethodID(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JMethodID;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticMethodID');
  {$ENDIF}
  Result := Env^^.GetStaticMethodID(Env, AClass, Name, Sig);
end;

function TJNIEnv.GetStaticObjectField(AClass: JClass; FieldID: JFieldID): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticObjectField');
  {$ENDIF}
  Result := Env^^.GetStaticObjectField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStaticShortField(AClass: JClass; FieldID: JFieldID): JShort;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStaticShortField');
  {$ENDIF}
  Result := Env^^.GetStaticShortField(Env, AClass, FieldID);
end;

function TJNIEnv.GetStringChars(Str: JString; var IsCopy: JBoolean): PJChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStringChars');
  {$ENDIF}
  Result := Env^^.GetStringChars(Env, Str, IsCopy);
end;

function TJNIEnv.GetStringLength(Str: JString): JSize;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStringLength');
  {$ENDIF}
   Result := Env^^.GetStringLength(Env, Str);
end;

function TJNIEnv.GetStringUTFChars(Str: JString; var IsCopy: JBoolean): PAnsiChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStringUTFChars');
  {$ENDIF}
  Result := Env^^.GetStringUTFChars(Env, Str, @IsCopy);
end;

function TJNIEnv.GetStringUTFLength(Str: JString): JSize;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStringUTFLength');
  {$ENDIF}
  Result := Env^^.GetStringUTFLength(Env, Str);
end;

function TJNIEnv.GetSuperclass(Sub: JClass): JClass;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetSuperclass');
  {$ENDIF}
  Result := Env^^.GetSuperclass(Env, Sub);
end;

function TJNIEnv.IsAssignableFrom(Sub: JClass; Sup: JClass): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: IsAssignableFrom');
  {$ENDIF}
  Result := Env^^.IsAssignableFrom(Env, Sub, Sup);
end;

function TJNIEnv.IsInstanceOf(Obj: JObject; AClass: JClass): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: IsInstanceOf');
  {$ENDIF}
  Result := Env^^.IsInstanceOf(Env, Obj, AClass);
end;

function TJNIEnv.IsSameObject(Obj1: JObject; Obj2: JObject): JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: IsSameObject');
  {$ENDIF}
  Result := Env^^.IsSameObject(Env, Obj1, Obj2);
end;

function TJNIEnv.MonitorEnter(Obj: JObject): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: MonitorEnter');
  {$ENDIF}
  Result := Env^^.MonitorEnter(Env, Obj);
end;

function TJNIEnv.MonitorExit(Obj: JObject): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: MonitorExit');
  {$ENDIF}
  Result := Env^^.MonitorExit(Env, Obj);
end;

function TJNIEnv.NewBooleanArray(Len: JSize): JBooleanArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewBooleanArray');
  {$ENDIF}
  Result := Env^^.NewBooleanArray(Env, Len);
end;

function TJNIEnv.NewByteArray(Len: JSize): JByteArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewByteArray');
  {$ENDIF}
  Result := Env^^.NewByteArray(Env, Len);
end;

function TJNIEnv.NewCharArray(Len: JSize): JCharArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewCharArray');
  {$ENDIF}
  Result := Env^^.NewCharArray(Env, Len);
end;

function TJNIEnv.NewDoubleArray(Len: JSize): JDoubleArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewDoubleArray');
  {$ENDIF}
  Result := Env^^.NewDoubleArray(Env, Len);
end;

function TJNIEnv.NewFloatArray(Len: JSize): JFloatArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewFloatArray');
  {$ENDIF}
  Result := Env^^.NewFloatArray(Env, Len);
end;

function TJNIEnv.NewGlobalRef(LObj: JObject): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewGlobalRef');
  {$ENDIF}
  Result := FEnv^^.NewGlobalRef(Env, LObj);
end;

function TJNIEnv.NewIntArray(Len: JSize): JIntArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewIntArray');
  {$ENDIF}
  Result:= Env^^.NewIntArray(Env, Len);
end;

function TJNIEnv.NewLongArray(Len: JSize): JLongArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewLongArray');
  {$ENDIF}
  Result := Env^^.NewLongArray(Env, Len);
end;

function TJNIEnv.NewObject(AClass: JClass; MethodID: JMethodID ): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewObject');
  {$ENDIF}
  Result := Env^^.NewObject(Env, AClass, MethodID );
end;

function TJNIEnv.NewObject(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar): JObject;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewObject');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := NewObject(AClass, MethodID);
end;

function TJNIEnv.NewObjectA(AClass: JClass; MethodID: JMethodID; Args: PJValue): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewObjectA');
  {$ENDIF}
  Result := Env^^.NewObjectA(Env, AClass, MethodID, Args);
end;

function TJNIEnv.NewObjectA(AClass: JClass; MethodID: JMethodID;
  const Args: array of const): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewObjectA');
  {$ENDIF}
  Result := Env^^.NewObjectA(Env, AClass, MethodID, ArgsToJValues(Args) );
end;

function TJNIEnv.NewObjectA(AClass: JClass; const Name: PAnsiChar;
  const Sig: PAnsiChar; const Args: array of const): JObject;
var MethodID: jmethodid = nil;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewObjectA');
  {$ENDIF}
  MethodID := Env^^.GetMethodID(Env, AClass, Name, Sig);
  Result := NewObjectA(AClass, MethodID, Args);
end;

function TJNIEnv.NewObjectV(AClass: JClass; MethodID: JMethodID; Args: va_list): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewObjectV');
  {$ENDIF}
  Result := Env^^.NewObjectV(Env, AClass, MethodID, Args);
end;

function TJNIEnv.NewObjectArray(Len: JSize; AClass: JClass; Init: JObject): JObjectArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewObjectArray');
  {$ENDIF}
  Result := Env^^.NewObjectArray(Env, Len, AClass, Init);
end;

function TJNIEnv.NewShortArray(Len: JSize): JShortArray;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewShortArray');
  {$ENDIF}
  Result := Env^^.NewShortArray(Env, Len);
end;

function TJNIEnv.NewString(const Unicode: PJChar; Len: JSize): JString;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewString');
  {$ENDIF}
  Result := Env^^.NewString(Env, Unicode, Len);
end;

function TJNIEnv.NewStringUTF(const UTF: PAnsiChar): JString;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewStringUTF');
  {$ENDIF}
  Result := Env^^.NewStringUTF(Env, UTF);
end;

function TJNIEnv.RegisterNatives(AClass: JClass; const Methods: PJNINativeMethod; NMethods: JInt): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: RegisterNatives');
  {$ENDIF}
  Result := Env^^.RegisterNatives(Env, AClass, Methods, NMethods);
end;

function TJNIEnv.UnregisterNatives(AClass: JClass): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: UnregisterNatives');
  {$ENDIF}
  Result := Env^^.UnregisterNatives(Env, AClass);
end;

procedure TJNIEnv.ReleaseBooleanArrayElements(AArray: JBooleanArray; Elems: PJBoolean; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseBooleanArrayElements');
  {$ENDIF}
  Env^^.ReleaseBooleanArrayElements(Env, AArray, Elems, Mode);
end;

procedure TJNIEnv.ReleaseByteArrayElements(AArray: JByteArray; Elems: PJByte; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseByteArrayElements');
  {$ENDIF}
  Env^^.ReleaseByteArrayElements(Env, AArray, Elems, Mode);
end;

procedure TJNIEnv.ReleaseCharArrayElements(AArray: JCharArray; Elems: PJChar; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseCharArrayElements');
  {$ENDIF}
  Env^^.ReleaseCharArrayElements(Env, AArray, Elems, Mode);
end;

procedure TJNIEnv.ReleaseDoubleArrayElements(AArray: JDoubleArray; Elems: PJDouble; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseDoubleArrayElements');
  {$ENDIF}
  Env^^.ReleaseDoubleArrayElements(Env, AArray, Elems, Mode);
end;

procedure TJNIEnv.ReleaseFloatArrayElements(AArray: JFloatArray; Elems: PJFloat; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseFloatArrayElements');
  {$ENDIF}
  Env^^.ReleaseFloatArrayElements(Env, AArray, Elems, Mode);
end;

procedure TJNIEnv.ReleaseIntArrayElements(AArray: JIntArray; Elems: PJInt; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseIntArrayElements');
  {$ENDIF}
  Env^^.ReleaseIntArrayElements(Env, AArray, Elems, Mode);
end;

procedure TJNIEnv.ReleaseLongArrayElements(AArray: JLongArray; Elems: PJLong; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseLongArrayElements');
  {$ENDIF}
  Env^^.ReleaseLongArrayElements(Env, AArray, Elems, Mode);
end;

procedure TJNIEnv.ReleaseShortArrayElements(AArray: JShortArray; Elems: PJShort; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseShortArrayElements');
  {$ENDIF}
  Env^^.ReleaseShortArrayElements(Env, AArray, Elems, Mode);
end;

procedure TJNIEnv.ReleaseStringChars(Str: JString; const Chars: PJChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseStringChars');
  {$ENDIF}
  Env^^.ReleaseStringChars(Env, Str, Chars);
end;

procedure TJNIEnv.ReleaseStringUTFChars(Str: JString; const Chars: PAnsiChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseStringUTFChars');
  {$ENDIF}
  Env^^.ReleaseStringUTFChars(Env, Str, Chars);
end;

procedure TJNIEnv.SetBooleanArrayRegion(AArray: JBooleanArray; Start: JSize;
  Len: JSize; Buf: PJBoolean);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetBooleanArrayRegion');
  {$ENDIF}
  Env^^.SetBooleanArrayRegion(Env, AArray, Start, Len, Buf);
end;

procedure TJNIEnv.SetBooleanField(Obj: JObject; FieldID: JFieldID; Val: JBoolean);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetBooleanField');
  {$ENDIF}
  Env^^.SetBooleanField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetByteArrayRegion(AArray: JByteArray; Start: JSize;
  Len: JSize; Buf: PJByte);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetByteArrayRegion');
  {$ENDIF}
  Env^^.SetByteArrayRegion(Env, AArray, Start, Len, Buf);
end;

procedure TJNIEnv.SetByteField(Obj: JObject; FieldID: JFieldID; Val: JByte);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetByteField');
  {$ENDIF}
  Env^^.SetByteField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetCharArrayRegion(AArray: JCharArray; Start: JSize;
  Len: JSize; Buf: PJChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetCharArrayRegion');
  {$ENDIF}
  Env^^.SetCharArrayRegion(Env, AArray, Start, Len, Buf);
end;

procedure TJNIEnv.SetCharField(Obj: JObject; FieldID: JFieldID; Val: JChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetCharField');
  {$ENDIF}
  Env^^.SetCharField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetDoubleArrayRegion(AArray: JDoubleArray; Start: JSize;
  Len: JSize; Buf: PJDouble);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetDoubleArrayRegion');
  {$ENDIF}
  Env^^.SetDoubleArrayRegion(Env, AArray, Start, Len, Buf);
end;

procedure TJNIEnv.SetDoubleField(Obj: JObject; FieldID: JFieldID; Val: JDouble);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetDoubleField');
  {$ENDIF}
  Env^^.SetDoubleField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetFloatArrayRegion(AArray: JFloatArray; Start: JSize;
  Len: JSize; Buf: PJFloat);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetFloatArrayRegion');
  {$ENDIF}
  Env^^.SetFloatArrayRegion(Env, AArray, Start, Len, Buf);
end;

procedure TJNIEnv.SetFloatField(Obj: JObject; FieldID: JFieldID; Val: JFloat);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetFloatField');
  {$ENDIF}
  Env^^.SetFloatField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetIntArrayRegion(AArray: JIntArray; Start: JSize;
  Len: JSize; Buf: PJInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetIntArrayRegion');
  {$ENDIF}
  Env^^.SetIntArrayRegion(Env, AArray, Start, Len, Buf);
end;

procedure TJNIEnv.SetIntField(Obj: JObject; FieldID: JFieldID; Val: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetIntField');
  {$ENDIF}
  Env^^.SetIntField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetLongArrayRegion(AArray: JLongArray; Start: JSize;
  Len: JSize; Buf: PJLong);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetLongArrayRegion');
  {$ENDIF}
  Env^^.SetLongArrayRegion(Env, AArray, Start, Len, Buf);
end;

procedure TJNIEnv.SetLongField(Obj: JObject; FieldID: JFieldID; Val: JLong);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetLongField');
  {$ENDIF}
  Env^^.SetLongField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetObjectArrayElement(AArray: JObjectArray; Index: JSize; Val: JObject);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetObjectArrayElement');
  {$ENDIF}
  Env^^.SetObjectArrayElement(Env, AArray, Index, Val);
end;

procedure TJNIEnv.SetObjectField(Obj: JObject; FieldID: JFieldID; Val: JObject);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetObjectField');
  {$ENDIF}
  Env^^.SetObjectField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetShortArrayRegion(AArray: JShortArray; Start: JSize;
  Len: JSize; Buf: PJShort);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetShortArrayRegion');
  {$ENDIF}
  Env^^.SetShortArrayRegion(Env, AArray, Start, Len, Buf);
end;

procedure TJNIEnv.SetShortField(Obj: JObject; FieldID: JFieldID; Val: JShort);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetShortField');
  {$ENDIF}
  Env^^.SetShortField(Env, Obj, FieldID, Val);
end;

procedure TJNIEnv.SetStaticBooleanField(AClass: JClass; FieldID: JFieldID; Val: JBoolean);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticBooleanField');
  {$ENDIF}
  Env^^.SetStaticBooleanField(Env, AClass, FieldID, Val);
end;

procedure TJNIEnv.SetStaticByteField(AClass: JClass; FieldID: JFieldID; Val: JByte);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticByteField');
  {$ENDIF}
  Env^^.SetStaticByteField(Env, AClass, FieldID, Val);
end;

procedure TJNIEnv.SetStaticCharField(AClass: JClass; FieldID: JFieldID; Val: JChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticCharField');
  {$ENDIF}
  Env^^.SetStaticCharField(Env, AClass, FieldID, Val);
end;

procedure TJNIEnv.SetStaticDoubleField(AClass: JClass; FieldID: JFieldID; Val: JDouble);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticDoubleField');
  {$ENDIF}
  Env^^.SetStaticDoubleField(Env, AClass, FieldID, Val);
end;

procedure TJNIEnv.SetStaticFloatField(AClass: JClass; FieldID: JFieldID; Val: JFloat);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticFloatField');
  {$ENDIF}
  Env^^.SetStaticFloatField(Env, AClass, FieldID, Val);
end;

procedure TJNIEnv.SetStaticIntField(AClass: JClass; FieldID: JFieldID; Val: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticIntField');
  {$ENDIF}
  Env^^.SetStaticIntField(Env, AClass, FieldID, Val);
end;

procedure TJNIEnv.SetStaticLongField(AClass: JClass; FieldID: JFieldID; Val: JLong);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticLongField');
  {$ENDIF}
  Env^^.SetStaticLongField(Env, AClass, FieldID, Val);
end;

procedure TJNIEnv.SetStaticObjectField(AClass: JClass; FieldID: JFieldID; Val: JObject);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticObjectField');
  {$ENDIF}
  Env^^.SetStaticObjectField(Env, AClass, FieldID, Val);
end;

procedure TJNIEnv.SetStaticShortField(AClass: JClass; FieldID: JFieldID; Val: JShort);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: SetStaticShortField');
  {$ENDIF}
  Env^^.SetStaticShortField(Env, AClass, FieldID, Val);
end;

function TJNIEnv.Throw(Obj: JThrowable): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: Throw');
  {$ENDIF}
  Result := Env^^.Throw(Env, Obj);
end;

function TJNIEnv.ThrowNew(AClass: JClass; const Msg: PAnsiChar): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ThrowNew');
  {$ENDIF}
  Result := Env^^.ThrowNew(Env, AClass, Msg);
end;

procedure TJNIEnv.DeleteWeakGlobalRef(Ref: JWeak);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: DeleteWeakGlobalRef');
  {$ENDIF}
  VersionCheck('DeleteWeakGlobalRef', JNI_VERSION_1_2);
  Env^^.DeleteWeakGlobalRef(Env, Ref);
end;

function TJNIEnv.EnsureLocalCapacity(Capacity: JInt): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: EnsureLocalCapacity');
  {$ENDIF}
  Result := Env^^.EnsureLocalCapacity(Env, Capacity);
end;

function TJNIEnv.ExceptionCheck: JBoolean;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ExceptionCheck');
  {$ENDIF}
  VersionCheck('ExceptionCheck', JNI_VERSION_1_2);
  Result := Env^^.ExceptionCheck(Env)
end;

function TJNIEnv.FromReflectedField(field: JObject): JFieldID;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: FromReflectedField');
  {$ENDIF}
  VersionCheck('FromReflectedField', JNI_VERSION_1_2);
  Result := Env^^.FromReflectedField(Env, Field);
end;

function TJNIEnv.FromReflectedMethod(method: JObject): JMethodID;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: FromReflectedMethod');
  {$ENDIF}
  VersionCheck('FromReflectedMethod', JNI_VERSION_1_2);
  Result := Env^^.FromReflectedMethod(Env, Method);
end;

function TJNIEnv.GetPrimitiveArrayCritical(AArray: JArray; var IsCopy: JBoolean): Pointer;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetPrimitiveArrayCritical');
  {$ENDIF}
  VersionCheck('GetPrimitiveArrayCritical', JNI_VERSION_1_2);
  Result := Env^^.GetPrimitiveArrayCritical(Env, AArray, IsCopy);
end;

function TJNIEnv.GetStringCritical(Str: JString; var IsCopy: JBoolean): PJChar;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStringCritical');
  {$ENDIF}
  VersionCheck('GetStringCritical', JNI_VERSION_1_2);
  Result := Env^^.GetStringCritical(Env, Str, IsCopy);
end;

procedure TJNIEnv.GetStringRegion(Str: JString; Start: JSize; Len: JSize;
  Buf: PJChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStringRegion');
  {$ENDIF}
  VersionCheck('GetStringRegion', JNI_VERSION_1_2);
  Env^^.GetStringRegion(Env, Str, Start, Len, Buf);
end;

procedure TJNIEnv.GetStringUTFRegion(Str: JString; Start: JSize; Len: JSize;
  Buf: PAnsiChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetStringUTFRegion');
  {$ENDIF}
  VersionCheck('GetStringUTFRegion', JNI_VERSION_1_2);
  Env^^.GetStringUTFRegion(Env, Str, Start, Len, Buf);
end;

function TJNIEnv.NewLocalRef(Ref: JObject): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewLocalRef');
  {$ENDIF}
  VersionCheck('NewLocalRef', JNI_VERSION_1_2);
  Result := Env^^.NewLocalRef(Env, Ref);
end;

function TJNIEnv.NewWeakGlobalRef(Obj: JObject): JWeak;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewWeakGlobalRef');
  {$ENDIF}
  VersionCheck('NewWeakGlobalRef', JNI_VERSION_1_2);
  Result := Env^^.NewWeakGlobalRef(Env, Obj);
end;

function TJNIEnv.PopLocalFrame(AResult: JObject): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: PopLocalFrame');
  {$ENDIF}
  VersionCheck('PopLocalFrame', JNI_VERSION_1_2);
  Result := Env^^.PopLocalFrame(Env, AResult);
end;

function TJNIEnv.PushLocalFrame(Capacity: JInt): JInt;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: PushLocalFrame');
  {$ENDIF}
  VersionCheck('PushLocalFrame', JNI_VERSION_1_2);
  Result := Env^^.PushLocalFrame(Env, Capacity);
end;

procedure TJNIEnv.ReleasePrimitiveArrayCritical(AArray: JArray; CArray: Pointer; Mode: JInt);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleasePrimitiveArrayCritical');
  {$ENDIF}
  VersionCheck('ReleasePrimitiveArrayCritical', JNI_VERSION_1_2);
  Env^^.ReleasePrimitiveArrayCritical(Env, AArray, CArray, Mode);
end;

procedure TJNIEnv.ReleaseStringCritical(Str: JString; CString: PJChar);
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ReleaseStringCritical');
  {$ENDIF}
  VersionCheck('ReleaseStringCritical', JNI_VERSION_1_2);
  Env^^.ReleaseStringCritical(Env, Str, CString);
end;

function TJNIEnv.ToReflectedField(AClass: JClass; FieldID: JFieldID; IsStatic: JBoolean): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ToReflectedField');
  {$ENDIF}
  VersionCheck('ToReflectedField', JNI_VERSION_1_2);
  Result := Env^^.ToReflectedField(Env, AClass, FieldID, IsStatic);
end;

function TJNIEnv.ToReflectedMethod(AClass: JClass; MethodID: JMethodID; IsStatic: JBoolean): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: ToReflectedMethod');
  {$ENDIF}
  VersionCheck('ToReflectedMethod', JNI_VERSION_1_2);
  Result := Env^^.ToReflectedMethod(Env, AClass, MethodID, IsStatic);
end;

function TJNIEnv.NewDirectByteBuffer(Address: Pointer; Capacity: JLong): JObject;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: NewDirectByteBuffer');
  {$ENDIF}
  VersionCheck('NewDirectByteBuffer', JNI_VERSION_1_4);
  Result := Env^^.NewDirectByteBuffer(Env, Address, Capacity);
end;

function TJNIEnv.GetDirectBufferAddress(Buf: JObject): Pointer;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetDirectBufferAddress');
  {$ENDIF}
  VersionCheck('GetDirectBufferAddress', JNI_VERSION_1_4);
  Result := Env^^.GetDirectBufferAddress(Env, Buf);
end;

function TJNIEnv.GetDirectBufferCapacity(Buf: JObject): JLong;
begin
  {$IFDEF jnidebug}
  __android_log_write(ANDROID_LOG_INFO, 'TJNIEnv', 'Call: GetDirectBufferCapacity');
  {$ENDIF}
  VersionCheck('GetDirectBufferCapacity', JNI_VERSION_1_4);
  Result := Env^^.GetDirectBufferCapacity(Env, Buf);
end;

{ TJavaVM }
constructor TJavaVM.Create;
begin
  {$ifdef jniexternals}
  {$IFDEF DYNAMIC_LINKING}
    // We need to know which DLL to load if we're linking at runtime
    raise Exception.Create('No JDK version specified');
  {$ELSE}
    FJNI_GetDefaultJavaVMInitArgs := JNI_GetDefaultJavaVMInitArgs;
    FJNI_CreateJavaVM := JNI_CreateJavaVM;
    FJNI_GetCreatedJavaVMs := JNI_GetCreatedJavaVMs;
    FIsInitialized := True;
  {$ENDIF}
  {$endif}
end;

constructor TJavaVM.Create(AJavaVM: PJavaVM);
begin
  if AJavaVM <> nil then FJavaVM := AJavaVM;
end;

function TJavaVM.DestroyJavaVM(): JInt;
begin
  Result := FJavaVM^^.DestroyJavaVM(FJavaVM) ;
end;

function TJavaVM.AttachCurrentThread(var PEnv: PPJNIEnv; Args: pointer
  ): JInt;
begin
  Result := FJavaVM^^.AttachCurrentThread(FJavaVM, @PEnv, Args) ;
end;

function TJavaVM.DetachCurrentThread(): JInt;
begin
  Result := FJavaVM^^.DetachCurrentThread(FJavaVM) ;
end;

function TJavaVM.GetEnv(PEnv: Ppointer; Version: JInt): JInt;
begin
  Result := FJavaVM^^.GetEnv(FJavaVM, @PEnv, JNI_VERSION_1_4)
end;

function TJavaVM.AttachCurrentThreadAsDaemon(var PEnv: PPJNIEnv; Args: pointer): JInt;
begin
  Result := FJavaVM^^.AttachCurrentThreadAsDaemon(FJavaVM, @PEnv, Args) ;
end;

{$ifdef jniexternals}
{$IFDEF MSWINDOWS}

constructor TJavaVM.Create(JDKVersion: Integer);
begin
  if JDKVersion = JNI_VERSION_1_1 then
    Create(JDKVersion, 'javai.dll')
  else
  if JDKVersion = JNI_VERSION_1_2 then
    Create(JDKVersion, 'jvm.dll')
  else
    raise Exception.Create('Unknown JDK Version');
end;

constructor TJavaVM.Create(JDKVersion: Integer; const JVMDLLFilename: string);
begin
  FIsInitialized := False;
  FJVMDLLFile := JVMDLLFilename;
  Version := JDKVersion;

  FJavaVM := nil;
  FEnv := nil;
  FDLLHandle := LoadLibrary(PChar(FJVMDLLFile));
  if FDLLHandle = 0 then
    raise Exception.CreateFmt('LoadLibrary failed trying to load %s', [FJVMDLLFile]);

  @FJNI_CreateJavaVM := GetProcAddress(FDLLHandle, 'JNI_CreateJavaVM');
  if not Assigned(FJNI_CreateJavaVM) then
  begin
    FreeLibrary(FDLLHandle);
    raise Exception.CreateFmt('GetProcAddress failed to locate JNI_CreateJavaVM in library %s', [FJVMDLLFile]);
  end;

  @FJNI_GetDefaultJavaVMInitArgs := GetProcAddress(FDLLHandle, 'JNI_GetDefaultJavaVMInitArgs');
  if not Assigned(FJNI_GetDefaultJavaVMInitArgs) then
  begin
    FreeLibrary(FDLLHandle);
    raise Exception.CreateFmt('GetProcAddress failed to locate JNI_GetDefaultJavaVMInitArgs in library %s', [FJVMDLLFile]);
  end;

  @FJNI_GetCreatedJavaVMs := GetProcAddress(FDLLHandle, 'JNI_GetCreatedJavaVMs');
  if not Assigned(FJNI_GetCreatedJavaVMs) then
  begin
    FreeLibrary(FDLLHandle);
    raise Exception.CreateFmt('GetProcAddress failed to locate JNI_GetCreatedJavaVMs in library %s', [FJVMDLLFile]);
  end;
  FIsInitialized := True;
end;

destructor TJavaVM.Destroy;
begin
  if FDLLHandle <> 0 then
    FreeLibrary(FDLLHandle);
  inherited Destroy;
end;

{$ENDIF} // MSWINDOWS

{$IFDEF LINUX}

constructor TJavaVM.Create(JDKVersion: Integer);
begin
  if JDKVersion = JNI_VERSION_1_1 then
    Create(JDKVersion, 'libjava.so')
  else
  if JDKVersion = JNI_VERSION_1_2 then
    Create(JDKVersion, 'libjvm.so')
  else
    raise Exception.Create('Unknown JDK Version');
end;

constructor TJavaVM.Create(JDKVersion: Integer; const JVMDLLFilename: string);
begin
  FIsInitialized := False;
  FJVMDLLFile := JVMDLLFilename;
  Version := JDKVersion;

  FJavaVM := nil;
  FEnv := nil;
  FDLLHandle := dlopen(PChar(FJVMDLLFile), RTLD_NOW);
  if FDLLHandle = nil then
    raise Exception.CreateFmt('dlopen failed trying to load %s', [FJVMDLLFile]);

  @FJNI_CreateJavaVM := dlsym(FDLLHandle, 'JNI_CreateJavaVM');
  if not Assigned(FJNI_CreateJavaVM) then
  begin
    dlclose(FDLLHandle);
    raise Exception.CreateFmt('dlsym failed to locate JNI_CreateJavaVM in library %s', [FJVMDLLFile]);
  end;

  @FJNI_GetDefaultJavaVMInitArgs := dlsym(FDLLHandle, 'JNI_GetDefaultJavaVMInitArgs');
  if not Assigned(FJNI_GetDefaultJavaVMInitArgs) then
  begin
    dlclose(FDLLHandle);
    raise Exception.CreateFmt('dlsym failed to locate JNI_GetDefaultJavaVMInitArgs in library %s', [FJVMDLLFile]);
  end;
  @FJNI_GetCreatedJavaVMs := dlsym(FDLLHandle, 'JNI_GetCreatedJavaVMs');
  if not Assigned(FJNI_GetCreatedJavaVMs) then
  begin
    dlclose(FDLLHandle);
    raise Exception.CreateFmt('dlsym failed to locate JNI_GetCreatedJavaVMs in library %s', [FJVMDLLFile]);
  end;
  FIsInitialized := True;
end;

destructor TJavaVM.Destroy;
begin
  if FDLLHandle <> nil then
    dlclose(FDLLHandle);
  inherited Destroy;
end;

{$ENDIF} // LINUX

function TJavaVM.GetCreatedJavaVMs(PJVM: PJavaVM; JSize1: JSize; var JSize2: JSize): JInt;
begin
  if not FIsInitialized then
    raise Exception.Create('JavaVM has not been initialized');
  Result := FJNI_GetCreatedJavaVMs(PJVM, JSize1, JSize2);
end;

function TJavaVM.GetDefaultJavaVMInitArgs(Args: PJDK1_1InitArgs): JInt;
begin
  if not FIsInitialized then
    raise Exception.Create('JavaVM has not been initialized');
  Result := FJNI_GetDefaultJavaVMInitArgs(Args)
end;

function TJavaVM.LoadVM(const Options: JDK1_1InitArgs): JInt;
begin
  FJDK1_1InitArgs := Options;
  Result := FJNI_CreateJavaVM(@FJavaVM, @FEnv, @FJDK1_1InitArgs)
end;

function TJavaVM.LoadVM(const Options: JavaVMInitArgs): JInt;
begin
  FJavaVMInitArgs := Options;
  Result := FJNI_CreateJavaVM(@FJavaVM, @FEnv, @FJavaVMInitArgs);
end;
{$endif}

procedure TJavaVM.SetVersion(const Value: JInt);
begin
  FVersion := Value;
end;


end.



//
//
//  Jni for Pascal
//
//  Original Source Code : /fpc/2.7.1/packages/jni/src/jni.pas
//
//
//
//
//
//
unit And_jni;

{$ifdef fpc} {$mode delphi} {$packrecords c} {$endif}

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

{$IfDef DCC}
type
 pChar = MarshaledAString;
{$EndIf}

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

     jsize=jint;           // "cardinal indices and sizes"

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

     _JavaVM={$ifdef packedrecords}packed{$endif} record
      functions:PJNIInvokeInterface;
     end;

     C_JNIEnv=^JNINativeInterface;
     JNIEnv=^JNINativeInterface;
     JavaVM=^JNIInvokeInterface;

     PPJNIEnv=^PJNIEnv;
     PJNIEnv=^JNIEnv;

     PPJavaVM=^PJavaVM;
     PJavaVM=^JavaVM;

     JNINativeInterface={$ifdef packedrecords}packed{$endif} record
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
      GetStringUTFChars:function(Env:PJNIEnv;Str:JString; IsCopy:PJBoolean):pchar;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
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

     JNIInvokeInterface={$ifdef packedrecords}packed{$endif} record
      reserved0:pointer;
      reserved1:pointer;
      reserved2:pointer;

      DestroyJavaVM:function(PVM:PJavaVM):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      AttachCurrentThread:function(PVM:PJavaVM;PEnv:PPJNIEnv;Args:pointer):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      DetachCurrentThread:function(PVM:PJavaVM):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      GetEnv:function(PVM:PJavaVM;PEnv:Ppointer;Version:JInt):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
      AttachCurrentThreadAsDaemon:function(PVM:PJavaVM;PEnv:PPJNIEnv;Args:pointer):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
     end;

     JavaVMAttachArgs={$ifdef packedrecords}packed{$endif} record
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
{$endif}

(*
 * Prototypes for functions exported by loadable shared libs.  These are
 * called by JNI, not provided by JNI.
 *)

const curVM  : PJavaVM = nil;
      curEnv : PJNIEnv = nil;

(*
function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
*)
implementation


//function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
//begin
// curVM   := vm;
// result  := JNI_VERSION_1_6;
//end;


//procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
//begin
//end;

end.



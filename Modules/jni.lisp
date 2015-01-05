;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;;
;;;;	File:		jni.lisp
;;;;	Contents:	Java JNI definitions.
;;;;	Author:		Roger Corman
;;;;	Created:	5/27/01
;;;;

(in-package :win32)

(defwintype jint :long)
(defwintype jlong (:long 2))
(defwintype jbyte :char)

;;
;; JNI Types
;;
(defwintype jboolean :unsigned-char)
(defwintype jchar :unsigned-short)
(defwintype jshort :short)
(defwintype jfloat :single-float)
(defwintype jdouble :double-float)
(defwintype jsize jint)

(defwinstruct _jobject ())
(defwintype jobject (_jobject *))
(defwintype jclass jobject)
(defwintype jthrowable jobject)
(defwintype jstring jobject)
(defwintype jarray jobject)
(defwintype jbooleanArray jarray)
(defwintype jbyteArray jarray)
(defwintype jcharArray jarray)
(defwintype jshortArray jarray)
(defwintype jintArray jarray)
(defwintype jlongArray jarray)
(defwintype jfloatArray jarray)
(defwintype jdoubleArray jarray)
(defwintype jobjectArray jarray)
(defwintype jweak jobject)
(defwintype jvalue jlong)

#|
typedef union jvalue {
    jboolean z;
    jbyte    b;
    jchar    c;
    jshort   s;
    jint     i;
    jlong    j;
    jfloat   f;
    jdouble  d;
    jobject  l;
} jvalue;
|#

(defwinstruct _jfieldID ())
(defwintype jfieldID (_jfieldID *))
(defwinstruct _jmethodID ())
(defwintype jmethodID (_jmethodID *))

;;
;; jboolean constants
;;
(defwinconstant JNI_FALSE 0)
(defwinconstant JNI_TRUE  1)

;;
;; possible return values for JNI functions.
;;
(defwinconstant JNI_OK 			0)			;; success
(defwinconstant JNI_ERR 		-1)			;; unknown error
(defwinconstant JNI_EDETACHED 	-2)			;; thread detached from the VM
(defwinconstant JNI_EVERSION 	-3)			;; JNI version error
(defwinconstant JNI_ENOMEM 		-4)			;; not enough memory
(defwinconstant JNI_EEXIST 		-5)			;; VM already created
(defwinconstant JNI_EINVAL 		-6)			;; invalid arguments

;;
;; used in ReleaseScalarArrayElements
;;
(defwinconstant JNI_COMMIT 		1)
(defwinconstant JNI_ABORT 		2)

;;
;; used in RegisterNatives to describe native method name, signature,
;; and function pointer.
;;
(defwinstruct JNINativeMethod
	((name (:char *))
	 (signature (:char *))
	 (fnPtr (:void *))))

(defwintype ... (:void *))		;; figure out how to handle this

;;
;; JNI Native Method Interface.
;;
#! (:export t :auto-ansi nil)
interface JNIEnv
{
	void *reserved0();
	void *reserved1();
	void *reserved2();
	void *reserved3();

	jint GetVersion();
	jclass DefineClass(char* name, jobject loader, jbyte* buf, jsize len);
    jclass FindClass(char* name);
    jmethodID FromReflectedMethod(jobject method);
    jfieldID FromReflectedField(jobject field);
    jobject ToReflectedMethod(jclass cls, jmethodID methodID, jboolean isStatic);
    jclass GetSuperclass(jclass sub);
    jboolean IsAssignableFrom(jclass sub, jclass sup);
    jobject ToReflectedField(jclass cls, jfieldID fieldID, jboolean isStatic);
    jint Throw(jthrowable obj);
    jint ThrowNew(jclass clazz, char* msg);
    jthrowable ExceptionOccurred();
    void ExceptionDescribe();
    void ExceptionClear();
    void FatalError(char* msg);

    jint PushLocalFrame(jint capacity);
    jobject PopLocalFrame(jobject result);

    jobject NewGlobalRef(jobject lobj);
    void DeleteGlobalRef(jobject gref);
    void DeleteLocalRef(jobject obj);
    jboolean IsSameObject(jobject obj1, jobject obj2);
    jobject NewLocalRef(jobject ref);
    jint EnsureLocalCapacity(jint capacity);

    jobject AllocObject(jclass clazz);
    jobject NewObject(jclass clazz, jmethodID methodID /*, ... */);
    jobject NewObjectV(jclass clazz, jmethodID methodID, va_list args);
    jobject NewObjectA(jclass clazz, jmethodID methodID, jvalue *args);

    jclass GetObjectClass(jobject obj);
    jboolean IsInstanceOf(jobject obj, jclass clazz);

    jmethodID GetMethodID(jclass clazz, char* name, char* sig);

    jobject CallObjectMethod(jobject obj, jmethodID methodID /*, ... */);
    jobject CallObjectMethodV(jobject obj, jmethodID methodID, va_list args);
    jobject CallObjectMethodA(jobject obj, jmethodID methodID, jvalue * args);

    jboolean CallBooleanMethod(jobject obj, jmethodID methodID /*, ... */);
    jboolean CallBooleanMethodV(jobject obj, jmethodID methodID, va_list args);
    jboolean CallBooleanMethodA(jobject obj, jmethodID methodID, jvalue * args);

    jbyte CallByteMethod(jobject obj, jmethodID methodID /*, ... */);
    jbyte CallByteMethodV(jobject obj, jmethodID methodID, va_list args);
    jbyte CallByteMethodA(jobject obj, jmethodID methodID, jvalue *args);

    jchar CallCharMethod(jobject obj, jmethodID methodID /*, ... */);
    jchar CallCharMethodV(jobject obj, jmethodID methodID, va_list args);
    jchar CallCharMethodA(jobject obj, jmethodID methodID, jvalue *args);

    jshort CallShortMethod(jobject obj, jmethodID methodID /*, ... */);
    jshort CallShortMethodV(jobject obj, jmethodID methodID, va_list args);
    jshort CallShortMethodA(jobject obj, jmethodID methodID, jvalue *args);

    jint CallIntMethod(jobject obj, jmethodID methodID /*, ... */);
    jint CallIntMethodV(jobject obj, jmethodID methodID, va_list args);
    jint CallIntMethodA(jobject obj, jmethodID methodID, jvalue *args);

    jlong CallLongMethod(jobject obj, jmethodID methodID /*, ... */);
    jlong CallLongMethodV(jobject obj, jmethodID methodID, va_list args);
    jlong CallLongMethodA(jobject obj, jmethodID methodID, jvalue *args);

    jfloat CallFloatMethod(jobject obj, jmethodID methodID /*, ... */);
    jfloat CallFloatMethodV(jobject obj, jmethodID methodID, va_list args);
    jfloat CallFloatMethodA(jobject obj, jmethodID methodID, jvalue *args);

    jdouble CallDoubleMethod(jobject obj, jmethodID methodID /*, ... */);
    jdouble CallDoubleMethodV(jobject obj, jmethodID methodID, va_list args);
    jdouble CallDoubleMethodA(jobject obj, jmethodID methodID, jvalue *args);

    void CallVoidMethod(jobject obj, jmethodID methodID /*, ... */);
    void CallVoidMethodV(jobject obj, jmethodID methodID, va_list args);
    void CallVoidMethodA(jobject obj, jmethodID methodID, jvalue * args);

    jobject CallNonvirtualObjectMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jobject CallNonvirtualObjectMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    jobject CallNonvirtualObjectMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue * args);

    jboolean CallNonvirtualBooleanMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jboolean CallNonvirtualBooleanMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    jboolean CallNonvirtualBooleanMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue * args);

    jbyte CallNonvirtualByteMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jbyte CallNonvirtualByteMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    jbyte CallNonvirtualByteMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue *args);

    jchar CallNonvirtualCharMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jchar CallNonvirtualCharMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    jchar CallNonvirtualCharMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue *args);

    jshort CallNonvirtualShortMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jshort CallNonvirtualShortMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    jshort CallNonvirtualShortMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue *args);

    jint CallNonvirtualIntMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jint CallNonvirtualIntMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    jint CallNonvirtualIntMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue *args);

    jlong CallNonvirtualLongMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jlong CallNonvirtualLongMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    jlong CallNonvirtualLongMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue *args);

    jfloat CallNonvirtualFloatMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jfloat CallNonvirtualFloatMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    jfloat CallNonvirtualFloatMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue *args);

    jdouble CallNonvirtualDoubleMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    jdouble CallNonvirtualDoubleMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
	jdouble CallNonvirtualDoubleMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue *args);

    void CallNonvirtualVoidMethod(jobject obj, jclass clazz, jmethodID methodID /*, ... */);
    void CallNonvirtualVoidMethodV(jobject obj, jclass clazz, jmethodID methodID, va_list args);
    void CallNonvirtualVoidMethodA(jobject obj, jclass clazz, jmethodID methodID, jvalue * args);

    jfieldID GetFieldID(jclass clazz, char* name, char* sig);

    jobject GetObjectField(jobject obj, jfieldID fieldID);
    jboolean GetBooleanField(jobject obj, jfieldID fieldID);
    jbyte GetByteField(jobject obj, jfieldID fieldID);
    jchar GetCharField(jobject obj, jfieldID fieldID);
    jshort GetShortField(jobject obj, jfieldID fieldID);
    jint GetIntField(jobject obj, jfieldID fieldID);
    jlong GetLongField(jobject obj, jfieldID fieldID);
    jfloat GetFloatField(jobject obj, jfieldID fieldID);
    jdouble GetDoubleField(jobject obj, jfieldID fieldID);

    void SetObjectField(jobject obj, jfieldID fieldID, jobject val);
    void SetBooleanField(jobject obj, jfieldID fieldID, jboolean val);
    void SetByteField(jobject obj, jfieldID fieldID, jbyte val);
    void SetCharField(jobject obj, jfieldID fieldID, jchar val);
    void SetShortField(jobject obj, jfieldID fieldID, jshort val);
    void SetIntField(jobject obj, jfieldID fieldID, jint val);
    void SetLongField(jobject obj, jfieldID fieldID, jlong val);
    void SetFloatField(jobject obj, jfieldID fieldID, jfloat val);
    void SetDoubleField(jobject obj, jfieldID fieldID, jdouble val);

    jmethodID GetStaticMethodID(jclass clazz, char* name, char* sig);

    jobject CallStaticObjectMethod(jclass clazz, jmethodID methodID /*, ... */);
    jobject CallStaticObjectMethodV(jclass clazz, jmethodID methodID, va_list args);
    jobject CallStaticObjectMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    jboolean CallStaticBooleanMethod(jclass clazz, jmethodID methodID /*, ... */);
    jboolean CallStaticBooleanMethodV(jclass clazz, jmethodID methodID, va_list args);
    jboolean CallStaticBooleanMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    jbyte CallStaticByteMethod(jclass clazz, jmethodID methodID /*, ... */);
    jbyte CallStaticByteMethodV(jclass clazz, jmethodID methodID, va_list args);
    jbyte CallStaticByteMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    jchar CallStaticCharMethod(jclass clazz, jmethodID methodID /*, ... */);
    jchar CallStaticCharMethodV(jclass clazz, jmethodID methodID, va_list args);
    jchar CallStaticCharMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    jshort CallStaticShortMethod(jclass clazz, jmethodID methodID /*, ... */);
    jshort CallStaticShortMethodV(jclass clazz, jmethodID methodID, va_list args);
    jshort CallStaticShortMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    jint CallStaticIntMethod(jclass clazz, jmethodID methodID /*, ... */);
    jint CallStaticIntMethodV(jclass clazz, jmethodID methodID, va_list args);
    jint CallStaticIntMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    jlong CallStaticLongMethod(jclass clazz, jmethodID methodID /*, ... */);
    jlong CallStaticLongMethodV(jclass clazz, jmethodID methodID, va_list args);
    jlong CallStaticLongMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    jfloat CallStaticFloatMethod(jclass clazz, jmethodID methodID /*, ... */);
    jfloat CallStaticFloatMethodV(jclass clazz, jmethodID methodID, va_list args);
    jfloat CallStaticFloatMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    jdouble CallStaticDoubleMethod(jclass clazz, jmethodID methodID /*, ... */);
    jdouble CallStaticDoubleMethodV(jclass clazz, jmethodID methodID, va_list args);
    jdouble CallStaticDoubleMethodA(jclass clazz, jmethodID methodID, jvalue *args);

    void CallStaticVoidMethod(jclass cls, jmethodID methodID /*, ... */);
    void CallStaticVoidMethodV(jclass cls, jmethodID methodID, va_list args);
    void CallStaticVoidMethodA(jclass cls, jmethodID methodID, jvalue * args);

    jfieldID GetStaticFieldID(jclass clazz, char* name, char* sig);
    jobject GetStaticObjectField(jclass clazz, jfieldID fieldID);
    jboolean GetStaticBooleanField(jclass clazz, jfieldID fieldID);
    jbyte GetStaticByteField(jclass clazz, jfieldID fieldID);
    jchar GetStaticCharField(jclass clazz, jfieldID fieldID);
    jshort GetStaticShortField(jclass clazz, jfieldID fieldID);
    jint GetStaticIntField(jclass clazz, jfieldID fieldID);
    jlong GetStaticLongField(jclass clazz, jfieldID fieldID);
    jfloat GetStaticFloatField(jclass clazz, jfieldID fieldID);
    jdouble GetStaticDoubleField(jclass clazz, jfieldID fieldID);

    void SetStaticObjectField(jclass clazz, jfieldID fieldID, jobject value);
    void SetStaticBooleanField(jclass clazz, jfieldID fieldID, jboolean value);
    void SetStaticByteField(jclass clazz, jfieldID fieldID, jbyte value);
    void SetStaticCharField(jclass clazz, jfieldID fieldID, jchar value);
    void SetStaticShortField(jclass clazz, jfieldID fieldID, jshort value);
    void SetStaticIntField(jclass clazz, jfieldID fieldID, jint value);
    void SetStaticLongField(jclass clazz, jfieldID fieldID, jlong value);
    void SetStaticFloatField(jclass clazz, jfieldID fieldID, jfloat value);
    void SetStaticDoubleField(jclass clazz, jfieldID fieldID, jdouble value);

    jstring NewString(jchar* unicode, jsize len);
    jsize GetStringLength(jstring str);
    jchar* GetStringChars(jstring str, jboolean* isCopy);
    void ReleaseStringChars(jstring str, jchar* chars);

    jstring NewStringUTF(char* utf);
    jsize GetStringUTFLength(jstring str);
    char* GetStringUTFChars(jstring str, jboolean* isCopy);
    void ReleaseStringUTFChars(jstring str, char* chars);

    jsize GetArrayLength(jarray array);

    jobjectArray NewObjectArray(jsize len, jclass clazz, jobject init);
    jobject GetObjectArrayElement(jobjectArray array, jsize index);
    void SetObjectArrayElement(jobjectArray array, jsize index, jobject val);

    jbooleanArray NewBooleanArray(jsize len);
    jbyteArray NewByteArray(jsize len);
    jcharArray NewCharArray(jsize len);
    jshortArray NewShortArray(jsize len);
    jintArray NewIntArray(jsize len);
    jlongArray NewLongArray(jsize len);
    jfloatArray NewFloatArray(jsize len);
    jdoubleArray NewDoubleArray(jsize len);

    jboolean*  GetBooleanArrayElements(jbooleanArray array, jboolean* isCopy);
    jbyte * GetByteArrayElements(jbyteArray array, jboolean* isCopy);
    jchar*  GetCharArrayElements(jcharArray array, jboolean* isCopy);
    jshort * GetShortArrayElements(jshortArray array, jboolean* isCopy);
    jint * GetIntArrayElements(jintArray array, jboolean* isCopy);
    jlong * GetLongArrayElements(jlongArray array, jboolean* isCopy);
    jfloat * GetFloatArrayElements(jfloatArray array, jboolean* isCopy);
    jdouble * GetDoubleArrayElements(jdoubleArray array, jboolean* isCopy);

    void ReleaseBooleanArrayElements(jbooleanArray array, jboolean* elems, jint mode);
    void ReleaseByteArrayElements(jbyteArray array, jbyte* elems, jint mode);
    void ReleaseCharArrayElements(jcharArray array, jchar* elems, jint mode);
    void ReleaseShortArrayElements(jshortArray array, jshort* elems, jint mode);
    void ReleaseIntArrayElements(jintArray array, jint* elems, jint mode);
    void ReleaseLongArrayElements(jlongArray array, jlong* elems, jint mode);
    void ReleaseFloatArrayElements(jfloatArray array, jfloat* elems, jint mode);
    void ReleaseDoubleArrayElements(jdoubleArray array, jdouble* elems, jint mode);

    void GetBooleanArrayRegion(jbooleanArray array, jsize start, jsize l, jboolean* buf);
    void GetByteArrayRegion(jbyteArray array, jsize start, jsize len, jbyte* buf);
    void GetCharArrayRegion(jcharArray array, jsize start, jsize len, jchar* buf);
    void GetShortArrayRegion(jshortArray array, jsize start, jsize len, jshort* buf);
    void GetIntArrayRegion(jintArray array, jsize start, jsize len, jint* buf);
    void GetLongArrayRegion(jlongArray array, jsize start, jsize len, jlong* buf);
    void GetFloatArrayRegion(jfloatArray array, jsize start, jsize len, jfloat* buf);
    void GetDoubleArrayRegion(jdoubleArray array, jsize start, jsize len, jdouble* buf);

    void SetBooleanArrayRegion(jbooleanArray array, jsize start, jsize l, jboolean* buf);
    void SetByteArrayRegion(jbyteArray array, jsize start, jsize len, jbyte* buf);
    void SetCharArrayRegion(jcharArray array, jsize start, jsize len, jchar* buf);
    void SetShortArrayRegion(jshortArray array, jsize start, jsize len, jshort* buf);
    void SetIntArrayRegion(jintArray array, jsize start, jsize len, jint* buf);
    void SetLongArrayRegion(jlongArray array, jsize start, jsize len, jlong* buf);
    void SetFloatArrayRegion(jfloatArray array, jsize start, jsize len, jfloat* buf);
    void SetDoubleArrayRegion(jdoubleArray array, jsize start, jsize len, jdouble* buf);

    jint RegisterNatives(jclass clazz, JNINativeMethod* methods, jint nMethods);
    jint UnregisterNatives(jclass clazz);

    jint MonitorEnter(jobject obj);
    jint MonitorExit(jobject obj);

    jint GetJavaVM(JavaVM** vm);

    void GetStringRegion(jstring str, jsize start, jsize len, jchar* buf);
    void GetStringUTFRegion(jstring str, jsize start, jsize len, char* buf);

    void * GetPrimitiveArrayCritical(jarray array, jboolean* isCopy);
    void ReleasePrimitiveArrayCritical(jarray array, void* carray, jint mode);

    jchar*  GetStringCritical(jstring string, jboolean* isCopy);
    void ReleaseStringCritical(jstring string, jchar* cstring);

    jweak NewWeakGlobalRef(jobject obj);
    void DeleteWeakGlobalRef(jweak ref);

    jboolean ExceptionCheck();
};
!#

(defwinstruct JavaVMOption
	((optionString (:char *))
	 (extraInfo (:void *))))

(defwinstruct JavaVMInitArgs
	((version jint)
	 (nOptions jint)
	 (options (JavaVMOption *))
	 (ignoreUnrecognized jint #| jboolean |#)))

(defwinstruct JavaVMAttachArgs
	((version jint)
	 (name (:char *))
	 (group jobject)))

;; These structures will be VM-specific.

(defwinstruct JDK1_1InitArgs
	((version jint)
	 (properties ((:char *) *))
	 (checkSource jint)
	 (nativeStackSize jint)
	 (javaStackSize jint)
	 (minHeapSize jint)
	 (maxHeapSize jint)
	 (verifyMode jint)
	 (classpath (:char *))	
     (vfprintf (:void *))	;; function pointer
     (exit (:void *))		;; function pointer
     (abort (:void *))		;; function pointer
	 (enableClassGC jint)
	 (enableVerboseGC jint)
	 (disableAsyncGC jint)
	 (verbose jint)
	 (debugging #|jboolean |# jint)
     (debugPort jint)))

(defwinstruct JDK1_1AttachArgs((__padding (:void *)))) ;; C compilers don't allow empty structures.

(defwinconstant JDK1_2 t)

;; End VM-specific.

;;
;; JNI Invocation Interface.
;;
#! (:export t)
interface JavaVM 
{
    void *reserved0();
    void *reserved1();
    void *reserved2();

    jint DestroyJavaVM();
    jint AttachCurrentThread(void** penv, void* args);
    jint DetachCurrentThread();
    jint GetEnv(void** penv, jint version);
};
!#

#! (:library "jvm" :export t :pascal "JNICALL")

jint JNICALL JNI_GetDefaultJavaVMInitArgs(void* args);
jint JNICALL JNI_CreateJavaVM(JavaVM** pvm, void** penv, void* args);
jint JNICALL JNI_GetCreatedJavaVMs(JavaVM**, jsize, jsize*);
jint JNICALL JNI_OnLoad(JavaVM* vm, void* reserved);
void JNICALL JNI_OnUnload(JavaVM* vm, void* reserved);
!#
(defwinconstant JNI_VERSION_1_1 #x00010001)
(defwinconstant JNI_VERSION_1_2 #x00010002)

#|		test

(defun start-java-vm ()
	(ct:with-fresh-foreign-block (vm-args 'JavaVMInitArgs)
		(setf (ct:cref JavaVMInitArgs vm-args version) JNI_VERSION_1_2)
		(setf (ct:cref JavaVMInitArgs vm-args ignoreUnrecognized) 0)
		(let* ((jvm (ct:malloc (ct:sizeof '(JavaVM *))))
			   (env (ct:malloc (ct:sizeof '(JNIEnv *))))
			   (numOpts 0)
			   (options (ct:malloc (* (ct:sizeof 'JavaVMOption) numOpts))))
			(setf (ct:cref JavaVMInitArgs vm-args nOptions) numOpts)
			(setf (ct:cref JavaVMInitArgs vm-args options) options)
			;(setf (ct:cref JavaVMOption (ct:cref (JavaVMOption *) options 0) optionString)
			;		(ct:lisp-string-to-c-string "-cp d:/java/jdk1.3/jrt/lib/rt.jar")) 
			(JNI_CreateJavaVM jvm env vm-args)
			(values (ct:cref ((JavaVM *) *) jvm 0)
				    (ct:cref ((JNIEnv *) *) env 0)))))

(defun test ()
	(multiple-value-bind (jvm env)
		(start-java-vm)
		(let (cls mid result)
			(setf cls (JNIEnv-FindClass env "java.lang.System"))
			(setf mid (JNIEnv-GetStaticMethodID env cls "currentTimeMillis" "()J"))
			(setf result (JNIEnv-CallStaticLongMethodV env cls mid ct:null))
			(JavaVM-DestroyJavaVM jvm)
			result)))

(defun get-java-vms ()
	(let* ((max-vms 128)
		   (buf (ct:malloc (* (ct:sizeof '(JavaVM *)) max-vms)))
		   (num-returned (ct:malloc (ct:sizeof 'jsize)))
			result)
		(setf result (JNI_GetCreatedJavaVMs buf max-vms num-returned))
		(let ((returned (ct:cref (jsize *) num-returned 0))
			  (vms '()))
			(dotimes (i returned)
				(push (ct:cref ((JavaVM *) *) buf i) vms))
			vms)))		        


|#
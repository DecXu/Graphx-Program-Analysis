#include <jni.h>       // JNI header provided by JDK
#include <iostream>    // C++ standard IO header
#include "HelloJNI.h"  // Generated
using namespace std;

// Implementation of the native method sayHello()
JNIEXPORT jobject JNICALL Java_HelloJNI_sayHello(JNIEnv *env, jclass thisObj, jobject hashMapInfo) 
{
   // 用于拼接字符串的数组
   // char buff[100] = {0};
    // 用于拼接字符串的“游标”指针
   // char *pos = buff;

   // if (hashMapInfo == NULL)
   //     return env->NewStringUTF(buff);

    // 获取HashMap类entrySet()方法ID
    jclass hashmapClass = env->FindClass("java/util/HashMap");
    jmethodID entrySetMID = env->GetMethodID(hashmapClass, "entrySet", "()Ljava/util/Set;");
    // 调用entrySet()方法获取Set对象
    jobject setObj = env->CallObjectMethod(hashMapInfo, entrySetMID);
    // 调用size()方法获取HashMap键值对数量
//  jmethodID sizeMID = env->GetMethodID(hashmapClass, "size", "()I");
//  jint size = env->CallIntMethod(hashMapInfo, sizeMID);

    // 获取Set类中iterator()方法ID
    jclass setClass = env->FindClass("java/util/Set");
    jmethodID iteratorMID = env->GetMethodID(setClass, "iterator", "()Ljava/util/Iterator;");
    // 调用iterator()方法获取Iterator对象
    jobject iteratorObj = env->CallObjectMethod(setObj, iteratorMID);

    // 获取Iterator类中hasNext()方法ID
    // 用于while循环判断HashMap中是否还有数据
    jclass iteratorClass = env->FindClass("java/util/Iterator");
    jmethodID hasNextMID = env->GetMethodID(iteratorClass, "hasNext", "()Z");
    // 获取Iterator类中next()方法ID
    // 用于读取HashMap中的每一条数据
    jmethodID nextMID = env->GetMethodID(iteratorClass, "next", "()Ljava/lang/Object;");

    // 获取Map.Entry类中getKey()和getValue()的方法ID
    // 用于读取“课程-分数”键值对，注意：内部类使用$符号表示
    jclass entryClass = env->FindClass("java/util/Map$Entry");
    jmethodID getKeyMID = env->GetMethodID(entryClass, "getKey", "()Ljava/lang/Object;");
    jmethodID getValueMID = env->GetMethodID(entryClass, "getValue", "()Ljava/lang/Object;");

    // HashMap只能存放引用数据类型，不能存放int等基本数据类型
    // 使用Long类的longValue()方法获取int数据
    jclass longClass = env->FindClass("java/lang/Long");
    jmethodID longMID = env->GetMethodID(longClass, "longValue", "()J");

     // 使用Character类的charValue()方法获取int数据
    jclass charClass = env->FindClass("java/lang/Character");
    jmethodID CharacterMID = env->GetMethodID(charClass, "charValue", "()C");

    // 循环检测HashMap中是否还有数据
    while (env->CallBooleanMethod(iteratorObj, hasNextMID)) 
    {
        // 读取一条数据
        jobject entryObj = env->CallObjectMethod(iteratorObj, nextMID);
        
        // 提取数据中key值：Long类型，并转为long类型
        jobject scoreObj = env->CallObjectMethod(entryObj, getKeyMID);
        if (scoreObj == NULL)
            continue;
        long score = env->CallLongMethod(scoreObj, longMID);
        cout << "key: " << score << endl;

        // 提取数据中value值：EdgeArray类型
        jobject edgearray = env->CallObjectMethod(entryObj, getValueMID);
        if (edgearray == NULL)
            continue;
        jclass cls = env->GetObjectClass(edgearray);
        jfieldID fid = env->GetFieldID(cls, "edges", "Ljava/util/ArrayList;");
        jfieldID fid2 = env->GetFieldID(cls, "labels", "Ljava/util/ArrayList;");

        jobject edges = env->GetObjectField(edgearray, fid);
        jobject labels = env->GetObjectField(edgearray, fid2);

        jclass arrayClass = env->FindClass("java/util/ArrayList");
        jmethodID sizeMid = env->GetMethodID(arrayClass, "size", "()I");

        jint size = env->CallIntMethod(edges, sizeMid);
        //int size = env->CallIntMethod(edges, sizeMid);
        cout << size << endl;

        jmethodID getMid = env->GetMethodID(arrayClass, "get", "(I)Ljava/lang/Object;");
        for(jint i = 0; i < size; i++)
        {
           jobject element = env->CallObjectMethod(edges, getMid, i);
           jobject element2 = env->CallObjectMethod(labels, getMid, i);
           long tt = env->CallLongMethod(element, longMID);
           char ttt = env->CallCharMethod(element2, CharacterMID);
           cout << "i: " << i << " array[i]: " << tt << "|" << ttt << endl;
           env->DeleteLocalRef(element);
           env->DeleteLocalRef(element2);
        }
        //cout << size << endl;



        //jobject edges = env->GetObjectField(edgearray, fid);

      /*  // 提取数据中key值：String类型课程名字
        jstring courseJS = (jstring) env->CallObjectMethod(entryObj, getKeyMID);
        if (courseJS == NULL)   // HashMap允许null类型
            continue;
        // jstring转C风格字符串
        const char *courseStr = env->GetStringUTFChars(courseJS, NULL);

        // 提取数据中value值：Integer类型分数，并转为int类型
        jobject scoreObj = env->CallObjectMethod(entryObj, getValueMID);
        if (scoreObj == NULL)
            continue;
        int score = (int) env->CallIntMethod(scoreObj, valueMID);

        // 拼接字符串，sprintf函数返回拼接字符个数
        int strLen = sprintf(pos, "%s: ", courseStr);
        pos += strLen;
        int numLen = sprintf(pos, "%d. ", score);
        pos += numLen;

        // 释放UTF字符串资源
        env->ReleaseStringUTFChars(courseJS, courseStr);*/
        // 释放JNI局部引用资源
        env->DeleteLocalRef(cls);
        env->DeleteLocalRef(arrayClass);
        env->DeleteLocalRef(edges);
        env->DeleteLocalRef(labels);
        env->DeleteLocalRef(entryObj);
        env->DeleteLocalRef(edgearray);
        env->DeleteLocalRef(scoreObj);
    }

    // 释放JNI局部引用: jclass jobject
    env->DeleteLocalRef(hashmapClass);
    env->DeleteLocalRef(setObj);
    env->DeleteLocalRef(setClass);
    env->DeleteLocalRef(iteratorObj);
    env->DeleteLocalRef(iteratorClass);
    env->DeleteLocalRef(entryClass);
    env->DeleteLocalRef(longClass);
    env->DeleteLocalRef(charClass);

    // 生成jstring字符串并返回
    //return env->NewStringUTF(buff);




    /*
    {
       // 读取一条数据
        jobject entryObj = env->CallObjectMethod(iteratorObj, nextMID);

        // 提取数据中key值：Long类型
        jobject scoreObj = env->CallObjectMethod(entryObj, getKeyMID);
        if (scoreObj == NULL)
            continue;
        long score = (long) env->CallLongMethod(scoreObj, getKeyMID);
        cout << score;
    }*/




	/*cout << "Hello World from C++!" << endl;
   jclass cls = env->GetObjectClass(test);
   jfieldID fid = env->GetFieldID(cls, "int_value", "I");
   jfieldID fid2 = env->GetFieldID(cls, "test2", "LTest2;");
   jfieldID fid3 = env->GetFieldID(cls, "str", "Ljava/lang/String;");

   jobject test2 = env->GetObjectField(test,fid2);
   jclass cls2 = env->GetObjectClass(test2);
   jfieldID fid4 = env->GetFieldID(cls2, "int_value2", "I");
   cout << "test2 int_value2 : " << env->GetIntField(test2, fid4) << endl;




   if(fid == NULL)
   {
      return 0;
   }
   cout << env->GetIntField(test, fid) << endl;
   env->SetIntField(test, fid, 20);*/
   /*cout << int_value;
   jclass jcls=env->FindClass("Test");
   if(jcls == 0)
   {
      cout << "Test is null" << endl;
      return;
   }

   jobject jobj = env->AllocObject(jcls);
   jfieldID valId = env->GetFieldID(jcls, "int_value", "I");
   jint intVal = env->GetIntField(jobj, valId);
   intVal = intVal + 20;
   env->SetIntField(jobj, valId, intVal);
   cout << "in c :" << intVal << endl;*/
   return hashMapInfo;
}

package com.embarcadero.rtl;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

public class ProxyInterface implements InvocationHandler {
    long pointer;

    public native void cleanNative(long j);

    public native Object dispatchToNative(String str, Object[] objArr, long j);

    public Object CreateProxyClass(Class listenerClass, long pointer) throws ClassNotFoundException {
        this.pointer = pointer;
        return Proxy.newProxyInstance(listenerClass.getClassLoader(), new Class[]{listenerClass}, this);
    }

    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        Object Obj = dispatchToNative(method.getName(), args, this.pointer);
        cleanNative(this.pointer);
        return Obj;
    }
}

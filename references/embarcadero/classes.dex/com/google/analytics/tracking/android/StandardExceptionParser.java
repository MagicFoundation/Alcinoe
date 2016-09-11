package com.google.analytics.tracking.android;

import android.content.Context;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.support.v4.os.EnvironmentCompat;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

public class StandardExceptionParser implements ExceptionParser {
    private final TreeSet<String> includedPackages;

    public StandardExceptionParser(Context context, Collection<String> additionalPackages) {
        this.includedPackages = new TreeSet();
        setIncludedPackages(context, additionalPackages);
    }

    public void setIncludedPackages(Context context, Collection<String> additionalPackages) {
        this.includedPackages.clear();
        Set<String> packages = new HashSet();
        if (additionalPackages != null) {
            packages.addAll(additionalPackages);
        }
        if (context != null) {
            try {
                String appPackage = context.getApplicationContext().getPackageName();
                this.includedPackages.add(appPackage);
                ActivityInfo[] ai = context.getApplicationContext().getPackageManager().getPackageInfo(appPackage, 15).activities;
                if (ai != null) {
                    for (ActivityInfo sx : ai) {
                        packages.add(sx.packageName);
                    }
                }
            } catch (NameNotFoundException e) {
                Log.i("No package found");
            }
        }
        for (String packageName : packages) {
            boolean needToAdd = true;
            Iterator i$ = this.includedPackages.iterator();
            while (i$.hasNext()) {
                String oldName = (String) i$.next();
                if (packageName.startsWith(oldName)) {
                    needToAdd = false;
                } else {
                    if (oldName.startsWith(packageName)) {
                        this.includedPackages.remove(oldName);
                    }
                    if (needToAdd) {
                        this.includedPackages.add(packageName);
                    }
                }
            }
            if (needToAdd) {
                this.includedPackages.add(packageName);
            }
        }
    }

    protected Throwable getCause(Throwable t) {
        Throwable result = t;
        while (result.getCause() != null) {
            result = result.getCause();
        }
        return result;
    }

    protected StackTraceElement getBestStackTraceElement(Throwable t) {
        StackTraceElement[] elements = t.getStackTrace();
        if (elements == null || elements.length == 0) {
            return null;
        }
        for (StackTraceElement e : elements) {
            String className = e.getClassName();
            Iterator i$ = this.includedPackages.iterator();
            while (i$.hasNext()) {
                if (className.startsWith((String) i$.next())) {
                    return e;
                }
            }
        }
        return elements[0];
    }

    protected String getDescription(Throwable cause, StackTraceElement element, String threadName) {
        StringBuilder descriptionBuilder = new StringBuilder();
        descriptionBuilder.append(cause.getClass().getSimpleName());
        if (element != null) {
            String[] classNameParts = element.getClassName().split("\\.");
            String className = EnvironmentCompat.MEDIA_UNKNOWN;
            if (classNameParts != null && classNameParts.length > 0) {
                className = classNameParts[classNameParts.length - 1];
            }
            descriptionBuilder.append(String.format(" (@%s:%s:%s)", new Object[]{className, element.getMethodName(), Integer.valueOf(element.getLineNumber())}));
        }
        if (threadName != null) {
            descriptionBuilder.append(String.format(" {%s}", new Object[]{threadName}));
        }
        return descriptionBuilder.toString();
    }

    public String getDescription(String threadName, Throwable t) {
        return getDescription(getCause(t), getBestStackTraceElement(getCause(t)), threadName);
    }
}

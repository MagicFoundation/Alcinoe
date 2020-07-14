All the jar can be found here:
https://developers.facebook.com/docs/android/downloads/
https://search.maven.org/search?q=g:com.facebook.android

the source code can be downloaded from
https://github.com/facebook/facebook-android-sdk

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

repositories {
    mavenCentral()
}

dependencies {
    compile 'com.facebook.android:facebook-core:5.15.1'
    compile 'com.facebook.android:facebook-login:5.15.1'
    compile 'com.facebook.android:facebook-share:5.15.1'
    compile 'com.facebook.android:facebook-common:5.15.1'
}

and then run: gradlew app:dependencies

+--- com.facebook.android:facebook-core:5.15.1
|    +--- com.parse.bolts:bolts-android:1.4.0
|    |    +--- com.parse.bolts:bolts-tasks:1.4.0
|    |    \--- com.parse.bolts:bolts-applinks:1.4.0
|    |         \--- com.parse.bolts:bolts-tasks:1.4.0
|    +--- com.android.support:support-annotations:27.0.2
|    +--- com.android.support:support-core-utils:27.0.2
|    |    +--- com.android.support:support-annotations:27.0.2
|    |    \--- com.android.support:support-compat:27.0.2
|    |         +--- com.android.support:support-annotations:27.0.2
|    |         \--- android.arch.lifecycle:runtime:1.0.3
|    |              +--- android.arch.lifecycle:common:1.0.3
|    |              \--- android.arch.core:common:1.0.0
|    \--- com.android.installreferrer:installreferrer:1.1
+--- com.facebook.android:facebook-login:5.15.1
|    +--- com.facebook.android:facebook-core:5.15.1 (*)
|    +--- com.facebook.android:facebook-common:5.15.1
|    |    +--- com.facebook.android:facebook-core:5.15.1 (*)
|    |    +--- com.android.support:support-v4:27.0.2
|    |    |    +--- com.android.support:support-compat:27.0.2 (*)
|    |    |    +--- com.android.support:support-media-compat:27.0.2
|    |    |    |    +--- com.android.support:support-annotations:27.0.2
|    |    |    |    \--- com.android.support:support-compat:27.0.2 (*)
|    |    |    +--- com.android.support:support-core-utils:27.0.2 (*)
|    |    |    +--- com.android.support:support-core-ui:27.0.2
|    |    |    |    +--- com.android.support:support-annotations:27.0.2
|    |    |    |    \--- com.android.support:support-compat:27.0.2 (*)
|    |    |    \--- com.android.support:support-fragment:27.0.2
|    |    |         +--- com.android.support:support-compat:27.0.2 (*)
|    |    |         +--- com.android.support:support-core-ui:27.0.2 (*)
|    |    |         +--- com.android.support:support-core-utils:27.0.2 (*)
|    |    |         \--- com.android.support:support-annotations:27.0.2
|    |    +--- com.android.support:appcompat-v7:27.0.2
|    |    |    +--- com.android.support:support-annotations:27.0.2
|    |    |    +--- com.android.support:support-core-utils:27.0.2 (*)
|    |    |    +--- com.android.support:support-fragment:27.0.2 (*)
|    |    |    +--- com.android.support:support-vector-drawable:27.0.2
|    |    |    |    +--- com.android.support:support-annotations:27.0.2
|    |    |    |    \--- com.android.support:support-compat:27.0.2 (*)
|    |    |    \--- com.android.support:animated-vector-drawable:27.0.2
|    |    |         +--- com.android.support:support-vector-drawable:27.0.2 (*)
|    |    |         \--- com.android.support:support-core-ui:27.0.2 (*)
|    |    +--- com.android.support:cardview-v7:27.0.2
|    |    |    \--- com.android.support:support-annotations:27.0.2
|    |    +--- com.android.support:customtabs:27.0.2
|    |    |    +--- com.android.support:support-compat:27.0.2 (*)
|    |    |    \--- com.android.support:support-annotations:27.0.2
|    |    \--- com.google.zxing:core:3.3.3
|    \--- com.android.support:appcompat-v7:27.0.2 (*)
+--- com.facebook.android:facebook-share:5.15.1
|    +--- com.facebook.android:facebook-core:5.15.1 (*)
|    +--- com.facebook.android:facebook-common:5.15.1 (*)
|    \--- com.android.support:appcompat-v7:27.0.2 (*)
\--- com.facebook.android:facebook-common:5.15.1 (*)
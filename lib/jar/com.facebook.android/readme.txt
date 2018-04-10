All the jar can be found here:
https://developers.facebook.com/docs/android/downloads/

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'com.facebook.android:facebook-core:4.31.0'
  implementation 'com.facebook.android:facebook-common:4.31.0'
  implementation 'com.facebook.android:facebook-login:4.31.0'
  implementation 'com.facebook.android:facebook-share:4.31.0'
}

and then run: gradlew app:dependencies

+--- com.facebook.android:facebook-core:4.31.0
|    +--- com.parse.bolts:bolts-android:1.4.0
|    |    +--- com.parse.bolts:bolts-tasks:1.4.0
|    |    \--- com.parse.bolts:bolts-applinks:1.4.0
|    |         \--- com.parse.bolts:bolts-tasks:1.4.0
|    +--- com.android.support:support-annotations:27.0.2
|    \--- com.android.support:support-core-utils:27.0.2
|         +--- com.android.support:support-annotations:27.0.2
|         \--- com.android.support:support-compat:27.0.2
|              +--- com.android.support:support-annotations:27.0.2
|              \--- android.arch.lifecycle:runtime:1.0.3
|                   +--- android.arch.lifecycle:common:1.0.3
|                   \--- android.arch.core:common:1.0.0
+--- com.facebook.android:facebook-common:4.31.0
|    +--- com.facebook.android:facebook-core:4.31.0 (*)
|    +--- com.android.support:support-v4:27.0.2
|    |    +--- com.android.support:support-compat:27.0.2 (*)
|    |    +--- com.android.support:support-media-compat:27.0.2
|    |    |    +--- com.android.support:support-annotations:27.0.2
|    |    |    \--- com.android.support:support-compat:27.0.2 (*)
|    |    +--- com.android.support:support-core-utils:27.0.2 (*)
|    |    +--- com.android.support:support-core-ui:27.0.2
|    |    |    +--- com.android.support:support-annotations:27.0.2
|    |    |    \--- com.android.support:support-compat:27.0.2 (*)
|    |    \--- com.android.support:support-fragment:27.0.2
|    |         +--- com.android.support:support-compat:27.0.2 (*)
|    |         +--- com.android.support:support-core-ui:27.0.2 (*)
|    |         +--- com.android.support:support-core-utils:27.0.2 (*)
|    |         \--- com.android.support:support-annotations:27.0.2
|    +--- com.android.support:appcompat-v7:27.0.2
|    |    +--- com.android.support:support-annotations:27.0.2
|    |    +--- com.android.support:support-core-utils:27.0.2 (*)
|    |    +--- com.android.support:support-fragment:27.0.2 (*)
|    |    +--- com.android.support:support-vector-drawable:27.0.2
|    |    |    +--- com.android.support:support-annotations:27.0.2
|    |    |    \--- com.android.support:support-compat:27.0.2 (*)
|    |    \--- com.android.support:animated-vector-drawable:27.0.2
|    |         +--- com.android.support:support-vector-drawable:27.0.2 (*)
|    |         \--- com.android.support:support-core-ui:27.0.2 (*)
|    +--- com.android.support:cardview-v7:27.0.2
|    |    \--- com.android.support:support-annotations:27.0.2
|    +--- com.android.support:customtabs:27.0.2
|    |    +--- com.android.support:support-compat:27.0.2 (*)
|    |    \--- com.android.support:support-annotations:27.0.2
|    \--- com.google.zxing:core:3.3.0
+--- com.facebook.android:facebook-login:4.31.0
|    +--- com.facebook.android:facebook-core:4.31.0 (*)
|    +--- com.facebook.android:facebook-common:4.31.0 (*)
|    \--- com.android.support:appcompat-v7:27.0.2 (*)
\--- com.facebook.android:facebook-share:4.31.0
     +--- com.facebook.android:facebook-core:4.31.0 (*)
     \--- com.facebook.android:facebook-common:4.31.0 (*)

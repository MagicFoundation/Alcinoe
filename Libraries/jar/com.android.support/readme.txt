You can download the archive from: 
https://maven.google.com/com/android/support/support-compat/27.1.0/support-compat-27.1.0.aar
https://maven.google.com/com/android/support/support-fragment/27.1.0/support-fragment-27.1.0.aar
https://maven.google.com/com/android/support/support-core-utils/27.1.0/support-core-utils-27.1.0.aar
https://maven.google.com/com/android/support/support-core-ui/27.1.0/support-core-ui-27.1.0.aar
https://maven.google.com/com/android/support/support-media-compat/27.1.0/support-media-compat-27.1.0.aar
https://maven.google.com/com/android/support/appcompat-v7/27.1.0/appcompat-v7-27.1.0.aar
https://maven.google.com/com/android/support/cardview-v7/27.1.0/cardview-v7-27.1.0.aar
https://maven.google.com/com/android/support/customtabs/27.1.0/customtabs-27.1.0.aar
https://maven.google.com/com/android/support/support-vector-drawable/27.1.0/support-vector-drawable-27.1.0.aar
https://maven.google.com/com/android/support/animated-vector-drawable/27.1.0/animated-vector-drawable-27.1.0.aar
https://maven.google.com/com/android/support/support-annotations/27.1.0/support-annotations-27.1.0.jar

v4 support library:
https://developer.android.com/topic/libraries/support-library/packages.html
Prior to Support Library revision 24.2.0, there was a single v4 support library. 
That library was divided into multiple modules to improve efficiency. For backwards 
compatibility, if you list support-v4 in your Gradle script, your APK will include 
all of the v4 modules. However, to reduce APK size, we recommend that you just 
list the specific modules your app needs

com.android.support:support-compat:27.1.0
com.android.support:support-core-utils:27.1.0
com.android.support:support-core-ui:27.1.0
com.android.support:support-media-compat:27.1.0
com.android.support:support-fragment:27.1.0
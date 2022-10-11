All the jar can be found here:
https://maven.google.com

You can also download the POM and JAR files at:
maven.google.com/<group_path>/<library>/<version>/<library>-<version>.<ext>

  https://maven.google.com/com/google/android/gms/play-services-tasks/12.0.1/play-services-tasks-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-location/12.0.1/play-services-location-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-basement/12.0.1/play-services-basement-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-base/12.0.1/play-services-base-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-auth/12.0.1/play-services-auth-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-auth-base/12.0.1/play-services-auth-base-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-auth-api-phone/12.0.1/play-services-auth-api-phone-12.0.1.aar
      
*************************

  I don't know what to do with theses file, their jar seam empty and their is just a third_party_licenses.txt & third_party_licenses.json
  in it that i don't know how to include - so I skip them for now
  
  https://maven.google.com/com/google/android/gms/play-services-tasks-license/12.0.1/play-services-tasks-license-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-location-license/12.0.1/play-services-location-license-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-basement-license/12.0.1/play-services-basement-license-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-base-license/12.0.1/play-services-base-license-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-auth-base-license/12.0.1/play-services-auth-base-license-12.0.1.aar
  https://maven.google.com/com/google/android/gms/play-services-auth-api-phone-license/12.0.1/play-services-auth-api-phone-license-12.0.1.aar
      
*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies {
    implementation 'com.google.android.gms:play-services-tasks:12.0.1'
    implementation 'com.google.android.gms:play-services-location:12.0.1'
    implementation 'com.google.android.gms:play-services-basement:12.0.1'
    implementation 'com.google.android.gms:play-services-base:12.0.1'
}

and then run: gradlew app:dependencies

+--- com.google.android.gms:play-services-tasks:12.0.1
|    +--- com.google.android.gms:play-services-basement:12.0.1
|    |    +--- com.android.support:support-v4:26.1.0
|    |    |    +--- com.android.support:support-compat:26.1.0
|    |    |    |    +--- com.android.support:support-annotations:26.1.0
|    |    |    |    \--- android.arch.lifecycle:runtime:1.0.0
|    |    |    |         +--- android.arch.lifecycle:common:1.0.0
|    |    |    |         \--- android.arch.core:common:1.0.0
|    |    |    +--- com.android.support:support-media-compat:26.1.0
|    |    |    |    +--- com.android.support:support-annotations:26.1.0
|    |    |    |    \--- com.android.support:support-compat:26.1.0 (*)
|    |    |    +--- com.android.support:support-core-utils:26.1.0
|    |    |    |    +--- com.android.support:support-annotations:26.1.0
|    |    |    |    \--- com.android.support:support-compat:26.1.0 (*)
|    |    |    +--- com.android.support:support-core-ui:26.1.0
|    |    |    |    +--- com.android.support:support-annotations:26.1.0
|    |    |    |    \--- com.android.support:support-compat:26.1.0 (*)
|    |    |    \--- com.android.support:support-fragment:26.1.0
|    |    |         +--- com.android.support:support-compat:26.1.0 (*)
|    |    |         +--- com.android.support:support-core-ui:26.1.0 (*)
|    |    |         \--- com.android.support:support-core-utils:26.1.0 (*)
|    |    \--- com.google.android.gms:play-services-basement-license:12.0.1
|    \--- com.google.android.gms:play-services-tasks-license:12.0.1
+--- com.google.android.gms:play-services-location:12.0.1
|    +--- com.google.android.gms:play-services-base:12.0.1
|    |    +--- com.google.android.gms:play-services-basement:12.0.1 (*)
|    |    +--- com.google.android.gms:play-services-tasks:12.0.1 (*)
|    |    \--- com.google.android.gms:play-services-base-license:12.0.1
|    +--- com.google.android.gms:play-services-basement:12.0.1 (*)
|    +--- com.google.android.gms:play-services-tasks:12.0.1 (*)
|    \--- com.google.android.gms:play-services-location-license:12.0.1
+--- com.google.android.gms:play-services-basement:12.0.1 (*)
\--- com.google.android.gms:play-services-base:12.0.1 (*)
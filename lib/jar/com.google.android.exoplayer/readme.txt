download the jar from 
https://bintray.com/google/exoplayer/

  https://bintray.com/google/exoplayer/exoplayer-core#files/com/google/android/exoplayer/exoplayer-core
  https://bintray.com/google/exoplayer/exoplayer-hls#files/com/google/android/exoplayer/exoplayer-hls
  https://bintray.com/google/exoplayer/exoplayer-dash#files/com/google/android/exoplayer/exoplayer-dash
  https://bintray.com/google/exoplayer/exoplayer-smoothstreaming#files/com/google/android/exoplayer/exoplayer-smoothstreaming
  https://bintray.com/google/exoplayer/exoplayer-ui#files/com/google/android/exoplayer/exoplayer-ui

source code: 
https://github.com/google/ExoPlayer

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies {
    implementation 'com.google.android.exoplayer:exoplayer-core:2.6.1'
    implementation 'com.google.android.exoplayer:exoplayer-hls:2.6.1'
    implementation 'com.google.android.exoplayer:exoplayer-dash:2.6.1'
    implementation 'com.google.android.exoplayer:exoplayer-smoothstreaming:2.6.1'
    implementation 'com.google.android.exoplayer:exoplayer-ui:2.6.1'
}

and then run: gradlew app:dependencies

+--- com.google.android.exoplayer:exoplayer-core:2.6.1
|    \--- com.android.support:support-annotations:27.0.0
+--- com.google.android.exoplayer:exoplayer-hls:2.6.1
|    +--- com.google.android.exoplayer:exoplayer-core:2.6.1 (*)
|    \--- com.android.support:support-annotations:27.0.0
+--- com.google.android.exoplayer:exoplayer-dash:2.6.1
|    +--- com.google.android.exoplayer:exoplayer-core:2.6.1 (*)
|    \--- com.android.support:support-annotations:27.0.0
+--- com.google.android.exoplayer:exoplayer-smoothstreaming:2.6.1
|    +--- com.google.android.exoplayer:exoplayer-core:2.6.1 (*)
|    \--- com.android.support:support-annotations:27.0.0
\--- com.google.android.exoplayer:exoplayer-ui:2.6.1
     +--- com.google.android.exoplayer:exoplayer-core:2.6.1 (*)
     \--- com.android.support:support-annotations:27.0.0
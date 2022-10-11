download the jar from 
https://search.maven.org/search?q=g:com.vk

source code: 
https://github.com/VKCOM/vk-android-sdk

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'com.vk:androidsdk:+'
}

and then run: gradlew app:dependencies

\--- com.vk:androidsdk:+ -> 2.2.3
     +--- org.jetbrains.kotlin:kotlin-stdlib-jdk7:1.3.71
     |    \--- org.jetbrains.kotlin:kotlin-stdlib:1.3.71
     |         +--- org.jetbrains.kotlin:kotlin-stdlib-common:1.3.71
     |         \--- org.jetbrains:annotations:13.0
     +--- com.squareup.okhttp3:okhttp:3.12.1
     |    \--- com.squareup.okio:okio:1.15.0
     +--- com.squareup.okhttp3:logging-interceptor:3.12.1
     |    \--- com.squareup.okhttp3:okhttp:3.12.1 (*)
     \--- androidx.collection:collection:1.1.0
          \--- androidx.annotation:annotation:1.1.0
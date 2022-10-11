download the jar from 
https://search.maven.org/search?q=g:com.squareup.okhttp3%20AND%20a:okhttp
https://search.maven.org/search?q=g:com.squareup.okhttp3%20AND%20a:logging-interceptor

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'com.squareup.okhttp3:okhttp:4.7.2'
  implementation 'com.squareup.okhttp3:logging-interceptor:4.7.2'
}

and then run: gradlew app:dependencies

+--- com.squareup.okhttp3:okhttp:4.7.2
|    +--- com.squareup.okio:okio:2.6.0
|    |    +--- org.jetbrains.kotlin:kotlin-stdlib:1.3.70 -> 1.3.71
|    |    |    +--- org.jetbrains.kotlin:kotlin-stdlib-common:1.3.71
|    |    |    \--- org.jetbrains:annotations:13.0
|    |    \--- org.jetbrains.kotlin:kotlin-stdlib-common:1.3.70 -> 1.3.71
|    \--- org.jetbrains.kotlin:kotlin-stdlib:1.3.71 (*)
\--- com.squareup.okhttp3:logging-interceptor:4.7.2
     \--- com.squareup.okhttp3:okhttp:4.7.2 (*)
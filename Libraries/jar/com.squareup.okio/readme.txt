download the jar from 
https://search.maven.org/search?q=g:com.squareup.okio%20AND%20a:okio

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'com.squareup.okio:okio:2.6.0'
}

and then run: gradlew app:dependencies

\--- com.squareup.okio:okio:2.6.0
     +--- org.jetbrains.kotlin:kotlin-stdlib:1.3.70
     |    +--- org.jetbrains.kotlin:kotlin-stdlib-common:1.3.70
     |    \--- org.jetbrains:annotations:13.0
     \--- org.jetbrains.kotlin:kotlin-stdlib-common:1.3.70
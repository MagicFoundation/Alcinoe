download the jar from 
https://search.maven.org/search?q=g:org.jetbrains.kotlin%20AND%20a:kotlin-stdlib
https://search.maven.org/search?q=g:org.jetbrains.kotlin%20AND%20a:kotlin-stdlib-jdk7
https://search.maven.org/search?q=g:org.jetbrains.kotlin%20AND%20a:kotlin-stdlib-common

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'org.jetbrains.kotlin:kotlin-stdlib-jdk7:1.3.72'
  implementation 'org.jetbrains.kotlin:kotlin-stdlib:1.3.72'
  implementation 'org.jetbrains.kotlin:kotlin-stdlib-common:1.3.72'
}

and then run: gradlew app:dependencies

+--- org.jetbrains.kotlin:kotlin-stdlib-jdk7:1.3.72
|    \--- org.jetbrains.kotlin:kotlin-stdlib:1.3.72
|         +--- org.jetbrains.kotlin:kotlin-stdlib-common:1.3.72
|         \--- org.jetbrains:annotations:13.0
+--- org.jetbrains.kotlin:kotlin-stdlib:1.3.72 (*)
\--- org.jetbrains.kotlin:kotlin-stdlib-common:1.3.72
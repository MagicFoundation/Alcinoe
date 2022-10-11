download the jar from 
http://search.maven.org/#search%7Cga%7C1%7C%20com.parse.bolts

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'com.parse.bolts:bolts-android:1.4.0'
  implementation 'com.parse.bolts:bolts-applinks:1.4.0'
  implementation 'com.parse.bolts:bolts-tasks:1.4.0'
}

and then run: gradlew app:dependencies

+--- com.parse.bolts:bolts-android:1.4.0
|    +--- com.parse.bolts:bolts-tasks:1.4.0
|    \--- com.parse.bolts:bolts-applinks:1.4.0
|         \--- com.parse.bolts:bolts-tasks:1.4.0
+--- com.parse.bolts:bolts-applinks:1.4.0 (*)
\--- com.parse.bolts:bolts-tasks:1.4.0
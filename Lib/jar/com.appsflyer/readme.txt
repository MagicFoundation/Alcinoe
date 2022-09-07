download the jar from 
https://search.maven.org/search?q=a:af-android-sdk

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'com.appsflyer:af-android-sdk:4.8.17'
}

and then run: gradlew app:dependencies

\--- com.appsflyer:af-android-sdk:4.8.17 (n)
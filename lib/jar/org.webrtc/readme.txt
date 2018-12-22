download the jar from 
https://bintray.com/google/webrtc/google-webrtc

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'org.webrtc:google-webrtc:1.0.25331'
}

and then run: gradlew app:dependencies

\--- org.webrtc:google-webrtc:1.0.25331
     \--- com.google.code.findbugs:jsr305:3.+ -> 3.0.2
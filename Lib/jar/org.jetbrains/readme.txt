download the jar from 
https://search.maven.org/search?q=g:org.jetbrains%20AND%20a:annotations

*************************

I rename the jar for exemple annotations.jar to jetbrains-annotations.jar
because it's very stupid to have a library named annotations.jar 

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies { 
  implementation 'org.jetbrains:annotations:19.0.0'
}

and then run: gradlew app:dependencies

\--- org.jetbrains:annotations:19.0.0
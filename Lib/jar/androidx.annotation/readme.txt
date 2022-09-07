All the jar can be found here:
https://maven.google.com

You can also download the POM and JAR files at:
maven.google.com/<group_path>/<library>/<version>/<library>-<version>.<ext>

https://maven.google.com/androidx/annotation/annotation/1.1.0/annotation-1.1.0.jar

*************************

I rename the jar for exemple annotation.jar to androidx-annotation.jar
because it's very stupid to have a library named annotation.jar 

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies {
    implementation 'androidx.annotation:annotation:1.1.0'
}

and then run: gradlew app:dependencies

\--- androidx.annotation:annotation:1.1.0



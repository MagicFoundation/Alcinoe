All the jar can be found here:
https://maven.google.com

You can also download the POM and JAR files at:
maven.google.com/<group_path>/<library>/<version>/<library>-<version>.<ext>

  https://maven.google.com/com/android/installreferrer/installreferrer/1.0/installreferrer-1.0.aar

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies {
    implementation 'com.android.installreferrer:installreferrer:1.0'
}

and then run: gradlew app:dependencies

\--- com.android.installreferrer:installreferrer:1.0
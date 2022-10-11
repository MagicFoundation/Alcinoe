All the jar can be found here:
https://maven.google.com

You can also download the POM and JAR files at:
mhttps://dl.google.com/android/maven2/<group_path>/<library>/<version>/<library>-<version>.<ext>

  https://dl.google.com/android/maven2/com/android/billingclient/billing/3.0.1/billing-3.0.1.aar
  
*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies {
    implementation 'com.android.billingclient:billing:3.0.1'
}

and then run: gradlew app:dependencies

\--- com.android.billingclient:billing:3.0.1
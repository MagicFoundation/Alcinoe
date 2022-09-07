All the jar can be found here:
https://maven.google.com

You can also download the POM and JAR files at:
maven.google.com/<group_path>/<library>/<version>/<library>-<version>.<ext>

https://maven.google.com/android/arch/lifecycle/common/1.1.1/common-1.1.1.jar
https://maven.google.com/android/arch/lifecycle/runtime/1.1.1/runtime-1.1.1.aar
https://maven.google.com/android/arch/lifecycle/livedata-core/1.1.1/livedata-core-1.1.1.aar
https://maven.google.com/android/arch/lifecycle/viewmodel/1.1.1/viewmodel-1.1.1.aar

*************************

I rename the jar for for exemple common.jar to arch-lifecycle-common.jar
because it's very stupid to have a library named common.jar 

*************************

to know all the dependancies needed by a libraries, create a android
studio project, add the dependancies: 

dependencies {
    implementation 'android.arch.lifecycle:common:1.1.1'
    implementation 'android.arch.lifecycle:runtime:1.1.1'
    implementation 'android.arch.lifecycle:livedata-core:1.1.1'
    implementation 'android.arch.lifecycle:viewmodel:1.1.1'
}

and then run: gradlew app:dependencies

+--- android.arch.lifecycle:common:1.1.1
|    \--- com.android.support:support-annotations:26.1.0
+--- android.arch.lifecycle:runtime:1.1.1
|    +--- android.arch.lifecycle:common:1.1.1 (*)
|    +--- android.arch.core:common:1.1.1
|    |    \--- com.android.support:support-annotations:26.1.0
|    \--- com.android.support:support-annotations:26.1.0
+--- android.arch.lifecycle:livedata-core:1.1.1
|    +--- android.arch.lifecycle:common:1.1.1 (*)
|    +--- android.arch.core:common:1.1.1 (*)
|    \--- android.arch.core:runtime:1.1.1
|         +--- com.android.support:support-annotations:26.1.0
|         \--- android.arch.core:common:1.1.1 (*)
\--- android.arch.lifecycle:viewmodel:1.1.1
     \--- com.android.support:support-annotations:26.1.0



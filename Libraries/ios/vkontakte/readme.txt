download the source of the framework from 
https://github.com/VKCOM/vk-ios-sdk

DON'T CHOOSE THE MASTER ! select the last release (1.5.1 for example)

Current version
vk-ios-sdk-1.5.1

*************************

copy source of the framework in a directory in the mac and run from this directory (open a terminal at this folder)
xcodebuild -project VK-ios-sdk.xcodeproj -target VKSdkFramework -configuration Release clean build

You can also run: 
lipo -info build/Release-iphoneos/VKSdkFramework.framework/VKSdkFramework
to check that ther is only armv7 and arm64

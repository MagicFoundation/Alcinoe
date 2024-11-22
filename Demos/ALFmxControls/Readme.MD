To Deploy on iOS App Store
--------------------------

* Deploy the application from the IDE.
* Copy the generated .ipa file to your desktop: 
  .\iOSDevice64\Release\ALFmxControlsDemo.ipa → /Users/<username>/Desktop/
* Use the following command to upload the app to the App Store: 
  xcrun altool --upload-app --type ios --file "/Users/<username>/Desktop/ALFmxControlsDemo.ipa" --username "<AppStoreUsername>" --password "<AppStorePassword>"

To Deploy on macOS App Store
----------------------------

* Deploy the application from the IDE.
* Use the following command to upload the app to the App Store:
  xcrun altool --upload-app --type macos --file "/Users/<username>/PAServer/scratch-dir/<username>-VMWare/ALFmxControlsDemo.pkg" --username "<AppStoreUsername>" --password "<AppStorePassword>"

rm -r bin/
ant debug
~/android-sdk-linux/platform-tools/adb uninstall com.pascal.lcltest
~/android-sdk-linux/platform-tools/adb install bin/LCLExample-debug.apk
~/android-sdk-linux/platform-tools/adb logcat

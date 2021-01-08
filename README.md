# VRD Project (Vape Recycling Displays)

## How to deploy to Web using JSaddle

## How to deploy to Android

1. If using WSL - start adb server on port 5038
```sh
$ adb -a -P 5038 nodaemon server
```
2. Ensure device can be seen in list of emulators
```sh
$ adb -a -H 192.168.55.33 -P 5038 devices
```
OR set ANDROID_ADB_SERVER_ADDRESS and ANDROID_ADB_SERVER_PORT values in ~/.zshrc or ~/.bashrc
```sh
export ANDROID_ADB_SERVER_ADDRESS=192.168.55.33
export ANDROID_ADB_SERVER_PORT=5038
```
3. Specify output path and Android attribute in nix-build
```sh
$ nix-build -o android-result -A android.<symbol>
```
4. Deploy to android device using adb install (or the default bin/deploy script provided by Reflex)
```sh
$ android-result/bin/deploy
```

## How to deploy to iOS

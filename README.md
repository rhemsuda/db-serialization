# Database Serialization Project
This project was intended on serving a larger purpose, but since contracts fell through due to COVID-19, it serves as example code for my personal projects.

## How to test using REPL

1. Run Nix-shell command to load cabal packages into nix environment
```sh
$ nix-shell -A shells.ghc
```
2. Run the cabal repl for whatever cabal package you wish to run
```sh
$ cabal repl backend
```

## How to deploy to Web using JSaddle

## How to deploy to Android
> Important: Before you start, you will need to install Android Studio and get familiar with the Emulator and Android Debug Bridge (ADB). You can find the download steps here: 
https://developer.android.com/studio?gclid=CjwKCAiAo5qABhBdEiwAOtGmbruE3GqMEWTLYS6S8HhaYxfoy5yGKmOCYam8rUMAOaOKjHmYHkCDkBoCCHwQAvD_BwE&gclsrc=aw.ds. 

> Note: If you are using WSL, you will also need to download the adb cli on your linux machine: https://developer.android.com/studio/command-line/adb

1. If not using WSL skip this step. Otherwise, start adb server on port 5038 on the host machine.
```sh
$ adb -a -P 5038 nodaemon server
```
2. Ensure device can be seen in list of emulators. The IP and PORT are that of the host machine's adb service.
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
$ nix-build -o android-result -A android.app
```
4. Deploy to android device using adb install (or the default bin/deploy script provided by Reflex)
```sh
$ android-result/bin/deploy
```

## How to deploy to iOS


## How to create database using postgresql

1. Install Postgres
```sh
$ sudo apt-get install postgresql libpq-dev
```
2. Create user and alter password
```sh
$ sudo -u postgres createuser -se <username>
$ sudo -u postgres psql -c "alter role <username> with password 'test'"
```
3. Create the vrd database
```sh
$ sudo -u postgres psql -c "create database vrd"
```

## Basic postgres commands

1. List tables
```sh
$ psql -w -d vrd -c "\dt"
```
2. Select from table
```sh
$ psql -w -d vrd -c "SELECT * from v_r_d_registry"
```
3. Drop table
```sh
$ psql -w -d vrd -c "DROP TABLE v_r_d_registry"
```

## How to test Backend

1. Curl request
```sh
$ curl -X POST -d '{"storeNumber": "0001", "storeLocationCode": "ON", "storeName": "Waterloo Vapes"}' -H 'Accept: application/json' -H 'Content-Type: application/json' http://localhost:8081/shop/create
```

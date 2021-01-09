# VRD Project (Vape Recycling Displays)

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


## How to create database using postgresql

1. Install Postgres
```sh
$ apt install postgres libpq-dev
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

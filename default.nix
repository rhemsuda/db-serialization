# default.nix

(import ./reflex-platform {
  config.android_sdk.accept_license = true;
}).project ({ pkgs, ... }: {
  useWarp = true;
  
  packages = {
    common = ./common;
    backend = ./backend;
    app = ./app;
    vrd = ./vrd;
    landing = ./landing;
  };

  shells = {
    ghc = ["app" "vrd" "landing" "common" "backend"];
    ghcjs = ["app" "vrd" "landing" "common"];
  };

  android.app = {
    executableName = "app";
    applicationId = "org.solvex.vrd.app";
    displayName = "VaporCartel Wallet";
  };

  ios.app = {
    executableName = "app";
    bundleIdentifier = "org.solvex.vrd.app";
    bundleName = "VaporCartel Wallet";
  };
  
  android.vrd = {
    executableName = "vrd";
    applicationId = "org.solvex.vrd.display";
    displayName = "VRD";
  };
})

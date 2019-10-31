{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ ... }: {
  android.applicationId = "ca.srid.modus.dev";
  android.displayName = "Modus";
  ios.bundleIdentifier = "ca.srid.modus.dev";
  ios.bundleName = "Modus";
})

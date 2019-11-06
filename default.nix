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
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "ca.srid.modus.dev";
  android.displayName = "Modus";
  ios.bundleIdentifier = "ca.srid.modus.dev";
  ios.bundleName = "Modus";

  packages = {
    clay = pkgs.fetchFromGitHub {
      owner = "sebastiaanvisser";
      repo = "clay";
      rev = "54dc9eaf0abd180ef9e35d97313062d99a02ee75";
      sha256 = "0y38hyd2gvr7lrbxkrjwg4h0077a54m7gxlvm9s4kk0995z1ncax";
    };
    shower = hackGet ./dep/shower;  # Marked as broken in nixpkgs (19.09)
    rib = hackGet ./dep/rib;
    pandoc-include-code = hackGet ./dep/pandoc-include-code;
  };

  overrides = self: super: with pkgs.haskell.lib; {
    clay = dontCheck super.clay;
  };

})

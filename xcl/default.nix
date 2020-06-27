with import <nixpkgs> {};
stdenv.mkDerivation rec {
    name = "xcl";
    env = buildEnv {
        name = name;
        paths = buildInputs;
    };
    buildInputs = [
        nodejs-10_x
        watchexec
    ];
    shellHook = ''
        export PATH=$PATH:$(npm bin)
    '';
}

with import <nixpkgs> {};
stdenv.mkDerivation rec {
    name = "dev-env";
    env = buildEnv {
        name = name;
        paths = buildInputs;
    };
    buildInputs = [
        glibcLocales
        gnome3.gtk
        emacs
        git

        nodejs-10_x
    ];
    shellHook = ''
        export GTK_THEME=Adwaita
        export HOME=$PWD/.nix

        eval_forms="(require 'package)"
        eval_forms="$eval_forms (package-initialize)"
        eval_forms="$eval_forms (add-to-list 'package-archives '(\\\"melpa\\\" . \\\"https://melpa.org/packages/\\\") t)"
        eval_forms="$eval_forms (add-to-list 'load-path \\\"~/.emacs.d/json-rpc-request.el\\\")"
        alias emacs="emacs --eval=\"(progn $eval_forms)\""

        if [ ! -e $HOME ]; then
            echo "creating .nix directory"
            mkdir -p $HOME

            # dependencies
            emacs -batch --eval="(progn (package-initialize) (package-refresh-contents) (package-install 'ov) (package-install 'json-rpc))"
            # for best practices
            emacs -batch --eval="(progn (package-initialize) (package-refresh-contents) (package-install 'package-lint))"
        fi

        if [ ! -e ./ext--json-rpc-request ]; then
            git submodule init && git submodule update
        fi

        export PATH=$PATH:$(npm bin)
        cat default.nix | grep '[a]lias'
    '';
}

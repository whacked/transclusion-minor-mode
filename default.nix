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

        nodejs
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

			emacs -batch --eval="(progn (package-initialize) (package-refresh-contents) (package-install 'ov) (package-install 'json-rpc))"
        fi

        if [ ! -e $HOME/.emacs.d/json-rpc-request.el ]; then
			git clone https://github.com/whacked/json-rpc-request.el .nix/.emacs.d/json-rpc-request.el
        fi
    '';
}

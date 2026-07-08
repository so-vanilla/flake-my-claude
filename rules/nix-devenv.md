# Nix and devenv guidelines

- `github.com/so-vanilla/*` 配下では `flake.nix` を使用してビルドや依存を定義する。
- dev shell は `devenv` で定義し、`flake.nix` に devShell を重複定義しない。
- `github.com/so-vanilla/*` 以外のリポジトリでは、明示指示がない限り `flake.nix` を追加しない。
- `devenv init` が `.gitignore` を変更した場合は、プロジェクト方針に合わせて元に戻す。
- `so-vanilla/*` 配下では、会社環境向けに `.git/info/exclude` を調整し、devenv 関連ファイルを Git 管理する。
- `flake.nix` の input には `flake-utils` を使う。
- `nix-community/comma` の `,` コマンドを使う場合は、事前にユーザー確認を取る。

# MVScript

MVScript is a domain-specific language written in Haskell that transpiles to Rust, with much simpler syntax, allowing for powerful yet simple usage of the [mathvis](https://github.com/mathvis/mathvis-core) animation library.
A [language reference](https://github.com/mathvis/mvscript-docs) with comprehensive examples will be available alongside version 1.0.0 of MVScript and Mathvis.

This project is still very much in an early stage and will definitely contain a lot of bugs, so be patient, and feel free to leave an issue, so that bugs are fixed as fast as possible.

![CI](https://github.com/mathvis/mvscript/actions/workflows/haskell.yml/badge.svg)
[![GitHub contributors](https://img.shields.io/github/contributors/mathvis/mvscript)](https://github.com/mathvis/mvscript/graphs/contributors)

## Prerequisites

- Make sure `~/.local/bin` is in your path.
- `git` and `curl`, `wget` or `fetch` must be installed.

## Installation

To install the MVScript compiler (mvscc), run the `install.sh` script using `curl`, `wget` or `fetch`.

| Method | Command |
|---|---|
|curl|`sh -c "$(curl https://raw.githubusercontent.com/mathvis/mvscript/refs/heads/main/install.sh)"`|
|wget|`sh -c "$(wget -O- https://raw.githubusercontent.com/mathvis/mvscript/refs/heads/main/install.sh)"`|
|fetch|`sh -c "$(fetch -o - https://raw.githubusercontent.com/mathvis/mvscript/refs/heads/main/install.sh)"`|

## Using mvscc

To run the MVScript compiler, run `mvscc <source> [config]`, where `config` is a .toml file. Check the included file in the repository for the available options. A comprehensive guide of every option will be available on the website when version 1.0.0 comes out. The default file is found in `$HOME/.mvscc/config.toml`

## Contributing

If you have any ideas or want to work on the project, don't hesitate to leave an issue or a pull request. Before leaving an issue, go through the already open ones and check if it is already being addressed.

## License

MVScript and mvscc are released under the [GNU General Public License v3.0](https://github.com/mathvis/mvscript/blob/main/LICENSE).

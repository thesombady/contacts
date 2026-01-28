# Contacts
Contacts is redefined version of `cbook` using a proper markup language.
In contrast to the previous version, which was written in `Python`
`contacts` is written in Haskell; the reason for why is simple.
I wanted a specific syntax on the `TOML` file, which was not
supported by any TOML-library (to my knowledge) but did not violate
the defined parameters of (TOML)[https://www.toml.io/en].
Therefore, I implemented a parser for that allows for this syntax,
and is a subset of the entirety of TOML. With time, the parser
may also be extended to include full `Toml 1.0.0` capabilities.

## Installation
In order to use `contacts` one is required to have a working `GHC`
installation, with `Cabal`. Secondly, it by defaults installs the binary
in `~/.lib/bin`. Either create this directory and add it to `PATH` or change
the installation location in `install.sh` to `$HOME/.local/bin`

```sh
git clone https://www.github.com/thesombady/contacts.git
cd contacts
sh install.sh
```
See that the installation is working for the suite tests, and then
add the executable to the PATH, and start building the address-book.

## Usage
Build your contact book using `Toml` format, after installation it's located
in `~/.contacts/addresses.toml`. This project could be used to allow for
autocompletion of contacts in for example [Aerc](https://aerc-mail.org/).

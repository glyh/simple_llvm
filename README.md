# Simple_LLVM

A simple C-like language with LLVM as backend.

### Building

- Clone the repo and init the switch
```sh
git clone https://github.com/glyh/simple_llvm
cd simple_llvm
opam switch create . --deps-only --with-test -y
```
- For developing, you may want to have LSP and other stuffs available
```sh
opam install --switch=. -y ocamlformat ocaml-lsp-server utop
```
- Update the environment, for example if you're on bash: 
```bash
eval $(opam env)
```
- Build and run the package
```sh
dune exec simple_llvm
```

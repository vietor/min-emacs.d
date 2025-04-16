Emacs config
===========================

This repository contains my personal Emacs configuration.

# Languages

## Python

Use Language Server

```bash
pip install python-lsp-server python-lsp-ruff
```

## Javascript

Use Language Server

```bash
npm install -g typescript-language-server typescript prettier
```

## Java

Use Language Server ```https://github.com/eclipse-jdtls/eclipse.jdt.ls```.  
Depencency directories in ```${user-emacs-space-directory}```:

| *Path*                | *Usage*                          |
|-----------------------|----------------------------------|
| java.assists          | include plugins for lombook*.jar |
| java-ls.caches        | cache for jdt://contents files   |
| java-ls.workspaces    | store project's workspace files  |
| jdt-language-server-* | eclipse-jdtls's files            |

# Toolkit

## Tree-sitter

Use bundle ```https://github.com/emacs-tree-sitter/tree-sitter-langs```.  
Target directories is ```${user-emacs-space-directory}/tree-sitter/```

* Rename script for `Linux`
```bash
cd tree-sitter

for file in *.so; do
    mv "$file" "libtree-sitter-$file"
done
```

* Rename script for `Windows`

```cmd
@echo off
cd tree-sitter

for /f %%f in ('dir /b *.dll') do (
  ren %%f libtree-sitter-%%f
)
```


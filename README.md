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
| eclipse.assists       | include plugins for lombook*.jar |
| eclipse.caches        | cache for jdt://contents files   |
| eclipse.workspaces    | store project's workspace files  |
| jdt-language-server-* | eclipse-jdtls's files            |

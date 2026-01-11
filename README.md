# Dessert

Dessert (short for Deserialize/Serialize Transforms) is a DSL for describing data formats. It aims to be a simple yet powerful language combining the power of regular expressions with the familiarity of standard data types to produce parsers or interpreters for practically any format.

The current documentation available is the [Dessert Language Specification](./docs/SPEC-0.1.adoc).

## Installation

This repository includes two components:

- `\lang` which includes the Dessert runtime internals. The target audience is developers looking to integrate Dessert into their projects.
- `\cli` which includes the Dessert CLI. The target audience is users and developers looking to use Dessert or develop definitions for it.

To use the Dessert CLI, the supported way is to either build from source or to use one of the precompiled binaries. A precompiled build for Windows is available in [Releases](https://github.com/aescarias/dessert/releases) for you to try out. For other platforms, you will currently need to build from source.

## Building CLI from Source

To build the Dessert CLI from source, you will need a copy of the [Go runtime](https://go.dev) (Go 1.25 or greater) and preferably also a copy of [Git](https://git-scm.com/)  (using Git is not strictly required but steps below will assume Git).

1. Clone the repository using Git

    ```sh
    git clone https://github.com/aescarias/dessert dessert
    cd dessert
    ```

2. Setup a Go workspace. This will allow you to use the Dessert library without having to `go get` it.

    ```sh
    go work init
    go work use ./lang ./cli
    ```

3. Build the `cli` module (on Windows, `dessert` would instead be `dessert.exe`).

    ```sh
    go build -o dessert ./cli
    ```

(for a smaller file size, you can also specify `-ldflags "-s -w"` to strip debug information from the produced executable)

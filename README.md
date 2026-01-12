# Dessert

Dessert (short for Deserialize/Serialize Transforms) is a DSL for describing data formats. It aims to be a simple yet powerful language combining the power of regular expressions with the familiarity of standard data types to produce parsers or interpreters for practically any format.

The current documentation available is the [Dessert Language Specification](./docs/SPEC-0.1.adoc).

## Installation

To use the Dessert CLI, you can either use one of the precompiled binaries or build from source. A precompiled build for Windows is available in [Releases](https://github.com/aescarias/dessert/releases) for you to try out. For other platforms, you will currently need to build from source.

## Building from Source

To build the Dessert CLI from source, you will need a copy of the [Go runtime](https://go.dev) (Go 1.25 or greater) and preferably also a copy of [Git](https://git-scm.com/)  (using Git is not strictly required but steps below will assume Git).

1. Clone the repository using Git

    ```sh
    git clone https://github.com/aescarias/dessert dessert
    cd dessert
    ```

2. Build the `cli` module (on Windows, `dessert` would instead be `dessert.exe`)

    ```sh
    go build -o .\dessert .\cli
    ```

    (for a smaller file size, you can also specify `-ldflags "-s -w"` to strip debug information from the resulting executable)

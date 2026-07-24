# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

`netrics` is an R package (part of the [stocnet](https://github.com/stocnet) ecosystem) providing the *analytic engine* for network analysis: marks, measures, memberships, and motifs for nodes, ties, and networks. It depends on `{manynet}`, which owns network classes, coercion, and network-level logical tests (`is_*()`). `{autograph}` (drawing) and `{migraph}` (modelling/testing) build on top of `{netrics}`.

Full package documentation — common dev commands, function family naming conventions, function body conventions, file organization, test conventions, and branching/CI — lives in [.github/CONTRIBUTING.md](.github/CONTRIBUTING.md#package-architecture) (the "Package architecture" section). Read it before adding or restructuring functions, or when you need the exact `devtools`/testing commands for this repo.

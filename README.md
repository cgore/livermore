![Jesse Livermore](Jesse%20Livermore.jpeg)

[Jesse Lauriston Livermore](https://en.wikipedia.org/wiki/Jesse_Lauriston_Livermore)

**[View PDF Documentation](documentation/livermore.pdf)**

# Introduction

Livermore is an evolutionary rules-based AI library in Common Lisp.

# License

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* Neither the name of Christopher Mark Gore nor the names of other contributors may be used to endorse or promote products derived from this software without specific prior written permission.

**THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS *"AS IS"* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.**

# Initial Setup

## OS Level Setup

### Debian/Ubuntu/etc.

```shell
apt-get install rlwrap sbcl texlive-full
```

### Red Hat/Fedora/etc.

```shell
sudo dnf install sbcl rlwrap curl texlive-scheme-full
```

## Initial Quicklisp Setup

```shell
curl -O https://beta.quicklisp.org/quicklisp.lisp

sbcl --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql:add-to-init-file)' \
     --eval '(quit)'
```

```lisp
(ql:quickload "sigma")
```

McCLIM is only needed for the optional `livermore/trade-chart` system.

# How To Use

Clone this repository and [sigma](https://github.com/cgore/sigma) where ASDF can see them, then:

```lisp
(asdf:load-system :livermore)
(asdf:test-system :livermore)
```

`(use-package :livermore)` re-exports the core library. Experiment starters such as `start-inde-tmscs-experiment` and `start-stocks-tsc-experiment` are also exported; pass a small trial count to run them quickly:

```lisp
(livermore:start-inde-tmscs-experiment 20)
(livermore:start-multiplexer-experiment 2 50)
```

The stock-market TSC from the MS thesis lives in `livermore/stocks-tsc` and uses the bundled `^dji` table. A full 1500-day run is `start-stocks-tsc-experiment`; the tests run a short synthetic series and check that the published DJI window still loads.

The McCLIM chart is optional:

```lisp
(asdf:load-system :livermore/trade-chart)
(livermore/trade-chart:demo)
```

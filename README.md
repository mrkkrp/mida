# MIDA

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Hackage](https://img.shields.io/hackage/v/mida.svg?style=flat)](https://hackage.haskell.org/package/mida)
[![Build Status](https://travis-ci.org/mrkkrp/mida.svg?branch=master)](https://travis-ci.org/mrkkrp/mida)

MIDA is a minimalistic declarative language for algorithmic generation of
MIDI files. MIDA is not interactive in sense that you cannot control result
of its activity in real time, it is intended for producers and should be
used with a DAW. MIDA can help you create variative elements in your music
in a very simple way. Since MIDI can control a lot of different instruments,
power of MIDA is truly great.

Main reason for MIDA development is to create software tool that can
be used in such a way that does not change established workflow, so
people could use familiar plugins and software instruments.

Currently MIDA can be used to translate source files into `.mid` files, and
also in interactive mode that will help you to understand how MIDA language
works.

MIDA is a
[member of Linux Audio consortium](http://linuxaudio.org/members.html).

## Installation

1. Install [Haskell Platform](https://www.haskell.org/platform/);
2. Install [Cabal](https://www.haskell.org/cabal/);
3. Download and untar git repository of MIDA, or clone it:

   ```
   $ git clone https://github.com/mrkkrp/mida.git master
   ```

4. Go to the root directory of the repository and execute:

   ```
   $ cabal update
   $ cabal configure
   $ cabal install --only-dependencies
   $ cabal build
   # sh install.sh
   ```

5. Done (you can use `uninstall.sh` to uninstall the program).

## Example

Here is a simple example of MIDA program that demonstrates syntax and
declarative nature of the language.

```
#
# Example of MIDA program
# Recommended tempo is 60
#

dur0 = 12 6 6
dur1 = @[dur0]
pch0 = {0 5 7} + [c5 {e5 d5} c6 {f5 g5}]
pch1 = pch0
vel0 = {7..11} * 10
vel1 = vel0
```

See [MIDA Manual](https://mrkkrp.github.io/mida/) for more information.

## How to control other things?

If you're into this sort of thing, you may like my another project, called
[ALGA](https://github.com/mrkkrp/alga). It allows to control automation in
various DAWs.

## License

Copyright © 2014, 2015 Mark Karpov

Distributed under GNU GPL, version 3.

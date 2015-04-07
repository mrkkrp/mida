# MIDA

MIDA is a minimalistic language for algorithmic generation of MIDI
files. MIDA is not interactive in sense that you cannot control result
of its activity in real time, it is intended for producers and should
be used with a DAW. MIDA can help you create variative elements in
your music in a very simple way. Since MIDI can control a lot of
different instruments, power of MIDA is truly great.

Main reason for MIDA development is to create software tool that can
be used in such a way that does not change established workflow, so
people could use familiar plugins and software instruments.

The core concept of MIDA is building systems with complex behaviors
from very basic and easy-to-understand elements and powerful means of
their composition.

Currently MIDA can be used to translate source files into `.mid` files, and
also in interactive mode that will help you to understand how MIDA language
works.

See [MIDA Manual](https://mrkkrp.github.io/mida/) for more information.

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

## License

Copyright © 2014, 2015 Mark Karpov

Distributed under GNU GPL, version 3.

# Installation

## Installation on Windows

1. Download installer for MS Windows;
2. Run it;
3. Agree with everything;
4. Done.

## Installation on Linux

1. Download self-extracting archive;
2. Go to the directory where the archive is located and execute:

   ```
   # sh mida-0.3.0.sh
   ```

3. Done.

## Compilation from Source

This is more general way and it works on every platform on which
Haskell Platform is supported.

1. Install [Haskell Platform](https://www.haskell.org/platform/);
2. Install [Cabal](https://www.haskell.org/cabal/);
3. Download and untar git repository of MIDA, or clone it:

   ```
   $ git clone https://github.com/mrkkrp/mida.git master
   ```

4. Go to the root directory of the repository and execute:

   ```
   $ cabal update
   $ cabal install cabal-install
   $ cabal configure
   $ cabal build
   ```

   It's very likely that Cabal will not compile MIDA from the first
   time so you'll need to install all dependencies that Cabal will
   list for you.

5. After compilation you should manually copy executable file
   `dist/build/mida/mida` into some directory where your system will
   see the executable. Linux users can run the following script (it
   also copies the documentation and midarm for uninstallation.):

   ```
   # sh install.sh
   ```

6. Done.

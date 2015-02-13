# Installation

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

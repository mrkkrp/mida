#!/bin/sh
#
# MIDA Installation script
#
# Copyright (c) 2014, 2015 Mark Karpov
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program.  If not, see <http://www.gnu.org/licenses/>.

### constants

I_DIRS="/usr/share/{licenses,doc}/mida"

### functions

bad_exit() # prints error message and exits the program
{
    echo "failed" 1>&2
    exit 1
}

### main

echo 'MIDA installation has been started;'

# 1. check if actual user is root (must be root to install the software)

echo -n 'actual user must be root...'
test $(id -u) -gt 0 && bad_exit
echo 'ok'

# 2. check if there is compiled executable

echo -n 'searching for executable...'
test -f dist/build/mida/mida || bad_exit
echo 'ok'

# 3. strip the executable is there is `strip' program and the executable is
# not stripped already

if which strip > /dev/null
then echo -n 'stripping the binary...'
     strip dist/build/mida/mida
     test $(id -u) -gt 0 && bad_exit
     echo 'ok'
else echo 'cannot find strip utility!'
fi

# 4. creating directories

echo 'creating directories...'
eval install -vdm755 $I_DIRS
if test $? -eq 0
then echo 'creating directories: ok'
else bad_exit
fi

# 5. copying new files

echo 'copying new files...'
install -vDm755 dist/build/mida/mida /usr/bin/
install -vDm644 LICENSE.md           /usr/share/licenses/mida/
install -vDm644 doc/*.{texi,html}    /usr/share/doc/mida/
install -vDm644 doc/mida.1.gz        /usr/share/man/man1/
echo 'copying new files: ok'

# 6. done

echo 'done.'

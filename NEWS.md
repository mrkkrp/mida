# History of changes

## MIDA 0.3.1

* fixed bug in batch mode;

* added tab completion based on contents of current input line, including
  completion of file names for some commands;

* detection and rejection of recursive definitions;

* improved interface;

* wholly refactored (and sometimes rewritten) code (it's finally
  sufficiently elegant);

* more intuitive logic of evaluation in some corner cases;

* optional displaying of simplified principles (for debug and educational
  purposes).

## MIDA 0.3.0

* improved interface and parsing;

* added `:prv` and `:load` special commands;

* added additional parameters: modulation, breath, aftertouch, and pitch
  bend;

* fixed minor bugs.

## MIDA 0.2.0

* better command line experience: history, auto-completion and more;

* fixed bug with infinite translation of scores that consist of elements
  which have zero duration;

* introduced conception of sections;

* all operations have become fully polymorphic.

## MIDA 0.1.0

Initial release.

## MIDA 1.0.0

* Use the `path-io` library.

* Use YAML files for configuration.

* Cosmetic changes in the interface.

## MIDA 0.4.6

* Support for Stack and other maintenance stuff;

* Refactoring, more type-safe code and better libraries used.

## MIDA 0.4.5

* Various cosmetic changes;

* Make the executable Cabal-installable.

## MIDA 0.4.4

* Cosmetic corrections in source code;

* Switch to better libraries.

## MIDA 0.4.3

* Various corrections and cosmetic changes;

* Improved printing of syntax trees;

* Allow more flexible looping expressions.

## MIDA 0.4.2

* Introduced simplifications on the level of internal language;

* Changed default values of some parameters;

* Some characters have been replaced with pretty Unicode symbols;

* Improved manual.

## MIDA 0.4.1

* Dramatically improved efficiency, now very long compositions can be
  quickly rendered;

* Lexer: changed style of comments, shell-like comments adopted;

* Changed alias of notes: dièse is written now as `s` not as `#`, so middle
  octave is: `c5`, `cs5`, `d5`, `ds5`, `e5`, etc.;

* Now there are alias for all supported (in MIDI) pitches from 0 to 127,
  that is: from `c0` to `g10` (alias `c10` -- `g10` added);

* Changed alias of modulation signs, reason for such change is purely
  technical -- all alias now are predefined definitions, rather than
  syntactic sugar, so all alias must be valid identifiers;

* Identifiers can contain underline sign (`_`);

* Arbitrary number of files can be specified for loading (from command line,
  as well as from interactive REPL);

* Many purely technical changes that are difficult to explain concisely, but
  they should be mentioned, most important being addition of test suite and
  ability to generate source files from syntax trees;

* Added command line options: `--license` and `--version`.

## MIDA 0.4.0

* Fixed bug in batch mode;

* Added tab completion based on contents of current input line, including
  completion of file names for some commands;

* Detection and rejection of recursive definitions;

* Improved interface;

* Wholly refactored (and sometimes rewritten) code (it's finally
  sufficiently elegant);

* More intuitive logic of evaluation in some corner cases;

* Optional displaying of simplified principles (for debug and educational
  purposes);

* Ability to set tempo and program for preview;

* Improved documentation.

## MIDA 0.3.0

* Improved interface and parsing;

* Added `:prv` and `:load` special commands;

* Added additional parameters: modulation, breath, aftertouch, and pitch
  bend;

* Fixed minor bugs.

## MIDA 0.2.0

* Better command line experience: history, auto-completion and more;

* Fixed bug with infinite translation of scores that consist of elements
  which have zero duration;

* Introduced conception of sections;

* All operations have become fully polymorphic.

## MIDA 0.1.0

* Initial release.

# History of changes

## MIDA 0.4.2

* introduced simplifications on the level of internal language;

* changed default values of some parameters;

* some characters have been replaced with pretty Unicode symbols;

* improved manual.

## MIDA 0.4.1

* dramatically improved efficiency, now very long compositions can be
  quickly rendered;

* lexer: changed style of comments, shell-like comments adopted;

* changed alias of notes: dièse is written now as `s` not as `#`, so middle
  octave is: `c5`, `cs5`, `d5`, `ds5`, `e5`, etc.;

* now there are alias for all supported (in MIDI) pitches from 0 to 127,
  that is: from `c0` to `g10` (alias `c10` -- `g10` added);

* changed alias of modulation signs, reason for such change is purely
  technical -- all alias now are predefined definitions, rather than
  syntactic sugar, so all alias must be valid identifiers;

* identifiers can contain underline sign (`_`);

* arbitrary number of files can be specified for loading (from command line,
  as well as from interactive REPL);

* many purely technical changes that are difficult to explain concisely, but
  they should be mentioned, most important being addition of test suite and
  ability to generate source files from syntax trees;

* added command line options: `--license` and `--version`.

## MIDA 0.4.0

* fixed bug in batch mode;

* added tab completion based on contents of current input line, including
  completion of file names for some commands;

* detection and rejection of recursive definitions;

* improved interface;

* wholly refactored (and sometimes rewritten) code (it's finally
  sufficiently elegant);

* more intuitive logic of evaluation in some corner cases;

* optional displaying of simplified principles (for debug and educational
  purposes);

* ability to set tempo and program for preview;

* improved documentation.

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

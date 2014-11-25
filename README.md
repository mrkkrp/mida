# MIDA

## Description

MIDA is a minimalistic language for creating generative MIDI
files. MIDA is not interactive in sense that you cannot control result
of its activity in real time, it is intended for producers and should
be used with a DAW. MIDA can help you create variative elements in
your music in very simple way. Since MIDI can control a lot of
different instruments, power of MIDA is truly great.

See [MIDA Manual](http://mrkkrp.github.io/mida/) for more information.

For installation instructions see `INSTALL.md`.

## Information

My final goal is to create software tools that can be used to bring
generative music to producers in such a way that does not change the
established workflow, so people could use familiar plugins and software
instruments.

Before starting work on MIDA, I looked at some well-known tools to program
music. In particular, Sound Collider and its bindings for Clojure (Overtone)
were looking promising, but I decided that all that stuff will be difficult
to learn for non-programmers and without a good API for automated
timbre-building it will be quite difficult to create sexy sound with these
tools.

It's funny that most examples of music created with Overtone try to imitate
conventional electronic music instead of using unique possibilities to
actually program music. They are also more concerned with process
(live-programming) than result, while I'm interested in the result only.

So, my principal decision was to create a set of tools:

- tool that will produce MIDI files algorithmically (MIDA);
- tool that can change tuning, temperament, and other parameters of already
  created MIDI file;
- tool to program automation for different DAWs.

## License

Copyright (c) 2014 Mark Karpov

Distributed under GNU GPL.

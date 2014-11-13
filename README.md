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

Major improvements of MIDA will be undertaken in the near future. So,
consider version 0.1.0 as some kind of beta. Although it has proved to be
practically useful in music production, I want you to remember that it is
subject of improvements that can change the program in such a way that some
scripts that work with MIDA 0.1.0 will not work with improved versions. I
will try to keep the differences as minimal as possible.

My final goal is to create software tools that can be used to bring
generative music to producers in such a way that does not change the
established workflow.

Before starting work on MIDA I looked at some well-known tools to program
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

Having all these tools is enough to create something fresh in context of
contemporary popular music. One can use familiar plugins, synths, etc. but
get totally different effects.

It's important that these tools must be free software, so people can modify
them to suit their needs. Proprietary programs should not be used as they
constrain your freedom from practical and ethical point of view.

## License

Copyright (c) 2014 Mark Karpov

Distributed under GNU GPL.

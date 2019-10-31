# modus

Text is the format for life to store our notes and information. Modus provides
the functionality needed to process and view them in structured manner. Keep
your data as plain text files, without compromising on functionality.

## Architecture

This is a single-page web app written in Haskell using Obelisk (Reflex FRP).

A text file may be of different formats, ranging from good ol' markdown to any
arbitrary structured format. You can use, for example, YAML to record some data,
or design your own format for it. In Modus there will be a parser for each of
these types, and a renderer that displays them on the frontend. Writing and
editing content, however, is outside the scope of moduls; you are adviced to use
your own text editor for that.

## How to run modus locally

1. [Install obelisk](https://github.com/obsidiansystems/obelisk#installing-obelisk)
2. Run `ob run`

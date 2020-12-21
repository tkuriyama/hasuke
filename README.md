# hasuke

Some Ukulele tools in Haskell.

<hr>

## Motivation

Original problem: internet tabs tend to not print very well!

`hasuke` defines a pure Haskell representation of Ukulele scores (as tabs and/or chords and lyrics). For simplicity, `aeson` is used to auto-derive JSON for de/serialization of the Haskell representation. There can then be a pipeline such as the below (flowing from left to right):

| Sources     | Json         | Haskell | Output     |
| ---         | ---          | ---     | ---        |
| Internet    | Parsed       |         | HTML       |
| Inspiration | Hand-written |         | Midi / Mp3 |
|             |              |         | MusicXML   |
|             |              |         | etc        |


See the [project tracker](https://github.com/tkuriyama/hasuke/projects/1) for current features and backlog.

## Examples

See the [scores](https://github.com/tkuriyama/hasuke/tree/master/scores) directory for some sample scores and generated outputs. 


## Build

`stack install`, or follow the build pipeline in `all.do` ([`redo`](https://redo.readthedocs.io/en/latest/) is not required, but if it is installed, the full build pipeline can be fun by the command `redo all`).


## Dependencies

A noteworthy dependency for generating Midi files is the [Euterpea](http://www.euterpea.com/) library for defining music in Haskell.

* [official docs](http://www.euterpea.com/)
* [some installation and usage examples]((https://github.com/tkuriyama/hsom)


As of this writing, Euterpea does not officially support installation via Stack. See the `stack.yaml` for a configuration that currently works on Linux (Ubuntu). The configuration also works on MacOS Catalina, and should work on Big Sur eventually -- again at the time of this writing in late 2020, Haskell's interface to the Core Midi on Big Sur appears to be broken.

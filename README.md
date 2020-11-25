# hasuke

Some Ukulele tools in Haskell.

<hr>

## Motivations

Original problem: internet tabs tend to not print very well!

`hasuke` defines a pure Haskell representation of Ukulele scores (as tabs and/or chords and lyrics). For simplicity, `aeson` is used to auto-derive JSON for de/serialization of the Haskell representation. There can then be a pipeline such as the below (flowing from left to right):

| Sources     | Json         | Haskell | Output     |
| ---         | ---          | ---     | ---        |
| Internet    | Parsed       |         | HTML       |
| Inspiration | Hand-written |         | Midi / Mp3 |
|             |              |         | MusicXML   |
|             |              |         | etc        |


[Project tracker](https://github.com/tkuriyama/hasuke/projects/1).


## Build

`stack install`, or follow the build pipeline in `all.do` ([`redo`](https://redo.readthedocs.io/en/latest/) is not required, but if it is installed, the full build pipeline can be fun by the command `redo all`).


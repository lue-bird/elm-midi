## still TODO

  - implement running status (ugh)

## [elm midi](https://dark.elm.dmy.fr/packages/lue-bird/elm-midi/latest/)

a bytes parser for MIDI (`.mid`) files.

Some ideas on what you can use this for:
  - creating animations for songs
  - creating rhythm games
  - analyzing songs
  - ...

A basic example for parsing a selected file using
  - [`File.Select.file`](https://dark.elm.dmy.fr/packages/elm/file/latest/File-Select#file)
  - [`File.toBytes`](https://dark.elm.dmy.fr/packages/elm/file/latest/File#toBytes)
  - [`Bytes.Parser.run`](https://dark.elm.dmy.fr/packages/zwilias/elm-bytes-parser/latest/Bytes-Parser#run)

```elm
update event =
    case event of
        MidiFileSelectClicked ->
            \state ->
                ( state
                , File.Select.file [ "audio/midi" ] MidiFileSelected
                )

        MidiFileSelected midiFile ->
            \state ->
                ( state
                , midiFile |> File.toBytes |> Task.perform SelectedMidiFileBytesReceived
                )

        SelectedMidiFileBytesReceived midiFileBytes ->
            \state ->
                ( { state
                    | midi =
                        midiFileBytes
                            |> Bytes.Parser.run Midi.file
                  }
                , Cmd.none
                )
```
→ [complete example](https://github.com/lue-bird/elm-midi/tree/master/example)

## not supported

- encoding

## created with the help of

  - [this summary sheet](https://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html)
  - [this wonderful wiki](https://www.recordingblogs.com/wiki/musical-instrument-digital-interface-midi)
  - [this spec for all messages](http://midi.teragonaudio.com/tech/midispec.htm) and [this spec for general info about the format and some details](http://midi.teragonaudio.com/tech/midifile.htm)
  - [these (old) midi.org reference tables](https://www.midi.org/specifications-old/category/reference-tables)
  - [this blog for a few message explanations](https://web.archive.org/web/20090117232701/http://eamusic.dartmouth.edu/~wowem/hardware/midi.html)
  - [the german wikipedia page for context and all events](https://de.wikipedia.org/wiki/MIDI) (for some reason the English version omits the specific events)

module Midi exposing
    ( file, Parser
    , trackNotes
    , File, Track, Event, Message(..)
    , Key(..), Quality, KeySignature(..), N1To7(..)
    , SmpteTime, OnOrOff(..)
    , ManufacturerId(..)
    , MessageMeta(..), MessageMetaSequenceNumber(..), MessageMetaKeySignature, MessageMetaTimeSignature, MessageMetaSequencerSpecific, MessageMetaUnspecific
    , MessageSystem(..), MessageSystemRealTime(..), MessageSystemCommon(..), MessageSystemExclusive, MessageSystemSongPositionPointer, MessageSystemSongSelect, MessageSystemTimeCodeQuarterFrame
    , MessageChannel
    , MessageChannelSpecific(..), MessageChannelAftertouch, MessageNoteOff, MessageNoteOn, MessagePitchBend, MessagePolyphonicAftertouch, MessageProgramChange
    , MessageChannelControl, MessageChannelControlChange, MessageChannelMode(..), MessageChannelModeWithAllNotesOff, MonoModeOmni(..)
    )

{-| MIDI (`.mid`) file representation and parsing.


## parse

@docs file, Parser


## observe

@docs trackNotes


## representation

This representation is as little opinionated as possible, reflecting the binary decoding
as closely as possible.
For example, notes aren't represented as a List of durations, key etc. but just as on and off events
just as specified in the file.

@docs File, Track, Event, Message


## general

@docs Key, Quality, KeySignature, N1To7
@docs SmpteTime, OnOrOff
@docs ManufacturerId


## meta message

@docs MessageMeta, MessageMetaSequenceNumber, MessageMetaKeySignature, MessageMetaTimeSignature, MessageMetaSequencerSpecific, MessageMetaUnspecific


## system message

@docs MessageSystem, MessageSystemRealTime, MessageSystemCommon, MessageSystemExclusive, MessageSystemSongPositionPointer, MessageSystemSongSelect, MessageSystemTimeCodeQuarterFrame


## channel message

@docs MessageChannel
@docs MessageChannelSpecific, MessageChannelAftertouch, MessageNoteOff, MessageNoteOn, MessagePitchBend, MessagePolyphonicAftertouch, MessageProgramChange


### control

@docs MessageChannelControl, MessageChannelControlChange, MessageChannelMode, MessageChannelModeWithAllNotesOff, MonoModeOmni

-}

import Bytes exposing (Endianness(..))
import Bytes.Parser as Parser
import Duration exposing (Duration)
import Quantity
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


{-| MIDI (`.mid`) file data.
-}
type alias File =
    RecordWithoutConstructorFunction
        { format : Int
        , -- ticks per beat
          timeDivision : Int
        , tracks : List Track
        }


{-| `zwilias/elm-bytes-parser` with a context string and error string.
-}
type alias Parser parsed =
    Parser.Parser String String parsed


type alias Track =
    List Event


type alias Event =
    RecordWithoutConstructorFunction
        { durationToNextEvent : Duration
        , message : Message
        }


type Message
    = MessageChannel MessageChannel
    | MessageMeta MessageMeta
    | MessageSystem MessageSystem


type MessageSystem
    = MessageSystemCommon MessageSystemCommon
    | MessageSystemRealTime MessageSystemRealTime


type MessageSystemRealTime
    = -- when synchronization is required: sent each midi clock (24 times per quarter note)
      MessageSystemTimingClock
    | -- start playing the current sequence
      MessageSystemStart
    | -- restart at the point the sequence was stopped
      MessageSystemContinue
    | -- pause the current sequence
      MessageSystemStop
    | -- optional. After first being sent, receivers will expect Active Sensing messages at least each 300ms,
      -- or it will turn off all voices and return to normal, non-active sensing operation.
      MessageSystemActiveSensing
    | -- set all receivers in the system to power-up status
      -- in a midi file this is used as an escape hatch when introducing meta events
      MessageSystemReset
    | -- reserved for future messages
      MessageSystemRealTimeUndefined


type MessageSystemCommon
    = -- sometimes shortened to sysex or sys ex
      MessageSystemExclusive MessageSystemExclusive
    | MessageSystemTimeCodeQuarterFrame MessageSystemTimeCodeQuarterFrame
    | MessageSystemSongSelect MessageSystemSongSelect
    | MessageSystemSongPositionPointer MessageSystemSongPositionPointer
    | MessageSystemTuneRequest
    | -- reserved for future messages
      MessageSystemCommonUndefined


type alias MessageSystemTimeCodeQuarterFrame =
    RecordWithoutConstructorFunction
        { messageType : Int, values : Int }


{-| allows manufacturers to create their own proprietary messages
(such as bulk dumps, patch parameters, and other non-spec data)
and provides a mechanism for creating additional MIDI Specification messages.
-}
type alias MessageSystemExclusive =
    RecordWithoutConstructorFunction
        { manufacturerId : ManufacturerId
        , -- bulk dumps such as patch parameters and other non-spec data
          data : List Int
        }


{-| Assigned Manufacturer ID number.
If you are looking for a specific code, try sifting through [this list of ids](https://studiocode.dev/doc/midi-manufacturers/)
-}
type ManufacturerId
    = -- educational or development use only; should never appear in a commercial design
      ManufacturerIdNonCommercialUse
    | ManufacturerIdCommercialUse Int


type alias MessageSystemSongSelect =
    RecordWithoutConstructorFunction
        { index : Int }


type alias MessageSystemSongPositionPointer =
    RecordWithoutConstructorFunction
        { -- 1 MIDI beat = six MIDI clocks
          beatsSinceSongStart : Int
        }


type alias MessageChannel =
    RecordWithoutConstructorFunction
        { channel : Int, message : MessageChannelSpecific }


type MessageChannelSpecific
    = MessageNoteOn MessageNoteOn
    | MessageNoteOff MessageNoteOff
    | -- traditionally used for modulation
      MessagePolyphonicAftertouch MessagePolyphonicAftertouch
    | -- modulation wheel, volume slider, panning, etc.
      MessageChannelControl MessageChannelControl
    | -- voice memory / patch number: what timbre number to play (not the actual voice parameters)
      MessageProgramChange MessageProgramChange
    | -- Similar to PolyphonicAftertouch but the value is associated with a specific channel,
      -- not separate for each key
      MessageChannelAftertouch MessageChannelAftertouch
    | -- portamento values that will shift the pitch up or down
      MessagePitchBend MessagePitchBend


type MessageChannelMode
    = MessageChannelLocalControl OnOrOff
    | MessageChannelModeWithAllNotesOff MessageChannelModeWithAllNotesOff


{-| Start to enable or disable a mode.
-}
type OnOrOff
    = On
    | Off


type MessageChannelModeWithAllNotesOff
    = MessageChannelModeAllNotesOff
    | MessageChannelModeOmni OnOrOff
    | -- (plus poly off)
      MessageChannelModeMono MonoModeOmni
    | -- (plus mono off)
      MessageChannelModePoly


type MonoModeOmni
    = MonoModeOmniOff { channelCount : Int }
    | MonoModeOmniOn


type alias MessageNoteOn =
    RecordWithoutConstructorFunction
        { key : Key, velocity : Int }


type alias MessageNoteOff =
    RecordWithoutConstructorFunction
        { key : Key, velocity : Int }


type alias MessagePolyphonicAftertouch =
    RecordWithoutConstructorFunction
        { key : Key, pressure : Int }


type MessageChannelControl
    = MessageChannelControlChange MessageChannelControlChange
    | MessageChannelMode MessageChannelMode


type alias MessageChannelControlChange =
    RecordWithoutConstructorFunction
        { controller : Int, value : Int }


type alias MessageProgramChange =
    RecordWithoutConstructorFunction
        { newProgram : Int }


type alias MessageChannelAftertouch =
    RecordWithoutConstructorFunction
        { pressure : Int }


type alias MessagePitchBend =
    RecordWithoutConstructorFunction
        { value : Int }


type MessageMeta
    = MessageMetaSequenceNumber MessageMetaSequenceNumber
    | MessageMetaText String
    | MessageMetaCopyrightNotice String
    | MessageMetaTrackName String
    | MessageMetaInstrumentName String
    | MessageMetaLyric String
    | MessageMetaMarker String
    | MessageMetaCuePoint String
    | MessageMetaChannelPrefix Int
    | MessageMetaSetTempo { microsecondsPerQuarterNote : Int }
    | -- time at which the track chunk is supposed to start
      MessageMetaOffset SmpteTime
    | MessageMetaTimeSignature MessageMetaTimeSignature
    | MessageMetaKeySignature MessageMetaKeySignature
    | MessageMetaSequencerSpecific MessageMetaSequencerSpecific
    | MessageMetaUnspecific MessageMetaUnspecific


type alias SmpteTime =
    RecordWithoutConstructorFunction
        { hour : Int
        , minute : Int
        , second : Int
        , frame : Int
        , frame100ths : Int
        }


{-| Example

  - 6/8
  - the metronome clicks every 3 eighth-notes
  - 8 notated 32nd-notes per quarter-note

```
{ notation =
    { numerator = 6
    , denominatorPowerOf2 = 3
    , number32ndNotesPerQuarterNote = 8
    }
, clocksPerMetronomeClick = 32 -- half a bar: (3 \* 24) // 2
}
```

-}
type alias MessageMetaTimeSignature =
    RecordWithoutConstructorFunction
        { -- what's shown, for example 3/8
          notation :
            { numerator : Int
            , -- 2 for example would be /4, 3 would be /8, ...
              denominatorPowerOf2 : Int
            , number32ndNotesPerQuarterNote : Int
            }
        , -- 24 midi clocks are one quarter note
          clocksPerMetronomeClick : Int
        }


type MessageMetaSequenceNumber
    = -- use a specific given id
      MessageMetaSequenceId Int
    | -- use the index of the sequence in the file, starting with 0
      MessageMetaSequenceUseDefaultIndex


type alias MessageMetaKeySignature =
    RecordWithoutConstructorFunction
        { keySignature : KeySignature
        , quality : Quality
        }


type alias MessageMetaSequencerSpecific =
    RecordWithoutConstructorFunction
        { manufacturerId : ManufacturerId
        , data : List Int
        }


type alias MessageMetaUnspecific =
    RecordWithoutConstructorFunction
        { type_ : Int
        , data : List Int
        }


{-| [`zwilias/elm-bytes-parser`](https://dark.elm.dmy.fr/packages/zwilias/elm-bytes-parser/latest/) for midi file `Bytes`.
Run on `Bytes` using [`Bytes.Parser.run`](https://dark.elm.dmy.fr/packages/zwilias/elm-bytes-parser/latest/Bytes-Parser#run)

The advantage of using this parser instead of a `Bytes.Decode.Decoder` is that you'll get nice error messages with extra context.

-}
file : Parser File
file =
    parseMidiHeader
        |> Parser.andThen
            (\header ->
                Parser.succeed
                    (\tracks ->
                        { format = header.format
                        , timeDivision = header.timeDivision
                        , tracks = tracks
                        }
                    )
                    |> -- chunk length
                       Parser.ignore (Parser.bytes 4)
                    |> Parser.ignore tracksCode
                    |> Parser.keep
                        (Parser.repeat track header.trackCount)
            )
        |> Parser.inContext "MIDI file"


onlyCode : Int -> Parser ()
onlyCode specificCode =
    Parser.andThen
        (\code ->
            if code == specificCode then
                Parser.succeed ()

            else
                Parser.fail ("invalid code " ++ (code |> String.fromInt))
        )
        Parser.unsignedInt8
        |> Parser.inContext ("code " ++ (specificCode |> String.fromInt))


tracksCode : Parser ()
tracksCode =
    Parser.succeed ()
        -- now it's normal again
        |> Parser.ignore (onlyCode 0x4D)
        |> Parser.ignore (onlyCode 0x54)
        |> Parser.ignore (onlyCode 0x72)
        |> Parser.ignore (onlyCode 0x6B)
        |> Parser.inContext "tracks code"


parseMidiHeader :
    Parser
        { format : Int
        , trackCount : Int
        , -- ticks per beat
          timeDivision : Int
        }
parseMidiHeader =
    Parser.succeed
        (\format trackCount timeDivision ->
            { format = format
            , trackCount = trackCount
            , timeDivision = timeDivision
            }
        )
        -- |> Parser.ignore headerCode
        {-
           headerCode : Parser ()
           headerCode =
               Parser.succeed ()
                   |> Parser.ignore (onlyCode 0x4D)
                   |> Parser.ignore (onlyCode 0x54)
                   |> Parser.ignore (onlyCode 0x68)
                   |> Parser.ignore (onlyCode 0x64)
                   |> Parser.inContext "header code"
        -}
        |> -- chunk length
           Parser.ignore (Parser.bytes 4)
        |> Parser.keep fileFormat
        |> Parser.keep fileTrackCount
        |> Parser.keep fileTimeDivision
        |> Parser.inContext "header"


fileFormat : Parser Int
fileFormat =
    Parser.unsignedInt16 BE
        |> Parser.inContext "file format"


fileTrackCount : Parser Int
fileTrackCount =
    Parser.unsignedInt16 BE
        |> Parser.inContext "file track count"


fileTimeDivision : Parser Int
fileTimeDivision =
    Parser.unsignedInt16 BE
        |> Parser.inContext "time division"


intVariableByteLengthBE : Parser Int
intVariableByteLengthBE =
    Parser.andThen
        (\mostSignificantByte ->
            let
                mostSignificantBits =
                    mostSignificantByte |> remainderBy 128
            in
            if mostSignificantByte >= 128 then
                mostSignificantBits |> Parser.succeed

            else
                Parser.map
                    (\leastSignificantBits ->
                        mostSignificantBits * 128 + leastSignificantBits
                    )
                    (Parser.map (\b -> b |> remainderBy 128)
                        Parser.unsignedInt8
                    )
        )
        Parser.unsignedInt8


eventDurationToNextEvent : Parser Duration
eventDurationToNextEvent =
    Parser.map (\millis -> Duration.milliseconds (millis |> toFloat))
        intVariableByteLengthBE
        |> Parser.inContext "event delta time"


track : Parser Track
track =
    Parser.loop
        (\eventStackSoFar ->
            Parser.oneOf
                [ Parser.succeed
                    (eventStackSoFar
                        |> List.reverse
                        |> Parser.Done
                    )
                    |> Parser.ignore eventDurationToNextEvent
                    |> Parser.ignore trackEndCode
                , Parser.succeed
                    (\event_ ->
                        eventStackSoFar |> (::) event_ |> Parser.Loop
                    )
                    |> Parser.keep event
                ]
        )
        []
        |> Parser.inContext "track"


trackEndCode : Parser ()
trackEndCode =
    Parser.succeed ()
        |> Parser.ignore (onlyCode 0xFF)
        |> Parser.ignore (onlyCode 0x2F)
        |> Parser.ignore (onlyCode 0x00)
        |> Parser.inContext "track end code"


event : Parser Event
event =
    Parser.succeed
        (\durationToNextEvent message_ ->
            { durationToNextEvent = durationToNextEvent, message = message_ }
        )
        |> Parser.keep eventDurationToNextEvent
        |> Parser.keep message
        |> Parser.inContext "event"


message : Parser Message
message =
    Parser.oneOf
        [ Parser.map MessageChannel messageChannel
        , Parser.map MessageMeta messageMeta
        , Parser.map MessageSystem messageSystem
        ]
        |> Parser.inContext "event message"


unsignedInt7 : Parser Int
unsignedInt7 =
    Parser.andThen
        (\unsignedInt8 ->
            if unsignedInt8 <= 127 then
                unsignedInt8 |> Parser.succeed

            else
                Parser.fail ("int " ++ (unsignedInt8 |> String.fromInt) ++ " >= 128 are invalid")
        )
        Parser.unsignedInt8


messageMeta : Parser MessageMeta
messageMeta =
    Parser.oneOf
        [ Parser.succeed MessageMetaSequenceNumber
            |> Parser.ignore (onlyCode 0x00)
            |> Parser.keep messageMetaSequenceNumber
            |> Parser.inContext "sequence number meta message"
        , Parser.succeed MessageMetaText
            |> Parser.ignore (onlyCode 0x01)
            |> Parser.keep (unsignedInt7 |> Parser.andThen Parser.string)
            |> Parser.inContext "text meta message"
        , Parser.succeed MessageMetaCopyrightNotice
            |> Parser.ignore (onlyCode 0x02)
            |> Parser.keep (unsignedInt7 |> Parser.andThen Parser.string)
            |> Parser.inContext "copyright notice meta message"
        , Parser.succeed MessageMetaTrackName
            |> Parser.ignore (onlyCode 0x03)
            |> Parser.keep (unsignedInt7 |> Parser.andThen Parser.string)
            |> Parser.inContext "track name meta message"
        , Parser.succeed MessageMetaInstrumentName
            |> Parser.ignore (onlyCode 0x04)
            |> Parser.keep (unsignedInt7 |> Parser.andThen Parser.string)
            |> Parser.inContext "instrument name meta message"
        , Parser.succeed MessageMetaLyric
            |> Parser.ignore (onlyCode 0x05)
            |> Parser.keep (unsignedInt7 |> Parser.andThen Parser.string)
            |> Parser.inContext "lyric meta message"
        , Parser.succeed MessageMetaMarker
            |> Parser.ignore (onlyCode 0x06)
            |> Parser.keep (unsignedInt7 |> Parser.andThen Parser.string)
            |> Parser.inContext "marker meta message"
        , Parser.succeed MessageMetaCuePoint
            |> Parser.ignore (onlyCode 0x07)
            |> Parser.keep (unsignedInt7 |> Parser.andThen Parser.string)
            |> Parser.inContext "cue point meta message"
        , Parser.succeed MessageMetaChannelPrefix
            |> Parser.ignore (onlyCode 0x20)
            |> Parser.ignore (onlyCode 0x01)
            |> Parser.keep Parser.unsignedInt8
            |> Parser.inContext "channel prefix meta message"
        , Parser.succeed
            (\tempoInMicrosecondsPerQuarterNote ->
                MessageMetaSetTempo { microsecondsPerQuarterNote = tempoInMicrosecondsPerQuarterNote }
            )
            |> Parser.ignore (onlyCode 51)
            |> Parser.ignore (onlyCode 0x03)
            |> Parser.keep (unsignedInt24BE |> Parser.inContext "microseconds per quarter-note")
            |> Parser.inContext "set tempo meta message"
        , Parser.succeed MessageMetaOffset
            |> Parser.ignore (onlyCode 54)
            |> Parser.ignore (onlyCode 0x05)
            |> Parser.keep smpteTime
            |> Parser.inContext "offset meta message"
        , Parser.map MessageMetaTimeSignature messageMetaTimeSignature
        , Parser.map MessageMetaKeySignature messageMetaKeySignature
        , Parser.map MessageMetaSequencerSpecific messageMetaSequencerSpecific
        , Parser.map MessageMetaUnspecific messageMetaUnspecific
        ]
        |> Parser.inContext "meta message"


smpteTime : Parser SmpteTime
smpteTime =
    Parser.succeed
        (\hour minute second frame frame100ths ->
            { hour = hour, minute = minute, second = second, frame = frame, frame100ths = frame100ths }
        )
        |> Parser.keep Parser.unsignedInt8
        |> Parser.keep Parser.unsignedInt8
        |> Parser.keep Parser.unsignedInt8
        |> Parser.keep Parser.unsignedInt8
        |> Parser.keep Parser.unsignedInt8
        |> Parser.inContext "smpte time"


messageMetaTimeSignature : Parser MessageMetaTimeSignature
messageMetaTimeSignature =
    Parser.succeed
        (\numerator denominatorPowerOf2 number32ndNotesPerQuarterNote clocksPerMetronomeClick ->
            { notation =
                { numerator = numerator
                , denominatorPowerOf2 = denominatorPowerOf2
                , number32ndNotesPerQuarterNote = number32ndNotesPerQuarterNote
                }
            , clocksPerMetronomeClick = clocksPerMetronomeClick
            }
        )
        |> Parser.ignore (onlyCode 58)
        |> Parser.ignore (onlyCode 0x04)
        |> Parser.keep Parser.unsignedInt8
        |> Parser.keep Parser.unsignedInt8
        |> Parser.keep Parser.unsignedInt8
        |> Parser.keep Parser.unsignedInt8
        |> Parser.inContext "time signature meta message"


unsignedInt24BE : Parser Int
unsignedInt24BE =
    Parser.succeed
        (\msb lsbs ->
            msb * 0x0100 + lsbs
        )
        |> Parser.keep Parser.unsignedInt8
        |> Parser.keep (Parser.unsignedInt16 BE)


messageMetaSequenceNumber : Parser MessageMetaSequenceNumber
messageMetaSequenceNumber =
    Parser.oneOf
        [ Parser.succeed MessageMetaSequenceId
            |> Parser.ignore (onlyCode 0x02)
            |> Parser.keep (Parser.unsignedInt16 BE)
        , Parser.succeed MessageMetaSequenceUseDefaultIndex
            |> Parser.ignore (onlyCode 0x00)
        ]


messageMetaKeySignature : Parser MessageMetaKeySignature
messageMetaKeySignature =
    Parser.succeed
        (\keySignature_ quality_ ->
            { keySignature = keySignature_, quality = quality_ }
        )
        |> Parser.ignore (onlyCode 0x59)
        |> Parser.ignore (onlyCode 0x02)
        |> Parser.keep keySignature
        |> Parser.keep quality
        |> Parser.inContext "key signature meta message"


quality : Parser Quality
quality =
    Parser.oneOf
        [ Parser.succeed Major
            |> Parser.ignore (onlyCode 0x00)
        , Parser.succeed Minor
            |> Parser.ignore (onlyCode 0x01)
        ]
        |> Parser.inContext "quality"


keySignature : Parser KeySignature
keySignature =
    Parser.andThen
        (\sharps ->
            case sharps |> toKeySignature of
                Nothing ->
                    Parser.fail ("Amount of sharps or flats too high: " ++ (sharps |> abs |> String.fromInt))

                Just keySignature_ ->
                    keySignature_ |> Parser.succeed
        )
        Parser.unsignedInt8


toKeySignature : Int -> Maybe KeySignature
toKeySignature value =
    if value <= -1 then
        Maybe.map Flats (value |> abs |> to1To7)

    else if value == 0 then
        CKey |> Just

    else
        Maybe.map Sharps (value |> to1To7)


to1To7 : Int -> Maybe N1To7
to1To7 =
    \value ->
        case value of
            1 ->
                N1 |> Just

            2 ->
                N2 |> Just

            3 ->
                N3 |> Just

            4 ->
                N4 |> Just

            5 ->
                N5 |> Just

            6 ->
                N6 |> Just

            7 ->
                N7 |> Just

            _ ->
                Nothing


variableLengthBytes : Parser (List Int)
variableLengthBytes =
    Parser.andThen
        (\length ->
            Parser.repeat Parser.unsignedInt8 length
        )
        intVariableByteLengthBE


manufacturerId : Parser ManufacturerId
manufacturerId =
    Parser.oneOf
        [ Parser.succeed ManufacturerIdNonCommercialUse |> Parser.ignore (onlyCode 0x7D)
        , Parser.map ManufacturerIdCommercialUse
            (Parser.oneOf
                [ Parser.succeed identity
                    |> Parser.ignore (onlyCode 0x00)
                    |> Parser.keep (Parser.unsignedInt16 BE)
                , unsignedInt7
                ]
            )
        ]


{-| This message may be used for requirements of a particular sequencer.
-}
messageMetaSequencerSpecific : Parser MessageMetaSequencerSpecific
messageMetaSequencerSpecific =
    Parser.succeed
        (\manufacturerId_ data ->
            { manufacturerId = manufacturerId_
            , data = data
            }
        )
        |> Parser.ignore (onlyCode 0x7F)
        |> Parser.keep manufacturerId
        |> Parser.keep variableLengthBytes
        |> Parser.inContext "sequencer-specific meta-message"


messageMetaUnspecific : Parser MessageMetaUnspecific
messageMetaUnspecific =
    Parser.succeed (\type_ data -> { type_ = type_, data = data })
        |> Parser.ignore (onlyCode 0xFF)
        |> Parser.keep (unsignedInt7 |> Parser.inContext "type")
        |> Parser.keep variableLengthBytes
        |> Parser.inContext "unspecific meta message"


messageSystem : Parser MessageSystem
messageSystem =
    Parser.oneOf
        [ Parser.map MessageSystemCommon messageSystemCommon
        , Parser.map MessageSystemRealTime messageSystemRealTime
        ]
        |> Parser.inContext "system message"


messageSystemWithCode : Int -> Parser a -> Parser a
messageSystemWithCode specificCode messageInfoParser =
    Parser.unsignedInt8
        |> Parser.andThen
            (\code ->
                let
                    messageKindCode =
                        code // 16
                in
                if messageKindCode /= 0x0F then
                    Parser.fail ("Invalid code " ++ (messageKindCode |> String.fromInt))

                else
                    let
                        systemMessageKindCode =
                            code |> remainderBy 16
                    in
                    if systemMessageKindCode == specificCode then
                        messageInfoParser

                    else
                        Parser.fail
                            ("Invalid code "
                                ++ (systemMessageKindCode |> String.fromInt)
                                ++ " /= "
                                ++ (specificCode |> String.fromInt)
                            )
            )


messageSystemCommon : Parser MessageSystemCommon
messageSystemCommon =
    Parser.oneOf
        [ messageSystemWithCode 0
            (Parser.map MessageSystemExclusive messageSystemExclusive)
            |> Parser.inContext "system exclusive"
        , messageSystemWithCode 1
            (Parser.map MessageSystemTimeCodeQuarterFrame messageSystemTimeCodeQuarterFrame)
            |> Parser.inContext "time code quarter frame"
        , messageSystemWithCode 2
            (Parser.map MessageSystemSongPositionPointer messageSystemSongPositionPointer)
            |> Parser.inContext "position pointer"
        , messageSystemWithCode 3
            (Parser.map MessageSystemSongSelect messageSystemSongSelect)
            |> Parser.inContext "song select"
        , messageSystemWithCode 4
            (Parser.succeed MessageSystemCommonUndefined)
            |> Parser.inContext "undefined"
        , messageSystemWithCode 5
            (Parser.succeed MessageSystemCommonUndefined)
            |> Parser.inContext "undefined"
        , messageSystemWithCode 6
            (Parser.succeed MessageSystemTuneRequest)
            |> Parser.inContext "tune request"
        , messageSystemWithCode 7
            (Parser.fail "misplaced system exclusive end code")
        ]
        |> Parser.inContext "common"


messageSystemTimeCodeQuarterFrame : Parser MessageSystemTimeCodeQuarterFrame
messageSystemTimeCodeQuarterFrame =
    unsignedInt7
        |> Parser.map
            (\info ->
                { messageType = info // 16, values = info |> remainderBy 16 }
            )


messageSystemExclusive : Parser MessageSystemExclusive
messageSystemExclusive =
    Parser.succeed
        (\manufacturerId_ data ->
            { manufacturerId = manufacturerId_, data = data }
        )
        |> Parser.keep manufacturerId
        |> Parser.keep
            (Parser.loop
                (\soFar ->
                    Parser.oneOf
                        [ Parser.succeed (soFar |> List.reverse |> Parser.Done)
                            |> Parser.ignore (onlyCode 0xF7 |> Parser.inContext "data end code")
                        , Parser.succeed (\piece -> soFar |> (::) piece |> Parser.Loop)
                            |> Parser.keep unsignedInt7
                        ]
                )
                []
            )


messageSystemSongSelect : Parser MessageSystemSongSelect
messageSystemSongSelect =
    Parser.succeed (\index -> { index = index })
        |> Parser.keep unsignedInt7
        |> Parser.inContext "selected song"


messageSystemSongPositionPointer : Parser MessageSystemSongPositionPointer
messageSystemSongPositionPointer =
    Parser.succeed (\lsb msb -> { beatsSinceSongStart = msb * 128 + lsb })
        |> Parser.keep unsignedInt7
        |> Parser.keep unsignedInt7
        |> Parser.inContext "song position pointer"


messageSystemRealTime : Parser MessageSystemRealTime
messageSystemRealTime =
    Parser.oneOf
        [ messageSystemWithCode 8
            (Parser.succeed MessageSystemTimingClock)
            |> Parser.inContext "timing clock"
        , messageSystemWithCode 9
            (Parser.succeed MessageSystemRealTimeUndefined)
            |> Parser.inContext "undefined"
        , messageSystemWithCode 10
            (Parser.succeed MessageSystemStart)
            |> Parser.inContext "start"
        , messageSystemWithCode 11
            (Parser.succeed MessageSystemContinue)
            |> Parser.inContext "continue"
        , messageSystemWithCode 12
            (Parser.succeed MessageSystemStop)
            |> Parser.inContext "stop"
        , messageSystemWithCode 13
            (Parser.succeed MessageSystemRealTimeUndefined)
            |> Parser.inContext "undefined"
        , messageSystemWithCode 14
            (Parser.succeed MessageSystemActiveSensing)
            |> Parser.inContext "active sensing"
        , messageSystemWithCode 15
            (Parser.succeed MessageSystemReset)
            |> Parser.inContext "reset"
        ]
        |> Parser.inContext "real time"


messageChannel : Parser MessageChannel
messageChannel =
    Parser.oneOf
        [ messageWithCode ( 8, Parser.map MessageNoteOff messageNoteOff )
            |> Parser.inContext "message note off"
        , messageWithCode ( 9, Parser.map MessageNoteOn messageNoteOn )
            |> Parser.inContext "message note on"
        , messageWithCode ( 10, Parser.map MessagePolyphonicAftertouch messagePolyphonicAftertouch )
            |> Parser.inContext "message polyphonic aftertouch"
        , messageWithCode ( 11, Parser.map MessageChannelControl messageControl )
            |> Parser.inContext "message control change"
        , messageWithCode ( 12, Parser.map MessageProgramChange messageProgramChange )
            |> Parser.inContext "message program change"
        , messageWithCode ( 13, Parser.map MessageChannelAftertouch messageChannelAftertouch )
            |> Parser.inContext "message channel aftertouch"
        , messageWithCode ( 14, Parser.map MessagePitchBend messagePitchBend )
            |> Parser.inContext "message pitch bend"
        ]
        |> Parser.inContext "message to channel"


messageWithCode : ( Int, Parser MessageChannelSpecific ) -> Parser MessageChannel
messageWithCode ( code, messageParser ) =
    withChannelRequireCode code
        |> Parser.andThen
            (\channel ->
                Parser.map
                    (\messageParsed ->
                        { channel = channel
                        , message = messageParsed
                        }
                    )
                    messageParser
            )


withChannelRequireCode : Int -> Parser Int
withChannelRequireCode specificCode =
    Parser.unsignedInt8
        |> Parser.andThen
            (\codeThenChannel ->
                let
                    actualCode =
                        codeThenChannel // 16
                in
                if actualCode == specificCode then
                    codeThenChannel
                        |> remainderBy 16
                        |> Parser.succeed

                else
                    Parser.fail ("Invalid code " ++ (actualCode |> String.fromInt))
            )
        |> Parser.inContext ("code " ++ (specificCode |> String.fromInt) ++ " and channel")


messageControl : Parser MessageChannelControl
messageControl =
    Parser.oneOf
        [ Parser.map MessageChannelMode messageChannelMode
        , Parser.map MessageChannelControlChange messageControlChange
        ]


messageChannelMode : Parser MessageChannelMode
messageChannelMode =
    Parser.oneOf
        [ Parser.succeed MessageChannelLocalControl
            |> Parser.ignore (onlyCode 122)
            |> Parser.keep
                (Parser.oneOf
                    [ Parser.succeed Off |> Parser.ignore (onlyCode 0)
                    , Parser.succeed On |> Parser.ignore (onlyCode 127)
                    ]
                )
        , Parser.map MessageChannelModeWithAllNotesOff modeWithAllNotesOff
        ]


modeWithAllNotesOff : Parser MessageChannelModeWithAllNotesOff
modeWithAllNotesOff =
    Parser.oneOf
        [ Parser.succeed MessageChannelModeAllNotesOff
            |> Parser.ignore (onlyCode 123)
            |> Parser.ignore (onlyCode 0)
            |> Parser.inContext "all notes off"
        , Parser.succeed MessageChannelModeOmni
            |> Parser.keep
                (Parser.oneOf
                    [ Parser.succeed Off
                        |> Parser.ignore (onlyCode 124)
                        |> Parser.ignore (onlyCode 0)
                        |> Parser.inContext "off"
                    , Parser.succeed On
                        |> Parser.ignore (onlyCode 125)
                        |> Parser.ignore (onlyCode 0)
                        |> Parser.inContext "on"
                    ]
                )
            |> Parser.inContext "omni mode"
        , Parser.succeed MessageChannelModeMono
            |> Parser.ignore (onlyCode 126)
            |> Parser.keep
                (Parser.oneOf
                    [ Parser.succeed MonoModeOmniOn
                        |> Parser.ignore (onlyCode 0)
                        |> Parser.inContext "omni on"
                    , Parser.succeed
                        (\channelCount ->
                            MonoModeOmniOff { channelCount = channelCount }
                        )
                        |> Parser.keep unsignedInt7
                        |> Parser.inContext "omni off"
                    ]
                )
            |> Parser.inContext "mono mode"
        , Parser.succeed MessageChannelModePoly
            |> Parser.ignore (onlyCode 127)
            |> Parser.ignore (onlyCode 0)
            |> Parser.inContext "poly mode"
        ]


messageControlChange : Parser MessageChannelControlChange
messageControlChange =
    Parser.map2
        (\controller value ->
            { controller = controller, value = value }
        )
        messageControlChangeController
        messageControlChangeValue


messageControlChangeValue : Parser Int
messageControlChangeValue =
    unsignedInt7
        |> Parser.inContext "value"


messageControlChangeController : Parser Int
messageControlChangeController =
    unsignedInt7
        |> Parser.inContext "controller"


messageProgramChange : Parser MessageProgramChange
messageProgramChange =
    Parser.map
        (\program ->
            { newProgram = program }
        )
        programChangeProgram


programChangeProgram : Parser Int
programChangeProgram =
    unsignedInt7
        |> Parser.inContext "program"


messagePitchBend : Parser MessagePitchBend
messagePitchBend =
    Parser.succeed (\value -> { value = value })
        |> Parser.keep messagePitchBendValue


messagePitchBendValue : Parser Int
messagePitchBendValue =
    unsignedInt7
        |> Parser.inContext "value"


pressure : Parser Int
pressure =
    unsignedInt7
        |> Parser.inContext "pressure amount"


decodeNote : Parser Key
decodeNote =
    Parser.andThen
        (\byte ->
            noteMap
                -- find map the matching note
                |> List.foldl
                    (\( noteCode, note ) soFar ->
                        case soFar of
                            Just alreadyFound ->
                                alreadyFound |> Just

                            Nothing ->
                                if noteCode == byte then
                                    note |> Just

                                else
                                    Nothing
                    )
                    Nothing
                |> Maybe.map Parser.succeed
                |> Maybe.withDefault (Parser.fail "Invalid note code")
        )
        unsignedInt7
        |> Parser.inContext "note"


noteMap : List ( Int, Key )
noteMap =
    [ ( 35, B1 ) --	Acoustic Bass Drum
    , ( 59, B3 ) --	Ride Cymbal 2
    , ( 36, C2 ) --	Bass Drum 1
    , ( 60, C4 ) --	Hi Bongo
    , ( 37, CSharp2 ) --	Side Stick
    , ( 61, CSharp4 ) --	Low Bongo
    , ( 38, D2 ) --	Acoustic Snare
    , ( 62, D4 ) --	Mute Hi Conga
    , ( 39, DSharp2 ) --	Hand Clap
    , ( 63, DSharp4 ) --	Open Hi Conga
    , ( 40, E2 ) --	Electric Snare
    , ( 64, E4 ) --	Low Conga
    , ( 41, F2 ) --	Low Floor Tom
    , ( 65, F4 ) --	High Timbale
    , ( 42, FSharp2 ) --	Closed Hi Hat
    , ( 66, FSharp4 ) --	Low Timbale
    , ( 43, G2 ) --	High Floor Tom
    , ( 67, G4 ) --	High Agogo
    , ( 44, GSharp2 ) --	Pedal Hi-Hat
    , ( 68, GSharp4 ) --	Low Agogo
    , ( 45, A2 ) --	Low Tom
    , ( 69, A4 ) --	Cabasa
    , ( 46, ASharp2 ) --	Open Hi-Hat
    , ( 70, ASharp4 ) --	Maracas
    , ( 47, B2 ) --	Low-Mid Tom
    , ( 71, B4 ) --	Short Whistle
    , ( 48, C3 ) --	Hi Mid Tom
    , ( 72, C5 ) --	Long Whistle
    , ( 49, CSharp3 ) --	Crash Cymbal 1
    , ( 73, CSharp5 ) --	Short Guiro
    , ( 50, D3 ) --	High Tom
    , ( 74, D5 ) --	Long Guiro
    , ( 51, DSharp3 ) --	Ride Cymbal 1
    , ( 75, DSharp5 ) --	Claves
    , ( 52, E3 ) --	Chinese Cymbal
    , ( 76, E5 ) --	Hi Wood Block
    , ( 53, F3 ) --	Ride Bell
    , ( 77, F5 ) --	Low Wood Block
    , ( 54, FSharp3 ) --	Tambourine
    , ( 78, FSharp5 ) --	Mute Cuica
    , ( 55, G3 ) --	Splash Cymbal
    , ( 79, G5 ) --	Open Cuica
    , ( 56, GSharp3 ) --	Cowbell
    , ( 80, GSharp5 ) --	Mute Triangle
    , ( 57, A3 ) --	Crash Cymbal 2
    , ( 81, A5 ) --	Open Triangle
    , ( 58, ASharp3 ) --	Vibraslap
    ]


messagePolyphonicAftertouch : Parser MessagePolyphonicAftertouch
messagePolyphonicAftertouch =
    Parser.succeed
        (\key pressure_ ->
            { key = key, pressure = pressure_ }
        )
        |> Parser.keep decodeNote
        |> Parser.keep pressure
        |> Parser.inContext "message polyphonic aftertouch"


messageChannelAftertouch : Parser MessageChannelAftertouch
messageChannelAftertouch =
    Parser.succeed
        (\pressure_ -> { pressure = pressure_ })
        |> Parser.keep pressure
        |> Parser.inContext "message channel aftertouch"


noteMessage : Parser { key : Key, velocity : Int }
noteMessage =
    Parser.succeed
        (\key velocity ->
            { key = key, velocity = velocity }
        )
        |> Parser.keep decodeNote
        |> Parser.keep noteVelocity
        |> Parser.inContext "note message"


noteVelocity : Parser Int
noteVelocity =
    unsignedInt7
        |> Parser.inContext "note velocity"


messageNoteOff : Parser MessageNoteOff
messageNoteOff =
    noteMessage


messageNoteOn : Parser MessageNoteOff
messageNoteOn =
    noteMessage


{-| major or minor?
-}
type Quality
    = Major
    | Minor


{-| A set of ♭ or ♯ symbols at the beginning of a sequence, see for example

  - [scales with sharp key signatures](https://en.wikipedia.org/wiki/Key_signature#Scales_with_sharp_key_signatures)
  - [scales with flat key signatures](https://en.wikipedia.org/wiki/Key_signature#Scales_with_flat_key_signatures)

-}
type KeySignature
    = -- ♭ until ♭♭♭♭♭♭♭
      Flats N1To7
    | CKey
    | -- ♯ until ♯♯♯♯♯♯
      Sharps N1To7


{-| How many of them in the interval 1-7
-}
type N1To7
    = N1
    | N2
    | N3
    | N4
    | N5
    | N6
    | N7


{-| All available 12 tone equal temperament note pitches.
-}
type Key
    = B1
    | B3
    | C2
    | C4
    | CSharp2
    | CSharp4
    | D2
    | D4
    | DSharp2
    | DSharp4
    | E2
    | E4
    | F2
    | F4
    | FSharp2
    | FSharp4
    | G2
    | G4
    | GSharp2
    | GSharp4
    | A2
    | A4
    | ASharp2
    | ASharp4
    | B2
    | B4
    | C3
    | C5
    | CSharp3
    | CSharp5
    | D3
    | D5
    | DSharp3
    | DSharp5
    | E3
    | E5
    | F3
    | F5
    | FSharp3
    | FSharp5
    | G3
    | G5
    | GSharp3
    | GSharp5
    | A3
    | A5
    | ASharp3


{-| Collect all notes in a [track](#Track). Doesn't store absolute start times.
-}
trackNotes : Track -> List { key : Key, velocity : Int, duration : Duration }
trackNotes =
    \track_ ->
        let
            notesFolded :
                { sinceLastNoteOn : Maybe Duration
                , noteStack : List { key : Key, velocity : Int, duration : Duration }
                }
            notesFolded =
                track_
                    |> List.foldl
                        (\event_ soFar ->
                            case soFar.sinceLastNoteOn of
                                Nothing ->
                                    case event_.message of
                                        MessageChannel toChannel ->
                                            case toChannel.message of
                                                MessageNoteOn _ ->
                                                    { soFar | sinceLastNoteOn = event_.durationToNextEvent |> Just }

                                                _ ->
                                                    soFar

                                        _ ->
                                            soFar

                                Just durationSinceLastNoteOn ->
                                    let
                                        withAddedDelta =
                                            { soFar
                                                | sinceLastNoteOn =
                                                    durationSinceLastNoteOn |> Quantity.plus event_.durationToNextEvent |> Just
                                            }
                                    in
                                    case event_.message of
                                        MessageChannel toChannel ->
                                            case toChannel.message of
                                                MessageNoteOff noteOff ->
                                                    { noteStack =
                                                        soFar.noteStack
                                                            |> (::)
                                                                { duration = durationSinceLastNoteOn
                                                                , velocity = noteOff.velocity
                                                                , key = noteOff.key
                                                                }
                                                    , sinceLastNoteOn = Nothing
                                                    }

                                                _ ->
                                                    withAddedDelta

                                        _ ->
                                            withAddedDelta
                        )
                        { sinceLastNoteOn = Nothing, noteStack = [] }
        in
        notesFolded.noteStack |> List.reverse

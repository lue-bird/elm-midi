module Midi exposing
    ( file, Parser
    , trackNotes
    , File, Track, Event, Message(..), MessageMeta
    , ChannelMessage(..), MessageChannelAftertouch, MessageControlChange, MessageNoteOff, MessageNoteOn, Note(..), MessagePitchBend, MessagePolyphonicAftertouch, MessageProgramChange
    )

{-| MIDI file representation and parsing.
Does not support encoding and real-time system events

Created with the help of

  - [this summary sheet](https://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html)
  - [these (old) midi.org reference tables](https://www.midi.org/specifications-old/category/reference-tables)
  - [this blog for a few message explanations](https://web.archive.org/web/20090117232701/http://eamusic.dartmouth.edu/~wowem/hardware/midi.html)


## parse

@docs file, Parser


## observe

@docs trackNotes


## representation

@docs File, Track, Event, Message, MessageMeta


### channel message

@docs ChannelMessage, MessageChannelAftertouch, MessageControlChange, MessageNoteOff, MessageNoteOn, Note, MessagePitchBend, MessagePolyphonicAftertouch, MessageProgramChange

-}

import Bytes exposing (Endianness(..))
import Bytes.Parser as Parser
import Duration exposing (Duration)
import Quantity
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


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
    { eventQueue : List Event
    , durationToNextEventToEnd : Duration
    }


type alias Event =
    RecordWithoutConstructorFunction
        { durationToNextEvent : Duration, message : Message }


type Message
    = MessageToChannel MessageToChannel
    | MessageMeta MessageMeta
    | MessageSystemCommon MessageSystemCommon


type MessageSystemCommon
    = -- sometimes shortened to sysex or sys ex
      MessageSystemExclusive MessageSystemExclusive
    | MessageSystemTimeCodeQuarterFrame MessageSystemTimeCodeQuarterFrame
    | MessageSystemSongSelect MessageSystemSongSelect
    | MessageSystemSongPositionPointer MessageSystemSongPositionPointer
    | MessageSystemTuneRequest
    | MessageSystemTimingClock
    | MessageSystemStart
    | MessageSystemContinue
    | MessageSystemStop
    | MessageSystemActiveSensing
    | MessageSystemReset
    | -- reserved for future messages
      MessageSystemUndefined


type alias MessageSystemTimeCodeQuarterFrame =
    RecordWithoutConstructorFunction
        { messageType : Int, values : Int }


{-| allows manufacturers to create their own proprietary messages
(such as bulk dumps, patch parameters, and other non-spec data)
and provides a mechanism for creating additional MIDI Specification messages.
-}
type alias MessageSystemExclusive =
    RecordWithoutConstructorFunction
        { manufacturerId : Int
        , -- bulk dumps such as patch parameters and other non-spec data
          data : List Int
        }


type alias MessageSystemSongSelect =
    RecordWithoutConstructorFunction
        { index : Int }


type alias MessageSystemSongPositionPointer =
    RecordWithoutConstructorFunction
        { -- 1 MIDI beat = six MIDI clocks
          beatsSinceSongStart : Int
        }


type alias MessageToChannel =
    RecordWithoutConstructorFunction
        { channel : Int, message : ChannelMessage }


type ChannelMessage
    = MessageNoteOn MessageNoteOn
    | MessageNoteOff MessageNoteOff
    | -- traditionally used for modulation
      MessagePolyphonicAftertouch MessagePolyphonicAftertouch
    | -- modulation wheel, volume slider, panning, etc.
      MessageControlChange MessageControlChange
    | -- voice memory number: what timbre number to play (not the actual voice parameters)
      MessageProgramChange MessageProgramChange
    | -- Similar to PolyphonicAftertouch but the value is associated with a specific channel,
      -- not separate for each key
      MessageChannelAftertouch MessageChannelAftertouch
    | -- portamento values that will shift the pitch up or down
      MessagePitchBend MessagePitchBend


type alias MessageNoteOn =
    RecordWithoutConstructorFunction
        { note : Note, velocity : Int }


type alias MessageNoteOff =
    RecordWithoutConstructorFunction
        { note : Note, velocity : Int }


type alias MessagePolyphonicAftertouch =
    RecordWithoutConstructorFunction
        { note : Note, pressure : Int }


type alias MessageControlChange =
    RecordWithoutConstructorFunction
        { controller : Int, value : Int }


type alias MessageProgramChange =
    RecordWithoutConstructorFunction
        { program : Int }


type alias MessageChannelAftertouch =
    RecordWithoutConstructorFunction
        { pressure : Int }


type alias MessagePitchBend =
    RecordWithoutConstructorFunction
        { value : Int }


type alias MessageMeta =
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
                    (\durationToNextEventToEnd ->
                        { durationToNextEventToEnd = durationToNextEventToEnd
                        , eventQueue = eventStackSoFar |> List.reverse
                        }
                            |> Parser.Done
                    )
                    |> Parser.keep eventDurationToNextEvent
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


message : Parser Message
message =
    Parser.oneOf
        [ Parser.map MessageToChannel messageToChannel
        , Parser.map MessageMeta
            (Parser.succeed (\type_ data -> { type_ = type_, data = data })
                |> Parser.ignore (onlyCode 0xFF)
                |> Parser.keep (unsignedInt7 |> Parser.inContext "type")
                |> Parser.keep variableLengthBytes
            )
        , Parser.map MessageSystemCommon messageSystemCommon
        ]
        |> Parser.inContext "event message"


messageSystemCommon : Parser MessageSystemCommon
messageSystemCommon =
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
                    case systemMessageKindCode of
                        0 ->
                            Parser.map MessageSystemExclusive messageSystemExclusive

                        1 ->
                            Parser.map MessageSystemTimeCodeQuarterFrame messageSystemTimeCodeQuarterFrame

                        2 ->
                            Parser.map MessageSystemSongPositionPointer messageSystemSongPositionPointer

                        3 ->
                            Parser.map MessageSystemSongSelect messageSystemSongSelect

                        4 ->
                            Parser.succeed MessageSystemUndefined

                        5 ->
                            Parser.succeed MessageSystemUndefined

                        6 ->
                            Parser.succeed MessageSystemTuneRequest

                        -- 7 is misplaced system exclusive end code
                        8 ->
                            Parser.succeed MessageSystemTimingClock

                        9 ->
                            Parser.succeed MessageSystemUndefined

                        10 ->
                            Parser.succeed MessageSystemStart

                        11 ->
                            Parser.succeed MessageSystemContinue

                        12 ->
                            Parser.succeed MessageSystemStop

                        13 ->
                            Parser.succeed MessageSystemUndefined

                        14 ->
                            Parser.succeed MessageSystemActiveSensing

                        15 ->
                            Parser.succeed MessageSystemReset

                        _ ->
                            Parser.fail ("Unexpected placement of system exclusive message code " ++ (systemMessageKindCode |> String.fromInt))
            )


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
        (\manufacturerId data ->
            { manufacturerId = manufacturerId, data = data }
        )
        |> Parser.keep unsignedInt7
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


messageToChannel : Parser MessageToChannel
messageToChannel =
    Parser.oneOf
        [ messageWithCode ( 8, Parser.map MessageNoteOff messageNoteOff )
            |> Parser.inContext "message note off"
        , messageWithCode ( 9, Parser.map MessageNoteOn messageNoteOn )
            |> Parser.inContext "message note on"
        , messageWithCode ( 10, Parser.map MessagePolyphonicAftertouch messagePolyphonicAftertouch )
            |> Parser.inContext "message polyphonic aftertouch"
        , messageWithCode ( 11, Parser.map MessageControlChange messageControlChange )
            |> Parser.inContext "message control change"
        , messageWithCode ( 12, Parser.map MessageProgramChange messageProgramChange )
            |> Parser.inContext "message program change"
        , messageWithCode ( 13, Parser.map MessageChannelAftertouch messageChannelAftertouch )
            |> Parser.inContext "message channel aftertouch"
        , messageWithCode ( 14, Parser.map MessagePitchBend messagePitchBend )
            |> Parser.inContext "message pitch bend"
        ]
        |> Parser.inContext "message to channel"


messageWithCode : ( Int, Parser ChannelMessage ) -> Parser MessageToChannel
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


messageValue : Parser Int
messageValue =
    unsignedInt7
        |> Parser.inContext "value"


messageControlChange : Parser MessageControlChange
messageControlChange =
    Parser.map2
        (\controller value ->
            { controller = controller, value = value }
        )
        messageControlChangeController
        messageValue


messageControlChangeController : Parser Int
messageControlChangeController =
    unsignedInt7
        |> Parser.inContext "controller"


messageProgramChange : Parser MessageProgramChange
messageProgramChange =
    Parser.map
        (\program ->
            { program = program }
        )
        programChangeProgram


programChangeProgram : Parser Int
programChangeProgram =
    unsignedInt7
        |> Parser.inContext "program"


messagePitchBend : Parser MessagePitchBend
messagePitchBend =
    Parser.succeed
        (\value -> { value = value })
        |> Parser.keep messageValue


pressure : Parser Int
pressure =
    unsignedInt7
        |> Parser.inContext "pressure amount"


decodeNote : Parser Note
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


noteMap : List ( Int, Note )
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
        (\note pressure_ ->
            { note = note, pressure = pressure_ }
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


noteMessage : Parser { note : Note, velocity : Int }
noteMessage =
    Parser.succeed
        (\note velocity ->
            { note = note, velocity = velocity }
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


variableLengthBytes : Parser (List Int)
variableLengthBytes =
    Parser.andThen
        (\length ->
            Parser.repeat Parser.unsignedInt8 length
        )
        intVariableByteLengthBE


type Note
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


trackNotes : Track -> List { note : Note, velocity : Int, duration : Duration }
trackNotes =
    \track_ ->
        let
            notesFolded :
                { sinceLastNoteOn : Maybe Duration
                , noteStack : List { note : Note, velocity : Int, duration : Duration }
                }
            notesFolded =
                track_.eventQueue
                    |> List.foldl
                        (\event_ soFar ->
                            case soFar.sinceLastNoteOn of
                                Nothing ->
                                    case event_.message of
                                        MessageToChannel toChannel ->
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
                                        MessageToChannel toChannel ->
                                            case toChannel.message of
                                                MessageNoteOff noteOff ->
                                                    { noteStack =
                                                        soFar.noteStack
                                                            |> (::)
                                                                { duration = durationSinceLastNoteOn
                                                                , velocity = noteOff.velocity
                                                                , note = noteOff.note
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

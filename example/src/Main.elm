module Main exposing (main)

import Browser
import Bytes
import Bytes.Parser
import Element as Ui
import Element.Input as UiInput
import File
import File.Select
import Html.Attributes
import Midi
import Task


type alias State =
    { midi : Result String Midi.File }


type Event
    = MidiFileSelectClicked
    | MidiFileSelected File.File
    | SelectedMidiFileBytesReceived Bytes.Bytes


main : Program () State Event
main =
    Browser.element
        { init = \() -> init
        , update = reactTo
        , subscriptions = \_ -> Sub.none
        , view =
            \state ->
                state
                    |> ui
                    |> Ui.layout []
        }


init : ( State, Cmd event_ )
init =
    ( { midi = Err "no midi file selected, yet" }
    , Cmd.none
    )


reactTo : Event -> (State -> ( State, Cmd Event ))
reactTo event =
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
                            |> Result.mapError Debug.toString
                  }
                , Cmd.none
                )


ui : State -> Ui.Element Event
ui =
    \state ->
        Ui.column [ Ui.centerX, Ui.centerY ]
            [ UiInput.button
                [ Ui.centerX, Ui.padding 24 ]
                { label = Ui.text "select midi file"
                , onPress = MidiFileSelectClicked |> Just
                }
            , case state.midi of
                Err error ->
                    Ui.paragraph []
                        [ Ui.text
                            (error
                                |> String.replace "Custom { at = " "{ @"
                                |> String.replace "InContext { label = " "{ "
                                |> String.replace "BadOneOf" "?"
                            )
                        ]

                Ok midiFile ->
                    Ui.paragraph
                        [ Ui.width (Ui.px 600)
                        ]
                        [ Ui.text (midiFile |> Debug.toString) ]
            ]

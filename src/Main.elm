module Main exposing (main)

import Browser
import Element exposing (Element, alignTop, column, fill, height, padding, paragraph, px, row, spacing, text, textColumn, width, wrappedRow)
import Element.Input as Input
import List.Extra


type alias Model =
    List (List ( String, String ))


type alias Msg =
    Model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view =
            \model ->
                Element.layout
                    [ width fill
                    , height fill
                    , padding 8
                    ]
                    (view model)
        , update = update
        }


init : Model
init =
    []


view : Model -> Element Msg
view model =
    row
        [ spacing 8
        , width fill
        , height fill
        ]
        [ viewLines model
        , modelToString model
            |> String.split "\n"
            |> List.map (\line -> paragraph [] [ text line ])
            |> textColumn [ width fill, alignTop ]
        ]


viewLines : Model -> Element Msg
viewLines model =
    (model ++ [ [] ])
        |> List.indexedMap
            (\i line ->
                viewLine line
                    |> Element.map
                        (\newLine ->
                            if i == List.length model then
                                model ++ [ newLine ]

                            else
                                List.Extra.setAt i newLine model
                        )
            )
        |> column
            [ spacing 8
            , width fill
            , alignTop
            ]


viewLine : List ( String, String ) -> Element (List ( String, String ))
viewLine chunks =
    (chunks ++ [ ( "", "" ) ])
        |> List.indexedMap
            (\i chunk ->
                viewChunk chunk
                    |> Element.map
                        (\newChunk ->
                            if i == List.length chunks then
                                chunks ++ [ newChunk ]

                            else
                                List.Extra.setAt i newChunk chunks
                        )
            )
        |> wrappedRow
            [ spacing 8
            , width fill
            ]


viewChunk : ( String, String ) -> Element ( String, String )
viewChunk ( text, ruby ) =
    let
        w : Element.Length
        w =
            if String.isEmpty text && String.isEmpty ruby then
                px 40

            else
                fill
    in
    column [ spacing 4, width w ]
        [ Input.text [ width w ]
            { label = Input.labelHidden ""
            , text = ruby
            , onChange = \newRuby -> ( text, newRuby )
            , placeholder = Nothing
            }
        , Input.text [ width w ]
            { label = Input.labelHidden ""
            , text = text
            , onChange = \newText -> ( newText, ruby )
            , placeholder = Nothing
            }
        ]


modelToString : Model -> String
modelToString lines =
    lines
        |> List.map lineToString
        |> String.join "\n"


lineToString : List ( String, String ) -> String
lineToString chunks =
    let
        middle : String
        middle =
            chunks
                |> List.map chunkToString
                |> String.join " "
    in
    "<p>" ++ middle ++ "</p>"


chunkToString : ( String, String ) -> String
chunkToString ( text, ruby ) =
    if String.isEmpty ruby then
        text

    else
        "<ruby>" ++ text ++ "<rp>(</rp><rt>" ++ ruby ++ "</rt><rp>)</rp></ruby>"


update : Msg -> Model -> Model
update msg _ =
    msg
        |> List.map (List.Extra.removeWhen (\( a, b ) -> String.isEmpty a && String.isEmpty b))
        |> List.Extra.removeWhen List.isEmpty

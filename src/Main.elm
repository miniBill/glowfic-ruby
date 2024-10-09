module Main exposing (main)

import Browser
import Element exposing (Element, alignBottom, alignTop, column, el, fill, fillPortion, height, padding, paragraph, px, row, spacing, text, textColumn, width, wrappedRow)
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Parser
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
    let
        stringVersion : String
        stringVersion =
            modelToString model
    in
    column
        [ spacing 8
        , width fill
        , height fill
        ]
        [ row
            [ spacing 8
            , width fill
            , height fill
            ]
            [ viewLines model
            , stringVersion
                |> String.split "\n"
                |> List.map (\line -> paragraph [] [ text line ])
                |> textColumn
                    [ width fill
                    , alignTop
                    , Font.size 14
                    ]
            ]
        , viewPreview stringVersion
        ]


viewPreview : String -> Element Msg
viewPreview input =
    case Html.Parser.run Html.Parser.allCharRefs input of
        Ok parsed ->
            Html.Parser.nodesToHtml parsed
                |> Html.div []
                |> Element.html
                |> el [ width fill, alignBottom ]

        Err _ ->
            text "Could not parse HTML"


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
            , width (fillPortion 3)
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
            [ spacing 2
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
                Element.minimum
                    (max 50
                        (10 * max (String.length text) (String.length ruby))
                    )
                    fill
    in
    column [ spacing 2, width w ]
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
                |> String.concat
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

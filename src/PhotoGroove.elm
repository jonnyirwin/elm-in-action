module PhotoGroove exposing (main)

import Browser
import Browser.Dom exposing (Error)
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type ThumbnailSize
    = Small
    | Medium
    | Large


type Msg
    = ClickedPhoto String
    | ClickedSurpriseMe
    | SelectThumbnailSize ThumbnailSize
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loading ->
                [ text "Loading..." ]

            Loaded photos selectedUrl ->
                viewLoaded model.chosenSize photos selectedUrl

            Errored err ->
                [ text ("Error: " ++ err) ]


viewLoaded : ThumbnailSize -> List Photo -> String -> List (Html Msg)
viewLoaded chosenSize photos selectedUrl =
    [ h1 [] [ text (sizeToString chosenSize) ]
    , button [ onClick ClickedSurpriseMe ] [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ] (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img [ src (urlPrefix ++ thumb.url), classList [ ( "selected", selectedUrl == thumb.url ) ], onClick (ClickedPhoto thumb.url) ] []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label [] [ input [ type_ "radio", name "size", onClick (SelectThumbnailSize size), checked (size == chosenSize) ] [], text (sizeToString size) ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


type alias Photo =
    { url : String }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status, chosenSize : ThumbnailSize }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Small
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                _ ->
                    ( model, Cmd.none )

        SelectThumbnailSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok responseString) ->
            case String.split "," responseString of
                (firstPhoto :: _) as urls ->
                    let
                        photos =
                            List.map Photo urls
                    in
                    ( { model | status = Loaded photos firstPhoto }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Error retrieving Photos" }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        _ ->
            status

initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString GotPhotos
        }

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

module Pages.Top exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Page exposing (Page)



{-
   Topページ。ボタンを押してページを遷移するだけなので、特にアクションはない。
-}
-- MODEL


type Model
    = TopModel


init : Cmd Msg -> ( Model, Cmd Msg )
init msg =
    ( TopModel, msg )



-- UPDATE


type Msg
    = ChangePage Page


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view _ =
    div []
        [ button [ onClick <| ChangePage Page.MultiHttpRequest ] [ text "Multiple Http Request" ]
        ]

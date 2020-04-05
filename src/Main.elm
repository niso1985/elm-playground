module Main exposing (main)

import Browser
import Html exposing (Html)
import Page exposing (Page)
import Pages.MultiHttpRequest
import Pages.Top



{-
   ページルーティングのサンプル。
   URLを使わないルーティングを行う場合はこれをそのまま利用可能。
   もしページごとに値を引き回したい場合は、各ModelにGlobal的な値をもたせると良い。
-}
-- MODEL


type Model
    = TopModel Pages.Top.Model
    | MultiHttpRequestModel Pages.MultiHttpRequest.Model


init : () -> ( Model, Cmd Msg )
init _ =
    updateWith TopModel GotTopMsg (Pages.Top.init Cmd.none)



-- UPDATE


type Msg
    = ChangePage Page
    | GotTopMsg Pages.Top.Msg
    | GotMultiHttpRequestMsg Pages.MultiHttpRequest.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangePage page, _ ) ->
            changePageTo page

        ( GotTopMsg subMsg, TopModel _ ) ->
            case subMsg of
                Pages.Top.ChangePage page ->
                    changePageTo page

        ( GotMultiHttpRequestMsg subMsg, MultiHttpRequestModel subModel ) ->
            case subMsg of
                Pages.MultiHttpRequest.ChangePage page ->
                    changePageTo page

                _ ->
                    updateWith MultiHttpRequestModel GotMultiHttpRequestMsg (Pages.MultiHttpRequest.update subMsg subModel)

        ( _, _ ) ->
            ( model, Cmd.none )


changePageTo : Page -> ( Model, Cmd Msg )
changePageTo page =
    case page of
        Page.Top ->
            updateWith TopModel GotTopMsg (Pages.Top.init Cmd.none)

        Page.MultiHttpRequest ->
            updateWith MultiHttpRequestModel GotMultiHttpRequestMsg (Pages.MultiHttpRequest.init Cmd.none)


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        TopModel subModel ->
            Html.map GotTopMsg (Pages.Top.view subModel)

        MultiHttpRequestModel subModel ->
            Html.map GotMultiHttpRequestMsg (Pages.MultiHttpRequest.view subModel)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

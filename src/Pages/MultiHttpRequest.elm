module Pages.MultiHttpRequest exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Page exposing (Page)
import Task exposing (Task)



{-
   複数HTTPリクエストのサンプル
-}
-- Model


type alias Model =
    { results : List String }


init : Cmd Msg -> ( Model, Cmd Msg )
init msg =
    ( { results = [] }, msg )



-- UPDATE


type Msg
    = ChangePage Page -- TOPページに戻る用のMsg
    | RequestSequential
    | GotSequential (Result ErrorMessage (List String))
    | RequestParallel
    | GotParallel (Result ErrorMessage String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestSequential ->
            -- Task.attemptでTask.sequenceしたTaskを呼び出す
            ( { results = [] }, Task.attempt GotSequential requestSequential )

        GotSequential result ->
            -- GotSequentialは「最終的に成功したか失敗したか」の１回しか呼び出されない
            case result of
                Ok response ->
                    ( { model | results = response }, Cmd.none )

                Err e ->
                    -- とりあえず文字列にして結果の代わりに表示でもしておく
                    ( { model | results = [ e ] }, Cmd.none )

        RequestParallel ->
            ( { results = [] }, requestParallel )

        GotParallel result ->
            -- GotParallelはリクエストが発生した回数分呼ばれる。なので、今回は４回呼ばれる。
            -- 複数の結果を受け取ることになるので、結果が得られるたびにmodelへ追加している
            case result of
                Ok response ->
                    ( { model | results = response :: model.results }, Cmd.none )

                Err e ->
                    ( { model | results = e :: model.results }, Cmd.none )

        ChangePage _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ button [ onClick <| ChangePage Page.Top ] [ text "戻る" ] ]
        , button [ onClick RequestSequential ] [ text "Sequential" ]
        , button [ onClick RequestParallel ] [ text "Parallel" ]
        , div [] <| List.map (\r -> p [] [ text r ]) model.results
        ]



-- Task


requestSequential : Task ErrorMessage (List String)
requestSequential =
    let
        -- URLはほんとはUrl.fromStringとかで扱うべきだけどとりあえず
        request1 =
            requestGet "https://dog.ceo/api/breeds/image/random"

        request2 b =
            let
                _ =
                    Debug.log "Response1: " b

                -- 前のAPIのレスポンスを使って、次のAPIを呼び出すようなAPIが簡単に見つからなかったので、
                -- 以前の結果を使っているよという証明で、Debug.logを使ってconsole.logに書き出しています。
            in
            requestGet "https://data.ripple.com/v2/ledgers/"

        request3 c =
            let
                _ =
                    Debug.log "Response2: " c
            in
            requestGet "https://openlibrary.org/people/george08/lists.json"

        request4 =
            requestGet "https://binaryjazz.us/wp-json/genrenator/v1/genre/"
    in
    -- 以前のリクエストの結果を使って、次のリクエストを行うにはTask.andThenを使う
    -- Task.sequenceはList (Task a)をTask (List a)にするもの。
    -- よって、このTaskは最終的に「request3の結果とrequest4の結果」を取得することになる
    -- どこかのTaskが失敗した時点で以降の処理が中断し、エラーが返される
    Task.sequence
        [ request1
            |> Task.andThen (\r -> request2 r)
            |> Task.andThen (\r -> request3 r)
        , request4
        ]


requestParallel : Cmd Msg
requestParallel =
    let
        request1 =
            requestGet "https://dog.ceo/api/breeds/image/random"

        request2 =
            requestGet "https://data.ripple.com/v2/ledgers/"

        request3 =
            requestGet "https://openlibrary.org/people/george08/lists.json"

        request4 =
            requestGet "https://binaryjazz.us/wp-json/genrenator/v1/genre/"
    in
    -- 並列に実行する場合はCmd.batchを使う
    -- 並列に実行するので、以前のリクエストを使って次のリクエストを行うことはできない
    -- 必ず全てのリクエストの結果が必要な場合などに使うと良い
    Cmd.batch
        [ Task.attempt GotParallel <| request1
        , Task.attempt GotParallel <| request2
        , Task.attempt GotParallel <| request3
        , Task.attempt GotParallel <| request4
        ]


type alias ErrorMessage =
    String


jsonResolver : Http.Resolver ErrorMessage String
jsonResolver =
    -- いちいちHttpErrorをUpdateで処理するのは二度手間なので、Resolverで処理する
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err <| "Bad URL: " ++ url

                Http.Timeout_ ->
                    Err "Request Timeout"

                Http.NetworkError_ ->
                    Err "Network Error"

                -- BadStatusでもbodyが得られるので、レスポンスによってメッセージを分けたいのであれば、BadStatus用のDecoderを用意して、デコードしてやればよい
                Http.BadStatus_ metadata body ->
                    Err <| "BadStatus: " ++ String.fromInt metadata.statusCode ++ "\nBody: " ++ body

                Http.GoodStatus_ metadata body ->
                    -- 大抵の場合はここで以下のような形でJSONのデコードを行う
                    {-
                       case Json.Decode.decodeString decoder body of
                           Ok value ->
                               Ok value
                           Err err ->
                               Err <| "レスポンスのデコードに失敗しました。\n" ++ Json.Decode.errorToString err
                    -}
                    -- decoderはこの関数の引数で渡すと良い
                    Ok body


requestGet : String -> Task ErrorMessage String
requestGet url =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = jsonResolver
        , timeout = Nothing
        }

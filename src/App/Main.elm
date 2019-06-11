module Main exposing (main)
import Url exposing (Url)
import Browser exposing (Document)
import Browser.Navigation as Nav


import Html exposing (..)
import Page exposing (Page)
-- import Page.Home as Home
import Page.NotFound as NotFound
import Page.Settings as Settings

import Route exposing (Route)

main : Program Url Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

type Model
    = NotFound
    -- | Home Home.Model
    | Settings Settings.Model

type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    -- | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg

init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url navKey = changeRouteTo (Route.fromUrl url)
 
changeRouteTo : Maybe Route -> ( Model, Cmd Msg )
changeRouteTo maybeRoute =
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )

        Just Route.Settings -> ( NotFound, Cmd.none )
            -- Settings.init
            --     |> updateWith Settings GotSettingsMsg model

        -- Just Route.Home ->
        --     Home.init
        --         |> updateWith Home GotHomeMsg model


-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of

        NotFound ->
            Page.view Page.Other NotFound.view

        Settings settings ->
            viewPage Page.Other GotSettingsMsg (Settings.view settings)

        -- Home home ->
        --     viewPage Page.Home GotHomeMsg (Home.view home)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model, Cmd.none )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotSettingsMsg subMsg, Settings settings ) ->
            Settings.update subMsg settings
                |> updateWith Settings GotSettingsMsg model

        -- ( GotHomeMsg subMsg, Home home ) ->
        --     Home.update subMsg home
        --         |> updateWith Home GotHomeMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound ->
            Sub.none

        Settings settings ->
            Sub.map GotSettingsMsg (Settings.subscriptions settings)

        -- Home home ->
        --     Sub.map GotHomeMsg (Home.subscriptions home)



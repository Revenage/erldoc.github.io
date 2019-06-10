module Page.Settings exposing (Model, Msg, init, subscriptions, update, view)

-- import Api exposing (Cred)
-- import Api.Endpoint as Endpoint
import Browser.Navigation as Nav
import Html exposing (Html, button, div, fieldset, h1, input, li, text, textarea, ul)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
-- import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
-- import Json.Decode.Pipeline exposing (hardcoded, required)
-- import Json.Encode as Encode
import Loading
import Route
import Task



-- MODEL


type alias Model =
    { darkMode : Bool
    , status : Status
    }


-- type alias Form =
--     { avatar : String
--     , bio : String
--     , email : String
--     , username : String
--     , password : String
--     }


type Status
    = Loading
    -- | LoadingSlowly
    | Loaded
    | Failed


-- type Problem
--     = InvalidEntry ValidatedField String
--     | ServerError String


init : () -> ( Model, Cmd Msg )
init session =
    ( { 
        darkMode = False
      , status = Loading
      }
      , Cmd.none
    -- , Cmd.batch
    --     [ Api.get Endpoint.user (Session.cred session) (Decode.field "user" formDecoder)
    --         |> Http.send CompletedFormLoad
    --     , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
    --     ]
    )



view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Settings"
    , content =
        case model.status of
            Loaded ->
                text "loaded"

            Loading ->
                text ""


            Failed ->
                text "Error loading page."
                               
            -- Nothing ->
            --     text "Sign in to view your settings."
    }




-- UPDATE


type Msg = ChangeMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMode ->
            ( { model | darkMode = not model.darkMode }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

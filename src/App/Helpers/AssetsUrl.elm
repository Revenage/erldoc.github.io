module App.Helpers.AssetsUrl exposing (assetsUrl)


assetsUrl : String -> String
assetsUrl path =
    "/erldoc" ++ path

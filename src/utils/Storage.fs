module Storage

open Model

let private STORAGE_KEY = "todo-app"

let private decoder =
    Thoth.Json.Decode.Auto.generateDecoder<Model> ()

let load (): Model option =
    Browser.WebStorage.localStorage.getItem (STORAGE_KEY)
    |> unbox
    |> Option.bind
        (Thoth.Json.Decode.fromString decoder
         >> function
         | Ok r -> Some r
         | _ -> None)

let save (model: Model) =
    Browser.WebStorage.localStorage.setItem (STORAGE_KEY, Thoth.Json.Encode.Auto.toString (1, model))

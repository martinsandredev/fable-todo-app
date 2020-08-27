module ClassList

let internal classList classes =
    classes
    |> List.fold (fun complete ->
        function
        | (name, true) -> complete + " " + name
        | _ -> complete) ""
    |> Fable.React.Props.ClassName

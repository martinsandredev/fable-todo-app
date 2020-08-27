module Button

open Fable.React
open Fable.React.Props
open ClassList

type Appearance =
    | Default
    | Icon
    | Text

type ButtonProps =
    { OnClick: Browser.Types.MouseEvent -> unit
      Appearance: Appearance
      ClassName: Option<string> }

let foldNewClassName classname =
    classname
    |> Option.fold (fun _ classname -> classname) ""

let getAppearanceClassName appearance =
    match appearance with
    | Icon -> "button button--icon"
    | _ -> "button"

let Button (props: ButtonProps) children =
    button
        [ classList [ ((getAppearanceClassName props.Appearance), true)
                      (foldNewClassName props.ClassName, props.ClassName.IsSome) ]
          OnClick props.OnClick ]
        children

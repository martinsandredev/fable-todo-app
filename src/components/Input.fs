module Input

open Fable.React
open Fable.React.Props

[<Literal>]
let ENTER_KEY = 13.

let internal onEnter fn =
    function
    | (event: Browser.Types.KeyboardEvent) when event.keyCode = ENTER_KEY -> fn ()
    | _ -> ()
    |> OnKeyDown

type InputProps =
    { Value: string
      OnChange: Browser.Types.Event -> unit
      Placeholder: string
      AutoFocus: bool
      OnEnter: unit -> unit }

let Input (props: InputProps) =
    input [ ClassName "input"
            Value props.Value
            OnChange props.OnChange
            Placeholder props.Placeholder
            AutoFocus props.AutoFocus
            onEnter props.OnEnter ]

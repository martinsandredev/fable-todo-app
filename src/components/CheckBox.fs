module CheckBox

open Fable.React
open Fable.React.Props
open IconCheck
open CommonTypes
open ClassList

type CheckBoxProps =
    { Checked: bool
      OnChange: Browser.Types.Event -> unit }

let CheckBox (props: CheckBoxProps) =
    label [ classList [ ("checkbox", true)
                        ("checkbox--checked", props.Checked) ] ] [
        IconCheck { Size = Small }
        input [ Checked props.Checked
                OnChange props.OnChange
                Type "checkbox" ]
    ]

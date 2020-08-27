module IconBase

open Fable.React
open Fable.React.Props
open CommonTypes

type IconBaseProps = { Size: Size; ViewBox: string }

let IconBase (props) children =
    svg
        [ classList [ ("icon", true)
                      ("icon--middle", props.Size = Middle)
                      ("icon--large", props.Size = Large) ]
          ViewBox props.ViewBox
          PreserveAspectRatio "xMidYMid meet"
          X "0"
          Y "0" ]
        children

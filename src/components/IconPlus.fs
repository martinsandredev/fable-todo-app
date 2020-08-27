module IconPlus

open Fable.React
open Fable.React.Props
open IconBase

type IconPlusProps = { Size: CommonTypes.Size }

let IconPlus (props: IconPlusProps) =
    IconBase
        { Size = props.Size
          ViewBox = "0 0 512 512" }
        [ g [] [
            path [ D
                       "M176 448a32 32 0 0 0 32 32h32a32 32 0 0 0 32-32V304h-96zm64-416h-32a32 32 0 0 0-32 32v144h96V64a32 32 0 0 0-32-32z" ] []
            path [ D "M448 240v32a32 32 0 0 1-32 32H32a32 32 0 0 1-32-32v-32a32 32 0 0 1 32-32h384a32 32 0 0 1 32 32z" ] []
          ] ]

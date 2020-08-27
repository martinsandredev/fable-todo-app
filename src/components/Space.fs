module Space

open Fable.React
open Fable.React.Props

type SpaceProps =
    { Direction: CommonTypes.Direction
      Size: CommonTypes.Size }

let Space (props: SpaceProps) children = div [ ClassName "space" ] children

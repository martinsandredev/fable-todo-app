module Model

type Visibility =
    | All
    | Active
    | Completed

type Entry =
    { description: string
      completed: bool
      editing: bool
      id: int }

type Model =
    { entries: Entry list
      field: string
      uid: int
      visibility: Visibility }

module Level0 = Level0
module Level1 = Level1

module MakeFormat = Format.Make
module MakeBox = Box.Make

include Format.Make(Level1)

type 'a state = 'a Level0.state

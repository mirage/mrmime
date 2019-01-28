module Level0 = Level0
module Level1 = Level1

module MakeFormat = Format.Make
module MakeBox = Box.Make

include Box.Make(Level1)

type 'a state = 'a Level0.state
type 'a t = Format.t -> 'a -> Format.t state


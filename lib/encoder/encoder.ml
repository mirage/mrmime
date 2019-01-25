module Level0 = Level0
module Level1 = Level1

module MakeFormat = Format.Make
module MakeBox = Box.Make

include Box.Make(Level1)

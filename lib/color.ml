type t = string

let ( ++ ) = ( ^ )
let reset = "\027[0m"
let bold = "\027[1m"

let apply ?(apply = true) color string =
  if apply then color ^ string ^ reset else string

module FG = struct
  let black = "\027[30m"
  let red = "\027[31m"
  let green = "\027[32m"
  let yellow = "\027[33m"
  let blue = "\027[34m"
  let magenta = "\027[35m"
  let cyan = "\027[36m"
  let gray = "\027[37m"
  let bright_white = "\027[97m"
end

module BG = struct
  let black = "\027[40m"
  let red = "\027[41m"
  let green = "\027[42m"
  let yellow = "\027[43m"
  let blue = "\027[44m"
  let magenta = "\027[45m"
  let cyan = "\027[46m"
  let gray = "\027[47m"
  let bright_white = "\027[107m"
end

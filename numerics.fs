module Cpu.Numerics
    type Word = byte

    let shiftRightWord (n:int) (x:Word) : Word = x >>> n
    let shiftLeftWord (n:int) (x:Word) : Word = x <<< n
    let andWord (a:Word) (b:Word) : Word = a &&& b
    let wordToByte (x:Word) : byte = x
    let incrementWord (x:Word) : Word = x + 1uy
    let addWord (a:Word) (b:Word) : Word = a + b
    let subWord (a:Word) (b:Word) : Word = a - b
    let mulWord (a:Word) (b:Word) : Word = a * b
    let divWord (a:Word) (b:Word) : Word = a / b
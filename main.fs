module Cpu.Main
    open System
    open Cpu.Numerics

    let pcReg = 7

    type Memory = Word array
    type Registers = Word array
    type MemoryUpdate =
        { address : Word
          value : Word   }
    type Operation =
        | NoOperation
        | SetRegNum of Word * Word          // Reg[A] <- B
        | CopyRegReg of Word * Word         // Reg[A] <- Reg[B]
        | LoadRegMem of Word * Word         // Reg[A] <- Mem[B]
        | StoreMemReg of Word * Word        // Mem[A] <- Reg[B]
        | AddRegReg of Word * Word          // Reg[A] <- Reg[A] + Reg[B]
        | SubRegReg of Word * Word          // Reg[A] <- Reg[A] - Reg[B]
        | MulRegReg of Word * Word          // Reg[A] <- Reg[A] * Reg[B]
        | DivRegReg of Word * Word          // Reg[A] <- Reg[A] / Reg[B]
    type CpuUpdateFunction = Operation -> (Registers * Memory) -> (Registers * (MemoryUpdate list))
    type MemoryUpdateFunction =  MemoryUpdate list -> Memory -> Memory

    // let updateOperation (opcode:Operation) (Registers * Memory) : ((Registers * Memory) -> (Registers * (MemoryUpdate list))) =
    let decodeOpcode (word:Word) : Operation =
        /// 76543210
        /// OpcoAaBb
        let isolateOpcode word = 
            word
            |> Cpu.Numerics.shiftRightWord 4
            |> Cpu.Numerics.wordToByte

        let isolateArguments (word:Word) =
            let argumentA : Word =
                word
                |> Cpu.Numerics.andWord 12uy
                |> Cpu.Numerics.shiftRightWord 2
            let argumentB : Word =
                word
                |> Cpu.Numerics.andWord 3uy
            (argumentA, argumentB)

        let arga, argb = isolateArguments word
        //printfn "\n  DEBUG: Full Opcode   : %X" word
        //printfn "  DEBUG: Decoded Opcode: %X" (isolateOpcode word)
        //printfn "  DEBUG: Decoded Arg A : %X" (arga)
        //printfn "  DEBUG: Decoded Arg B : %X\n" (argb)

        match (isolateOpcode word) with
        | 0x00uy -> NoOperation
        | 0x01uy -> SetRegNum( isolateArguments word )          // Reg[A] <- B
        | 0x02uy -> CopyRegReg( isolateArguments word )         // Reg[A] <- Reg[B]
        | 0x03uy -> LoadRegMem( isolateArguments word )         // Reg[A] <- Mem[B]
        | 0x04uy -> StoreMemReg( isolateArguments word )        // Mem[A] <- Reg[B]
        | 0x05uy -> AddRegReg( isolateArguments word )          // Reg[A] <- Reg[A] + Reg[B]
        | 0x06uy -> SubRegReg( isolateArguments word )          // Reg[A] <- Reg[A] - Reg[B]
        | 0x07uy -> MulRegReg( isolateArguments word )          // Reg[A] <- Reg[A] * Reg[B]
        | 0x08uy -> DivRegReg( isolateArguments word )          // Reg[A] <- Reg[A] / Reg[B]
        | _   -> NoOperation



    let init () : (Registers * Memory) =
        let reg = Array.create 8 0uy
        let mem = Array.create 256 0uy
        (reg, mem)

    let getNextOpcode (reg:Registers, mem:Memory) : Word =
        let pcValue = reg.[pcReg] |> int
        let opcode = mem.[pcValue]
        // printf "OpCode @ %X: %X\t" pcReg opcode
        opcode

    let updateCpu (operation:Operation) (reg:Registers, mem:Memory) : (Registers * (MemoryUpdate list)) =
        // Increment ProgramCounter
        let mutable newReg = reg
        newReg.[pcReg] <- Cpu.Numerics.incrementWord reg.[pcReg]

        let mutable memoryUpdateList = []
        match operation with
        | NoOperation       -> ()
        | SetRegNum(a, b)   -> newReg.[int a] <- b
        | CopyRegReg(a, b)  -> newReg.[int a] <- reg.[int b]
        | LoadRegMem(a, b)  -> newReg.[int a] <- mem.[int b]
        | StoreMemReg(a, b) -> memoryUpdateList <- [ {address = reg.[int a]; value = reg.[int b]} ]
        | AddRegReg(a, b)   -> newReg.[int a] <- Cpu.Numerics.addWord reg.[int a] reg.[int b]
        | SubRegReg(a, b)   -> newReg.[int a] <- Cpu.Numerics.subWord reg.[int a] reg.[int b]
        | MulRegReg(a, b)   -> newReg.[int a] <- Cpu.Numerics.mulWord reg.[int a] reg.[int b]
        | DivRegReg(a, b)   -> newReg.[int a] <- Cpu.Numerics.divWord reg.[int a] reg.[int b]

        ( newReg, memoryUpdateList )

    let updateMemory (memory:Memory) (memUpdateList:MemoryUpdate list) : Memory =
        let mutable newMemory = memory
        memUpdateList
        |> List.iter ( fun upd -> newMemory.[int upd.address] <- upd.value )
        newMemory

    let printRegMem (reg:Registers, mem:Memory) =
        printf "MEM:\t"
        mem |> Array.iter (fun x -> printf "%X\t" x )
        printf "\tREG:\t"
        reg |> Array.iteri (fun i x -> printf "%X\t" x )
        printf "\n"

    let tick (reg:Registers, mem:Memory) : (Registers * Memory) =
        let opcode = getNextOpcode (reg, mem)
        let operation = decodeOpcode opcode
        let newReg, memUpdateList = updateCpu operation (reg, mem)
        let newMem = updateMemory mem memUpdateList
        printRegMem (newReg, newMem)  // print for diagnostics
        ( newReg, newMem )

    [<EntryPoint>]
    let main argv =
        let mem : Word array =
            [|
            0x00uy    // nop
            0x13uy    // set r0 11b
            0x15uy    // set r1 01b
            0x51uy    // add r0 r1
            0x14uy    // set r1 00b
            0x44uy    // str [r1] r0
            0x00uy
            0x00uy
            |]

        let reg : Word array = Array.create 8 0uy
        
        printRegMem (reg, mem)

        (reg, mem)
        |> tick
        |> tick
        |> tick
        |> tick        
        |> tick        
        |> tick        
        |> tick                                
        |> ignore


        0 // return an integer exit code

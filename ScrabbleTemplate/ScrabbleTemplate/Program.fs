// Learn more about F# at http://fsharp.org


let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
   
    aux >> List.rev

module TrieDictionary =
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary

    type Dict =
        | Leaf of bool //(* empty csDict)
        | Node of bool * System.Collections.Generic.Dictionary<char, Dict> 

    type characterToDictionary = System.Collections.Generic.Dictionary<char, Dict> 

    let empty () = Leaf false

    let rec insert (word: string) (trie: Dict) =
        match trie with
        | Leaf _ when word.Length = 0 -> Leaf true
        | Node (_, characterToDictionary) when word.Length = 0 -> Node(true, characterToDictionary)
        | Leaf b ->
            let tmp = characterToDictionary ()
            tmp.[word.[0]] <- insert word.[1..] (empty ())
            Node(b, tmp)
        | Node (b, nextTrie) ->
            match nextTrie.TryGetValue word.[0] with
            | (true, nextnextTrie) ->
                nextTrie.[word.[0]] <- insert word.[1..] nextnextTrie
                Node(b, nextTrie)
            | (false, _)    ->
                nextTrie.[word.[0]] <- insert word.[1..] (empty())
                Node(b, nextTrie)
    

    let rec lookup (word: string) (trie: Dict) =
        match trie with
        | Leaf b when word.Length = 0 -> b
        | Leaf _ -> false
        | Node (b, _) when word.Length = 0 -> b
        | Node (b, nextTrie) ->
            match nextTrie.TryGetValue word.[0] with
            | (true, nextnextTrie) -> lookup word.[1..] nextnextTrie
            | (false, _) -> false

    let step (c: char) (trie: Dict) =
        match trie with
        | Node (_, nextTrie) ->
            match nextTrie.TryGetValue c with
            | (true, nextnextTrie) -> 
                match nextnextTrie with
                | Leaf b ->
                    Some (b, nextnextTrie)
                | Node (b, _) ->
                    Some (b, nextnextTrie)
            | (false, _) ->
                None
        | Leaf _ -> None
    

[<EntryPoint>]
let main argv =
    ScrabbleUtil.DebugPrint.toggleDebugPrint true // Change to false to supress debug output

    System.Console.BackgroundColor <- System.ConsoleColor.White
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()

    //let board        = ScrabbleUtil.StandardBoard.standardBoard ()
    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

//    let board      = ScrabbleUtil.RandomBoard.randomBoard ()
//    let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
//    let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)

//    let board      = ScrabbleUtil.HoleBoard.holeBoard ()
//    let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words     = readLines "./Dictionaries/English.txt"

    let handSize   = 7u
    let timeout    = None
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    let dictAPI =
        // Uncomment if you have implemented a dictionary. last element None if you have not implemented a GADDAG
        Some (TrieDictionary.empty, TrieDictionary.insert, TrieDictionary.step, None) 
        //None
        
    // Uncomment this line to call your client
    
    let (dictionary, time) =
        time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)
    let players =[("FSharpScrabble", dictionary, FSharpScrabble.Scrabble.startGame);("OxyphenButazone", dictionary, Oxyphenbutazone.Scrabble.startGame)]
    //let players =[("FSharpScrabble", dictionary, FSharpScrabble.Scrabble.startGame);("f2", dictionary, FSharpScrabble.Scrabble.startGame)]
    //let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 2


    do ScrabbleServer.Comm.startGame 
          board dictionary handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore

    0

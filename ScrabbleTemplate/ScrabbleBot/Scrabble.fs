namespace FSharpScrabble

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open MultiSet
open ScrabbleUtil.Dictionary

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =
    

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()
    
    let printCharecters chars = 
        chars |> List.iter (fun c -> forcePrint (sprintf "%c" c))

    
    let printPlayernumber playerNumber = forcePrint (sprintf "You are player %d\n" playerNumber)

    let printTurn turn = forcePrint (sprintf "It is turn %d\n" turn)

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    open Eval
    open MultiSet

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerTurn   : uint32
        numPlayers    : uint32
        occupiedSquares : Map<coord, (uint32 * (char*int))> //mapping a coordinate to a tuple of (id * tile)
    }

    let mkState b d pn h t np = {board = b; dict = d;  playerNumber = pn; hand = h; playerTurn = t; numPlayers = np; 
                occupiedSquares = Map.empty<coord, uint32 * (char*int)>}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerTurn st   = st.playerTurn 
    let occupiedSquares st  = st.occupiedSquares
    // Sets occupied squares on the board when opponent/or you plays a move. 
    let updateStateOnCMPlayed (incomingMoves: list<coord * (uint32 * (char * int))>) (oldState: state) =
        let rec processMoves moves state =
            match moves with
            | [] -> state
            | (coord, (id, (char, point))) :: rest ->
            let updatedOccupiedSquares = Map.add coord (id, (char, point)) state.occupiedSquares
            processMoves rest { state with occupiedSquares = updatedOccupiedSquares }

        processMoves incomingMoves oldState

    let updatePlayerTurn (st : state) =
        if st.playerTurn = 1u then {st with playerTurn = 2u}
        else {st with playerTurn = 1u}
// When you play a word remove played tiles from hand. 
    let removeOldTiles (ms : list<coord * (uint32 * (char * int))>) (st : state) =
        let rec removeOldTiles (ms : list<coord * (uint32 * (char * int))>) (st : state) =
            match ms with
            | [] -> st
            | (coord, (id, (char, point))) :: rest -> 
                let updatedHand = MultiSet.removeSingle id st.hand
                removeOldTiles rest {st with hand = updatedHand}
        removeOldTiles ms st
    //Remove tiles from Hand when you change tiles as a move.
    let removeTilesFromHand (tiles : (uint32 * uint32) list) (st : state) =
        let rec removeTilesFromHand (tiles : (uint32 * uint32) list) (st : state) =
            match tiles with
            | [] -> st
            | (id, amount) :: rest -> 
                let updatedHand = MultiSet.remove id amount st.hand
                removeTilesFromHand rest {st with hand = updatedHand}
        removeTilesFromHand tiles st
    // When you recieve new tiles if u played change. 
    let addNewTiles (newPieces : (uint32 * uint32) list) (st : state) =
        let rec addNewTiles (newPieces : (uint32 * uint32) list) (st : state) =
            match newPieces with
            | [] -> st
            | (id, amount) :: rest -> 
                let updatedHand = MultiSet.add id amount st.hand
                addNewTiles rest {st with hand = updatedHand}
        addNewTiles newPieces st
    
    let handToCharacters2 pieces hand =
        hand 
        |> MultiSet.fold (fun acc x i -> if x <> 0u then (Map.find x pieces)::acc else acc) [] 
        |> List.collect (fun s -> Set.toList (Set.map fst s))

    let handToCharacters (pieces: Map<uint32, tile>) (hand : MultiSet.MultiSet<uint32>) =
        MultiSet.fold (fun acc x i -> (Map.find x pieces)::acc) [] hand 
        // MultiSet.fold (fun acc x i -> (Map.find x pieces)::acc) [] |> List.collect (fun s -> Set.toList (Set.map fst s))

    let emptyYourHand (hand : MultiSet.MultiSet<uint32>) (st : state) =
        let updatedHand = MultiSet.empty
        {st with hand = updatedHand} 
       

    //takes list of characters and returns a list of all possible combinations of the characters of different lengths

    let rec generateCombinations chars =
        match chars with
        | [] -> [[]]
        | c::cs ->
            let withoutC = generateCombinations cs
            let withC = List.map (fun combo -> c::combo) withoutC
            withoutC @ withC
    
    let generateAllStringsFromList (chars: char list) =
        let combinations = generateCombinations chars
        List.collect (fun combo -> generateCombinations combo) combinations
 
    //Finder 1 ord hvis det er muligt med netop disse chars    
    let checkCombinationsInDictionary dictionary chars =
        //Starts of by making all possible combinations of the chars. List of lists of chars
        generateCombinations chars
        //Maps over the list of lists of chars and makes a string of the chars in the list
        |> List.map (fun combo -> 
            let word = String.concat "" (List.map string combo)
            //returns true when word is in dictionary
            if lookup word dictionary then Some word else None)
        |> List.tryFind Option.isSome
        |> Option.flatten
    
    //Finder alle ord med de chars den får
    let FindListOfWordsInDictionary dictionary chars =
        generateCombinations chars
        |> List.choose (fun combo -> 
            let word = String.concat "" (List.map string combo)
            if lookup word dictionary then Some word else None)
    
    let findListOfWordsInDictionaryFromBoard dictionary (strings: string list)  =
        strings|> List.choose (fun combo -> if lookup combo dictionary then Some combo else None)

    let addCharToFront (charToAdd: char) (lists: char list list) =
        List.map (fun sublist -> charToAdd :: sublist) lists
    
    let makeStringFromList (chars: char list) =
        String.concat "" (List.map string chars)

    let rec generatePermutations lst =
        match lst with
        | [] -> [[]]
        | _ ->
            [ for x in lst do
                for perm in generatePermutations (List.filter ((<>) x) lst) do
                    yield x :: perm ]
    let findAllPermutationsFromHandAndBoard (charsInHand: char list) (char: char) =
            let combination = generateCombinations charsInHand
            let allPermutations = List.collect (fun x -> generatePermutations x) combination

            List.map (fun list -> char :: list) allPermutations

    let findAllWordsFromHand (charsInHand: char list) (dict: Dict)=
            let combination = generateCombinations charsInHand
            let allPermutations = List.collect (fun x -> generatePermutations x) combination

            let combsToString = List.map (fun x -> makeStringFromList x) allPermutations
            debugPrint(sprintf "combsToString %A" combsToString)

            let checkCombsInDic = findListOfWordsInDictionaryFromBoard dict combsToString   
            debugPrint(sprintf "checkCombsInDic %A" checkCombsInDic)    
            checkCombsInDic
            
  
   
    let makeListOfCharsFromString (s : string) =
        s |> Seq.toList
    
    let charToUint char = 
        if (char = '?') then 0u
        else uint32(System.Char.ToUpper(char)) - 64u

    let SetToList (map: (uint32 * Set<'c * 'd>) list) =
        map |> List.collect (fun (x, s) -> Set.toList s |> List.map (fun (c, d) -> (x, (c, d))))

    let makeCordsToWordHorizontal (list: (uint32 * (char * int)) list) (center: coord) =
        let firstCoordX = fst center in
        let firstCoordY = snd center in
        List.mapi (fun i (id, (char, point)) -> (coord(firstCoordX + i, firstCoordY), (id, (char, point)))) list 
    
    let makeCordsVertical (list: (uint32 * (char * int)) list) (center: coord) =
        let firstCoordX = fst center in
        let firstCoordY = snd center in
        List.mapi (fun i (id, (char, point)) -> (coord(firstCoordX, firstCoordY + i), (id, (char, point)))) list 
    
    let mapToListOfChars (map: Map<coord, (uint32 * (char*int))>) : char list =
        Map.fold (fun acc _  (_,(char, _)) -> char :: acc) [] map

    let addToMap (coord, value) acc =
            Map.add coord value acc

    //makes all possible words from hand and board at cords
    let getWordFromBoard (map: Map<coord, (uint32 * (char * int))>) (hand: char list) (dict: Dict) =
        Map.fold (fun acc coord (_, (char, _)) ->
            // debugPrint(sprintf "here 1")
            let combinationWithHand = findAllPermutationsFromHandAndBoard hand char
            // debugPrint(sprintf "here 2")
            let combsToString = List.map (fun x -> makeStringFromList x) combinationWithHand
            // debugPrint(sprintf "here 3")
            let checkCombsInDic = findListOfWordsInDictionaryFromBoard dict combsToString
            // debugPrint(sprintf "here 4")
            match Map.tryFind coord acc with
            | Some(existingWords) -> Map.add coord (checkCombsInDic @ existingWords) acc
            | None -> Map.add coord checkCombsInDic acc
        ) Map.empty map

    let makeCordsToWords (map: Map<coord, string list>) pieces (direction: string) =
        Map.fold (fun acc coord words ->
            List.fold(fun acc word ->
                let wordAsChars = makeListOfCharsFromString word
                let findIdToChar =  List.map (fun x -> charToUint x) wordAsChars 
                let idToPoints = List.map (fun x -> (x, Map.find x pieces)) findIdToChar
                let formatTotuple = SetToList idToPoints
                if direction = "vertical" then
                    let move = makeCordsVertical formatTotuple coord
                    acc @ [move]
                else
                    let move = makeCordsToWordHorizontal formatTotuple coord
                    acc @ [move]
            ) acc words
        ) [] map


    let rec validateRestOfWordVertical (word: (coord * (uint32 * (char * int))) list) (occupiedSquares: Map<coord, (uint32 * (char*int))>) =
        match word with
        | [] -> true // Base case: All squares in the word are valid
        | (coord, _) :: rest ->
            let leftCoord = (fst coord - 1, snd coord)
            let rightCoord = (fst coord + 1, snd coord)
            let belowCoord = (fst coord, snd coord + 1)
            match Map.tryFind coord occupiedSquares with
            | Some(_) -> false // The current square is occupied
            | None ->
                match (Map.tryFind leftCoord occupiedSquares, Map.tryFind rightCoord occupiedSquares, Map.tryFind belowCoord occupiedSquares) with
                | (Some(_), _, _) -> false // The square to the left is occupied
                | (_, Some(_), _) -> false // The square to the right is occupied
                | (_, _, Some(_)) -> false // The square below is occupied
                | _ -> validateRestOfWordVertical rest occupiedSquares // Recur with the rest of the word list
        
    let validateWordVertical (word: (coord * (uint32 * (char * int))) list) (occupiedSquares: Map<coord, (uint32 * (char*int))>) =
       match word with
        | [] -> true // Base case: All squares in the word are valid
        | (currentCoord, _) :: _ ->
            let aboveCoord = (fst currentCoord, snd currentCoord - 1)
            match Map.tryFind aboveCoord occupiedSquares with
            | Some(_) -> false // The square above is occupied
            | None -> validateRestOfWordVertical (List.tail word) occupiedSquares // Recur with the tail of the word list


    let validateWordsOnBoardVertical (words: (coord * (uint32 * (char * int))) list list) (occupiedSquares: Map<coord, (uint32 * (char*int))>) =
        List.fold (fun acc word ->
            let validateWord = validateWordVertical word occupiedSquares
            if validateWord then acc @ [word]
            else acc
        ) [] words
    
    let rec validateRestOfWordHorizontal (word: (coord * (uint32 * (char * int))) list) (occupiedSquares: Map<coord, (uint32 * (char*int))>) =
        match word with
        | [] -> true // Base case: All squares in the word are valid
        | (coord, _) :: rest ->
            let aboveCoord = (fst coord, snd coord - 1)
            let rightCoord = (fst coord + 1, snd coord)
            let belowCoord = (fst coord, snd coord + 1)
            match Map.tryFind coord occupiedSquares with
            | Some(_) -> false // The current square is occupied
            | None ->
                match (Map.tryFind aboveCoord occupiedSquares, Map.tryFind rightCoord occupiedSquares, Map.tryFind belowCoord occupiedSquares) with
                | (Some(_), _, _) -> false // The square to the left is occupied
                | (_, Some(_), _) -> false // The square to the right is occupied
                | (_, _, Some(_)) -> false // The square below is occupied
                | _ -> validateRestOfWordHorizontal rest occupiedSquares // Recur with the rest of the word list
            
    let validateWordHorizontal (word: (coord * (uint32 * (char * int))) list) (occupiedSquares: Map<coord, (uint32 * (char*int))>) =
       match word with
        | [] -> true // Base case: All squares in the word are valid
        | (currentCoord, _) :: _ ->
            let leftCoord = (fst currentCoord - 1, snd currentCoord)
            match Map.tryFind leftCoord occupiedSquares with
            | Some(_) -> false // The square above is occupied
            | None -> validateRestOfWordHorizontal (List.tail word) occupiedSquares // Recur with the tail of the word list


    let validateWordsOnBoardHorizontal (words: (coord * (uint32 * (char * int))) list list) (occupiedSquares: Map<coord, (uint32 * (char*int))>) =
        List.fold (fun acc word ->
            let validateWord = validateWordHorizontal word occupiedSquares
            if validateWord then acc @ [word]
            else acc
        ) [] words
    
    let removeFirstLetterFromWords (words: (coord * (uint32 * (char * int))) list list) =
        List.map (fun word -> List.tail word) words

    let longestFoundWord (lists:'a list list) =
        List.maxBy List.length lists

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            // debugPrint(sprintf "good 1")
            let isYourTurn = (State.playerTurn st) = (State.playerNumber st)
            let numberOfPlayers = st.numPlayers
   

            
            if isYourTurn || numberOfPlayers = 1u then 
                if st.occupiedSquares.IsEmpty then
                    // debugPrint(sprintf "good 2")
                    let characters = State.handToCharacters2 pieces (st.hand)
                    // debugPrint(sprintf "good 3")
                    let foundWords = State.findAllWordsFromHand characters st.dict
                    // debugPrint(sprintf "good 3")
                    let longestWord = foundWords |> List.maxBy (fun x -> x.Length)
                    // debugPrint(sprintf "good 4")
                    let longestWordAsChars = State.makeListOfCharsFromString longestWord
                    // debugPrint(sprintf "good 5")
                    let findIdToChar =  List.map (fun x -> State.charToUint x) longestWordAsChars
                    // debugPrint(sprintf "good 6")
                    let idToPoints = List.map (fun x -> (x, Map.find x pieces)) findIdToChar
                    // debugPrint(sprintf "good 7")
                    let formatTotuple = State.SetToList idToPoints
                    // debugPrint(sprintf "good 8")
                    if foundWords.Length > 0 then
                        let move = State.makeCordsVertical formatTotuple st.board.center
                        send cstream (SMPlay move)
                    else
                        send cstream (SMChange (toList st.hand))
                else
                    let hand = State.handToCharacters2 pieces (st.hand)
                    // debugPrint (sprintf "So far So goood 1")
                    let mapOfCordsToWords = State.getWordFromBoard st.occupiedSquares hand st.dict      
                    // debugPrint (sprintf "So far So goood 2")
                    let makeCordsForWordsVertical = State.makeCordsToWords mapOfCordsToWords pieces "vertical"
                    // debugPrint (sprintf "So far So goood 3")
                    let validateWordsVertical = State.validateWordsOnBoardVertical makeCordsForWordsVertical st.occupiedSquares
                    // debugPrint (sprintf "So far So goood 4")
                    if(validateWordsVertical.Length = 0) then
                        // debugPrint (sprintf "So far So goood 5")
                        let makeCordsForWordsHorizontal = State.makeCordsToWords mapOfCordsToWords pieces "horizontal"
                        // debugPrint (sprintf "So far So goood 6")
                        let validateWordsHorizontal = State.validateWordsOnBoardHorizontal makeCordsForWordsHorizontal st.occupiedSquares
                        // debugPrint (sprintf "So far So goood 7")
                        if (validateWordsHorizontal.Length = 0) then
                            // debugPrint (sprintf "So far So goood 8")
                            send cstream (SMPass)
                        else
                            // debugPrint (sprintf "So far So goood 9")
                            let removeLetterFromBoardInWords = State.removeFirstLetterFromWords validateWordsHorizontal
                            // debugPrint (sprintf "So far So goood 10")
                            let move3 = State.longestFoundWord removeLetterFromBoardInWords
                            // debugPrint (sprintf "So far So goood 11")
                            send cstream (SMPlay move3)
                    else   
                        // debugPrint (sprintf "So far So goood 12")
                        let removeLetterFromBoardInWords = State.removeFirstLetterFromWords validateWordsVertical
                        // debugPrint (sprintf "So far So goood 13")
                        if removeLetterFromBoardInWords.Length > 0 then
                            // debugPrint (sprintf "So far So goood 14")
                            let move2 = State.longestFoundWord removeLetterFromBoardInWords
                            // debugPrint (sprintf "So far So goood 15")
                            send cstream (SMPlay move2)
                        else
                            // debugPrint (sprintf "So far So goood 16")
                            send cstream (SMChange (toList st.hand))
            

            // debugPrint (sprintf "So far So goood 17")
            let msg = recv cstream
            // debugPrint (sprintf "So far So goood 18")

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = State.updateStateOnCMPlayed ms st |> State.removeOldTiles ms |> State.addNewTiles newPieces |> State.updatePlayerTurn
                aux st'
            
            | RCM (CMChangeSuccess(newTiles)) ->
                (* Successful change by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                (*Works when changing all tiles from hand*)
                let st' = State.emptyYourHand st.hand st |> State.addNewTiles newTiles |> State.updatePlayerTurn
                aux st'
            | RCM (CMChange( playerID, newTiles)) ->
                
                (*Works when changing all tiles from hand*)
                //debugPrint (  sprintf "playerID %d", playerID)
                let st' =  State.updatePlayerTurn st
                aux st'
                

            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = State.updateStateOnCMPlayed ms st |> State.updatePlayerTurn 
                aux st'

            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = State.updatePlayerTurn st 

                aux st'
            | RCM (CMGameOver _) -> ()

            | RCM (CMPassed (_)) -> 
                let st' = State.updatePlayerTurn st 
                aux st'

            | RCM a -> failwith (sprintf "not implmented: %A" a)
            
            | RGPE err -> 
                printfn "Gameplay Error:\n%A" err;
                let st' = State.updatePlayerTurn st
                aux st'
        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers)
        
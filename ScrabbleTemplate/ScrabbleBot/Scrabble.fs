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
        occupiedSquares : Map<coord, (uint32 * (char*int))> //mapping a coordinate to a tuple of (id * tile)
    }

    let mkState b d pn h t = {board = b; dict = d;  playerNumber = pn; hand = h; playerTurn = t; 
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
    
    let handToCharacters pieces hand =
        hand |>
        MultiSet.fold (fun acc x i -> (Map.find x pieces)::acc) [] |> List.collect (fun s -> Set.toList (Set.map fst s))

    let emptyYourHand (hand : MultiSet.MultiSet<uint32>) (st : state) =
        let updatedHand = MultiSet.empty
        {st with hand = updatedHand} 
       

    let handToCharactersButKeepID pieces hand =
        hand |>
        MultiSet.fold (fun acc x i -> (Map.find x pieces)::acc) [] |> List.collect (fun s -> Set.toList s)

    //takes list of characters and returns a list of all possible combinations of the characters of different lengths
    let rec generateCombinations chars =
        match chars with
        | [] -> [[]]
        | c::cs ->
            let withoutC = generateCombinations cs
            let withC = List.map (fun combo -> c::combo) withoutC
            withoutC @ withC
    
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
    
    let optionStringToString (optString : option<string>) =
     match optString with
        | Some s -> s
        | None -> ""

    //cords to list of chars on an empty board. Not checking for center of board though.
    let convertToComplexType (charsAndUint32s: (uint32 * (char * int)) list) : (coord * (uint32 * (char * int))) list =
        charsAndUint32s |> List.mapi (fun i (u, (c, n)) -> ((0, i), (u, (c, n))))

    let makeListOfCharsFromString (s : string) =
        s |> Seq.toList
    

    let charToUint char = 
        if (char = '?') then 0u
        else uint32(System.Char.ToUpper(char)) - 64u
       

        
            
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            
            //check if it is the player's turn
            let isYourTurn = (State.playerTurn st) = (State.playerNumber st)
            //forcePrint (sprintf "Is it your turn? %b\n" isYourTurn)
            Print.printHand pieces (st.hand)
            
            //list of all characters in hand ex: ['T'; 'R'; 'Q'; 'O'; 'N'; 'L'; 'J'; 'I'; 'H'; 'G'; 'E'; 'D'; 'B'; 'A']
            let characters = State.handToCharacters pieces (st.hand)//characters
            debugPrint (sprintf "Printing characters: %A\n" characters)
            
            //list of all the unit values for the characters in hand ex: [1u; 2u; 4u; 5u; 7u; 8u; 9u; 10u; 12u; 14u; 15u; 17u; 18u; 20u]
            let indexesOfCharacters = toList st.hand //indexes
            debugPrint (sprintf "Printing indexes: %A\n" indexesOfCharacters)
            
            let foundWord = State.checkCombinationsInDictionary st.dict characters |> State.optionStringToString 
            let foundWords = State.FindListOfWordsInDictionary st.dict characters
            
            //finds the longest word from the list of found words
            let longestWord = foundWords |> List.maxBy (fun x -> x.Length)
            //debugPrint (sprintf "Longest word: %A\n" longestWord)

            //converts the longest word to a list of characters
            let longestWordAsChars = State.makeListOfCharsFromString longestWord
            //debugPrint (sprintf "Longest word as chars: %A\n" longestWordAsChars)
            
            //con
            let findIdToChar =  List.map (fun x -> State.charToUint x) longestWordAsChars
            debugPrint (sprintf "Find id to char: %A\n" findIdToChar)
                

            

            //print pieces
            debugPrint (sprintf "Printing pieces: %A\n" pieces)
           

            
            //center of board: ex (0, 0)
            let center = st.board.center

            forcePrint (sprintf "fandt et ord: %A \n" foundWords) 
            //debugPrint (sprintf "print print en liste pls %A", characters)
            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            if isYourTurn then 
                
                // TO DO: Tage nogle fede beslutninger om hvordan man spiller et ord når det er din tur. 
                // let input =  System.Console.ReadLine()
                // let move = RegEx.parseMove input
                // send cstream (SMPlay move)

                send cstream (SMChange (toList st.hand))

                //send cstream (SMPass)
            let msg = recv cstream


            
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = State.updateStateOnCMPlayed ms st |> State.removeOldTiles ms |> State.addNewTiles newPieces |> State.updatePlayerTurn
                aux st'
            
            | RCM (CMChangeSuccess( newTiles)) ->
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
                let st' = st
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn)
        
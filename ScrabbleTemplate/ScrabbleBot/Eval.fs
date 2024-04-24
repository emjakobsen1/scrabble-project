module internal Eval

    open StateMonad
    //open Types

    (* Code for testing *)

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1);] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a : SM<int>) (b : SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x + y)

    let sub (a : SM<int>) (b : SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x - y) 
            
    let div (a : SM<int>) (b : SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        if y = 0 then fail DivisionByZero
        else ret (x / y)
    
    let mul (a : SM<int>) (b : SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x * y)
    
    let modu (a : SM<int>) (b : SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        if y = 0 then fail DivisionByZero
        else ret (x % y)


    let toUpper (c : char) : SM<char> = ret (System.Char.ToUpper c)
    let toLower (c : char) : SM<char> = ret (System.Char.ToLower c)
    

    let eq (a : SM<int>) (b : SM<int>) : SM<bool> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x = y)
    
    let lt (a : SM<int>) (b : SM<int>) : SM<bool> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x < y)
    
    let isVowel (c : char) = 
        match c with
        | 'A' | 'E' | 'I' | 'O' | 'U' | 'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false
    


   


    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL  -> wordLength
        | PV p -> arithEval p >>= pointValue
        | Add (a, b) -> add (arithEval a) (arithEval b)
        | Sub (a, b) -> sub (arithEval a) (arithEval b)
        | Mul (a, b) -> mul (arithEval a) (arithEval b)
        | Div (a, b) -> div (arithEval a) (arithEval b)
        | Mod (a, b) -> modu (arithEval a) (arithEval b)
        | CharToInt c -> charEval c >>= fun c -> ret (int c)




    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= characterValue
        | ToUpper c -> charEval c >>= toUpper
        | ToUpper a -> charEval a >>= fun c -> ret (System.Char.ToUpper c)
        | ToLower a -> charEval a >>= fun c -> ret (System.Char.ToLower c)
        | IntToChar a -> arithEval a >>= fun x -> ret (char x)
       
  
    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a, b) -> eq (arithEval a) (arithEval b)
        | ALt (a, b) -> lt (arithEval a) (arithEval b)
        | Not b -> boolEval b >>= fun b -> ret (not b)
        | Conj (b1, b2) -> boolEval b1 >>= fun b1 -> boolEval b2 >>= fun b2 -> ret (b1 && b2)
        | IsVowel c -> (charEval c) >>= fun c -> ret (isVowel c)
        | IsLetter c -> (charEval c) >>= fun c -> ret (System.Char.IsLetter c)



    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm (w:word) (pos:int) (acc:int) =
        let state = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]
        stmntEval stm >>>= lookup "_result_" |> evalSM state


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

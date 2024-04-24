﻿// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> 

    let empty = R (Map.empty)

    let isEmpty (R ms) = Map.isEmpty ms

    let size (R ms) = Map.fold (fun count _ acc -> count + acc) 0u ms
    
    let contains (key : 'a) (R ms) = ms.ContainsKey key

    let numItems (key : 'a) (R ms) = 
        match Map.tryFind key ms with 
        | Some value -> value
        | None -> 0u

    let add (a : 'a) (n : uint32) (R ms) = R (ms.Add (a, (numItems a (R ms)+n))) 
    let addSingle (a : 'a) (ms : MultiSet<'a>)  = add a 1u ms
    
    let remove (a : 'a) (n : uint32) (R s) : MultiSet<'a> =
        if n >= numItems a (R s) then R ( Map.remove a s) else R(Map.add a ((numItems a (R s))-n) s)
            

    let removeSingle (a : 'a) (s : MultiSet<'a>) : MultiSet<'a>=  
        if contains a s then remove a 1u s else s


    let fold (f : 'b -> 'a -> uint32 -> 'b) (acc : 'b) (R s) = Map.fold f acc s
    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (R(s) : MultiSet<'a>) (acc : 'b) = Map.foldBack f s acc
    
    let ofList (_ : 'a list) : MultiSet<'a> =  failwith "not implemented yet"
    let toList (_ : MultiSet<'a>) : 'a list =  failwith "not implemented yet"


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> =  failwith "not implemented yet"

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> =  failwith "not implemented yet"
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> =  failwith "not implemented yet"
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented yet"
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented yet"
       
    

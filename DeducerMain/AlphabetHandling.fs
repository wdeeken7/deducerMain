module AlphabetHandling
open typeDefinitions

//Main Build Function
let buildAlphabet (userDefinedSetOfWords: AtomicWord[]) (userDefinedBinaryOperations: SyntacticalBinaryOperation[]) (userDefinedUnaryOperations: SyntacticalUnaryOperation[]) (userDefinedModalOperations: SyntacticalModalOperation[]) =
    //Must check that AtomicWord String , Binary/Unary/Modal Operation Strings are not shared!
    let mutable flag = false
    let commonStrings = 
        for a in userDefinedSetOfWords do
            for b in  userDefinedBinaryOperations do
                for c in userDefinedUnaryOperations do
                    for d in userDefinedModalOperations do
                        if ((a.ToString=b.ToString) || (a.ToString=c.ToString) || (a.ToString=d.ToString) || (b.ToString=c.ToString) || (b.ToString=d.ToString) || (c.ToString = d.ToString)) then
                            flag <- true
       
    if not(flag) then 
        flag <- false
        
        let multipleStringsAtomicWord = 
            for a in userDefinedSetOfWords do
                let mutable firstFlag = true
                for b in userDefinedSetOfWords do
                    if a.ToString = b.ToString then
                        if not(firstFlag) then
                            flag <- true
                        else
                            firstFlag <- false
        if not(flag) then
            let multipleStringsBinaryOperations = 
                for a in userDefinedBinaryOperations do
                    let mutable firstFlag = true
                    for b in userDefinedBinaryOperations do
                        if a.ToString = b.ToString then
                            if not(firstFlag) then
                                flag <- true
                            else
                                firstFlag <- false
            if not(flag) then
                let multipleStringsUnaryOperation = 
                    for a in userDefinedUnaryOperations do
                        let mutable firstFlag = true
                        for b in userDefinedUnaryOperations do
                            if a.ToString = b.ToString then
                                if not(firstFlag) then
                                    flag <- true
                                else
                                    firstFlag <- false
                if not(flag) then
                    let multipleStringsModalOperation = 
                        for a in userDefinedModalOperations do
                            let mutable firstFlag = true
                            for b in userDefinedModalOperations do
                                if a.ToString = b.ToString then
                                    if not(firstFlag) then
                                        flag <- true
                                    else
                                        firstFlag <- false
                    if not(flag) then
                        Alphabet(userDefinedSetOfWords, userDefinedBinaryOperations, userDefinedUnaryOperations, userDefinedModalOperations)
                    else
                        //Return Generic Alphabet 
                        Alphabet([|AtomicWord("A")|], [|SyntacticalBinaryOperation("OP", 1)|], [|SyntacticalUnaryOperation("UNOP", 1)|], [|SyntacticalModalOperation("MO", 2)|])
                else 
                    //Return Generic Alphabet 
                    Alphabet([|AtomicWord("A")|], [|SyntacticalBinaryOperation("OP", 1)|], [|SyntacticalUnaryOperation("UNOP", 1)|], [|SyntacticalModalOperation("MO", 2)|])
            else
                //Return Generic Alphabet 
                Alphabet([|AtomicWord("A")|], [|SyntacticalBinaryOperation("OP", 1)|], [|SyntacticalUnaryOperation("UNOP", 1)|], [|SyntacticalModalOperation("MO", 2)|])
         else 
            //Return Generic Alphabet 
            Alphabet([|AtomicWord("A")|], [|SyntacticalBinaryOperation("OP", 1)|], [|SyntacticalUnaryOperation("UNOP", 1)|], [|SyntacticalModalOperation("MO", 2)|])
     else
        //Return Generic Alphabet
        Alphabet([|AtomicWord("A")|], [|SyntacticalBinaryOperation("OP", 1)|], [|SyntacticalUnaryOperation("UNOP", 1)|], [|SyntacticalModalOperation("MO", 2)|])
         
                    
   
   
   
   
   
   //Checks whether there are copies of each element userWords
    //Functionality to be added/completed later.
    (*
    if flag = true then
        let rec multipleElement index = 
            if index < userDefinedSetOfWords.Length then
                for A in userDefinedSetOfWords do
                    if userDefinedSetOfWords[index] = A.ToString then
                        true
                    else 
                        (multipleElement (index + 1))
            else
                false
        if multipleElement = true then
            //Return a default alphabet.
        else
            *)
            

        



module AxiomHandling
open typeDefinitions
let buildAxiom (antecedent: SyntacticalExpression)(consequent: SyntacticalExpression) (alphabet: Alphabet) = 
    //Must check whether at least one dummy variable of the antecedent appears in the consequent
    let rec checkDummies index = 
        if index < antecedent.expressionArray.Length then 
            if antecedent.expressionArray.[index] :? AtomicWord then
                let toCheck = antecedent.expressionArray.[index]
                let mutable flag = false
                for x in consequent.expressionArray do
                    if (toCheck.equals x) then
                        flag <- true
                if flag then true
                else (checkDummies (index + 1))
            else 
                (checkDummies (index + 1))
        else
            false
    if (checkDummies 0) then
        Axiom(antecedent, consequent)
    else
        //Dummy Axiom if the user defined axiom is invalid.
        Axiom( SyntacticalExpression([|AtomicWord("A")|]), SyntacticalExpression([|AtomicWord("A")|]))
    //(checkDummies 0)
    
        
    
//The following function takes a SyntacticalExpresion and an axiom, and attempts to apply the axiom (Expression is compared against the antecedent). Since there are multiple ways of applying an axiom, it takes the "first" one, in the sense that 
//it is the first operation pattern that is recognizes. EXPRESSION IS IN PREFIX. 
let attemptAxiom (expression: SyntacticalExpression) (axiom: Axiom) = 
    let arrayOfAxiomaticTerms = (Array2D.zeroCreate 10 10 )
    //Keeps track of the axiom array index, to keep track whether the current term needs checked against a previously read term. 
    let mutable highestAxiomaticArrayIndex = -1
    let mutable badAxiomFlag = false
    let rec tryAxiom indexOfAxiom indexOfExpression indexOfAxiomaticTerms = 
        if indexOfAxiom < axiom.antecedent.expressionArray.Length then 
            if axiom.antecedent.expressionArray.[indexOfAxiom] :? Operation then 
                //The below expression might need to equal an atomic word.
                if ((expression.expressionArray.[indexOfExpression]).ToString = (axiom.antecedent.expressionArray.[indexOfAxiom]).ToString) then 
                    //The below function the array of terms, corresponding to the pattern of the axiom. 
                   (tryAxiom (indexOfAxiom + 1) (indexOfExpression + 1) indexOfAxiomaticTerms)
            else
                    //let termArray =  (Array.zeroCreate 100)
                    //The following function builds the term, and returns the last index, to be used for the recursive calls themselves.
                    let rec innerLoop (indexOfExpression : int) (indexOfTermArray : int) = 
                        if indexOfExpression < expression.expressionArray.Length then
                            if expression.expressionArray.[indexOfExpression] :? AtomicWord then
                                arrayOfAxiomaticTerms.[indexOfAxiomaticTerms, indexOfTermArray]  <- expression.expressionArray.[indexOfExpression]
                                (indexOfExpression)
                            elif expression.expressionArray.[indexOfExpression] :? SyntacticalBinaryOperation then
                                arrayOfAxiomaticTerms.[indexOfAxiomaticTerms, indexOfTermArray]  <- expression.expressionArray.[indexOfExpression]//termArray.[indexOfTermArray] <- expression.expressionArray.[indexOfExpression]
                                let indexForSecond = (innerLoop (indexOfExpression + 1) (indexOfTermArray + 1))
                                (innerLoop (indexOfExpression + 2) indexForSecond)
                                //(indexOfExpression)
                            else // expression.expressionArray.[indexOfExpression] :? SyntacticalUnaryOperation then
                                arrayOfAxiomaticTerms.[indexOfAxiomaticTerms, indexOfTermArray]  <- expression.expressionArray.[indexOfExpression]//termArray.[indexOfTermArray] <- expression.expressionArray.[indexOfExpression]
                                (innerLoop (indexOfExpression + 1) (indexOfTermArray + 1))
                                //(indexOfExpression)
                        else
                            indexOfExpression
                        
                        
                            //indexOfExpression
                    let lastIndex = (innerLoop indexOfExpression  0)
                    //For checking whether the term must be checked against a previously used term
                    if (fst (System.Double.TryParse(axiom.antecedent.expressionArray.[indexOfAxiom].ToString))) then
                        let currentDummyInAxiom = int (snd (System.Double.TryParse(axiom.antecedent.expressionArray.[indexOfAxiom].ToString)))
                        let rec checkingPrevTermForEquality index = 
                            if ( isNull arrayOfAxiomaticTerms.[indexOfAxiomaticTerms, index] ) && (isNull arrayOfAxiomaticTerms.[currentDummyInAxiom - 1, index] ) then
                                if arrayOfAxiomaticTerms.[indexOfAxiomaticTerms, index].ToString = arrayOfAxiomaticTerms.[(currentDummyInAxiom - 1), index].ToString then
                                    (checkingPrevTermForEquality (index + 1))
                                else
                                    false
                            elif ( isNull arrayOfAxiomaticTerms.[indexOfAxiomaticTerms, index]) then
                                false
                            elif ( isNull arrayOfAxiomaticTerms.[currentDummyInAxiom - 1, index]) then
                                false
                            else
                                true
                        if currentDummyInAxiom <= highestAxiomaticArrayIndex then
                            if (checkingPrevTermForEquality 0) then
                                //Continue
                                printfn "PASS multiplicity test"
                            else
                                badAxiomFlag <- true
                        else
                            highestAxiomaticArrayIndex <- currentDummyInAxiom
                                
                        //arrayOfAxiomaticTerms.[indexOfAxiomaticTerms] <- termArray
                        //highestAxiomaticArrayIndex <- (highestAxiomaticArrayIndex + 1)
                    (tryAxiom (indexOfAxiom + 1) (lastIndex + 1) (indexOfAxiomaticTerms + 1))

                
                    // must throw some type of error in the "else" statement
                    //Now, must find the "spot" in the antecedent array that the above term "fits" into. Equivalently, the same dummy variable that this function is at, btu
    //Since the dummy variables were labeled integers, in the order they appear in the axiom.
    (tryAxiom 0 0 0)
    let consequentArray = (Array.zeroCreate 10)
    //Leaves a "blank" in each spot of the array where an operation goes.
    let rec buildConsequent indexOfAxiomConsequent = 
        if indexOfAxiomConsequent < axiom.consequent.expressionArray.Length then 
            let a  = axiom.consequent.expressionArray.[indexOfAxiomConsequent].ToString
            let is_numeric =  (System.Double.TryParse(a))
            if fst is_numeric then 
                let toInt = ( int (snd is_numeric))
                consequentArray.[indexOfAxiomConsequent] <- arrayOfAxiomaticTerms.[(toInt - 1), *]
            else
                consequentArray.[indexOfAxiomConsequent] <- (Array.zeroCreate 1)
            (buildConsequent (indexOfAxiomConsequent + 1))
    (buildConsequent 0 )
    //Now, must convert the above array into an expression
    let consequentExpression = (Array.zeroCreate 10)
    let rec convertArrayToExpression index indexForExpressionArray = 
        if index < consequentArray.Length && not(isNull consequentArray.[index]) then
            let rec termControl index1 indexForExpressionArray = 
                if index < consequentArray.[index].Length && indexForExpressionArray < consequentExpression.Length then
                    consequentExpression.[indexForExpressionArray] <- consequentArray.[index].[index1]
                    (termControl (index1 + 1) (indexForExpressionArray + 1))
                else 
                    indexForExpressionArray
            
            (convertArrayToExpression (index + 1) (termControl 0 indexForExpressionArray))
    (convertArrayToExpression 0 0)
    SyntacticalExpression(consequentExpression)

            

        

                
            
        
        
    
                        
                        
                        
                    
                    


            
        
        
            

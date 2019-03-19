module attemptAxiomTemp

open typeDefinitions
open System



(*let attemptAxiom (axiom: Axiom) (expressionToApply: SyntacticalExpression) = 
    //The following function builds a two-d array of syntacticalexpresionterms. The first coord matches the dummy variable in the axiom definition.
    //Outer rec func traverses axiom.antecedent, inner rec funcs traverses the expression.
    //100 array length is arbitrary
    let anteArrayOfTerms = (Array.zeroCreate 100)
    let rec buildAnteArray (indexOfAxiom: int) (indexOfExpression: int) = 
        if indexOfAxiom < axiom.antecedent.expressionArray.Length then
            if (axiom.antecedent.expressionArray.[indexOfAxiom].ToString = expressionToApply.expressionArray.[indexOfExpression].ToString) then
                if expressionToApply.expressionArray.[indexOfExpression] :? Operation then
                    if expressionToApply.expressionArray.[indexOfExpression] :? SyntacticalBinaryOperation then
                        let individualTerm1 = (Array.zeroCreate 100)
                        let individualTerm2 = (Array.zeroCreate 100)
                        //Inner loop returns an index of the expression that will be used to begin the second term.
                        //tempIndex traverses the individual "term"
                        let rec innerLoop (tempIndexOfTerm: int)(indexOfExpressionTerm : int) =
                            individualTerm1.[tempIndexOfTerm] <- expressionToApply.expressionArray.[indexOfExpressionTerm]
                            if expressionToApply.expressionArray.[expressionTermIndex] :? SyntacticalBinaryOperation then
                                let indexToDo = (innerLoop (tempIndexOfTerm + 1) (indexOfExpressionTerm + 1))
                                individualTerm1.[(fst indexToDo) + 1] <- expressionToApply.expressionArray.[(snd indexToDo) + 1]
                                //indexToDo
                            elif expressionToApply.expressionArray.[expressionTermIndex] :? SyntacticalUnaryOperation then
                                individualTerm1.[tempIndexOfTerm] <- expressionToApply.expressionArray.[indexOfExpressionTerm]
                                (innerLoop (tempIndexOfTerm + 1) (indexOfExpressionTerm + 1) )
                            
                            elif expressionToApply.expressionArray.[expressionTermIndex] :? AtomicWord then
                                individualTerm1.[tempIndexOfTerm] <- expressionToApply.expressionArray.[expressionTermIndex]
                                //(tempIndexOfTerm, indexOfExpressionTerm)
                            else
                                (0, 0)
                        let indexForSecondTerm = snd (innerLoop 0 (indexOfExpression + 1))
                        let rec innerLoop2 (tempIndexOfTerm: int)(indexOfExpressionTerm : int) =
                            individualTerm1.[tempIndexOfTerm] <- expressionToApply.expressionArray.[indexOfExpressionTerm]
                            if expressionToApply.expressionArray.[expressionTermIndex] :? SyntacticalBinaryOperation then
                                let indexToDo = (innerLoop2 (tempIndexOfTerm + 1) (indexOfExpressionTerm + 1))
                                individualTerm1.[(fst indexToDo) + 1] <- expressionToApply.expressionArray.[(snd indexToDo) + 1]
                                indexToDo
                            elif expressionToApply.expressionArray.[expressionTermIndex] :? SyntacticalUnaryOperation then
                                individualTerm1.[tempIndexOfTerm] <- expressionToApply.expressionArray.[indexOfExpressionTerm]
                                (innerLoop2 (tempIndexOfTerm + 1) (indexOfExpressionTerm + 1) )
                                //(tempIndexOfTerm, indexOfExpressionTerm)
                            
                            elif expressionToApply.expressionArray.[expressionTermIndex] :? AtomicWord then
                                individualTerm1.[tempIndexOfTerm] <- expressionToApply.expressionArray.[expressionTermIndex]
                                //(tempIndexOfTerm, indexOfExpressionTerm)
                            else
                                (0, 0)
                        (innerLoop2 0 (indexForSecondTerm + 1))


                                

                            
                    elif expressionToApply.expressionArray.[indexOfExpression] :? SyntacticalUnaryOperation then
                        
                    elif expressionToApply.expressionArray.[indexOfExpression] :? SyntacticalModalOperation then
                    else
                        //throw error
                    
                        
                        
                else
                    //Again, must throw some type of error if statement is reached

            else
                //Must throw some type of error if this else is reached.
        
    
    *)
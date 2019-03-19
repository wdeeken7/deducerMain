module ExpressionHandling
open typeDefinitions
open System



//The following function evaluates an INFIX expression to a truth value
let evaluateExpression (expression: SemanticExpression ) (nullTruthValue : TruthValue) =
    
    //Must develop a function that takes an INFIX expression and converts it into INFIX!
    //The following function takes the objectArray and splits it into seperate arrays, depending on object type
    
   
    
    // let rec buildArrays index = 
       // if expressionArray.[index] :? SemanticBinaryOperation then  
    let rec eval index = 
        if(index < expression.expressionArray.Length) then
           
           if ( expression.expressionArray.[index] :? SemanticAtomicWord) then
                //Uses dummy truth values, which are not used. The below expression returns the truthvalue of the atomic word.
                (expression.expressionArray.[index].performOp (TruthValue("NULL"))(TruthValue("NULL")))
           elif (expression.expressionArray.[index] :? SemanticBinaryOperation) then 
                let val1 = (eval (index + 1))
                let val2 = (eval (index + 2))
                (expression.expressionArray.[index].performOp val1 val2)
           elif (expression.expressionArray.[index] :? SemanticUnaryOperation) then
                let val1 = (eval (index + 1))
                (expression.expressionArray.[index].performOp val1 nullTruthValue)
           else
                nullTruthValue
                
           (*
            if not(expression.SemanticAtomicWordArray.[index].word.ToString = "0") then
                expression.SemanticAtomicWordArray.[index].truthValue
            elif not(expression.SemanticBinaryOperationsArray.[index].operation.ToString = "0") then
                let val1 = (eval (index + 1))
                let val2 = (eval (index + 2))
                (expression.SemanticBinaryOperationsArray.[index].performOp val1 val2)
            else
                let val3 = (eval (index + 1))
                (expression.SemanticUnaryOperationsArray.[index].performOp val3)
                *)
        else
            //If this point is reached, the prefix was INCORRECT!
            nullTruthValue
        
        
           
        
    (eval 0)




     
            


module SemanticsHandling
open typeDefinitions
//returns SemanticAlphabet
//The order of the elements in the alphabet MUST correspond with the order of the truthtables!!!
let applySemanticsToAlphabet (alphabet: Alphabet)(truthValueForWord: TruthValue[]) (truthValueForBinaries: TruthValue[][][]) (truthValuesForUnaries: TruthValue[][][])   =
    //let index = 0
    
  
    let arrayOfSemanticAtomicWords = (Array.zeroCreate (alphabet.words.Length)) 
    let arrayOfSemanticBinaryOperations = (Array.zeroCreate (alphabet.binaryOperations.Length))
    let arrayOfSemanticUnaryOperations = (Array.zeroCreate (alphabet.unaryOperations.Length))
    let rec buildArrayOfSemanticWords index = 
        if (index < arrayOfSemanticAtomicWords.Length) then 
          
            let newSemanticWord =  SemanticAtomicWord(alphabet.words.[index], truthValueForWord.[index])
            arrayOfSemanticAtomicWords.[index] <-  newSemanticWord
            (buildArrayOfSemanticWords (index + 1))
            
        
            
    let rec buildArrayOfSemanticBinaryOperations index = 
        if index < arrayOfSemanticBinaryOperations.Length then
            let newSemanticBinary =  SemanticBinaryOperation(alphabet.binaryOperations.[index], truthValueForBinaries.[index])
            arrayOfSemanticBinaryOperations.[index] <- newSemanticBinary
            (buildArrayOfSemanticBinaryOperations (index + 1))
            
        
    let rec buildArrayOfSemanticUnaries index = 
        if index < arrayOfSemanticUnaryOperations.Length then
            let newSemanticUnary = SemanticUnaryOperation(alphabet.unaryOperations.[index], truthValuesForUnaries.[index])
            arrayOfSemanticUnaryOperations.[index] <- newSemanticUnary
    (buildArrayOfSemanticWords 0)
    (buildArrayOfSemanticBinaryOperations 0)
    (buildArrayOfSemanticUnaries 0)
    SemanticAlphabet(arrayOfSemanticAtomicWords ,  arrayOfSemanticBinaryOperations, arrayOfSemanticUnaryOperations)


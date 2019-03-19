// Learn more about F# at http://fsharp.org

open System
open System.IO
open typeDefinitions
open AlphabetHandling
open SemanticsHandling
open AxiomHandling
open ExpressionHandling

[<EntryPoint>]
let main argv =
    let filePath = "C:\\Users\\wdeek\\source\\repos\\DeducerMain\\testFile.txt" //(string System.Console.Read)
    System.Console.Out.Write(filePath)
    let sr = new StreamReader(filePath)
    
   
    let mutable flag = true
    //Define Syntactical Terms
    let arrayOfAtomicWords = [|AtomicWord("A"); AtomicWord("B"); AtomicWord("C"); AtomicWord("D"); AtomicWord("E"); AtomicWord("F")|]
    let arrayOfSyntacticalBinaryOps = [|SyntacticalBinaryOperation("AND", 2); SyntacticalBinaryOperation("OR", 2); SyntacticalBinaryOperation("IMPLIES", 1)|]
    let arrayOfSyntacticalUnaryOps = [|SyntacticalUnaryOperation("NOT", 0)|]
    let arrayOfSyntacticalModalOps = [|SyntacticalModalOperation("NULL", 0)|]
    let alphabet = (buildAlphabet arrayOfAtomicWords arrayOfSyntacticalBinaryOps arrayOfSyntacticalUnaryOps arrayOfSyntacticalModalOps)

    //Define Semantic Terms
    let nullTruthValue = TruthValue("NULL")
    let truth = TruthValue("TRUE")
    let falsehood = TruthValue("FALSE")
    let arrayOfTruthValues = [|truth; falsehood|]
    let arrayOfSemanticWords = [|truth; falsehood; truth; truth; falsehood; falsehood|]
    //defining truth tables for binary operations
    let arrayOfTruthValuesForBinaryAND = [| [|nullTruthValue; truth; falsehood|] ; [|truth; truth; falsehood|]; [|falsehood; falsehood; falsehood|] |]
    let arrayOfTruthValuesForBinaryOR = [| [|nullTruthValue; truth; falsehood|]; [|truth; truth; truth|]; [|falsehood; truth; falsehood|] |]
    let arrayOfTruthValuesForBinaryIMPLIES = [| [|nullTruthValue; truth; falsehood|]; [|truth; truth; truth|]; [|falsehood; falsehood; truth|] |]
    let arrayOfAllBinaries = [|arrayOfTruthValuesForBinaryAND; arrayOfTruthValuesForBinaryOR; arrayOfTruthValuesForBinaryIMPLIES|]
    //defining truth table for unary operation
    let arrayOfTruthValuesForUnaryNOT = [| [|truth; falsehood|]; [|falsehood; truth|] |]
    let arrayOfAllUnaries = [|arrayOfTruthValuesForUnaryNOT|]
    // build semantic alphabet.
    let semanticAlphabet = (applySemanticsToAlphabet alphabet arrayOfSemanticWords arrayOfAllBinaries arrayOfAllUnaries)
    //Test evaluate functionality for semantics
    let testExpressions =
        //First test expression: AND TRUE TRUE
        let testExpression1 = SemanticExpression([|semanticAlphabet.semanticBinaryOperations.[0]; semanticAlphabet.semanticWords.[0]; semanticAlphabet.semanticWords.[2]|])
        //Second test expression : AND TRUE FALSE
        let testExpression2 = SemanticExpression([|semanticAlphabet.semanticBinaryOperations.[0]; semanticAlphabet.semanticWords.[0]; semanticAlphabet.semanticWords.[1] |])
        //Third test expression: OR FALSE FALSE
        let testExpression3 = SemanticExpression([|semanticAlphabet.semanticBinaryOperations.[1]; semanticAlphabet.semanticWords.[4]; semanticAlphabet.semanticWords.[5] |])
        //Fourth test expression: IMPLIEs TRUE NOT FALSE
        let testExpression4 = SemanticExpression([|semanticAlphabet.semanticBinaryOperations.[2]; semanticAlphabet.semanticWords.[0]; semanticAlphabet.semanticUnaryOperations.[0]; semanticAlphabet.semanticWords.[1] |])
        //Fifth test expression: OR FALSE TRUE
        let testExpression5 = SemanticExpression([|semanticAlphabet.semanticBinaryOperations.[1]; semanticAlphabet.semanticWords.[4]; semanticAlphabet.semanticWords.[0]|])       
        System.Console.Out.WriteLine("")
        System.Console.Out.WriteLine("BEGIN TESTS")
        System.Console.Out.WriteLine("Result of " + semanticAlphabet.semanticBinaryOperations.[0].alias + 
            " " + semanticAlphabet.semanticWords.[0].truthValue.ToString + " " + semanticAlphabet.semanticWords.[2].truthValue.ToString +  " >")
        System.Console.Out.WriteLine( (evaluateExpression testExpression1 nullTruthValue).ToString )
        System.Console.Out.WriteLine("Result of AND TRUE FALSE >")
        System.Console.Out.WriteLine( (evaluateExpression testExpression2 nullTruthValue).ToString )
        System.Console.Out.WriteLine("Result of OR FALSE FALSE >")
        System.Console.Out.WriteLine( (evaluateExpression testExpression3 nullTruthValue).ToString )
        System.Console.Out.WriteLine("Result of OR FALSE TRUE")
        System.Console.Out.WriteLine( (evaluateExpression testExpression5 nullTruthValue).ToString )
        System.Console.Out.WriteLine("Result of IMPLIES TRUE NOT FALSE >")
        System.Console.Out.WriteLine( (evaluateExpression testExpression4 nullTruthValue).ToString )
    //syntactical expressions to test axiom handling
    let testAxiom =
        //Test Axiom (Modus Ponens)
        let semanticDummy1 = AtomicWord("1")
        let semanticDummy2 = AtomicWord("2")
        let semanticDummy3 = AtomicWord("3")
        let modusPonensAntecedent = SyntacticalExpression([|arrayOfSyntacticalBinaryOps.[0]; arrayOfSyntacticalBinaryOps.[2]; semanticDummy1; semanticDummy2; semanticDummy1|])
        let modusPonensConsequent = SyntacticalExpression([|semanticDummy2|])
        let expressionToApply = SyntacticalExpression([|arrayOfSyntacticalBinaryOps.[0]; arrayOfSyntacticalBinaryOps.[2]; arrayOfAtomicWords.[0]; arrayOfAtomicWords.[1]; arrayOfAtomicWords.[0] |])
        let axiomModusPonens = (buildAxiom modusPonensAntecedent modusPonensConsequent alphabet)
        let axiomApplied = (attemptAxiom expressionToApply axiomModusPonens)
        System.Console.Out.WriteLine("Testing modus ponens expression> ")
        System.Console.Out.WriteLine("Expression to apply axiom to> " + expressionToApply.ToString)
        System.Console.Out.WriteLine(axiomApplied.ToString)
        //Test expresson #2, AND IMPLIES A OR B C A, should assert OR B C
        let expressionToApply2 = SyntacticalExpression([|arrayOfSyntacticalBinaryOps.[0]; arrayOfSyntacticalBinaryOps.[2]; arrayOfAtomicWords.[0]; arrayOfSyntacticalBinaryOps.[1]; arrayOfAtomicWords.[1]; arrayOfAtomicWords.[2]; arrayOfAtomicWords.[0]|])
        let axiomApplied2 = (attemptAxiom expressionToApply2 axiomModusPonens)
        System.Console.Out.WriteLine("Testing modus ponens expression " + expressionToApply2.ToString + " >")
        System.Console.Out.WriteLine(axiomApplied2.ToString)
        //Test expression #3, AND IMPLIES AND A B C AND A B
        let expressionToApply3 = SyntacticalExpression([|arrayOfSyntacticalBinaryOps.[0]; arrayOfSyntacticalBinaryOps.[2]; arrayOfSyntacticalBinaryOps.[0]; arrayOfAtomicWords.[0]; arrayOfAtomicWords.[1]; arrayOfAtomicWords.[2]; arrayOfSyntacticalBinaryOps.[0]; arrayOfAtomicWords.[0]; arrayOfAtomicWords.[1]|])
        System.Console.Out.WriteLine("Testing expression " + expressionToApply3.ToString + " >")
        let axiomApplied3 = (attemptAxiom expressionToApply3 axiomModusPonens)
        System.Console.Out.WriteLine(axiomApplied3.ToString)
    //(testExpressions)

    //let testSynExpression1 = SyntacticalExpression[|arrayOf
    
    
    
                


            
      
    let v = System.Console.Read()
    
    0 // return an integer exit code

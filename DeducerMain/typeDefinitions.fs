module typeDefinitions


type TruthValue (userDef: string) = 
    member this.ToString = userDef
    member this.equals (valToCheck: TruthValue) = 
        if(this.ToString = valToCheck.ToString) then
            true
        else
            false
[<AllowNullLiteral>]
[<AbstractClass>]
type SyntacticalExpressionTerm ( userDef: string ) = 
    abstract ToString: string
    abstract equals : SyntacticalExpressionTerm -> bool

[<AbstractClass>]
type Operation (userDef: string, orderOfPrec: int) = 
    inherit SyntacticalExpressionTerm (userDef)
    abstract orderOfPrecedence: int
    
    member this.ToString = userDef
    member this.equals (toCompare: SyntacticalExpressionTerm) =
        if this.ToString = toCompare.ToString then true
        else false
[<AbstractClass>]
type SemanticExpressionTerm ( alias: string) = 
    abstract alias : string
    abstract performOp : TruthValue ->  TruthValue -> TruthValue
//Because of type constraints, this turns the given expression (in PREFIX) and maps each element to an array based on its type. The final array is an array of references to the other table.

    
    


type AtomicWord (userLetter: string) = 
    inherit SyntacticalExpressionTerm (userLetter)
    override this.ToString = userLetter
    override this.equals (ToCompare  : SyntacticalExpressionTerm) = 
        if this.ToString = ToCompare.ToString then true
        else false
    //member this.orderOfPrecedence = orderOfPrec
type SyntacticalBinaryOperation (userDefinedOperation: string, orderOfPrec: int) =
    inherit Operation (userDefinedOperation, orderOfPrec)
    override this.ToString = userDefinedOperation
    override this.orderOfPrecedence = orderOfPrec
    override this.equals (toCompare: SyntacticalExpressionTerm) = 
        if this.ToString = toCompare.ToString then true
        else false
type SyntacticalUnaryOperation (userDefinedOperation: string, orderOfPrec: int) = 
    inherit Operation (userDefinedOperation, orderOfPrec)
    override this.ToString = userDefinedOperation
    override this.orderOfPrecedence = orderOfPrec
    override this.equals (toCompare: SyntacticalExpressionTerm) = 
        if this.ToString = toCompare.ToString then true
        else false
type SyntacticalModalOperation (userDefinedOperation: string, orderOfPrec: int) =
    inherit Operation (userDefinedOperation, orderOfPrec)
    override this.ToString = userDefinedOperation
    override this.orderOfPrecedence = orderOfPrec
    override this.equals (toCompare: SyntacticalExpressionTerm) = 
        if this.ToString = toCompare.ToString then true
        else false
type Alphabet (userWords: AtomicWord[] , userBinaryOperations: SyntacticalBinaryOperation[] , userUnaryOperations: SyntacticalUnaryOperation[] , userModalOperations: SyntacticalModalOperation[]) =
    member this.words = userWords
    member this.binaryOperations = userBinaryOperations
    member this.unaryOperations = userUnaryOperations
    member this.modalOperations = userModalOperations
type SemanticAtomicWord (word: AtomicWord, value: TruthValue) = 
    inherit SemanticExpressionTerm (word.ToString)
    override this.alias = word.ToString
    member this.word = word
    member this.truthValue = value
    override this.performOp (val1 : TruthValue) (val2: TruthValue) = 
        this.truthValue
//The type below contains one possible set of arguments, and the last truth value of the array is the output
type SemanticOpDefHelper (array: TruthValue[]) = 
    member this.array = array
type SemanticBinaryOperation (op: SyntacticalBinaryOperation, array1: TruthValue[][]) =
    inherit SemanticExpressionTerm (op.ToString)
    override this.alias = op.ToString
    member this.operation = op
    member this.DefinitionTable = array1
    override this.performOp (value1: TruthValue) (value2: TruthValue) = 
        let val1 = value1.ToString
        let val2 = value2.ToString
        let rec firstCoord index1 = 
            if this.DefinitionTable.[0].[index1].ToString = val1 then
                index1
            else
                (firstCoord (index1 + 1))
        let rec secondCoord index1 = 
            if this.DefinitionTable.[index1].[0].ToString = val2 then
                index1
            else 
                (secondCoord (index1 + 1))
        this.DefinitionTable.[(secondCoord 0)].[(firstCoord 0)]
            
        
        
type SemanticUnaryOperation (op: SyntacticalUnaryOperation, array: TruthValue[][]) = 
    inherit SemanticExpressionTerm (op.ToString)
    override this.alias = op.ToString
    member this.operation = op
    member this.DefinitionTable = array
    //Value2 is NOT USED!!!
    override this.performOp (value1: TruthValue) (value2: TruthValue) = 
        let val1 = value1
        //let val2 = value2
        
        let rec findValue index = 
            if (this.DefinitionTable.[0].[index].equals val1) then
                this.DefinitionTable.[1].[index]
            else (findValue (index + 1))
        (findValue 0)
type SemanticAlphabet (userWords: SemanticAtomicWord[], userBins: SemanticBinaryOperation[], userUns: SemanticUnaryOperation[]) = 
    member this.semanticWords = userWords
    member this.semanticBinaryOperations = userBins
    member this.semanticUnaryOperations = userUns
type SyntacticalExpression (expressionArray: array<SyntacticalExpressionTerm>) = 
    member this.expressionArray = expressionArray
    member this.ToString = 
        let mutable string = ""
        for x in this.expressionArray do
            if (isNull x) then
                string <- string + "NULL"
            else
                string <- string + x.ToString
        string
            
            
type SemanticExpression (array : array<SemanticExpressionTerm>) =
    member this.expressionArray = array
//Both antecedent and consequent is in prefix, to aid in subsequent operations/calculations.
type Axiom (antecedent: SyntacticalExpression, consequent: SyntacticalExpression) = 
    member this.antecedent = antecedent
    member this.consequent = consequent

            
    
        
 //Semantic Modal to be implemented later....

    
            
        
        
    
    //member this.OrderOfPrecedence = userDefinedOrder
//type Axiom (userLetters



#if INTERACTIVE
#r "..\\CSharpQuotations\\bin\\Debug\\CSharpQuotations.exe"
#r "..\\packages\\Roslyn.Compilers.CSharp.1.2.20906.2\\lib\\net45\\Roslyn.Compilers.CSharp.dll"
#else
namespace CSharp.Quotations
#endif

open Roslyn.Compilers.CSharp
open System.Linq.Expressions
open System.Collections.Generic
open System
open System.Runtime.CompilerServices
open System.Reflection
open Microsoft.FSharp.Quotations


module QuotationCompiler =
    
    module Seq =
        let ofType<'a> (s : System.Collections.IEnumerable) =
            seq {
                for o in s do
                    match o  with
                        | :? 'a as a -> yield a
                        | _ -> () 
            }

    [<AutoOpen>]
    module Patterns =

        /// <summary>
        /// active pattern matching ForExpressions as For([variable] * [value], condition, [step], body, breakTarget, continueTarget)
        /// </summary>
        let (|For|_|) (expression : Expression) =
            match expression with
                | :? ForExpression as node -> Some <| For(node.Initializers, node.Test, node.Step |> Seq.toList, node.Body, node.BreakTarget, node.ContinueTarget)
                | _ -> None


        /// <summary>
        /// active pattern matching WhileExpression as While(condition, body, breakTarget, continueTarget)
        /// </summary>
        let (|While|_|) (expression : Expression) =
            match expression with
                | :? WhileExpression as node -> Some <| While(node.Test, node.Body, node.BreakTarget, node.ContinueTarget)
                | _ -> None


        /// <summary>
        /// active pattern matching DoWhileExpression as DoWhile(condition, body, breakTarget, continueTarget)
        /// </summary>
        let (|DoWhile|_|) (expression : Expression) =
            match expression with
                | :? DoWhileExpression as node -> Some <| DoWhile(node.Test, node.Body, node.BreakTarget, node.ContinueTarget)
                | _ -> None

        /// <summary>
        /// active pattern matching ForEachExpression as ForEach(variable, enumerable, body, breakTarget, continueTarget)
        /// </summary>
        let (|ForEach|_|) (expression : Expression) =
            match expression with
                | :? ForEachExpression as node -> Some <| ForEach(node.Variable, node.Enumerable, node.Body, node.BreakTarget, node.ContinueTarget)
                | _ -> None

        /// <summary>
        /// active pattern matching BinaryExpression as Binary(type, nodeType, left, right)
        /// </summary>
        let (|Binary|_|) (expression : Expression) =
            match expression with
                | :? BinaryExpression as node -> Some <| Binary(node.Type, node.NodeType, node.Left, node.Right)
                | _ -> None

        /// <summary>
        /// active pattern matching BlockExpression as Block([variable * value], body)
        /// </summary>
        let (|Block|_|) (expression : Expression) =
            match expression with
                | :? BlockExpression as node -> 
                    let variables = HashSet<ParameterExpression>(node.Variables)
                    let assignments = node.Expressions 
                                   |> Seq.ofType<BinaryExpression> 
                                   |> Seq.where (fun e -> e.NodeType = ExpressionType.Assign)
                                   |> Seq.choose (fun e -> match e.Left with
                                                            | :? ParameterExpression as p -> Some (p, e.Right)
                                                            | _ -> None)
                                   |> Seq.takeWhile (fun (v,_) -> variables.Contains(v))
                                   |> Seq.toList



                    Some <| Block(assignments, node.Expressions |> Seq.skip assignments.Length |> Seq.toList)
                | _ -> None

        /// <summary>
        /// active pattern matching ConditionalExpression as Conditional(condition, ifTrue, ifFalse)
        /// </summary>
        let (|Conditional|_|) (expression : Expression) =
            match expression with
                | :? ConditionalExpression as node -> Some <| Conditional(node.Test, node.IfTrue, node.IfFalse)
                | _ -> None

        /// <summary>
        /// active pattern matching DefaultExpression as Default(type)
        /// </summary>
        let (|Default|_|) (expression : Expression) =
            match expression with
                | :? DefaultExpression as node -> Some <| Default(node.Type)
                | _ -> None

        let (|Constant|_|) (expression : Expression) =
            match expression with
                | :? ConstantExpression as node -> Some <| Constant(node.Type, node.Value)
                | _ -> None

        let (|Invocation|_|) (expression : Expression) =
            match expression with
                | :? InvocationExpression as node -> Some <| Invocation(node.Expression, node.Arguments |> Seq.toList)
                | _ -> None



        let (|Index|_|) (expression : Expression) =
            match expression with
                | :? IndexExpression as node -> Some <| Index(node.Object, node.Indexer, node.Arguments |> Seq.toList)
                | _ -> None

        let (|Label|_|) (expression : Expression) =
            match expression with
                | :? LabelExpression as node -> Some <| Label(node.Target, node.DefaultValue)
                | _ -> None

        let (|Member|_|) (expression : Expression) =
            match expression with
                | :? MemberExpression as node -> Some <| Member(node.Expression, node.Member)
                | _ -> None

        let (|MemberInit|_|) (expression : Expression) =
            match expression with
                | :? MemberInitExpression as node -> Some <| MemberInit(node.NewExpression, node.Bindings)
                | _ -> None

        let (|MethodCall|_|) (expression : Expression) =
            match expression with
                | :? MethodCallExpression as node -> Some <| MethodCall(node.Object, node.Method, node.Arguments |> Seq.toList)
                | _ -> None

        let (|New|_|) (expression : Expression) =
            match expression with
                | :? NewExpression as node -> Some <| New(node.Type, node.Arguments |> Seq.toList)
                | _ -> None

        let (|NewArray|_|) (expression : Expression) =
            match expression with
                | :? NewArrayExpression as node -> Some <| NewArray(node.Type, node.Expressions |> Seq.toList)
                | _ -> None

        let (|Parameter|_|) (expression : Expression) =
            match expression with
                | :? ParameterExpression as node -> Some <| Parameter(node.Type, node.Name)
                | _ -> None

        let (|Switch|_|) (expression : Expression) =
            match expression with
                | :? SwitchExpression as node -> Some <| Switch(node.SwitchValue, node.Cases |> Seq.toList)
                | _ -> None

        let (|Try|_|) (expression : Expression) =
            match expression with
                | :? TryExpression as node -> Some <| Try(node.Body, node.Handlers |> Seq.toList, node.Finally)
                | _ -> None

        let (|Unary|_|) (expression : Expression) =
            match expression with
                | :? UnaryExpression as node -> Some <| Unary(node.Method, node.Operand)
                | _ -> None

        let (|Break|_|) (expression : Expression) =
            match expression with
                | :? GotoExpression as node when node.Kind = GotoExpressionKind.Break -> Some <| Break(node.Target)
                | _ -> None

        let (|Continue|_|) (expression : Expression) =
            match expression with
                | :? GotoExpression as node when node.Kind = GotoExpressionKind.Continue -> Some <| Continue(node.Target)
                | _ -> None

        let (|Return|_|) (expression : Expression) =
            match expression with
                | :? GotoExpression as node when node.Kind = GotoExpressionKind.Return -> Some <| Return(node.Target, node.Value)
                | _ -> None

        let (|Goto|_|) (expression : Expression) =
            match expression with
                | :? GotoExpression as node when node.Kind = GotoExpressionKind.Goto -> Some <| Goto(node.Target)
                | _ -> None

        let (|Lambda|_|) (expression : Expression) =
            match expression with
                | :? LambdaExpression as node -> Some <| Lambda(node.Parameters |> Seq.toList, node.Body)
                | _ -> None

        let (|Coerce|_|) (expression : Expression) =
            match expression with
                | :? UnaryExpression  as node when node.NodeType = ExpressionType.Convert -> Some <| Coerce(node.Operand, node.Type)
                | _ -> None



        let (|ForIntegerRangeLoop|_|) (expression : Expression) =
            match expression with
                | :? ForExpression as node -> 
                    let (variables,values) = node.Initializers
                    let variables = variables |> Seq.toList
                    let values = values |> Seq.toList |> List.map(fun ass -> match ass with | Binary(_,nt, l, r) when nt = ExpressionType.Assign -> r | _ -> ass)
                    if variables.Length = 1 && variables.[0].Type = typeof<int> then
                        let variable = List.head variables
                        let value = List.head values
                        if node.Step.Length = 1 then
                            let isSimpleStep = node.Step.[0].NodeType = ExpressionType.PostIncrementAssign
                            let upperBound = match node.Test with
                                                    | Binary(_,m, l, r) when l.Equals(variable) && m = ExpressionType.LessThan -> Some <| (Expression.Subtract(r, Expression.Constant(1)) :> Expression)
                                                    | Binary(_,m, l, r) when l.Equals(variable) && m = ExpressionType.LessThanOrEqual -> Some <| r
                                                    | _ -> None

                            if isSimpleStep then
                                match upperBound with
                                    | Some(u) -> Some <| ForIntegerRangeLoop(variable, value, u, node.Body)
                                    | None -> None
                            else
                                None
                        else
                            None
                    else
                        None
                    //Some <| For(node.Initializers, node.Test, node.Step, node.Body, node.BreakTarget, node.ContinueTarget)
                | _ -> None


    let rec private methodInfo (e : Expr) =
        match e with
            | Quotations.Patterns.Call(_,m,_) -> if m.IsGenericMethod then
                                                    m.GetGenericMethodDefinition()
                                                 else
                                                    m
            | ExprShape.ShapeCombination(o, args) -> methodInfo (Seq.head args)
            | ExprShape.ShapeVar(b) -> failwith ""
            | ExprShape.ShapeLambda(l, b) -> methodInfo b

    let private o_Addition = methodInfo <@ (+) @>
    let private o_Subtraction = methodInfo <@ (-) @>
    let private o_Multiplication = methodInfo <@ (*) @>
    let private o_Division = methodInfo <@ (/) @>

    let private o_GreaterThan = methodInfo <@ (>) @>
    let private o_GreaterThanOrEqual = methodInfo <@ (>=) @>
    let private o_LessThan = methodInfo <@ (<) @>
    let private o_LessThanOrEqual = methodInfo <@ (<=) @>

    let private m_unbox = methodInfo <@ unbox @>
    let private cbyte = methodInfo <@ fun a -> byte a @>
    let private csbyte = methodInfo <@ fun a -> sbyte a @>
    let private cint16 = methodInfo <@ fun a -> int16 a @>
    let private cuint16 = methodInfo <@ fun a -> uint16 a @>
    let private cint32 = methodInfo <@ fun a -> int32 a @>
    let private cuint32 = methodInfo <@ fun a -> uint32 a @>
    let private cint64 = methodInfo <@ fun a -> int64 a @>
    let private cuint64 = methodInfo <@ fun a -> uint64 a @>
//    let private cnativeint = methodInfo <@ fun a -> nativeint a @>
//    let private cunativeint = methodInfo <@ fun a -> unativeint a @>
    let private cfloat = methodInfo <@ fun a -> float a @>
    let private cfloat32 = methodInfo <@ fun a -> float32 a @>
    let private cdecimal = methodInfo <@ fun a -> decimal a @>
    let private cchar = methodInfo <@ fun a -> char a @>



    let private operator (m : MethodInfo) (a : Expr) (b : Expr) (t : Type) =
        let m = if m.GetGenericArguments().Length = 1 then
                    m.MakeGenericMethod(a.Type)
                else
                   m.MakeGenericMethod(a.Type, b.Type, t)
        Expr.Call(m, [a;b])

    let private conversion (s : Type) (t : Type) : MethodInfo =

        if t = typeof<byte> then
            cbyte.MakeGenericMethod(s)
        elif t = typeof<sbyte> then
            csbyte.MakeGenericMethod(s)
        elif t = typeof<int16> then
            cint16.MakeGenericMethod(s)
        elif t = typeof<uint16> then
            cuint16.MakeGenericMethod(s)
        elif t = typeof<int32> then
            cint32.MakeGenericMethod(s)
        elif t = typeof<uint32> then
            cuint32.MakeGenericMethod(s)
        elif t = typeof<int64> then
            cint64.MakeGenericMethod(s)
        elif t = typeof<uint64> then
            cuint64.MakeGenericMethod(s)
        elif t = typeof<float> then
            cfloat.MakeGenericMethod(s)
        elif t = typeof<float32> then
            cfloat32.MakeGenericMethod(s)
        elif t = typeof<decimal> then
            cdecimal.MakeGenericMethod(s)
        elif t = typeof<char> then
            cchar.MakeGenericMethod(s)

        else
            failwith "asdsd"

    type private CompilerBuilder() =
        member x.Bind(m : Option<'a>, f : 'a -> Option<'b>) : Option<'b> =
            match m with
                | Some(v) -> f v
                | None -> None
        member x.Return(v : 'a) = Some v
        member x.ReturnFrom(v : Option<'a>) = v
        member x.Zero() : Option<'a> = None

    let private compiler = CompilerBuilder()

    //Expression -> Expr
    let private variables = Dictionary<Type * string, Var>()

    let compileVariable (t : Type) (name : string) =
        match variables.TryGetValue((t, name)) with
            | (true,v) -> v
            | _ -> let v = Var(name, t, true)
                   variables.[(t, name)] <- v
                   v

    let compileParameter (p : ParameterExpression) =
        compileVariable p.Type p.Name

    let rec compile (e : Expression) = 
        match e with

            | ForIntegerRangeLoop(v,s,e,b) -> 
                compiler {
                    let! s = compile s
                    let! e = compile e
                    let! b = compile b
                    
                    return Expr.ForIntegerRangeLoop(compileParameter v, s, e, b)
                }

            | ForEach(v, e, b, _, _) ->
                compiler {
                    let v = compileParameter v
                    let! b = compile b
                    let! e = compile e

                    let elementType = v.Type

                    let enumType = typeof<IEnumerator<_>>.GetGenericTypeDefinition().MakeGenericType(elementType)
                    let enum = Var.Global(v.Name, enumType)

                    let ei = e.Type.GetInterfaces() |> Seq.tryFind (fun i -> i.Name.StartsWith("IEnumerable`1"))

                    let current = enum.Type.GetProperty("Current")
                    let getEnumerator = ei.Value.GetMethod("GetEnumerator")
                    let moveNext = typeof<System.Collections.IEnumerator>.GetMethod("MoveNext")
                    let dispose = typeof<IDisposable>.GetMethod("Dispose")

                    let current = Expr.PropertyGet(Expr.Var(enum), current)

                    let result = Expr.Let(enum, 
                                          Expr.Call(e, getEnumerator, []),
                                          Expr.Sequential(
                                              Expr.WhileLoop(
                                                Expr.Call(Expr.Var(enum), moveNext, []), 
                                                b.Substitute(fun vi -> if v = vi then Some(current) else None)
                                              ),
                                              Expr.Call(Expr.Var(enum), dispose, [])
                                          )
                                         )
                    return result

                }

            | While(c,b,_,_) ->
                compiler {
                    let! c = compile c
                    let! b = compile b
                    
                    return Expr.WhileLoop(c,b)
                }

            | DoWhile(c,b,_,_) ->
                compiler {
                    let! c = compile c
                    let! b = compile b
                    
                    //TODO: better representation of do { ... } while(...);
                    return Expr.Sequential(b, Expr.WhileLoop(c,b))
                }

            | Block(variables,body) ->
                let body = body |> List.choose compile

                if body.Length = 0 then
                    None
                else
                    let body = List.tail body |> List.fold (fun l r -> Expr.Sequential(l,r)) (List.head body)
                    let variables = variables |> List.choose (fun (var, value) -> compiler { let! value = compile value in return (compileParameter var, value) })

                    if variables.Length = 1 then
                        let (variable,value) = List.head variables
                        let mutable variable = variable
                        let a = fun x -> x

                        let funcType = typeof<Microsoft.FSharp.Core.FSharpFunc<_,_>>.GetGenericTypeDefinition()
                        if variable.Type.Name.StartsWith("Func") then
                            let rec makeFunctionType (t : list<Type>) =
                                match t with
                                    | [s;t] -> funcType.MakeGenericType(s,t)
                                    | t::ts -> funcType.MakeGenericType(t, makeFunctionType ts)
                                    | _ -> failwith "not possible"
                            
                            variable <- Var(variable.Name, makeFunctionType <| (Seq.toList <| variable.Type.GetGenericArguments()), variable.IsMutable)

                        Some <| Expr.Let(variable, value, body)
                    else
                        Some <|  body
            

            | Binary(t, ExpressionType.Assign, Member(e,m), value) ->
                compiler {
                    let! value = compile value
                    if e <> null then
                        let! e = compile e

                        match m with
                            | :? FieldInfo as f -> return Expr.FieldSet(e, f, value)
                            | :? PropertyInfo as p -> return Expr.PropertySet(e, p, value)
                            | _ -> failwith "not implemented"
                    else
                        match m with
                            | :? FieldInfo as f -> return Expr.FieldSet(f, value)
                            | :? PropertyInfo as p -> return Expr.PropertySet(p, value)
                            | _ -> failwith "not implemented"   
                }


            | Binary(t, nt,l,r) -> 
                compiler {
                    let! l = compile l
                    let! r = compile r
                            
                    match nt with
                        | ExpressionType.Subtract -> return operator o_Subtraction l r t
                        | ExpressionType.Add -> return operator o_Addition l r t
                        | ExpressionType.Multiply -> return operator o_Multiplication l r t
                        | ExpressionType.Divide -> return operator o_Division l r t

                        | ExpressionType.GreaterThan -> return operator o_GreaterThan l r t
                        | ExpressionType.GreaterThanOrEqual -> return operator o_GreaterThanOrEqual l r t
                        | ExpressionType.LessThan -> return operator o_LessThan l r t
                        | ExpressionType.LessThanOrEqual -> return operator o_LessThanOrEqual l r t

                        | ExpressionType.Assign -> match l with
                                                    | Quotations.Patterns.Var(v) -> return Expr.VarSet(v, r)
                                                    | _ -> failwith ""

                        | _ -> failwith ""
                }

            | Lambda(parameters,body) -> 
                compiler {
                    let rec buildLambda body args =
                        match args with
                            | [] -> body
                            | x::xs -> Expr.Lambda(x, buildLambda body xs)

                    let parameters = parameters |> List.map compileParameter
                    let! b = compile body
                    return buildLambda b parameters
                }

            | NewArray(t,args) ->
                compiler {
                    let args = args |> List.choose compile
                    return Expr.NewArray(t.GetElementType(), args)
                }

            | Constant(t,v) -> Some <| Expr.Value(v,t)
            | Parameter(t,n) -> Some <| Expr.Var(compileVariable t n)
            | Return(_,v) -> compile v
            | Label(_) -> None

            | Conditional(c,t,f) ->
                compiler {
                    let! c = compile c
                    let! t = compile t
                    let! f = compile f

                    return Expr.IfThenElse(c, t, f)
                }

            | Default(t) -> 
                compiler {
                    if t = typeof<System.Void> then
                        return Expr.Value(() :> obj, typeof<unit>)
                    else
                        return Expr.DefaultValue(t)
                }
            
            | MethodCall(e, m, args) ->
                compiler {
                    let args = args |> List.choose compile
                    
                    if e <> null then
                        let! e = compile e
                        return Expr.Call(e, m, args)
                    else
                        return Expr.Call(m, args)
                }

            | Invocation(func,args) ->
                let mutable ft = func.Type
                let n = func.ToString()

                let funcType = typeof<Microsoft.FSharp.Core.FSharpFunc<_,_>>.GetGenericTypeDefinition()
                if ft.Name.StartsWith("Func") then
                    let rec makeFunctionType (t : list<Type>) =
                        match t with
                            | [s;t] -> funcType.MakeGenericType(s,t)
                            | t::ts -> funcType.MakeGenericType(t, makeFunctionType ts)
                            | _ -> failwith "not possible"
                    ft <- makeFunctionType (ft.GetGenericArguments() |> Seq.toList)

                let f = compileVariable ft n

                compiler {
                    let args = args |> List.choose compile |> List.map (fun e -> [e])

                    return Expr.Applications(Expr.Var(f), args)

                }
          
            | Coerce(o,t) ->
                compiler {
                    let! o  = compile o

                    let ot = o.Type
                    let tt = t

                    if ot.IsPrimitive && tt.IsPrimitive then
                        let m = conversion ot tt
                        return Expr.Call(m, [o])
                    elif tt.IsAssignableFrom(ot) then
                        return Expr.Coerce(o, t)
                    else
                        return Expr.Call(m_unbox.MakeGenericMethod(tt), [Expr.Coerce(o, typeof<obj>)])
                }

            

            | Member(e, m) ->
                compiler {
                    if e <> null then
                        let! e = compile e
                        match m with
                            | :? FieldInfo as f -> return Expr.FieldGet(e, f)
                            | :? PropertyInfo as p -> return Expr.PropertyGet(e,p)
                            | _ -> failwith "not implemented"
                    else
                        match m with
                            | :? FieldInfo as f -> return Expr.FieldGet(f)
                            | :? PropertyInfo as p -> return Expr.PropertyGet(p)
                            | _ -> failwith "not implemented"   
                }

            | e -> failwithf "unsupported expression: %A" e


    //Expr -> F# Code
    [<AutoOpen>]
    module Pretty =
        open Quotations.Patterns
        open Quotations.DerivedPatterns

        module String =
            let join (sep : string) (strings : seq<string>) =
                String.Join(sep, strings)

        let lineBreak = System.Text.RegularExpressions.Regex("\r\n")
        let indent (str : string) =
            let lines = lineBreak.Split(str)
            lines |> Seq.map (fun str -> sprintf "    %s" str)
                  |> String.join "\r\n"

        let lines (str : string) =
            let lines = lineBreak.Split(str)
            lines.Length

        let rec prettyPrint (e : Expr) =
            match e with
                | Let(var, value, b) -> 
                    let value = prettyPrint value
                    let b = prettyPrint b
                    if var.IsMutable then
                        sprintf "let mutable %s = %s\r\n%s" var.Name value b
                    else
                        sprintf "let %s = %s\r\n%s" var.Name value b
                | WhileLoop(c,b) -> 
                    let c = prettyPrint c
                    let b = prettyPrint b
                    sprintf "while %s do\r\n%s" c (indent b)
                | ForIntegerRangeLoop(v,i,e,b) ->
                    let i = prettyPrint i
                    let e = prettyPrint e
                    let b = prettyPrint b
                    sprintf "for %s in %s..%s do\r\n%s" v.Name i e (indent b)
                | Lambda(v,b) ->
                    let rec parameters l =
                        match l with
                            | Lambda(p, b) -> let (r,parameters) = parameters b
                                              (r, p::parameters)
                            | _ -> (l, [])

                    let (r,p) = parameters e
                    let p = p |> Seq.map(fun pi -> pi.Name) |> String.join " "
                    let b = prettyPrint r

                    if lines b = 1 then
                        sprintf "fun %s -> %s" p b
                    else
                        sprintf "fun %s ->\r\n%s" p (indent b)
                | Sequential(l,r) ->
                    let l = prettyPrint l
                    let r = prettyPrint r
                    sprintf "%s\r\n%s" l r 
                | VarSet(v,value) ->
                    let value = prettyPrint value
                    sprintf "%s <- %s" v.Name value
                
                | Value(v,t) -> if t = typeof<unit> then "()" else sprintf "%A" v

                | Call(t,m,args) ->
                    let t = match t with | Some(t) -> prettyPrint t | _ -> m.DeclaringType.Name
                    let args = args |> List.map prettyPrint

                    match m.Name with
                        | "op_Addition" -> sprintf "%s + %s" (List.head args) (List.nth args 1)
                        | "op_Subtraction" -> sprintf "%s - %s" (List.head args) (List.nth args 1)
                        | "op_Multiply" -> sprintf "%s * %s" (List.head args) (List.nth args 1)
                        | "op_Division" -> sprintf "%s / %s" (List.head args) (List.nth args 1)
                        | "op_GreaterThan" -> sprintf "%s > %s" (List.head args) (List.nth args 1)
                        | "op_GreaterThanOrEqual" -> sprintf "%s >= %s" (List.head args) (List.nth args 1)
                        | "op_LessThan" -> sprintf "%s < %s" (List.head args) (List.nth args 1)
                        | "op_LessThanOrEqual" -> sprintf "%s <= %s" (List.head args) (List.nth args 1)
                        | "op_PipeRight" -> sprintf "%s |> %s" (List.head args) (List.nth args 1)
                        | "op_PipeLeft" -> sprintf "%s <| %s" (List.head args) (List.nth args 1)
                        | "op_ColonEquals" -> sprintf "%s := %s" (List.head args) (List.nth args 1)
                        | "op_Dynamic" -> sprintf "%s?%s" (List.head args) (List.nth args 1)

                        | "op_DynamicAssignment" -> sprintf "%s?%s <- %s" (List.head args) (List.nth args 1) (List.nth args 2)

                        | "op_Dereference" -> sprintf "!%s" (List.head args)
                        | "op_UnaryNegation" -> sprintf "-%s" (List.head args)
                        
                        | _ -> sprintf "%s.%s(%s)" t m.Name (String.join ", " args)

                | Application(f, a) ->
                    let f = prettyPrint f
                    let a = prettyPrint a
                    sprintf "%s (%s)" f a

                | PropertyGet(t,p,i) ->
                    let t = match t with | Some(t) -> prettyPrint t | _ -> p.DeclaringType.Name
                    let i = i |> List.map prettyPrint

                    if i.Length > 0 then
                        sprintf "%s.%s.[%s]" t p.Name (String.join ", " i)
                    else
                        sprintf "%s.%s" t p.Name

                | FieldGet(t,f) ->
                    let t = match t with 
                                | Some t -> prettyPrint t
                                | None -> f.DeclaringType.Name

                    sprintf "%s.%s" t f.Name


//                | Coerce(e,t) ->
//                    let e = prettyPrint e
//                    sprintf "%s :> %s" e t.Name

                | NewUnionCase(c,args) ->
                    let t = c.DeclaringType
                    if t.IsGenericType && t.GetGenericTypeDefinition() = typeof<list<_>>.GetGenericTypeDefinition() then
                        let rec elements le =
                            match le with
                                | NewUnionCase(c,[h;t]) when c.Name = "Cons" -> h::(elements t)
                                | _ -> []
                        let e = elements e |> Seq.map prettyPrint
                        sprintf "[%s]" (String.join "; " e)
                    else
                        let args = args |> Seq.map prettyPrint
                        sprintf "%s(%s)" c.Name (String.join ", " args)

                | NewArray(t,args) ->
                    let args = args |> Seq.map prettyPrint |> String.join "; "
                    sprintf "[| %s |]" args

                | TryFinally(b,f) ->
                    let b = prettyPrint b
                    let f = prettyPrint f
                    sprintf "try\r\n%s\r\nfinally\r\n%s" (indent b) (indent f)

                | IfThenElse(c,t,f) ->
                    let c = prettyPrint c
                    let t = prettyPrint t

                    if f <> Expr.Value(()) then
                        let f = prettyPrint f

                        if t.Length + f.Length < 20 then
                            sprintf "if %s then %s\r\nelse %s" c t f
                        else
                            sprintf "if %s then\r\n%s\r\nelse\r\n%s" c (indent t) (indent f) 
                    else
                        sprintf "if %s then\r\n%s" c (indent t)

                | e -> sprintf "%A" e

    type Expr with
        static member TryGetReflected m =
            match Quotations.Expr.TryGetReflectedDefinition m with
                | Some(v) -> Some(v)
                | None -> match CSharp.Quotations.Expr.TryGetReflectedDefinition m with
                            | (true,v) -> let argNames = if m.IsGenericMethod then m.GetGenericArguments() else [||]
                                          compile <| v
                            | _ -> None

        member x.Code = prettyPrint x
    
module Compiler =


    type CompilerState<'a>(types : HashSet<Type>, methods : HashSet<MethodInfo>, state : 'a) =
        member x.UsedTypes = types :> seq<Type>
        member x.UsedMethods = methods :> seq<MethodInfo>
        member x.State = state

        member x.WithType (t : Type) =
            let newTypes = HashSet(types)
            if newTypes.Add(t) then
                CompilerState<'a>(newTypes, methods, state)
            else
                x

        member x.WithMethod (mi : MethodInfo) =
            let newMethods = HashSet(methods)
            if newMethods.Add(mi) then
                CompilerState<'a>(types, newMethods, state)
            else
                x

        member x.WithState(newState : 'a) =
            CompilerState<'a>(types, methods, newState)

    type CompilerError<'a> = Success of 'a | Error of string

    type CompilerResult<'a, 's> = CompilerState<'s> -> CompilerState<'s> * CompilerError<'a>

    type CompilerBuilder() =
        member x.Bind(m : CompilerResult<'a,'s>, f : 'a -> CompilerResult<'b,'s>) : CompilerResult<'b,'s> =
            fun st -> let (st', m') = m st
                      match m' with
                        | Success m' -> let r = f m'
                                        r st' 
                        | Error e -> (st', Error e)

        member x.Return(v : 'a) : CompilerResult<'a, 's> =
            fun st -> (st,Success v)

        member x.ReturnFrom(m : CompilerResult<'a,'s>) : CompilerResult<'a,'s> =
            m

    let compiler = CompilerBuilder()

    module Compiler =
        let rec map (f : 'a -> CompilerResult<'b,'s>) (list : list<'a>) : CompilerResult<list<'b>, 's> =
            compiler {
                match list with
                    | x :: xs -> let! h = f x
                                 let! rest = map f xs
                                 return h::rest
                    | [] -> return []
            }
    
        let rec fold (f : 'x -> 'a -> CompilerResult<'x, 's>) (seed : 'x) (list : list<'a>) : CompilerResult<'x,'s> =
            compiler {
                match list with
                    | x::xs -> let! rest = fold f seed xs
                               let! h = f rest x
                               return h
                    | [] -> return seed
            }

    let x<'a> = compiler { return 1; }

    let test() =
        compiler {
            let! a = x
            return 2 * a
        }

    type ICompiler<'e,'c> =
        abstract member CompileTypeName : Type -> 'c
        abstract member CompileTypeDefinition : Type -> 'c
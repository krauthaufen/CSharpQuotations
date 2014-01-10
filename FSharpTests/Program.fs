// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open CSharp.Quotations
open CSharp.Quotations.QuotationCompiler
open Microsoft.FSharp.Quotations
open System.Reflection
open System


[<Microsoft.FSharp.Core.ReflectedDefinition>]
module FSharpTest =
    type Id = Id of int
        
    let Some (a : int) (b : int) =
            
        let mutable f = fun xi yi -> yi * xi

        let mutable x = a * b

        if x < 10 then
            x <- x + Test.TestClass.Test(10, 123)
        else
            x <- f x  (x - 10)

        while x > 10 do
            x <- x / 4

        for i in 1..10 do
            x <- x * i

        x

let printDefinition (t : Type) (name : string) =
    let mi = t.GetMethod(name, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static)
    match Expr.TryGetReflected(mi) with
        | Some(ex) -> printfn "%s" ex.Code
        | _ -> printfn "no definition"

let runTest() =
        
    let csharpType = typeof<Test.TestClass>
    let fsharpType = typeof<FSharpTest.Id>.DeclaringType

    printfn "C#"
    printDefinition csharpType "Some"
       
    printfn "\r\nF#"
    printDefinition fsharpType "Some"


[<EntryPoint>]
let main argv = 
    runTest()
    0 // return an integer exit code

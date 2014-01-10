CSharpQuotations
================

Quasi Quotations for C#

Goal
====

The goal of this project is to provide Quasi-Quotations for C# in a simple and robust way, similar to those integrated in F#.
The C# Quotations are represented using Linq-Expression trees and can therefore be modified and even compiled to IL code.

Quasi Quotations allow to implement a huge variety of cross-compiling tools, etc. and I learned to like them a lot when creating software in F#.

State
=====

Currently the basic quoting mechanism and a straightforward translation to Linq-Expressions is implemented.
Furthermore a very sketchy cross-compiler translating to FSharp-Quotation-Expressions is included.


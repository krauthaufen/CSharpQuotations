using Roslyn.Compilers.CSharp;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;

namespace CSharpQuoter
{
    class Program
    {
        private static Guid HashMD5 = new Guid(0x406ea660, 0x64cf, 0x4c82, 0xb6, 0xf0, 0x42, 0xd4, 0x81, 0x72, 0xa7, 0x99);
        private static Guid HashSHA1 = new Guid(0xff1816ec, 0xaa5e, 0x4d10, 0x87, 0xf7, 0x6f, 0x49, 0x63, 0x83, 0x34, 0x60);


        private static string Clean(SyntaxNode node)
        {
            var code = node.NormalizeWhitespace().ToString();

            code = code.Replace("\"", "\" + \"\\\"\" + @\"");

            code = code.Replace("\r", "\\r");
            code = code.Replace("\n", "\\n");

            return string.Format("@\"{0}\"", code);
        }

        private static void Run(string inputFile, string outputFile)
        {
            var syntax = SyntaxTree.ParseFile(inputFile);

            var root = syntax.GetRoot();



            var lineHidden = new[] { Syntax.Trivia(Syntax.LineDirectiveTrivia(Syntax.Token(SyntaxKind.HashToken), Syntax.Token(SyntaxKind.LineKeyword).WithTrailingTrivia(Syntax.Space), Syntax.Token(SyntaxKind.HiddenKeyword), Syntax.Token(SyntaxKind.None), Syntax.Token(SyntaxKind.EndOfDirectiveToken), true).WithTrailingTrivia(Syntax.CarriageReturnLineFeed)) };
            var lineDefault = new[] { Syntax.Trivia(Syntax.LineDirectiveTrivia(Syntax.Token(SyntaxKind.HashToken), Syntax.Token(SyntaxKind.LineKeyword).WithTrailingTrivia(Syntax.Space), Syntax.Token(SyntaxKind.DefaultKeyword), Syntax.Token(SyntaxKind.None), Syntax.Token(SyntaxKind.EndOfDirectiveToken), true).WithLeadingTrivia(Syntax.CarriageReturnLineFeed)) };

            Func<SyntaxNode, SyntaxTrivia[]> lineReset = o =>
            {
                var span = o.GetLocation().GetLineSpan(true).StartLinePosition.Line + 1;
                return new[] { Syntax.Trivia(Syntax.LineDirectiveTrivia(Syntax.Token(SyntaxKind.HashToken), Syntax.Token(SyntaxKind.LineKeyword).WithTrailingTrivia(Syntax.Space), Syntax.Literal(span.ToString(), span).WithTrailingTrivia(Syntax.Space), Syntax.Literal("\"" + inputFile + "\"", inputFile), Syntax.Token(SyntaxKind.EndOfDirectiveToken), true).WithLeadingTrivia(Syntax.CarriageReturnLineFeed)) };
            };



            var reflectedMethods = root.DescendantNodes().OfType<MethodDeclarationSyntax>().Where(c => c.AttributeLists.Any(l => l.Attributes.Any(a => a.Name.ToString() == "ReflectedDefinition"))).Concat<SyntaxNode>(
                                   root.DescendantNodes().OfType<PropertyDeclarationSyntax>().Where(c => c.AttributeLists.Any(l => l.Attributes.Any(a => a.Name.ToString() == "ReflectedDefinition")))).Concat<SyntaxNode>(
                                   root.DescendantNodes().OfType<ClassDeclarationSyntax>().Where(c => c.AttributeLists.Any(l => l.Attributes.Any(a => a.Name.ToString() == "ReflectedDefinition"))).SelectMany(cd => cd.DescendantNodes().Where(d => d is MethodDeclarationSyntax || d is PropertyDeclarationSyntax)))
                                   .ToArray();


            var namespaces = root.DescendantNodes().OfType<UsingDirectiveSyntax>().Select(u => Tuple.Create(u.Alias, u.Name.ToString())).ToArray();

            root = root.ReplaceNodes(reflectedMethods,
                    (o, r) =>
                    {
                        var cd = r as ClassDeclarationSyntax;
                        if (cd == null)
                        {

                            var aa = Syntax.AttributeArgumentList(Syntax.SeparatedList(Syntax.AttributeArgument(Syntax.IdentifierName(Clean(r)))));
                            var al = Syntax.AttributeList(Syntax.SeparatedList(Syntax.Attribute(Syntax.IdentifierName("CompiledReflectedDefinition"), aa)));


                            var namespaceAtt = Syntax.AttributeList(Syntax.SeparatedList<AttributeSyntax>(
                                                   namespaces
                                                  .Select(n =>
                                                  {
                                                      var attName = Syntax.IdentifierName("CompiledNamespace");

                                                      string key = null;
                                                      var value = n.Item2;
                                                      if (n.Item1 != null) key = n.Item1.ToString();


                                                      SeparatedSyntaxList<AttributeArgumentSyntax> args;
                                                      if (key == null) args = Syntax.SeparatedList(Syntax.AttributeArgument(Syntax.IdentifierName(string.Format("\"{0}\"", value))));
                                                      else args = Syntax.SeparatedList(new[]{ Syntax.AttributeArgument(Syntax.IdentifierName(string.Format("\"{0}\"", key))),
                                                                                                  Syntax.AttributeArgument(Syntax.IdentifierName(string.Format("\"{0}\"", value))) },
                                                                                       new[] { Syntax.Token(SyntaxKind.CommaToken) });

                                                      var attAargs = Syntax.AttributeArgumentList(args);
                                                      return Syntax.Attribute(attName, attAargs);
                                                  }),
                                                   namespaces.Skip(1).Select(_ => Syntax.Token(SyntaxKind.CommaToken))
                                               ));


                            var indentElement = r.GetLeadingTrivia().Where(t => t.Kind == SyntaxKind.WhitespaceTrivia).LastOrDefault();
                            var indent = indentElement != null ? new[] { indentElement } : new SyntaxTrivia[0];
                            //var other = r.GetLeadingTrivia().TakeWhile(t => t != indentElement).ToArray();

                            var leading = r.GetLeadingTrivia();
  
                            al = al.WithTrailingTrivia(Syntax.CarriageReturnLineFeed);
                            r = r.WithLeadingTrivia(indent);

                            namespaceAtt = namespaceAtt.WithTrailingTrivia(Syntax.CarriageReturnLineFeed);

                            al = al.WithLeadingTrivia(al.GetLeadingTrivia().Concat(lineHidden));
                            namespaceAtt = namespaceAtt.WithTrailingTrivia(indent.Concat(lineReset(o).Concat(al.GetTrailingTrivia())));

                            al = al.WithLeadingTrivia(leading.Concat(al.GetLeadingTrivia()));

                            var pd = r as PropertyDeclarationSyntax;
                            var md = r as MethodDeclarationSyntax;

                            if (pd != null) return pd.WithAttributeLists(Syntax.List(new[] { al, namespaceAtt }.Concat(pd.AttributeLists)));
                            else if (md != null) return md.WithAttributeLists(Syntax.List(new[] { al, namespaceAtt }.Concat(md.AttributeLists)));
                            else return r;
                        }
                        else
                        {
                            var arg = Clean(o);

                            cd = cd.ReplaceNodes(cd.DescendantNodes().OfType<MemberDeclarationSyntax>(),
                                (oi, ri) =>
                                {
                                    var aa = Syntax.AttributeArgumentList(Syntax.SeparatedList(Syntax.AttributeArgument(Syntax.IdentifierName(Clean(ri)))));
                                    var al = Syntax.AttributeList(Syntax.SeparatedList(Syntax.Attribute(Syntax.IdentifierName("CompiledReflectedDefinition"), aa)));

                                    var indent = ri.GetLeadingTrivia().Where(t => t.Kind != SyntaxKind.EndOfLineTrivia);
                                    al = al.WithLeadingTrivia(indent).WithTrailingTrivia(Syntax.CarriageReturnLineFeed);
                                    ri = ri.WithLeadingTrivia(indent);

                                    al = al.WithLeadingTrivia(lineHidden.Concat(al.GetLeadingTrivia()));
                                    al = al.WithTrailingTrivia(lineReset(oi).Concat(al.GetTrailingTrivia()));


                                    var pd = ri as PropertyDeclarationSyntax;
                                    var md = ri as MethodDeclarationSyntax;

                                    if (pd != null) return pd.AddAttributeLists(al);
                                    else if (md != null) return md.AddAttributeLists(al);
                                    else return ri;
                                });

                            return cd;
                        }
                    });

            var code = root.ToString();

            if (reflectedMethods.Length != 0)
            {
                code = string.Format("#line 1 \"{0}\"\r\n", inputFile) + code;



                var md5 = new MD5CryptoServiceProvider();
                var stream = new FileStream(inputFile, FileMode.Open);
                var hash = md5.ComputeHash(stream);
                stream.Close();

                var hashString = BitConverter.ToString(hash).Replace("-", string.Empty);// string.Concat(hash.Select(h => h.ToString("{x00}")));


                var sha1 = new SHA1CryptoServiceProvider();
                stream = new FileStream(inputFile, FileMode.Open);
                var hashSHA = sha1.ComputeHash(stream);
                stream.Close();

                var hashStringSHA = BitConverter.ToString(hashSHA).Replace("-", string.Empty);// string.Concat(hash.Select(h => h.ToString("{x00}")));



                //http://msdn.microsoft.com/en-us/library/ms173226(v=vs.110).aspx
                //9c fa 21 51 af a6 29 10 45 27 7a 9f e0 df ec 42
                //c1 35 5b 22 8c 63 68 f 71 6a 43 e8 a9 2a 61 14

                Console.WriteLine(hashString);

                code = string.Format("#pragma checksum \"{0}\" \"{{{1}}}\" \"{2}\"\r\n", inputFile, HashMD5, hashString) +
                    //string.Format("#pragma checksum \"{0}\" \"{{{1}}}\" \"{2}\"\r\n", Path.Combine(Path.GetDirectoryName(inputFile), Path.GetFileNameWithoutExtension(inputFile)), HashSHA1, hashStringSHA) + 
                       code;
            }
            //#pragma checksum "file.cs" "{3673e4ca-6098-4ec1-890f-8fceb2a794a2}" "{012345678AB}" // New checksum
            File.WriteAllText(outputFile, code);
        }

        private static void Error(string format, params object[] args)
        {
            Console.WriteLine(format, args);
            Environment.Exit(1);
        }

        private static void Usage()
        {
            Error("usage: csq.exe <mode> <input> [-o <output>] [-b <backupDir>]");
        }

        private static string FindArgument(string[] args, params string[] flags)
        {
            for (int i = 1; i < args.Length - 1; i++)
            {
                if (flags.Contains(args[i])) return args[i + 1];
            }

            return null;
        }


        public static void Main(string[] args)
        {

            if (args.Length < 2) Usage();
            var mode = args[0];

            var inputFiles = Directory.GetFiles(args[1], "*.cs");

            if (mode == "c" || mode == "compile")
            {
                var outputDir = FindArgument(args, "-o", "--output");
                var backupDir = FindArgument(args, "-b", "--backupDir", "--backupdir");
                if (outputDir == null) outputDir = args[1];

                if (outputDir == null && backupDir == null)
                    Usage();

                foreach (var inputFile in inputFiles)
                {
                    if (!File.Exists(inputFile))
                        Error("could not find input file \"{0}\"", inputFile);

                    var outputFile = Path.Combine(outputDir, Path.GetFileName(inputFile));
                    

                    if (backupDir != null)
                    {
                        if (!File.Exists(backupDir))
                            Directory.CreateDirectory(backupDir);

                        var backupFile = Path.Combine(backupDir, Path.GetFileName(inputFile));
                        File.Copy(inputFile, backupFile, true);
                        File.SetLastAccessTime(backupFile, File.GetLastAccessTime(inputFile));
                        File.SetLastWriteTime(backupFile, File.GetLastWriteTime(inputFile));
                        File.SetCreationTime(backupFile, File.GetCreationTime(inputFile));

                    }

                    Run(inputFile, outputFile);

                    if (backupDir != null)
                    {
                        File.Copy(outputFile, Path.Combine(backupDir, Path.GetFileName(inputFile) + ".gen"), true);
                    }
                }

            }
            else if (mode == "r" || mode == "restore")
            {
                var backupDir = FindArgument(args, "-b", "--backupDir", "--backupdir");

                if (backupDir == null)
                    Usage();

                foreach (var file in inputFiles)
                {
                    var backupFile = Path.Combine(backupDir, Path.GetFileName(file));

                    if (!File.Exists(backupFile))
                        Error("could not find backup file \"{0}\"", backupFile);


                    File.Copy(backupFile, file, true);
                    File.SetLastAccessTime(file, File.GetLastAccessTime(backupFile));
                    File.SetLastWriteTime(file, File.GetLastWriteTime(backupFile));
                    File.SetCreationTime(file, File.GetCreationTime(backupFile));
                }
            }
            else Usage();

        }
    }
}

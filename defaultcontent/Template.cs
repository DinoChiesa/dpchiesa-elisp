// @BASEFILENAME@
// ------------------------------------------------------------------
//
// Description goes here....
//
// Author: @AUTHOR@
// created on host: @ENV(COMPUTERNAME)@
// initialized on: @DATE@
//
// last saved: <2012-March-24 02:08:46>
// ------------------------------------------------------------------
//
@INSERTFILE(c:/Users/Dino/elisp/defaultcontent/Copyright.txt)@//
// ------------------------------------------------------------------

using System;
using System.Reflection;


// to allow fast ngen
[assembly: AssemblyTitle("@BASEFILENAME@")]
[assembly: AssemblyDescription("insert purpose here")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("Dino Chiesa")]
[assembly: AssemblyProduct("Tools")]
[assembly: AssemblyCopyright("Copyright (c) Dino Chiesa 2010")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyVersion("1.1.1.1")]


namespace Ionic.ToolsAndTests
{

  public class @BASEFILENAMELESSEXTENSION@
  {
    // ctor
    public @BASEFILENAMELESSEXTENSION@ () {}

    string _positionalParam1;
    string _param1;
    int _intParam = DefaultIntParamValue;
    bool _flag1;
    int _size;

    private readonly int DefaultIntParamValue = -99;

    // ctor
    public @BASEFILENAMELESSEXTENSION@ (string[] args)
    {
        for (int i=0; i < args.Length; i++)
        {
            switch (args[i])
            {
                case "-stringArg":
                    i++;
                    if (args.Length <= i) throw new ArgumentException(args[i]);
                    _param1 = args[i];
                    break;

                case "-s":
                    i++;
                    if (args.Length <= i) throw new Exception(args[i-1]);
                    if (args[i].ToUpper().EndsWith("K"))
                        _size = System.Int32.Parse(args[i].Substring(0,args[i].Length-1)) * 1024;
                    else if (args[i].ToUpper().EndsWith("KB"))
                        _size = System.Int32.Parse(args[i].Substring(0,args[i].Length-2)) * 1024;
                    else if (args[i].ToUpper().EndsWith("M"))
                        _size = System.Int32.Parse(args[i].Substring(0,args[i].Length-1)) * 1024*1024;
                    else if (args[i].ToUpper().EndsWith("MB"))
                        _size = System.Int32.Parse(args[i].Substring(0,args[i].Length-2)) * 1024*1024;
                    else
                        _size = Int32.Parse(args[i]);
                    break;


                case "-intArg":
                    i++;
                    if (args.Length <= i) throw new ArgumentException(args[i]);
                    if (_intParam != DefaultIntParamValue)
                        throw new ArgumentException(args[i]);
                    if (args[i].StartsWith("0x"))
                        _intParam = System.Int32.Parse(args[i].Substring(2), System.Globalization.NumberStyles.AllowHexSpecifier );
                    else
                        _intParam = System.Int32.Parse(args[i]);
                    break;


                case "-boolarg":
                    _flag1 = true;
                    break;

                case "-?":
                    throw new ArgumentException(args[i]);

                default:
                    if (_positionalParam1 != null)
                        throw new ArgumentException(args[i]);

                    _positionalParam1 = args[i];
                    break;
            }
        }

        // default values
        if (_positionalParam1 == null)
            _positionalParam1 = "default.value.for.positional.param";

        if (_param1 == null)
            _param1 = "default.value.for.param1";

        if (_param2 == 0)
            _param2 = DEFAULT_value_for_param2;

    }

    public void Run()
    {

        @DOT@

    }

    public static void Usage()
    {
        Console.WriteLine("\n@BASEFILENAMELESSEXTENSION@: <usage statement here>.\n");
        Console.WriteLine("Usage:\n  @BASEFILENAMELESSEXTENSION@ [-arg1 <value>] [-arg2]");
    }


    public static void Main(string[] args)
    {
      try
      {
        new @BASEFILENAMELESSEXTENSION@(args)
            .Run();
      }
      catch (System.Exception exc1)
      {
        Console.WriteLine("Exception: {0}", exc1.ToString());
        Usage();
      }
    }

  }
}



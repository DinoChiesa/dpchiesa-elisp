// @BASEFILENAME@
// ------------------------------------------------------------------
//
// Description goes here....
//
// Author: @AUTHOR@
// Created @DATE@
//
// Last saved: <2013-February-10 10:43:02>
// ------------------------------------------------------------------
//
@INSERTFILE(/Users/Dino/elisp/defaultcontent/Copyright.txt)@
//
// ------------------------------------------------------------------

package com.ionic.whatever;

import java.util.Hashtable;
import java.util.Enumeration;

public class @BASEFILENAMELESSEXTENSION@ {
    private final String patternString = "-([cv])"; // cmd line args supported

    // public @BASEFILENAMELESSEXTENSION@ () {} // uncomment if wanted

    public @BASEFILENAMELESSEXTENSION@ (String[] args)
        throws java.lang.Exception {
        SetCommandLineArgs(args, patternString);
    }


    private java.util.Hashtable<String, String> CommandLineArgs =
        new java.util.Hashtable<String, String> ();

    private void SetCommandLineArgs(String[] args, String patternString)
        throws java.lang.Exception {
        // Don't bother if no command line args were passed
        if (args == null) return;
        if (args.length == 0) return;

        // Parse command line args for args in the following format:
        //   /argname:argvalue /argname:argvalue /argname:argvalue ...

        // String patternString = "\\/([^:]+):(\\S+)";

        java.util.regex.Pattern p = java.util.regex.Pattern.compile(patternString);

        int L = args.length;
        for(int i=0; i < L; i++) {
            String arg = args[i];
            java.util.regex.Matcher m = p.matcher(arg);
            if (!m.matches()) {
                throw new java.lang.Exception("The command line arguments are improperly formed. Use a form like '-name value'.");
            }

            if (i+1 < L) {
                i++;
                CommandLineArgs.put(m.group(1), args[i]);
            }
            else {
                throw new java.lang.Exception("Incorrect arguments.");
            }
        }
    }

    private void MaybeShowCommandLineArgs() {
        String verbose = CommandLineArgs.get("v");
        if (verbose != null) {
            Enumeration e = CommandLineArgs.keys();
            while(e.hasMoreElements()) {
                // iterate through Hashtable keys Enumeration
                String s = (String) e.nextElement();
                System.out.println(s + ": " + CommandLineArgs.get(s));
            }
        }
    }

    public void Run() {

        MaybeShowCommandLineArgs();

        Enumeration e = CommandLineArgs.keys();
        String s;
        // iterate through Hashtable keys Enumeration
        while(e.hasMoreElements()) {
            s = (String) e.nextElement();
            System.out.println(s + ": " + CommandLineArgs.get(s));
        }

        @DOT@

    }


    public static void Usage() {
        System.out.println("@BASEFILENAMELESSEXTENSION@: <usage statement here>.\n");
        System.out.println("Usage:\n  java @BASEFILENAMELESSEXTENSION@ /arg1:<value> /arg1:<Value>");
    }


    public static void main(String[] args) {
        try {
            @BASEFILENAMELESSEXTENSION@ me = new @BASEFILENAMELESSEXTENSION@(args);
            me.Run();
        }
        catch (java.lang.Exception exc1) {
            System.out.println("Exception while doing whatever it is I was doing:" + exc1.toString());
            exc1.printStackTrace();
        }
    }

}

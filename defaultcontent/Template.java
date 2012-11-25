// @BASEFILENAME@
// ------------------------------------------------------------------
//
// Description goes here....
//
// Author: @AUTHOR@
// Created @DATE@
//
// Last saved: <2012-November-19 16:44:18>
// ------------------------------------------------------------------
//
@INSERTFILE(/Users/Dino/elisp/defaultcontent/Copyright.txt)@
//
// ------------------------------------------------------------------

package com.ionic.whatever;

import java.util.Hashtable;
import java.util.Enumeration;

public class @BASEFILENAMELESSEXTENSION@ {
    // ctor
    public @BASEFILENAMELESSEXTENSION@ () {}

    public @BASEFILENAMELESSEXTENSION@ (String[] args)
        throws java.lang.Exception {
        SetCommandLineArgs(args);
    }


    private java.util.Hashtable<String, String> CommandLineArgs =
        new java.util.Hashtable<String, String> ();


    private void SetCommandLineArgs(String[] args)
        throws java.lang.Exception {
        // Don't bother if no command line args were passed
        if (args == null) return;
        if (args.length == 0) return;

        // Parse command line args for args in the following format:
        //   /argname:argvalue /argname:argvalue /argname:argvalue ...

        String patternString = "\\/([^:]+):(\\S+)";

        java.util.regex.Pattern p = java.util.regex.Pattern.compile(patternString);

        for (String arg : args) {
            java.util.regex.Matcher m = p.matcher(arg);
            if (!m.matches()) {
                // If match not found, command line args are improperly formed.
                throw new java.lang.Exception("The command line arguments are improperly formed. Use /argname:argvalue.");
            }

            // Store command line arg and value
            CommandLineArgs.put(m.group(1), m.group(2));
        }
    }


    public void Run() {

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

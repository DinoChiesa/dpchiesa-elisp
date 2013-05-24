// @BASEFILENAME@
// ------------------------------------------------------------------
//
// Description goes here....
//
// Author: @AUTHOR@
// Created @DATE@
//
// Last saved: <2013-April-22 18:56:35>
// ------------------------------------------------------------------
//
@INSERTFILE(/Users/Dino/elisp/defaultcontent/Copyright.txt)@
//
// ------------------------------------------------------------------

package com.ionic.whatever;

import java.util.Hashtable;
import java.util.Enumeration;

public class @BASEFILENAMELESSEXTENSION@ {
    private final String optString = "vu:p:s:"; // getopt style

    // public @BASEFILENAMELESSEXTENSION@ () {} // uncomment if wanted

    public @BASEFILENAMELESSEXTENSION@ (String[] args)
        throws java.lang.Exception {
        GetOpts(args, optString);
    }

    private Hashtable<String, Object> Options = new Hashtable<String, Object> ();

    private void GetOpts(String[] args, String optString)
        throws java.lang.Exception {
        // Parse command line args for args in the following format:
        //   -a value -b value2 ... ...

        // sanity checks
        if (args == null) return;
        if (args.length == 0) return;
        if (optString == null) return;
        final String argPrefix = "-";
        String patternString = "^" + argPrefix + "([" + optString.replaceAll(":","") + "])";

        java.util.regex.Pattern p = java.util.regex.Pattern.compile(patternString);

        int L = args.length;
        for(int i=0; i < L; i++) {
            String arg = args[i];
            java.util.regex.Matcher m = p.matcher(arg);
            if (!m.matches()) {
                throw new java.lang.Exception("The command line arguments are improperly formed. Use a form like '-a value' or just '-b' .");
            }

            char ch = arg.charAt(1);
            int pos = optString.indexOf(ch);

            if ((pos != optString.length() - 1) && (optString.charAt(pos+1) == ':')) {
                if (i+1 < L) {
                    i++;
                    Options.put(m.group(1), args[i]);
                }
                else {
                    throw new java.lang.Exception("Incorrect arguments.");
                }
            }
            else {
                // a "no-value" argument, like -v for verbose
                Options.put(m.group(1), (Boolean) true);
            }
        }
    }

    private void MaybeShowOptions() {
        Boolean verbose = (Boolean) Options.get("v");
        if (verbose != null && verbose) {
            System.out.println("options:");
            Enumeration e = Options.keys();
            while(e.hasMoreElements()) {
                // iterate through Hashtable keys Enumeration
                String k = (String) e.nextElement();
                Object o = Options.get(k);
                String v = null;
                v = (o.getClass().equals(Boolean.class)) ?  "true" : (String) o;
                System.out.println("  " + k + ": " + v);
            }

            // enumerate properties here?
        }
    }


    public void Run() {

        MaybeShowOptions();
        Boolean verbose = (Boolean) Options.get("v");

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

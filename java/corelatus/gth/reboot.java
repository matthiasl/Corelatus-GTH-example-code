//----------------------------------------------------------------------
// Title: GTH Reboot Utility
// Author: Matthias Lang (matthias@corelatus.com)
// Created: July 2002
// 
// Copyright (c) 2002 Corelatus AB Stockholm
//
// This is demonstration code. Use at your own risk. Permission granted to
// copy, modify and integrate into other code.
//
// Use:
//      java corelatus.gth.reboot <hostname> [<bootmode>]
//
//      hostname: hostname or dotted-quad IP address the GTH host
//      bootmode: normal | failsafe  (defaults to 'normal')
//
// $Id: reboot.java,v 1.3 2009-01-29 12:03:09 matthias Exp $
//----------------------------------------------------------------------
package corelatus.gth;

import corelatus.gth.*;
import java.io.*;
import org.w3c.dom.*;

public class reboot {

    // Data
    private Client_conn c;       // connection to the GTH

    //----------------------------------------------------------------------
    // ctor
    public reboot(String hostname) throws IOException {
	c = new Client_conn(hostname);
    }

    //----------------------------------------------------------------------
    public static void main(String[] args) {
	try {
	    String bootmode = "normal";

	    if (args.length > 0) {
		reboot r = new reboot(args[0]);
		if (args.length == 2)
		    bootmode = args[1];
		r.boot(bootmode);
	    } else 
		reboot.usage();
	}
	catch (IOException e) {
	    die("io exception");
	}
    }

    //----------------------------------------------------------------------
    // Implementation

    // which = "normal" or "failsafe"
    public void boot(String how) throws IOException
    {
	if (!how.equals("failsafe") && ! how.equals("normal") )
	    die("Exiting: '" + how + "' is not a valid boot mode. Try 'normal' or 'failsafe'");

	System.out.println("Rebooting, new mode: " + how);
	c.send_command("<set name='os'>" 
		       + "<attribute name='boot mode' value='" 
		       + how + "'/></set>");
	Client_conn.assert_name(c.next_non_event(), "ok");
	c.send_command("<reset><resource name='cpu'/></reset>");
	try {
	    // if the GTH is quick, it might get an ok back. Usually it
	    // reboots before getting the response out.
	    Client_conn.assert_name(c.next_non_event(true), "ok"); 
	}
	catch (IOException e) {
	    // this is meant to happen
	}
    }

    public static void die(String reason) {
	System.out.println(reason);
	System.exit(-1);
    }

    private static void usage() {
	System.err.println("usage:");
	System.err.println("");
	System.err.println("  java corelatus.gth.reboot <hostname> [<bootmode>]");
	System.err.println("");
	System.err.println("  <bootmode> = normal | failsafe; defaults to 'normal'");
	System.exit(-1);
    }
}

// eof

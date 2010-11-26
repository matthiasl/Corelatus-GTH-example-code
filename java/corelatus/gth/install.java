//----------------------------------------------------------------------
// Title: GTH Image Installer demo code
// Author: Matthias Lang (matthias@corelatus.com)
// Created: June 2002
// 
// Copyright (c) 2002 Corelatus AB Stockholm
//
// This is demonstration code. Use at your own risk. Permission granted to
// copy, modify and integrate into other code.
//
// Use:
//      java corelatus.gth.install <hostname> [<filename> [failsafe]]
//
//      hostname: hostname or dotted-quad IP address the GTH host
//      filename: file containing the image. If none specified, the current
//                status is shown
//
//      If "failsafe" is specified, the failsafe system is upgraded.
//
// $Id: install.java,v 1.11 2009-01-29 12:03:08 matthias Exp $
//----------------------------------------------------------------------
package corelatus.gth;

import corelatus.gth.*;
import java.io.*;
import org.w3c.dom.*;

public class install {

    // Data
    private Client_conn c;       // connection to the GTH
    private String remote_host;
    private String current_system;
    private String failsafe_version;

    //----------------------------------------------------------------------
    // ctor
    public install(String hostname) throws IOException {
	remote_host = hostname;
	try {
	    c = new Client_conn(hostname);
	} catch (IOException e) {
	    System.out.println("Unable to connect to " + hostname);
	    System.exit(-1);
	}
    }

    // Show us which versions are currently installed, returns the current one
    public String info() throws IOException
    {
	String current;
	NodeList sys = image_info("system_image");
	NodeList fsafe = image_info("failsafe_image");

	failsafe_version = Client_conn.extract_att(fsafe, "version");
	
	System.out.println("System: " + 
			   Client_conn.extract_att(sys, "version") +
			   ". Failsafe: " + failsafe_version);
	
	if (Client_conn.extract_att(sys, "busy").equals("true")) 
	    current = "system";
	else 
	    current = "failsafe";

	System.out.println("Currently running image: " + current);       
	return current;
    }

    // Upgrade the system. No turning back.
    public void go(String system, String filename) {
	try {
	    String current = info();
	    String other = "normal";

	    if (current == "system") other = "failsafe";
	    if (current == system) reboot(other);

	    unlock(system + "_image");
	    upgrade(filename, system + "_image");

            if (failsafe_version.equals("o_release_10")) {  // damaged release, shouldn't be used
		    System.out.println("o_release_10 failsafe---working around version check problem");
		    current = other;
	    } else current = info();
	    other = "normal";
	    if (current == "system") other = "failsafe";
	    if (current == system) reboot(other);

	    reboot(other);
	    info();
	}
	catch (IOException e) {
	    System.out.println(e);
	}
    }

    //----------------------------------------------------------------------
    public static void main(String[] args) {
	try {
	    if (args.length > 0) {
		install g = new install(args[0]);
		if (args.length == 1)
		    g.info();
		else if (args.length == 2)
		    g.go("system", args[1]);
		else if (args.length == 3 && args[2].equals("failsafe")) 
		    g.go("failsafe", args[1]);
		else install.usage();
	    } else 
		install.usage();
	}
	catch (IOException e) {
	    die("io exception");
	}
    }

    //----------------------------------------------------------------------
    // Implementation

    // which = "normal" or "failsafe"
    private void reboot(String how) throws IOException
    {
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
	wait_for_boot();
    }

    // which = "system_image" or "failsafe_image"
    private void unlock(String which) throws IOException
    {
	System.out.println("Unlocking " + which);
	c.send_command("<set name='" + which + "'>" 
		       + "<attribute name='locked' value='false'/>"
		       + "</set>");
	Client_conn.assert_name(c.next_non_event(), "ok");
    }

    public void wait_for_boot() {
	try {
	    System.out.print("Waiting for the system to reboot");
	    watchdog w = new watchdog(60, true); // 60s timeout for reboot
	    Thread.sleep(10000);    // make sure the boot has started
	    boolean os_contacted = false;
	    while (true) {
		try {
		    c = new Client_conn(remote_host);
		    System.out.println("");
		    w.die();
		    return;
		}
		catch (IOException e) {
		    if (!os_contacted) {
			System.out.println("");
			System.out.print("Waiting for the API to start");
		    }
		    os_contacted = true;
		    Thread.sleep(1000);
		}
	    }
	}
	catch (InterruptedException e) {
	    die("sleep interrupted");
	}
    }

    // Do the actual upgrade. Assumes
    //        1. We're booted to the right system
    //        2. The target image is unlocked
    private void upgrade(String filename, String system) 
	throws FileNotFoundException, IOException
    {
	File file = new File(filename);
	if (!file.exists() || !file.canRead()) 
	    die("Unable to read image file: " + filename);

	if (file.length() < 1000000 || file.length() > 10000000) 
	    die("Image file is an unreasonable length");

	FileInputStream fis = new FileInputStream(file);
	int length = (new Long(file.length())).intValue();
	
	byte image[] = new byte[length];
	int result = fis.read(image, 0, length);
	
	if (result != length) die("failed to read the image file");

	watchdog w = new watchdog(180, true);
	System.out.println("Installing " + filename + " as " + system);
	c.send_command("<install name='" + system + "'/>");
	c.send_binary("binary/filesystem", image, length);

	Element ok = c.next_non_event();
	if (ok.getNodeName().equals("ok")) 
	    wait_for_completed();
	else 
	    fail(ok);
	w.die(); 
    }

    // An install is fully completed when we get an "install_done" event
    private void wait_for_completed() throws IOException
    {
	Element e = c.next_reply(true);
	if (e == null) die("response was empty");
	    
	if (e.getNodeName().equals("event")) {
	    Node i = e.getChildNodes().item(0);
	    if (i != null && i.getNodeName().equals("info")) {
		Node attr = i.getAttributes().item(0);
		if (attr != null) {
		    if (attr.getNodeValue().equals("install_done")) return;
		    if (attr.getNodeValue().equals("install_failed"))
			die("install failed");
		}
	    }
	}

	wait_for_completed();
    }

    public static void die(String reason) {
	System.out.println(reason);
	System.exit(-1);
    }

    //----
    // Something stuffed up, print why and die.
    private static void fail(Node error) {
	System.out.println("**aborting: " + Client_conn.flatten(error, 0));
	System.exit(-1);
    }

    private NodeList image_info(String image_name) throws IOException
    {
	c.send_command("<query><resource name='" + image_name + "'/></query>");
	Element state = c.next_non_event();
	Client_conn.assert_name(state, "state");
	Node resource = state.getChildNodes().item(0);
	Client_conn.assert_name(resource, "resource");
	return resource.getChildNodes();
    }

    private static void usage() {
	System.err.println("Version: $Id: install.java,v 1.11 2009-01-29 12:03:08 matthias Exp $ usage:");
	System.err.println("");
	System.err.println("  java corelatus.gth.install <hostname> [<filename> [failsafe]]");
	System.err.println("");
	System.err.println("  If no filename is specified, the current status is shown");
	System.err.println("  If the keyword 'failsafe' is present, the failsafe image is upgraded");
	System.err.println("  instead of the 'system' image.");
	System.exit(-1);
    }
}

// eof

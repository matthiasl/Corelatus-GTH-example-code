//----------------------------------------------------------------------
// Title: Layer 1 and Layer 2 statistics logging
// Author: Matthias Lang (matthias@corelatus.com)
// Created: November 2004
//
// Copyright (c) 2004 Corelatus AB Stockholm
//
// This is demonstration code. Use at your own risk. Permission granted to
// copy, modify and integrate into other code.
//
// Purpose:
//
//   Query L1 and log counters periodically to standard output
//   Query MTP-2 jobs and log counters periodically to standard output
//
// Use:
//      java corelatus.gth.l1_timeseries <hostname>
//
//      hostname: hostname or dotted-quad IP address the GTH host
//
//----------------------------------------------------------------------
package corelatus.gth;

import corelatus.gth.*;
import java.io.*;
import java.util.*;
import org.w3c.dom.*;

public class timeseries {

    // Data
    private Client_conn c;       // connection to the GTH
    private String[] pcms;       // names of the available spans
    private int n_pcms;

    //----------------------------------------------------------------------
    // ctor
    public timeseries(String hostname) throws IOException {
	c = new Client_conn(hostname);
	query_pcms();
    }

    //----------------------------------------------------------------------
    public static void main(String[] args) {
	try {
	    if (args.length == 2) {
		timeseries t = new timeseries(args[0]);
		t.log(Integer.parseInt(args[1]));
	    } else
		timeseries.usage();
	}
	catch (IOException e) {
	    die("io exception");
	}
    }

    //----------------------------------------------------------------------
    // Implementation

    private void log(int iterations) throws IOException
    {
	while (iterations-- > 0) {
	    System.out.println((new GregorianCalendar()).getTime());
	    log_pcms();
	    log_l2();
	    sleep(60 * 1000);
	}
    }

    private void sleep(int milliseconds) {
	try {
	    Thread.sleep(milliseconds);
	} catch (InterruptedException e) {
	    // we don't care.
	}
    }

    // Log L1 counters to stdout
    private void log_pcms() {
	try {
	    String resource_names = "";
	    int i;

	    for (i = 0; i < n_pcms; i++)
		resource_names += "<resource name='" + pcms[i] + "'/>";

	    c.send_command("<query>" + resource_names + "</query>");

	    Node child = c.next_non_event().getFirstChild();

	    while (child != null) {
		if (child.getNodeType() != Node.TEXT_NODE) {
		    Resource r = new Resource(child);
		    System.out.println(r.to_string());
		}
		child = child.getNextSibling();
	    }
	}
	catch (IOException e) {
	    die("io exception");
	}
    }

    // Log L2 counters to stdout
    private void log_l2() {
	try {
	    // Figure out what L2 jobs we have by querying the schedule
	    String alljobs = l2jobs();

	    // Query those jobs
	    c.send_command("<query>" + alljobs + "</query>");
	    Node child = c.next_non_event().getFirstChild();

	    while (child != null) {
		if (child.getNodeType() != Node.TEXT_NODE) {
		    NamedNodeMap attrs = child.getAttributes();
		    String id = attrs.getNamedItem("id").getNodeValue();
		    System.out.print(id + " ");

		    Node attribute = child.getFirstChild();
		    while (attribute != null) {
			if (child.getNodeType() != Node.TEXT_NODE) {
			    attrs = attribute.getAttributes();
			    String value = attrs.getNamedItem("value").getNodeValue();
			    System.out.print(value + " ");

			}
			attribute = attribute.getNextSibling();
		    }
		}
		child = child.getNextSibling();
		System.out.println();
	    }
	}
	catch (IOException e) {
	    die("io exception");
	}
    }

    // Return a string of jobs
    private String l2jobs() throws IOException {
	String alljobs = "";

	c.send_command("<query><resource name='schedule'/></query>");
	Node child = c.next_non_event().getFirstChild();

	while (child != null) {
	    if (child.getNodeType() != Node.TEXT_NODE) {
		NamedNodeMap attrs = child.getAttributes();
		String id = attrs.getNamedItem("id").getNodeValue();
		if (id.startsWith("m2mo"))
		    alljobs += "<job id='" + id + "'/>";
	    }
	    child = child.getNextSibling();
	}

	return alljobs;
    }


    public static void die(String reason) {
	System.out.println(reason);
	System.exit(-1);
    }

    private static void usage() {
	System.err.println("usage:");
	System.err.println("");
	System.err.println("  java corelatus.gth.timeseries <hostname> <minutes>");
	System.err.println("");
	System.err.println("(The program exits after <minutes>.)");
	System.exit(-1);
    }

    // Update the list of PCM spans on the given card
    private void query_pcms() {
	int i;

	n_pcms = 0;

	try {
	    c.send_command("<query><resource name='inventory'/></query>");
	    NodeList resources = c.next_non_event().getChildNodes();

	    // reserve at least as many as we need
	    pcms = new String[resources.getLength()];

	    for (i = 0; i < resources.getLength(); i++) {
		Node node = resources.item(i);
		if (node.getNodeName().equals("resource")) {
		    NamedNodeMap attribs = resources.item(i).getAttributes();
		    String name = attribs.getNamedItem("name").getNodeValue();
		    if (name.startsWith("pcm"))
			pcms[n_pcms++] = name;
		}
	    }

	}
	catch (IOException e) {
	    die("io exception");
	}
    }
}

// eof

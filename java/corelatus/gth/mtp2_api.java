//----------------------------------------------------------------------
// Title: GTH MTP-2 API code
// Author: Matthias Lang (matthias@corelatus.com)
// Created: February 2003
// 
// Copyright (c) 2003 Corelatus AB Stockholm
//
// This is demonstration code. Use at your own risk. Permission granted to
// copy, modify and integrate into other code.
//
// Purpose: Set up MTP-2 monitoring of synthetic input, directing
//          the resulting MSUs to a number of independent receiver
//          processes. The receiver processes are assumed to be running on
//          'localhost'.
//
// Use:
//      Connect a PCM loopback cable from PCM1 to PCM2
//
//      Start several receiver processes, see mtp2_receiver.java
//
//      Start the API process:
//        java corelatus.gth.mtp2_api <hostname> [N]
//
//      N is the number of ports, starting at 1200, are set up to receive
//      MTP-2 data. If N > 1, this program will automatically move
//      timeslots from one receiver process to another at random.
//
// Using this program on live systems will disrupt signalling.
//----------------------------------------------------------------------
package corelatus.gth;

import corelatus.gth.*;
import java.io.*;
import java.net.*;
import java.util.Random;
import org.w3c.dom.*;

public class mtp2_api {

    private class monitor_job {
	public monitor_job(String i, int t, int p) { id = i; port = p; ts = t;}
	public String id;
	public int ts;
	public int port;
    }

    int MAX_JOBS = 32;       
    int BASE_PORT = 1200;    // IP port we send to is 1200, 1201, etc.
    int JOBS_PER_DEST = 3;  // how many timeslots we send to each dest

    // Data
    private Client_conn c;       // connection to the GTH
    private String remote_host;
    
    int n_dests = 0;
    int n_jobs = 0;
    monitor_job[] jobs = new monitor_job[MAX_JOBS];

    //----------------------------------------------------------------------
    // ctor
    public mtp2_api(String hostname, int dests) throws IOException {
	remote_host = hostname;
	n_dests = dests;
	if (dests < 1) die("must have at least one destination");
	if (dests > 8) die("cannot have more than 8 destinations");
	try {
	    c = new Client_conn(hostname);
	} catch (IOException e) {
	    die("Unable to connect to " + hostname);
	}
    }

    //----------------------------------------------------------------------
    public static void main(String[] args) {
	try {
	    mtp2_api m = null;
	    if (args.length == 1) 
		m = new mtp2_api(args[0], 1);
	    else if (args.length == 2) 
		m = new mtp2_api(args[0], (new Integer(args[1])).intValue());
	    else usage();
	    m.go();
	}
	catch (NumberFormatException e) {
	    die("the second argument must be an integer between 1 and 8");
	}
	catch (IOException e) {
	    die("io exception");
	}
    }

    //----------------------------------------------------------------------
    // Implementation

    private void go() {
	mtp2_playback();
	enable_pcms();
	log("Enabling monitoring to " + n_dests + " dests");
	for (int i = 0; i < n_dests; i++) 
	    for (int j = 0; j < JOBS_PER_DEST; j++) {
		int timeslot = i * JOBS_PER_DEST + j + 1;
		int port = BASE_PORT + i;
		String id = enable_monitoring(timeslot, port);
		jobs[n_jobs++] = new monitor_job(id, timeslot, port);

	    }
	move_jobs();
    }

    // Set up synthetic MTP-2 playback. We do this by 
    //
    //  1. Putting a clip on the system with nonstop MSUs (and one ESU)
    //  2. Playing the clip on timeslot 1 of PCM1
    //  3. Using the switch to copy it to all timeslots of PCM1
    //
    private void mtp2_playback() {
	try {
	    log("Starting synthetic MTP-2 packet playback");

	    // read in the clip file
	    byte binary[] = new byte[10000];
	    FileInputStream msu = new FileInputStream("msus");
	    int length = msu.read(binary, 0, 10000);
	    if (length < 1000) die("short file read of msus");

	    // 1. The clip
	    c.send_command("<new><clip id='synth_mtp2'/></new>" );
	    c.send_binary("binary/audio", binary, length);
	    Client_conn.assert_name(c.next_non_event(), "job");

	    // 2. The player
	    c.send_command("<new><player loop='true'>"
			   + "<clip id='clip synth_mtp2'/>"
			   + "<pcm_sink span='1' timeslot='1'/>"
			   + "</player></new>" );
	    Client_conn.assert_name(c.next_non_event(), "job");

	    // 3. The switching
	    for (int i = 2; i < 32; i++) {
		c.send_command("<new><connection>"
			   + "<pcm_source span='2' timeslot='1'/>"
			   + "<pcm_sink span='1' timeslot='" + i + "'/>"
			   + "</connection></new>" );
		Client_conn.assert_name(c.next_non_event(), "job");
	    }
	} 
	catch (IOException e) {
	    die("mtp2_playback IO exception");
	}
    }
    
    //--------------------
    // Enable PCM1 and PCM2 for normal transmission 
    //
    // Series 1 Monitor cards are not guaranteed to work in this mode.
    // In practice they do, but the outgoing signal levels are out-of-spec.
    //
    private void enable_pcms() {
	try {
	    log("Enabling PCM 1 & 2");
	    c.send_command("<set name='pcm1'>" 
			   + "<attribute name='status' value='enabled'/>"
			   + "</set>");
	    Client_conn.assert_name(c.next_non_event(), "ok");

	    c.send_command("<set name='pcm2'>"
			   + "<attribute name='status' value='enabled'/>"
			   + "</set>");
	    Client_conn.assert_name(c.next_non_event(), "ok");
	} 
	catch (IOException e) {
	    die("mtp2_playback IO exception");
	}
    }

    //--------------------
    // Returns the local machine's IP address as a string
    private String local_ip_addr() throws UnknownHostException {
	InetAddress local = InetAddress.getLocalHost();
	String ip_addr = "";
	byte[] b = local.getAddress();

	for (int j = 0; j < 4; j++) {
	    int i = b[j];
	    if (i < 0) i += 256;
	    ip_addr += i;
	    if (j != 3) ip_addr += ".";
	}
	return ip_addr;
    }

    //--------------------
    // Returns a job-id
    private String enable_monitoring(int timeslot, int dest_port) {
	try {
	    String ip_addr = local_ip_addr();

	    c.send_command("<new><mtp2_monitor ip_addr='" + ip_addr
			   + "' ip_port='" + dest_port + "'>" +
			   "<pcm_source span='2' timeslot='"
			   + timeslot + "'/>" +
			   "</mtp2_monitor></new>");
	    Node reply = c.next_non_event();
	    Client_conn.assert_name(reply, "job");
	    return reply.getAttributes().item(0).getNodeValue();
	} 
	catch (IOException e) {
	    die("enable_monitoring IO exception: " + e);
	}
	return ""; // notreached
    }

    //--------------------
    // Shuffle jobs from one destination to another
    private void move_jobs() {
	Random r = new Random();
	while (true) {
	    try {
		Thread.sleep(/*3 * 60 **/ 1000);  //REVISIT: randomise
	    }
	    catch (Exception e) {die("sleep failed");}

	    int from = Math.abs(r.nextInt() % n_dests) + BASE_PORT;
	    int to = Math.abs(r.nextInt() % n_dests) + BASE_PORT;
	    int shifted = 0;
	    for (int i = 0; i < n_jobs; i++) {
		if (jobs[i].port == from) {
		    if (shifted++ == JOBS_PER_DEST) break;
		    log("Shifting " + i + " from " + from + " to " + to);
		    try {
			c.send_command("<delete id='" + jobs[i].id + "'/>");
			Client_conn.assert_name(c.next_non_event(), "ok");
			String id = enable_monitoring(jobs[i].ts, to);
			jobs[i] = new monitor_job(id, jobs[i].ts, to);
		    }
		    catch (IOException e) {
			die("move command failed");
		    }
		}
	    }
	}
    }

    //--------------------
    private static void log(String msg) {
	mtp2_receiver.log(msg);
    }

    private static void die(String reason) {
	System.err.println(reason);
	System.exit(-1);
    }

    //--------------------
    private static void usage() {
	System.err.println("usage: java <classname> <host> [<N>]");
	System.exit(-1);
    }
}

// eof

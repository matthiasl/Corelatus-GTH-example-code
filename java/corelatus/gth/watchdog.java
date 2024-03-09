//----------------------------------------------------------------------
// Title: Watchdog timer
// Author: Matthias Lang (matthias@corelatus.com)
// Created: June 2002
//
// Copyright (c) 2002 Corelatus AB
//
// This is demonstration code. Use at your own risk. Permission granted to
// copy, modify and integrate into other code.
//
// typical use:
//
//  w = new watchdog(100);
//  <do something>
//  w.stop();
//
//----------------------------------------------------------------------
package corelatus.gth;
import java.io.*;

public class watchdog extends Thread {
    private int timeout; // in tenths of seconds
    private boolean verbose;
    private volatile boolean live = true;

    public watchdog(int seconds) {
	timeout = 10 * seconds;
	verbose = false;
	start();
    }

    public watchdog(int seconds, boolean visible) {
	timeout = 10 * seconds;
	verbose = visible;
	start();
    }

    public void die() {
	live = false;
    }

    public void run() {
	try {
	    while (timeout > 0 && live) {
		timeout -= 1;
		Thread.sleep(100);
		if (verbose && timeout % 10 == 1) System.out.print(".");
	    }
	}
	catch (java.lang.InterruptedException e) {
	    System.out.println("watchdog sleep interrupted");
	}
	if (live) {
	    System.out.println("timeout, committing suicide");
	    System.exit(-1);
	}
    }
}

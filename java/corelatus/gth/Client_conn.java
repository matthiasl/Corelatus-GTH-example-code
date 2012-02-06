//----------------------------------------------------------------------
// Title: GTH API demonstration in Java
// Author: Matthias Lang (matthias@corelatus.com)
// Created: January 2001
//
// Copyright (c) 2001 Corelatus AB Stockholm
//
// This is demonstration code. Use at your own risk.
//----------------------------------------------------------------------

package corelatus.gth;

import java.io.*;
import java.net.*;
import org.apache.xerces.parsers.*;
import org.xml.sax.*;
import org.w3c.dom.*;

public class Client_conn {
    public static final int WELL_KNOWN_PORT = 2089;

    //======================================================================
    // Interface

    //----------------------------------------------------------------------
    // ctor
    //
    public Client_conn(String hostname)
	throws UnknownHostException,
	IOException
    {
	socket = new Socket(hostname, WELL_KNOWN_PORT);
	InputStreamReader isr = new InputStreamReader(socket.getInputStream());
	input = new BufferedReader(isr);
	output = new DataOutputStream(socket.getOutputStream());
    }

    //----------------------------------------------------------------------
    // Return the next reply on the socket
    //
    public Element next_reply()
	throws IOException
    {
	return next_reply(false);
    }

    public Element next_reply(boolean verbose)
	throws IOException
    {
	String content = fetch_content();

	if (content == null) {
	    System.out.println("fetched null content");
	    System.exit(-1);
	}
	if (verbose) System.out.println(content);

	try {
	    DOMParser parser = new DOMParser();

	    ByteArrayInputStream in =
		new ByteArrayInputStream(content.getBytes());
	    parser.parse(new InputSource(in));
	    Document doc = parser.getDocument();
	    return doc.getDocumentElement();
	}
	catch (SAXException e) {
	    System.out.println("unexpected saxexception in next_reply");
	    System.out.println(e);
	}
	return null;
    }

    //----------------------------------------------------------------------
    // Proof-of-concept. In failsafe 4, the <install> command closes
    // the socket after installing the password file/logo/start script.
    // It's a bug. This function re-opens the socket so we can continue
    // anyway.
    public void re_open()
	throws IOException
    {
	Socket old_socket = socket;
	socket = new Socket(old_socket.getInetAddress(), old_socket.getPort());
	InputStreamReader isr = new InputStreamReader(socket.getInputStream());
	input = new BufferedReader(isr);
	output = new DataOutputStream(socket.getOutputStream());
    }

    //----------------------------------------------------------------------
    // Returns the next reply on the socket which is not an event
    public Element next_non_event()
	throws java.io.IOException { return next_non_event(false); }

    public Element next_non_event(boolean verbose)
	throws java.io.IOException
    {
	Element e = next_reply(verbose);
	if (e.getNodeName().equals("event"))
	    return next_non_event();

	return e;
    }


    //----------------------------------------------------------------------
    // Send a request to the GTH. Automatically generates the header.
    public void send_command(String request)
	throws IOException
    {
	output.writeBytes(header(request));
	output.writeBytes(request);
    }

    public void send_binary(String type, byte binary[], int size)
	throws IOException
    {
	String header = "Content-type: " + type + "\r\n";
	header += "Content-length: " + size + "\r\n\r\n";
	output.writeBytes(header);
	output.write(binary, 0, size);
    }

    //----------------------------------------------------------------------
    // Given an XML node (as returned by the parser), print it
    // out recursively. Useful for debugging.
    //
    static public void print_tree(Node n, int depth) {
	System.out.println(flatten(n, depth));
    }

    static public String flatten(Node n, int depth) {
	if (n == null) {
	    return "null tree";
	}

	String rep = new String();
	NamedNodeMap attributes = n.getAttributes();

	// indent
	for (int x = 0; x < depth; x++)
	    rep += "  ";

	if (n.getNodeType() == Node.TEXT_NODE)
	    rep += "(" + n.getNodeValue() + ")";
	else
	    rep += n.getNodeName();

	if (attributes != null)
	    for (int x = 0; x < attributes.getLength(); x++) {
		Node attribute = attributes.item(x);
		rep += " " + attribute.getNodeName() +
		    " = " + attribute.getNodeValue();
	    }

	NodeList children = n.getChildNodes();

	if (children != null)
	    for (int x = 0; x < children.getLength(); x++)
		rep += flatten(children.item(x), depth + 1);

	return rep;
    }

    // Given a list of attribute nodes, return the value of the one whose
    // name is given
    public static String extract_att(NodeList n, String name)
	throws RuntimeException
    {

	for (int i = 0; i < n.getLength(); i++) {
	    Node child = n.item(i);

	    if (child.getNodeType() != Node.TEXT_NODE)
		if (att_value(child, "name").equals(name))
		    return att_value(child, "value");
	}
	throw new RuntimeException("no such field: " + name);
    }

    // Given a node, return the value of the attribute with the given name
    public static String att_value(Node n, String name)
    {
	NamedNodeMap attribs = n.getAttributes();

	if (attribs != null)
	    for (int i = 0; i < attribs.getLength(); i++) {
		Node a = attribs.item(i);
		if (a.getNodeName().equals(name)) {
		    return a.getNodeValue();
		}
	    }
	throw new RuntimeException("no such field " + name);
    }

    // Assert that a node is what we say it is
    public static void assert_name(Node n, String s)
	throws RuntimeException
    {
	if (n.getNodeName().equals(s)) return;
	print_tree(n, 10);

	assert_void(false, "expected element " + s + "; got " +
		    n.getNodeName());
    }

    private static void assert_void(boolean truth, String why)
    {
	if (!truth) throw new RuntimeException(why);
    }


    //======================================================================
    // Implementation

    private BufferedReader input;
    private DataOutputStream output;
    private Socket socket;

    //----------------------------------------------------------------------
    // Given a string with content, return the correct header.
    static private String header(String content) {
	Integer length = new Integer(content.length());

	String line = "Content-type: text/xml\r\n";
	line += "Content-length: " + length.toString() + "\r\n\r\n";

	return line;
    }

    //----------------------------------------------------------------------
    // Fetch a complete load of content; expected to be of type text/xml
    //
    // Only returns the content, not the header.
    private String fetch_content()
	throws IOException,
	IllegalArgumentException
    {
	String line1 = input.readLine();
	String line2 = input.readLine();

	if (line1 == null || line2 == null)
	    throw new IOException("reading input failed");

	if (!line1.equals("Content-type: text/xml"))
	    throw new IllegalArgumentException("Bad Content-type line: "
					       + line1);

	String length_line = "Content-length: ";
	if (!line2.startsWith(length_line))
	    throw new IllegalArgumentException("Bad Content-length line: "
					       + line2);

	String tail = line2.substring(length_line.length());
	int l = Integer.parseInt(tail) + 2;   // +2 for CRLF CRLF
	char[] array = new char[l];

	// InputStreamReader doesn't necessarily read the number of
	// characters you tell it to (why not?!), so we loop
	int read_so_far = 0;

	while (read_so_far < l) {
	    int thistime = input.read(array, read_so_far, l - read_so_far);
	    if (thistime <= 0)
		throw new IOException("reading input failed");
	    read_so_far += thistime;
	}

	return new String(array);
    }
}

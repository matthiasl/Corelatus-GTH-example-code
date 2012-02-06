//----------------------------------------------------------------------
// Purpose: Models a GTH resource.
// Author: Matthias Lang (matthias@corelatus.com)
//----------------------------------------------------------------------
package corelatus.gth;

import corelatus.gth.*;
import java.io.*;
import java.util.*;
import org.w3c.dom.*;

public class Resource {

    private class KeyValue {
	public String key;
	public String value;

	public KeyValue(String k, String v) {
	    key = k;
	    value = v;
	}
    }

    public Resource(Node n) {
	NamedNodeMap attrs = n.getAttributes();
	name = attrs.getNamedItem("name").getNodeValue();
	attributes = new LinkedList<KeyValue>();
	populate(n);
    }

    public String to_string() {
	String s = name;
	ListIterator i = attributes.listIterator(0);

	while (i.hasNext()) {
	    KeyValue kv = (KeyValue)i.next();
	    s += " " + kv.key + "=" + kv.value;
	    //s += " " + kv.value;
	}
	return s;
    }

    public String name;
    private LinkedList<KeyValue> attributes;

    //----------------------------------------------------------------------
    private void populate(Node n) {
	Node child = n.getFirstChild();

	while (child != null) {
	    if (child.getNodeType() != Node.TEXT_NODE) {
		NamedNodeMap attrs = child.getAttributes();
		String key = attrs.getNamedItem("name").getNodeValue();
		String value = attrs.getNamedItem("value").getNodeValue();
		attributes.add(new KeyValue(key, value));
	    }
	    child = child.getNextSibling();
	}
    }
}


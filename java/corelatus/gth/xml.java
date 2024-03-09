//----------------------------------------------------------------------
// Title: XML tag generation
// Author: Matthias Lang (matthias@corelatus.com)
// Created: December 2020
//
// Copyright (c) 2020 Corelatus AB
//
// This is demonstration code. Use at your own risk.
//
// Generate XML for the Corelatus GTH API. We don't have code here for
// every part of the API---this is a useful subset for the sample programs.
//
//----------------------------------------------------------------------
package corelatus.gth;

//import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

class xml {

    public static void main(String[] args)
    {
        System.out.println(xml.enable("pcm1A", make_map()));
    }

    public static String attrs_to_string(Map<String, String> attrs)
    {
        StringBuffer s = new StringBuffer("");
        attrs.forEach( (k, v) -> s.append(" " + k + "='" + v + "'") );
        return s.toString();
    }

    public static String attribute(String name, String value)
    {
        Map<String, String> map = make_map("name", name);
        map.put("value", value);
        return tag("attribute", map);
    }

    public static String clip(String id)
    {
        return tag("clip", make_map("id", id));
    }

    public static String custom(String name, Map<String, String> attrs)
    {
        return tag("custom", make_map("name", name), attrs_to_string(attrs));
    }

    public static String disable(String name)
    {
        return tag("disable", make_map("name", name));
    }

    public static String enable(String name, Map<String, String> attrs)
    {
        return tag("enable", make_map("name", name),
                   attribute_map_to_string(attrs));
    }

    public static String delete(String id)
    {
        return tag("delete", make_map("id", id));
    }

    public static String install(String name)
    {
        return tag("install", make_map("name", name));
    }

    public static String job(String id)
    {
        return tag("job", make_map("id", id));
    }

    public static String new_lapd_monitor(String ip, Integer port, String src)
    {
        return new_monitor("lapd_monitor", ip, port, src);
    }

    public static String map(String name)
    {
        return tag("map", make_map("target_type", "pcm_source"),
                   tag("sdh_source", make_map("name", name)));
    }

    public static String new_mtp2_monitor(String ip, Integer port, String src)
    {
        return new_monitor("mtp2_monitor", ip, port, src);
    }

    // <new> command is called 'g_new' here because 'new' is a keyword in Java
    public static String g_new(String children)
    {
        return tag("new", make_map(), children);
    }

    public static String pcm_source(String span, Integer timeslot)
    {
        Map<String, String> map = make_map("span", span);
        map.put("timeslot", timeslot.toString());
        return tag("pcm_source", map);
    }

    public static String pcm_source(String span,
                                    Integer timeslot,
                                    Integer first_bit,
                                    Integer bandwidth)
    {
        Map<String, String> map = make_map("span", span);
        map.put("timeslot", timeslot.toString());
        map.put("first_bit", first_bit.toString());
        map.put("bandwidth", bandwidth.toString());
        return tag("pcm_source", map);
    }

    public static String query_resource(String name)
    {
        return tag("query", make_map(), resource(name));
    }

    public static String query_job(String id)
    {
        return tag("query", make_map(),
                   tag("job", make_map("id", id)));
    }

    public static String recorder(String span,
                                  Integer timeslot,
                                  String host,
                                  Integer port)
    {
        return tag("new", make_map(),
                   tag("recorder", make_map(),
                       pcm_source(span, timeslot) + tcp_sink(host, port)));
    }

    public static String reset(String name)
    {
        return tag("reset", make_map(), resource(name));
    }

    public static String resource(String name)
    {
        return tag("resource", make_map("name", name));
    }

    public static String set(String name, Map<String, String> attrs)
    {
        return tag("set", make_map("name", name),
                   attribute_map_to_string(attrs));
    }

    public static String tag(String name, Map<String, String> attrs)
    {
        return "<" + name + attrs_to_string(attrs) + "/>";
    }

    public static String tag(String name,
                             Map<String, String> attrs,
                             String children)
    {
        return "<" + name + attrs_to_string(attrs) + ">" + children +
            "</" + name + ">";
    }

    public static String tcp_sink(String ip, Integer port)
    {
        Map<String, String> map = make_map("ip_addr", ip);
        map.put("ip_port", port.toString());
        return tag("tcp_sink", map);
    }

    public static String unmap(String name)
    {
        return tag("unmap", make_map("name", name));
    }

    public static String zero_job(String id)
    {
        return tag("zero", make_map(), tag("job", make_map("id", id)));
    }

    public static String zero_resource(String name)
    {
        return tag("zero", make_map(), resource(name));
    }

    //----------------------------------------------------------------------
    // Helpers

    public static Map<String, String> make_map()
    {
        return new HashMap<String, String>();
    }

    public static Map<String, String> make_map(String k, String v)
    {
        HashMap<String, String> map = new HashMap<String, String>();
        map.put(k, v);
        return map;
    }

    private static String attribute_map_to_string(Map<String, String> map)
    {
        StringBuffer sb = new StringBuffer();
        map.forEach( (k, v) -> sb.append(xml.attribute(k, v)) );
        return sb.toString();
    }

    private static String new_monitor(String type,
                                      String ip,
                                      Integer port,
                                      String source)
    {
        Map<String, String> map = make_map("ip_addr", ip);
        map.put("ip_port", port.toString());

        return g_new(tag(type, map, source));
    }

}

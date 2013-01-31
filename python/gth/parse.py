# Title: A parser for the responses which can come out of a Corelatus GTH.
#
# Author: Matthias Lang (matthias@corelatus.se)
#
# The GTH uses a text protocol which is a cut-down XML. It can be parsed
# using an XML parser (probably a DOM one), but that would be overkill.
#
from pyparsing import Word, quotedString, removeQuotes, ZeroOrMore, Or
from pyparsing import alphas, Dict, dictOf, Suppress, Forward, Literal

quotedString.setParseAction(removeQuotes)

gth_out_grammar = None

def _attlist():
    dict = dictOf(Word(alphas), Suppress("=") + quotedString)
    return dict

def _tag(tagname, children, expect_attrs = 1):
    # revisit: can we get rid of this repetition? How?
    if expect_attrs == 0:
        return Suppress("<") + Literal(tagname) + Suppress(">") \
            + children \
            + Suppress("</") + Suppress(tagname) + Suppress(">")

    else:
        return Suppress("<") + Literal(tagname) + _attlist() + Suppress(">") \
            + children \
            + Suppress("</") + Suppress(tagname) + Suppress(">")


def _empty_tag(tagname, expect_attrs = 1):
    if expect_attrs == 0:
        return Suppress("<") + Literal(tagname) + Suppress("/>")
    else:
        return Suppress("<") + Literal(tagname) + _attlist() + Suppress("/>")

# This is ugly but works. There must be a better way. In Erlang, I'd
# do this
#
#  collapse_attributes(_, []) -> [];
#  collapse_attributes(_, ['attribute', ['name', K], ['value', V] | T]) ->
#    [{K,V}|collapse_attributes(T)]
#
# I can't see how to do that in python because there's no pattern matching.
#
def _collapse_attributes(string, tokens):
    dict = {}
    for x in range(0, len(tokens) / 3):
        key = tokens[x*3+1][1]
        value =  tokens[x*3+2][1]
        dict[key] = value
    return dict

def gth_out():
    """
    A grammar to parse the responses which can come out of the GTH.
    This was written with reference to gth_out.dtd
    More information about the GTH API at http://www.corelatus.com/gth/api/
    """

    global gth_out_grammar
    if not gth_out_grammar:
        # Literals
        open    = Suppress("<")
        close   = Suppress(">")
        emclose = Suppress("/>")

        tagattr = Word(alphas) + Suppress("=") + quotedString

        ok      = _empty_tag("ok", 0)
        job     = _empty_tag("job")
        error   = _empty_tag("error") | _tag("error", Word(alphas + " "))

        event_child = open \
            + Word(alphas + "_0123456789").setResultsName("type") \
            + _attlist() + emclose
        event   = _tag("event", event_child, 0)

        attributes = ZeroOrMore(_empty_tag("attribute"))
        attributes.setParseAction(_collapse_attributes)
        resource = _empty_tag("resource") ^ _tag("resource", attributes)
        resources = ZeroOrMore(resource)

        # REVISIT: state grammar below is incomplete
        state   = _tag("state", resources, 0)

        gth_out_grammar = ok ^ job ^ event ^ state ^ resource ^ error

    return gth_out_grammar

def test():
    gth_out().parseString("<job id=\"m2mo9\"/>")
    gth_out().parseString("<ok/>")
    gth_out().parseString("<error reason='badarg'/>")
    gth_out().parseString("<event><l1_message name='3A' state='ok'/></event>")
    print "all ok"

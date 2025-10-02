# Copyright (c) 2020–2025, Corelatus AB
# All rights reserved.
#
# Licensed under the BSD 3-Clause License. See the LICENSE file
# in the project root for full license information.
#

"""A parser for the responses which can come out of a Corelatus GTH.
"""

# The GTH uses a text protocol which is a cut-down XML. It can be parsed
# using an XML parser (probably a DOM one), but that would be overkill.

from pyparsing import Word, quotedString, removeQuotes, ZeroOrMore
from pyparsing import alphas, dictOf, Suppress, Literal

quotedString.setParseAction(removeQuotes)

gth_out_grammar = None   # pylint: disable=invalid-name

# Literals
OPEN  = Suppress("<")
ENDOPEN = Suppress("</")
CLOSE = Suppress(">")
EMCLOSE = Suppress("/>")


def _attlist():
    p_dict = dictOf(Word(alphas), Suppress("=") + quotedString)
    return p_dict

def _tag(tagname, children, expect_attrs = 1):

    if expect_attrs == 0:
        return OPEN + Literal(tagname) + CLOSE \
            + children + ENDOPEN + Suppress(tagname) + CLOSE

    return OPEN + Literal(tagname) + _attlist() + CLOSE \
        + children + ENDOPEN + Suppress(tagname) + CLOSE

def _empty_tag(tagname, expect_attrs = 1):
    if expect_attrs == 0:
        return OPEN + Literal(tagname) + EMCLOSE

    return OPEN + Literal(tagname) + _attlist() + EMCLOSE

# This is ugly but works. There must be a better way. In Erlang, I'd
# do this
#
#  collapse_attributes(_, []) -> [];
#  collapse_attributes(_, ['attribute', ['name', K], ['value', V] | T]) ->
#    [{K,V}|collapse_attributes(T)]
#
# I can't see how to do that in python because there's no pattern matching.
#
def _collapse_attributes(_string, tokens):
    p_dict = {}
    for x in range(0, len(tokens) // 3):
        key = tokens[x*3+1][1]
        value =  tokens[x*3+2][1]
        p_dict[key] = value
    return p_dict

def gth_out():
    """
    A grammar to parse the responses which can come out of the GTH.
    This was written with reference to gth_out.dtd
    More information about the GTH API at https://www.corelatus.com/gth/api/
    """

    global gth_out_grammar    # pylint: disable=global-statement
    if not gth_out_grammar:
        ok      = _empty_tag("ok", 0)
        job     = _empty_tag("job")
        error   = _empty_tag("error") | _tag("error", Word(alphas + " "))

        event_child = OPEN \
            + Word(alphas + "_0123456789").setResultsName("type") \
            + _attlist() + EMCLOSE
        event   = _tag("event", event_child, 0)

        attributes = ZeroOrMore(_empty_tag("attribute"))
        attributes.setParseAction(_collapse_attributes)
        resource = _empty_tag("resource") ^ _tag("resource", attributes)
        resources = ZeroOrMore(resource) ^ error

        state  = _tag("state", resources, 0) | _tag("state", resource, 0)

        gth_out_grammar = ok ^ job ^ event ^ state ^ resource ^ error

    return gth_out_grammar

def _test():
    gth_out().parseString("<job id=\"m2mo9\"/>")
    gth_out().parseString("<ok/>")
    gth_out().parseString("<error reason='badarg'/>")
    gth_out().parseString("<event><l1_message name='3A' state='ok'/></event>")
    gth_out().parseString("<state><resource name=\"system_log\"/></state>")
    print("all ok")

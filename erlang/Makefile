ERLC=erlc

# The 'test' and 'live' modules are copied in from other parts of the GTH
# source tree.
test_modules=gth xml 
live_modules=gth_client_xml_parse gth_apilib gth_xml_parse
example_modules=save_to_pcap ss7_sniffer record_and_play_back
beams=$(addsuffix .beam, $(test_modules) $(live_modules) $(example_modules))

test_erls=$(addsuffix .erl, $(test_modules))
live_erls=$(addsuffix .erl, $(live_modules))
example_erls=$(addsuffix .erl, $(example_modules))

all: $(beams)

ship: $(test_erls) $(live_erls) $(example_erls) gth_api.hrl README
	zip gth_erlang_api.zip $^

clean:
	rm -f *.beam

spotless: clean
	rm -f $(test_erls) $(live_erls) gth_api.hrl

$(beams): gth_api.hrl

%.beam: %.erl
	$(ERLC) -W $<

$(test_erls): %: ../../erl/test/%
	cp $< .

$(live_erls): %: ../../erl/src/%
	cp $< .

gth_api.hrl: ../../erl/include/gth_api.hrl
	cp $< .

ERLC=erlc

live_modules=gth xml gth_client_xml_parse gth_apilib gth_xml_parse gth_xml_scan
example_modules=save_to_pcap ss7_sniffer record_and_play_back
beams=$(addsuffix .beam, $(live_modules) $(example_modules))

live_erls=$(addsuffix .erl, $(live_modules))
example_erls=$(addsuffix .erl, $(example_modules))

default:
	erl -make

ship: $(test_erls) $(live_erls) $(example_erls) gth_api.hrl README
	zip gth_erlang_api.zip $^

clean:
	rm -f *.beam

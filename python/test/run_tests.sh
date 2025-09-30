#!/bin/bash -eEu
#
# Run all tests. Two arguments: the SRC and DUT IP
# The test expects a loopback cable between SRC and DUT, from P2 to P1

# Set up error handling for all tests
export TOP_PID=$$
#trap "exit 2" TERM

trap 'echo "Error: script failed at line $LINENO (command: $BASH_COMMAND)" >&2' ERR


die()
{
    echo "Abort: $@" 1>&2
    kill -s TERM $TOP_PID
}

usage()
{
    echo "Usage: $0 <source-IP> <dut-IP>"
    echo ""
    exit 1
}

maybe_fetch_signalling()
{
    if [ ! -f 2019_03_signalling.raw ]; then
	wget https://www.corelatus.com/blog/static/2019_03_signalling.raw
    fi
}

is_optical_hardware()
{
    if ../gth.py $DUT_IP query inventory | grep sdh1; then
        return 0
    else
        return 1
    fi
}

play_and_isup()
{
    echo -e "\n--- ${FUNCNAME[0]} ---"

    if is_optical_hardware; then
        echo "skipping ISUP tests on optical hardware"
        # Making this test work would require a complex
        # test setup on optical hardware.
        return
    fi

    ../gth.py $DUT_IP enable pcm1A tx_enabled false
    ../gth.py $SRC_IP enable pcm2A
    ../sniff_isup.py $DUT_IP 1A 1 > sniff_isup.output &
    sniff_pid=$!
    ../play.py $SRC_IP 2A 1 2019_03_signalling.raw
    kill -TERM $sniff_pid
    wait $sniff_pid || true

    if ! diff -q sniff_isup.output sniff_isup.output.ref; then
        die "ISUP output disagrees with reference, see sniff_isup.output*"
    fi
}

record_and_play()
{
    echo -e "\n--- ${FUNCNAME[0]} ---"

    if is_optical_hardware; then
        echo "skipping record tests on optical hardware"
        # Making this test work would require a complex
        # test setup on optical hardware.
        return
    fi

    head -c 1000 /dev/random > random.raw
    ../record.py $DUT_IP 1A 1 8000 recording.raw &
    record_pid=$!
    sleep 0.5
    ../play.py $SRC_IP 2A 1 random.raw
    wait $record_pid || true

    if ! python3 ./file_compare.py random.raw recording.raw; then
        die "unable to recover playback data in the recording"
    fi

    rm recording.raw random.raw
}

gth_cli_e1()
{
    ../gth.py $DUT_IP enable pcm1A tx_enabled false
    ../gth.py $DUT_IP query pcm1A | grep code_violation_seconds
    ../gth.py $DUT_IP zero pcm1A
    ../gth.py $DUT_IP disable pcm1A
    ../gth.py $DUT_IP query pcm1A | grep status.disabled
}

gth_cli_sdh()
{
    ../gth.py $DUT_IP enable sdh1
    ../gth.py $DUT_IP query sdh1 | grep daisy_chain
    ../gth.py $DUT_IP zero sdh1
    ../gth.py $DUT_IP map sdh1:hop1_1:lop1_1_1
    ../gth.py $DUT_IP unmap pcm1
    ../gth.py $DUT_IP disable sdh1
    ../gth.py $DUT_IP query sdh1 | grep status.disabled
}

# Check the 'gth.py' script. We run each command it has, except 'reset'
gth_cli()
{
    echo -e "\n--- ${FUNCNAME[0]} ---"

    ../gth.py $DUT_IP query os | grep last.restart || die "query failed"
    ../gth.py $DUT_IP query bogus_resource && die "bogus query failed"

    ../gth.py $DUT_IP set application_log verbosity all || die "bad log query"
    verb=$(../gth.py $DUT_IP query application_log | grep verbosity)
    echo $verb

    # Don't test 'reset', it reboots the card
    # ../gth.py $DUT_IP reset

    if ../gth.py $DUT_IP query inventory | grep sdh1; then
        gth_cli_sdh
    else
        gth_cli_e1
    fi
}

aal5()
{
    echo -e "\n--- ${FUNCNAME[0]} ---"

    span="1A"
    if is_optical_hardware ; then
        ../gth.py $DUT_IP enable sdh1
        resource=$(../gth.py $DUT_IP map sdh1:hop1_1:lop1_1_1)
        span=$(echo $resource | sed -e 's/pcm//')
    fi
    ../aal5.py $DUT_IP $span 30 0 5
    if is_optical_hardware ; then
        ../gth.py $DUT_IP disable sdh1
    fi
}

transmit_ss7()
{
    if ! ../gth.py $DUT_IP query system_image | grep duplex; then
        echo "Skipping transmit_ss7 test; must be run on duplex DUT."
        return
    fi

    # This test has room for improvement. It would be better if we
    # checked that data is actually transmitted, e.g. with 'save_to_pcap'.
    ../transmit_ss7.py $DUT_IP 2A
}


if [ $# -ne 2 ]; then usage; fi

SRC_IP=$1
DUT_IP=$2

gth_cli
play_and_isup
record_and_play
aal5
transmit_ss7

echo "tests all done"

# Functions used by several tests

# Set up error handling for all tests
export TOP_PID=$$
trap "exit 2" TERM

die()
{
    echo $@ 1>&2
    kill -s TERM $TOP_PID
}

# Return 0 if the card is duplex
is_duplex_card()
{
    local IP=$1
    local rc=0
    ../query_set $IP system_image version | grep -q duplex || rc=$?
    echo "$rc"
}

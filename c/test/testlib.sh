# Functions used by several tests

# Set up error handling for all tests
export TOP_PID=$$
trap "exit 2" TERM

export BUILD_DIR=../build/unix-gcc/

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
    $BUILD_DIR/query_set $IP system_image version | grep -q duplex || rc=$?
    echo "$rc"
}

# Convert a numerical exit code to a signal name, e.g. SEGV
exit_code_to_name()
{
    echo $(kill -l $1)
}

card_arch()
{
    local IP=$1
    arch=$($BUILD_DIR/query_set $IP board architecture | cut -d = -f 2)
    echo "$arch"
}

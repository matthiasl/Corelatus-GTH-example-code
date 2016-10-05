using System;
using System.Linq;

namespace SaveToPcap
{
    public class Mtp2Packet : EventArgs
    {
        private readonly byte[] _header;
        private readonly byte[] _payload;

        private readonly Lazy<ushort> _tag;
        private readonly Lazy<ushort> _flags;

        private readonly uint _secs;
        private readonly uint _uSecs;

        public ushort Tag
        {
            get { return _tag.Value; }
        }

        public ushort Flags
        {
            get { return _flags.Value; }
        }

        public byte[] Payload
        {
            get { return _payload; }
        }

        public uint Seconds { get; private set; }
        public uint Microseconds { get; private set; }
 
        public Mtp2Packet(byte[] header, byte[] payload)
        {
            _header = header;
            _payload = payload;

            _tag = new Lazy<ushort>(() => EndianReader.ReadUInt16Big(_header, 0));
            _flags = new Lazy<ushort>(() => EndianReader.ReadUInt16Big(_header, 2));


            var tsHigh = EndianReader.ReadUInt16Big(_header, 4);
            var tsLow  =EndianReader.ReadUInt32Big(_header, 6);

            ulong ts = tsHigh;
            ts <<= 32;
            ts += tsLow;

            Seconds = (uint)(ts / 1000);
            Microseconds = (uint)((ts % 1000) * 1000);
        }

        public Mtp2Packet(byte[] data)
            : this(data.Take(10).ToArray(), data.Skip(10).ToArray())
        {
        }
    }
}

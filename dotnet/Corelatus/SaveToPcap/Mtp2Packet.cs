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
        private readonly Lazy<ushort> _tsHigh;
        private readonly Lazy<uint> _tsLow;

        public ushort Tag
        {
            get { return _tag.Value; }
        }

        public ushort Flags
        {
            get { return _flags.Value; }
        }

        public ushort TimestampHigh
        {
            get { return _tsHigh.Value; }
        }

        public uint TimestampLow
        {
            get { return _tsLow.Value; }
        }

        public byte[] Payload
        {
            get { return _payload; }
        }

        public Mtp2Packet(byte[] header, byte[] payload)
        {
            _header = header;
            _payload = payload;

            _tag = new Lazy<ushort>(() => EndianReader.ReadUInt16Big(_header, 0));
            _flags = new Lazy<ushort>(() => EndianReader.ReadUInt16Big(_header, 2));
            _tsHigh = new Lazy<ushort>(() => EndianReader.ReadUInt16Big(_header, 4));
            _tsLow = new Lazy<uint>(() => EndianReader.ReadUInt32Big(_header, 6));
        }

        public Mtp2Packet(byte[] data)
            : this(data.Take(10).ToArray(), data.Skip(10).ToArray())
        {
        }
    }
}

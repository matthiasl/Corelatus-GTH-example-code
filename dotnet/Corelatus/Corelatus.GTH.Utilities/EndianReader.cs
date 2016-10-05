using System;

namespace Corelatus.GTH.Utilities
{
    public static class EndianReader
    {
        public unsafe static UInt16 ReadUInt16Big(byte[] data, int offset)
        {
            UInt16 value;
            byte* ptr = (byte*)&value;

            *(ptr) = data[offset + 1];
            *(ptr + 1) = data[offset];

            return value;
        }

        public unsafe static UInt32 ReadUInt32Big(byte[] data, int offset)
        {
            UInt32 value;
            byte* ptr = (byte*)&value;

            *ptr = data[offset + 3];
            *(ptr + 1) = data[offset + 2];
            *(ptr + 2) = data[offset + 1];
            *(ptr + 3) = data[offset];

            return value;
        }

        public unsafe static UInt64 ReadUInt64Big(byte[] data, int offset)
        {
            UInt64 value;
            byte* ptr = (byte*)&value;

            *ptr = data[offset + 7];
            *(ptr + 1) = data[offset + 6];
            *(ptr + 2) = data[offset + 5];
            *(ptr + 3) = data[offset + 4];
            *(ptr + 4) = data[offset + 3];
            *(ptr + 5) = data[offset + 2];
            *(ptr + 6) = data[offset + 1];
            *(ptr + 7) = data[offset];

            return value;
        }
    }
}

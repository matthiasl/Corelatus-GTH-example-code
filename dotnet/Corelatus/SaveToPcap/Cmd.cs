using System;
using System.Collections.Generic;
using System.Linq;
using CLAP;
using PcapngUtils.Common;
using PcapngUtils.Pcap;

namespace SaveToPcap
{
    class Cmd
    {
        private PcapWriter _writer;

        [Verb(IsDefault=true)]
        public void Test(
            [Required]string ip,
            [DefaultValue(false)]bool fisu, 
            [DefaultValue(false)] bool esnf,
            [DefaultValue(true)][Aliases("m")]bool monitor,
            [DefaultValue(false)][Aliases("v")]bool verbose,
            [Required][Aliases("s")]string spans,
            [Required][Aliases("o")]string output)
        {
            Console.WriteLine("Output file is '{0}'",output);
            var header = SectionHeader.CreateEmptyHeader();
            header.LinkType=LinkTypes.Mtp2;
            _writer = new PcapWriter(output, header);

            var channels = DefineChannels(spans);

            Console.WriteLine("Openning Mtp2 receiver...");
            var receiver = new Mtp2Receiver(new Config
            {
                Address = ip,
                Esnf = esnf,
                Fisu = fisu,
                Monitoring = monitor,
                Verbose=verbose,
                Channels = channels
            });

            receiver.PacketReceived+=PacketReceived;
            using (receiver)
            {
                receiver.Start();
                Console.WriteLine("Mtp2 montoring started.");
                Console.WriteLine("Press any key to exit.");
                Console.ReadKey(true);
            }
            _writer.Dispose();
        }

        private static List<Channel> DefineChannels(string spans)
        {
            Console.WriteLine("Defining channels...");
            var channels = new List<Channel>();
            var parsedSpans = spans.Split(',');

            foreach (var span in parsedSpans)
            {
                var parts = span.Trim().Split(' ');
                var ch = parts[0];
                var tsArr = parts.Skip(1).Select(t => int.Parse(t)).ToArray();

                for (var j = 0; j < tsArr.Length; j++)
                {
                    Console.WriteLine("Channel '{0}:{1}' defined.", ch, tsArr[j]);
                    var c = new Channel
                    {
                        Span = ch,
                        Timeslots = new[] {tsArr[j]}
                    };
                    channels.Add(c);
                }
            }
            return channels;
        }

        private  void PacketReceived(object sender, Mtp2Packet e)
        {
            _writer.WriteUnsafe(e.Seconds, e.Microseconds, e.Payload);
        }
    }
}

using System;
using System.Collections.Generic;
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
            [Required][Aliases("t")]string timeslots,
            [Required][Aliases("o")]string output)
        {
            Console.WriteLine("Output file is '{0}'",output);
            var header = SectionHeader.CreateEmptyHeader();
            header.LinkType=LinkTypes.Mtp2;
            _writer = new PcapWriter(output, header);

            var parsedTs = timeslots.Split(' ');
            var tsArr = new int[parsedTs.Length];
            Console.Write("Timeslots are: ");
            for (var i = 0; i < parsedTs.Length; i++)
            {
                tsArr[i] = int.Parse(parsedTs[i]);
                Console.Write("{0},",tsArr[i]);
            }
            Console.WriteLine();

            var parsedSpans = spans.Split(' ');

            Console.WriteLine("Defining channels...");
            var channels = new List<Channel>();
            for(var j=0;j<tsArr.Length;j++)
                for (var i = 0; i < parsedSpans.Length; i++)
                {
                    Console.WriteLine("Channel '{0}:{1}' defined.", parsedSpans[i],tsArr[j]);
                    var c = new Channel
                    {
                        Span = parsedSpans[i],
                        Timeslots = new[] {tsArr[j]}
                    };
                    channels.Add(c);
                }

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

            receiver.Start();
            Console.WriteLine("Mtp2 montoring started.");
            Console.ReadKey(true);
        }

        private  void PacketReceived(object sender, Mtp2Packet e)
        {
            _writer.WriteUnsafe(e.TimestampHigh, e.TimestampLow, e.Payload);
        }
    }
}

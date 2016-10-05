using System.Collections.Generic;
using Corelatus.GTH;

namespace SaveToPcap
{
    public static class Extensions
    {
        public static bool EnableElectricalL1(this Device device,string span, bool monitoring)
        {
            var atts = new List<DeviceAttribute>
            {
                new DeviceAttribute("status", "enabled"),
                new DeviceAttribute("tx_enabled", "false"),
            };
            if (monitoring)
                atts.Add(new DeviceAttribute("monitoring", "true"));

            span = "pcm" + span;

            return device.Set(span, atts);
        }

        public static bool EnableOpticalL1(this Device device,string span)
        {
            return device.Enable(span, null);
        }

        public static bool EnableL1(this Device device,IEnumerable<Channel> channels, bool monitoring)
        {
            var value = device.QueryResourceAttribute("board", "architecture");
            var succeed = true;
            if (value.StartsWith("gth"))
            {
                foreach (var c in channels)
                    succeed &= EnableElectricalL1(device, c.Span, monitoring);
            }
            else
            {
                foreach (var c in channels)
                    succeed &= EnableOpticalL1(device, c.Span);
            }
            return succeed;
        }

        public static bool MonitorMtp2(this Device device, Mtp2Config config, ref string jobId)
        {
            var atts = new List<DeviceAttribute>();

            if (config.DropFisus)
                atts.Add(new DeviceAttribute("fisu", "no"));

            if (config.Esnf)
                atts.Add(new DeviceAttribute("esnf", "yes"));

            var res = device.NewMtp2Monitor(config.Tag, config.Channel.Span, config.Channel.Timeslots, config.Listener,
                atts, ref jobId);

            return res;
        }
    }
}

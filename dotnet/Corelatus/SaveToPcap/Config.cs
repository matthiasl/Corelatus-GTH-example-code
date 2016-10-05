using System.Collections.Generic;

namespace SaveToPcap
{
    public class Config
    {
        public string Address { get; set; }
        public bool Fisu { get; set; }
        public bool Esnf { get; set; }
        public bool Monitoring { get; set; }
        public bool Verbose { get; set; }
        public IEnumerable<Channel> Channels { get; set; } 
    }
}

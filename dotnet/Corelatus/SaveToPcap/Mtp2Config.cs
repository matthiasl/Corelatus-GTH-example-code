using Corelatus.GTH;

namespace SaveToPcap
{
    public class Mtp2Config
    {
        public Channel Channel { get; set; }
        public Listener Listener { get; set; }
        public bool DropFisus { get; set; }
        public bool Esnf { get; set; }
        public int Tag { get; set; }
    }
}

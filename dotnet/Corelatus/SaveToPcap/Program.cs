using CLAP;

namespace SaveToPcap
{
    class Program
    {
        private static void Main(string[] args)
        {
            Parser.RunConsole(args, new Cmd());
        }
    }
}

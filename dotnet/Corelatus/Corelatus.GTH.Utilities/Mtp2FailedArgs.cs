using System;

namespace Corelatus.GTH.Utilities
{
    public class Mtp2FailedArgs:EventArgs
    {
        public Exception Exception { get;private set; }

        internal Mtp2FailedArgs(Exception exception)
        {
            Exception = exception;
        }
    }
}

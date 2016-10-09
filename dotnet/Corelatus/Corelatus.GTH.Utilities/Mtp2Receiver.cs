using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Corelatus.GTH.Utilities
{
    public class Mtp2Receiver:IDisposable
    {
        public event EventHandler<Mtp2Packet> PacketReceived;
        public event EventHandler<Mtp2FailedArgs> Failed;
 
        private readonly Config _config;
        private Device _device;
        private Listener _listener;
        private DataConnection _dataConn;
        private bool _started;
        private readonly ManualResetEventSlim _waitHandle = new ManualResetEventSlim();

        private readonly List<string> _jobIds = new List<string>();

        public IEnumerable<string> GetJobIds()
        {
            return _jobIds.ToArray();
        } 

        public Mtp2Receiver(Config config)
        {
            _config = config;
        }

        public void Start()
        {
            Connect();
            EnableL1();
            OpenListener();
            MonitorMtp2();
            EstablishData();

            _started = true;
            Task.Factory.StartNew(Fetch, TaskCreationOptions.LongRunning);
        }

        private void OnPacketReceived(Mtp2Packet packet)
        {
            if (_started && PacketReceived != null)
                PacketReceived(this, packet);
        }

        private void OnFailed(Exception exception)
        {
            if (Failed != null)
                Failed(this, new Mtp2FailedArgs(exception));
        }

        private void Fetch()
        {
            var reader = _dataConn.GetReader();
            var lenBuffer = new byte[2];

            while (_started)
            {
                try
                {
                    if (_device.WaitForPacket(_dataConn))
                    {
                        reader.ReadExact(lenBuffer, 0, lenBuffer.Length);
                        var len=EndianReader.ReadUInt16Big(lenBuffer, 0);
                        var header = new byte[10];
                        reader.ReadExact(header, 0, header.Length);
                        var payload = new byte[len - header.Length];
                        reader.ReadExact(payload, 0, payload.Length);

                        var mtp2 = new Mtp2Packet(header, payload);
                        OnPacketReceived(mtp2);
                    }
                }
                catch (Exception e)
                {
                    OnFailed(e);
                }
            }

            _waitHandle.Set();
        }

        private void CleanResources()
        {
            foreach (var j in _jobIds)
                _device.Delete(j);

            _dataConn.Dispose();
            _listener.Dispose();
            _device.Dispose();
        }

        private void EstablishData()
        {
            _dataConn = _device.WaitForAccept(_listener) as DataConnection;
            if (_dataConn == null)
                throw new CorelatusException(string.Format("failed to open data connection for device '{0}'.", _config.Address));
        }

        private void MonitorMtp2()
        {
            var tag = 0;
            foreach (var c in _config.Channels)
            {
                var mtp2Config = new Mtp2Config
                {
                    Channel = c,
                    DropFisus = !_config.Fisu,
                    Esnf = _config.Esnf,
                    Listener = _listener,
                    Tag = tag
                };
                tag++;

                string jobId=null;
                if (!_device.MonitorMtp2(mtp2Config, ref jobId))
                    throw new CorelatusException(string.Format("failed to channel {0} for device '{1}'.", tag - 1,
                        _config.Address));
                _jobIds.Add(jobId);
            }
        }

        private void Connect()
        {
            _device = Device.Connect(_config.Address,_config.Verbose);
            if (_device == null)
                throw new CorelatusException(string.Format("failed to open device '{0}'.", _config.Address));
        }

        private void OpenListener()
        {
            _listener = _device.Listen() as Listener;
            if (_listener == null)
                throw new CorelatusException(string.Format("failed to open listener for device '{0}'.", _config.Address));
        }

        private void EnableL1()
        {
            if (!_device.EnableL1(_config.Channels, _config.Monitoring))
                throw new CorelatusException(string.Format("failed to enable L1 for device '{0}'.", _config.Address));
        }

        public void Dispose()
        {
            PacketReceived = null;
            _started = false;
            _waitHandle.Wait(10000);
            CleanResources();
        }
    }
}

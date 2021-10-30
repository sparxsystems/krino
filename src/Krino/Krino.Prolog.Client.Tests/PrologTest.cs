using Krino.Vertical.Utils.Serializers;
using NUnit.Framework;

namespace Krino.Prolog.Client.Tests
{
    public class PrologTest
    {
        [Test]
        public void Add()
        {
            var prolog = new Prolog("http://localhost:8123/");

            var serializer = new DataContractJsonStringSerializer();
            var arg = (string)serializer.Serialize("like(a, b)");
            var task = prolog.Add("{\"a\":1, \"b\":2}");
            task.Wait(3000);
        }

        [Test]
        public void Clear()
        {
            var prolog = new Prolog("http://localhost:8123/");
            var task = prolog.Clear();
            task.Wait(3000);
        }

        [Test]
        public void Evaluate()
        {
            var prolog = new Prolog("http://localhost:8123/");
            var task = prolog.Evaluate("bla");
            task.Wait(3000);
        }
    }
}
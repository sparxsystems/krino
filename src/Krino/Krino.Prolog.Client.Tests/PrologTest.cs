using NUnit.Framework;

namespace Krino.Prolog.Client.Tests
{
    public class PrologTest
    {
        [Test]
        public void Test1()
        {
            var prolog = new Prolog("http://127.0.0.1:8123/");
            var task = prolog.Evaluate("bla");
            task.Wait(3000);
        }
    }
}
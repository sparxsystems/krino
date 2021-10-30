using NUnit.Framework;

namespace Krino.Prolog.Client.Tests
{
    public class PrologTest
    {
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
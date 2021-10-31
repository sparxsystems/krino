using NUnit.Framework;
using System.Threading.Tasks;

namespace Krino.Prolog.Client.Tests
{
    public class PrologTest
    {
        [Test]
        public async Task Add()
        {
            using var prolog = new Prolog("http://localhost:8123/");
            await prolog.Add("like(a, b)");
        }

        [Test]
        public async Task Remove()
        {
            using var prolog = new Prolog("http://localhost:8123/");
            await prolog.Add("like(a, b)");
            await prolog.Add("like(c, d)");

            await prolog.Remove("like(a, b)");
        }

        [Test]
        public void Clear()
        {
            using var prolog = new Prolog("http://localhost:8123/");
            var task = prolog.Clear();
            task.Wait(3000);
        }

        [Test]
        public async Task Evaluate()
        {
            using var prolog = new Prolog("http://localhost:8123/");
            await prolog.Add("like(a, b)");

            await prolog.Evaluate("like(a, b)");
        }
    }
}
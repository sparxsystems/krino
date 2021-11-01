using NUnit.Framework;
using System;
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
        public async Task Clear()
        {
            using var prolog = new Prolog("http://localhost:8123/");

            await prolog.Add("like(a, b)");
            await prolog.Add("like(c, d)");

            Assert.IsTrue(await prolog.Evaluate("like(a, b)"));
            Assert.IsTrue(await prolog.Evaluate("like(c, d)"));

            await prolog.Clear();

            Assert.IsFalse(await prolog.Evaluate("like(a, b)"));
            Assert.IsFalse(await prolog.Evaluate("like(c, d)"));
        }

        [Test]
        public async Task Evaluate()
        {
            using var prolog = new Prolog("http://localhost:8123/");
            await prolog.Add("like(a, b)");

            var result = await prolog.Evaluate("like(a, b)");
            Assert.IsTrue(result);

            result = await prolog.Evaluate("like(x, y)");
            Assert.IsFalse(result);
        }
    }
}
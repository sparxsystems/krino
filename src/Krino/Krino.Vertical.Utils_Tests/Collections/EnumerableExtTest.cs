using Krino.Vertical.Utils.Collections;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils_Tests.Collections
{
    [TestFixture]
    public class EnumerableExtTest
    {
        [Test]
        public void FindSimilar()
        {
            List<string> source = new List<string>()
            {
                "abcd",
                "bcde",
                "wxyz"
            };

            List<string> result = source.FindSimilar("bcd", 1).ToList();
            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("abcd", result[0]);
            Assert.AreEqual("bcde", result[1]);
        }

        [Test]
        public void TakeUntil()
        {
            List<int> l = new List<int>() { 1, 2, 3, 4, 5 };

            List<int> result = l.TakeUntil(x => x < 3).ToList();
            Assert.AreEqual(3, result.Count);
            Assert.AreEqual(1, result[0]);
            Assert.AreEqual(2, result[1]);

            // This one has broken the condition but it shall be included.
            Assert.AreEqual(3, result[2]);

            // Empty sequence.
            l = new List<int>();
            result = l.TakeUntil(x => x < 3).ToList();
            Assert.AreEqual(0, result.Count);
        }
    }
}

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

        [Test]
        public void TakeFromLastOccuranceOf()
        {
            List<int> l = new List<int>() { 1, 3, 3, 4, 5 };

            List<int> result = l.TakeFromLastOccuranceOf(x => x == 3).ToList();
            Assert.AreEqual(3, result.Count);
            Assert.AreEqual(3, result[0]);
            Assert.AreEqual(4, result[1]);
            Assert.AreEqual(5, result[2]);

            // Empty sequence.
            l = new List<int>();
            result = l.TakeUntil(x => x < 3).ToList();
            Assert.AreEqual(0, result.Count);
        }

        [Test]
        public void GetVariations()
        {
            List<List<int>> source = new List<List<int>>()
            {
                new List<int>() {1, 2},
                new List<int>() {2, 21},
                new List<int>() {3, 31},
            };

            List<List<int>> variations = source.GetVariations().Select(x => x.ToList()).ToList();

            Assert.AreEqual(8, variations.Count());

            Assert.AreEqual(1, variations[0][0]);
            Assert.AreEqual(2, variations[0][1]);
            Assert.AreEqual(3, variations[0][2]);

            Assert.AreEqual(2, variations[1][0]);
            Assert.AreEqual(2, variations[1][1]);
            Assert.AreEqual(3, variations[1][2]);

            Assert.AreEqual(1, variations[2][0]);
            Assert.AreEqual(21, variations[2][1]);
            Assert.AreEqual(3, variations[2][2]);

            Assert.AreEqual(2, variations[3][0]);
            Assert.AreEqual(21, variations[3][1]);
            Assert.AreEqual(3, variations[3][2]);

            Assert.AreEqual(1, variations[4][0]);
            Assert.AreEqual(2, variations[4][1]);
            Assert.AreEqual(31, variations[4][2]);

            Assert.AreEqual(2, variations[5][0]);
            Assert.AreEqual(2, variations[5][1]);
            Assert.AreEqual(31, variations[5][2]);

            Assert.AreEqual(1, variations[6][0]);
            Assert.AreEqual(21, variations[6][1]);
            Assert.AreEqual(31, variations[6][2]);

            Assert.AreEqual(2, variations[7][0]);
            Assert.AreEqual(21, variations[7][1]);
            Assert.AreEqual(31, variations[7][2]);
        }

        [Test]
        public void IsSingle()
        {
            Assert.IsFalse("".IsSingle());
            Assert.IsTrue("a".IsSingle());
            Assert.IsFalse("ab".IsSingle());
        }
    }
}

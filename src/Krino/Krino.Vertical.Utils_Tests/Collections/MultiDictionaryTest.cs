using Krino.Vertical.Utils.Collections;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils_Tests.Collections
{
    [TestFixture]
    public class MultiDictionaryTest
    {
        [Test]
        public void Add()
        {
            MultiDictionary<string, string> dictionary = new MultiDictionary<string, string>();
            
            dictionary.Add("1", "2");
            Assert.AreEqual(1, dictionary.Count);
            Assert.IsTrue(dictionary.ContainsKey("1"));
            Assert.IsTrue(dictionary.Contains("1", "2"));
            Assert.AreEqual("2", dictionary["1"].First());

            dictionary.Add("1", "2");
            Assert.AreEqual(1, dictionary.Count);

            dictionary.Add("1", "3");
            Assert.AreEqual(2, dictionary.Count);
            Assert.IsTrue(dictionary.ContainsKey("1"));
            Assert.IsTrue(dictionary.Contains("1", "2"));
            Assert.IsTrue(dictionary.Contains("1", "3"));

            dictionary.Add("2", "1");
            Assert.AreEqual(3, dictionary.Count);
            Assert.IsTrue(dictionary.ContainsKey("1"));
            Assert.IsTrue(dictionary.ContainsKey("2"));
            Assert.IsTrue(dictionary.Contains("1", "2"));
            Assert.IsTrue(dictionary.Contains("1", "3"));
            Assert.IsTrue(dictionary.Contains("2", "1"));
        }

        [Test]
        public void Add_Sequence()
        {
            MultiDictionary<string, string> dictionary = new MultiDictionary<string, string>();

            dictionary.Add("1", new List<string>() { "2", "3" });
            Assert.AreEqual(2, dictionary.Count);
            Assert.IsTrue(dictionary.ContainsKey("1"));
            Assert.IsTrue(dictionary.Contains("1", "2"));
            Assert.IsTrue(dictionary.Contains("1", "3"));
        }

        [Test]
        public void Indexer()
        {
            MultiDictionary<string, string> dictionary = new MultiDictionary<string, string>();

            dictionary["1"] = new ReadOnlySet<string>(new HashSet<string>() { "2", "3" });
            ISet<string> result = dictionary["1"];
                
            Assert.AreEqual(2, result.Count);
            Assert.IsTrue(result.Contains("2"));
            Assert.IsTrue(result.Contains("3"));
        }

        [Test]
        public void Remove()
        {
            MultiDictionary<string, string> dictionary = new MultiDictionary<string, string>()
            {
                { "1", "2" },
                { "1", "3" },
                { "2", "1" },
                { "2", "2" },
            };

            Assert.IsFalse(dictionary.Remove("bla"));
            Assert.IsFalse(dictionary.Remove("bla", "blaa"));

            Assert.IsTrue(dictionary.Remove("1"));
            Assert.AreEqual(2, dictionary.Count);
            Assert.IsTrue(dictionary.Contains("2", "1"));
            Assert.IsTrue(dictionary.Contains("2", "2"));

            Assert.IsTrue(dictionary.Remove("2", "1"));
            Assert.AreEqual(1, dictionary.Count);
            Assert.IsTrue(dictionary.Contains("2", "2"));
        }

        [Test]
        public void TryGetValues()
        {
            MultiDictionary<string, string> dictionary = new MultiDictionary<string, string>()
            {
                { "1", "2" },
                { "1", "3" },
                { "2", "1" },
                { "2", "2" },
            };


            Assert.IsFalse(dictionary.TryGetValues("bla", out ISet<string> values));

            Assert.IsTrue(dictionary.TryGetValues("1", out values));
            Assert.AreEqual(2, values.Count);
            Assert.IsTrue(values.Contains("2"));
            Assert.IsTrue(values.Contains("3"));
        }
    }
}

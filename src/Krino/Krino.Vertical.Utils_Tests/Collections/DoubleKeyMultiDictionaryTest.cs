using Krino.Vertical.Utils.Collections;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Vertical.Utils_Tests.Collections
{
    [TestFixture]
    public class DoubleKeyMultiDictionaryTest
    {
        [Test]
        public void Add()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);

            Assert.AreEqual(1, dictionary.Count);
            Assert.AreEqual(10, dictionary.GetValues("k1", "k2").First());

            Assert.AreEqual("k2", dictionary.GetValuesForKey1("k1").First().Key);
            Assert.AreEqual(10, dictionary.GetValuesForKey1("k1").First().Value);

            Assert.AreEqual("k1", dictionary.GetValuesForKey2("k2").First().Key);
            Assert.AreEqual(10, dictionary.GetValuesForKey2("k2").First().Value);
        }

        [Test]
        public void GetValuesForKey1()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k3", 20);

            Assert.AreEqual(2, dictionary.GetValuesForKey1("k1").Count());
            Assert.IsTrue(dictionary.GetValuesForKey1("k1").Contains(new KeyValuePair<string, int>("k2", 10)));
            Assert.IsTrue(dictionary.GetValuesForKey1("k1").Contains(new KeyValuePair<string, int>("k3", 20)));
        }

        [Test]
        public void GetValuesForKey2()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k3", "k2", 20);

            Assert.AreEqual(2, dictionary.GetValuesForKey2("k2").Count());
            Assert.IsTrue(dictionary.GetValuesForKey2("k2").Contains(new KeyValuePair<string, int>("k1", 10)));
            Assert.IsTrue(dictionary.GetValuesForKey2("k2").Contains(new KeyValuePair<string, int>("k3", 20)));
        }

        [Test]
        public void GetValues()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k2", 20);

            IEnumerable<int> values = dictionary.GetValues("k1", "k2");

            Assert.AreEqual(2, values.Count());
            Assert.IsTrue(values.All(x => x == 10));

            values = dictionary.GetValues("k3", "k2");
            Assert.AreEqual(1, values.Count());
            Assert.AreEqual(20, values.First());
        }

        [Test]
        public void ContainsKey1()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k3", "k2", 20);

            Assert.IsTrue(dictionary.ContainsKey1("k1"));
            Assert.IsTrue(dictionary.ContainsKey1("k3"));

            Assert.IsFalse(dictionary.ContainsKey1("bla"));
        }

        [Test]
        public void ContainsKey2()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k3", "k4", 20);

            Assert.IsTrue(dictionary.ContainsKey2("k2"));
            Assert.IsTrue(dictionary.ContainsKey2("k2"));

            Assert.IsFalse(dictionary.ContainsKey1("bla"));
        }

        [Test]
        public void RemoveForKey1()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k2", 20);

            Assert.IsTrue(dictionary.RemoveForKey1("k1"));
            Assert.AreEqual(1, dictionary.Count);
            Assert.AreEqual(20, dictionary.GetValues("k3", "k2").First());
            Assert.IsFalse(dictionary.ContainsKey1("k1"));



            dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k2", 20);

            int counter = 0;
            bool result = dictionary.RemoveForKey1("k1", (k1, k2, v) =>
            {
                ++counter;
                return counter < 2;
            }
            );

            Assert.IsTrue(result);
            Assert.AreEqual(2, dictionary.Count);
            Assert.AreEqual(10, dictionary.GetValues("k1", "k2").First());
            Assert.AreEqual(20, dictionary.GetValues("k3", "k2").First());
            Assert.IsTrue(dictionary.ContainsKey1("k1"));


            Assert.IsFalse(dictionary.RemoveForKey1("bla1"));
        }


        [Test]
        public void RemoveForKey2()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k4", 20);

            Assert.IsTrue(dictionary.RemoveForKey2("k2"));
            Assert.AreEqual(1, dictionary.Count);
            Assert.AreEqual(20, dictionary.GetValues("k3", "k4").First());
            Assert.IsFalse(dictionary.ContainsKey2("k2"));


            dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k4", 20);

            int counter = 0;
            bool result = dictionary.RemoveForKey2("k2", (k1, k2, v) =>
            {
                ++counter;
                return counter < 2;
            }
            );

            Assert.IsTrue(result);
            Assert.AreEqual(2, dictionary.Count);
            Assert.AreEqual(10, dictionary.GetValues("k1", "k2").First());
            Assert.AreEqual(20, dictionary.GetValues("k3", "k4").First());
            Assert.IsTrue(dictionary.ContainsKey2("k2"));


            Assert.IsFalse(dictionary.RemoveForKey2("bla1"));
        }

        [Test]
        public void Remove()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k4", 20);

            Assert.IsTrue(dictionary.Remove("k1", "k2"));
            Assert.AreEqual(1, dictionary.Count);
            Assert.AreEqual(20, dictionary.GetValues("k3", "k4").First());



            dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k4", 20);

            int counter = 0;
            bool result = dictionary.Remove("k1", "k2", (k1, k2, v) =>
            {
                ++counter;
                return counter < 2;
            }
            );

            Assert.IsTrue(result);
            Assert.AreEqual(2, dictionary.Count);
            Assert.AreEqual(10, dictionary.GetValues("k1", "k2").First());
            Assert.AreEqual(20, dictionary.GetValues("k3", "k4").First());


            Assert.IsFalse(dictionary.Remove("bla1", "bla2"));
        }

        [Test]
        public void Remove_Sequence()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k4", 20);
            dictionary.Add("k3", "k5", 30);

            Tuple<string, string>[] toRemove = new Tuple<string, string>[] { Tuple.Create("k1", "k2"), Tuple.Create("k3", "k4"), };

            Assert.IsTrue(dictionary.Remove(toRemove));
            Assert.AreEqual(1, dictionary.Count);
            Assert.AreEqual(30, dictionary.GetValues("k3", "k5").First());
        }


        [Test]
        public void Clear()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k2", 10); // the same keys and the value

            dictionary.Add("k3", "k4", 20);

            dictionary.Clear();

            Assert.AreEqual(0, dictionary.Count);
        }

        [Test]
        public void Enumerable()
        {
            DoubleKeyMultiDictionary<string, string, int> dictionary = new DoubleKeyMultiDictionary<string, string, int>();

            dictionary.Add("k1", "k2", 10);
            dictionary.Add("k1", "k3", 10); // the same keys and the value

            dictionary.Add("k3", "k4", 20);

            Assert.AreEqual(3, dictionary.Count);
            Assert.IsTrue(dictionary.Any(x => x.Item1 == "k1" && x.Item2 == "k2" && x.Item3 == 10));
            Assert.IsTrue(dictionary.Any(x => x.Item1 == "k1" && x.Item2 == "k3" && x.Item3 == 10));
            Assert.IsTrue(dictionary.Any(x => x.Item1 == "k3" && x.Item2 == "k4" && x.Item3 == 20));
        }
    }
}

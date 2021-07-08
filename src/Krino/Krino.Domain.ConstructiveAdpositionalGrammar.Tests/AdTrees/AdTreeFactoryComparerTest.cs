using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeFactoryComparerTest
    {
        [Test]
        public void Comparer_Equals()
        {
            var adTreeFactory1 = new AdTreeFactory(EnglishPattern.A_O)
            {
                Right = new AdTreeFactory(EnglishPattern.A_O)
            };

            var adTreeFactory2 = new AdTreeFactory(EnglishPattern.A_O)
            {
                Right = new AdTreeFactory(EnglishPattern.A_O)
            };

            Assert.IsTrue(new AdTreeFactoryComparer().Equals(adTreeFactory1, adTreeFactory2));

            var hashSet = new HashSet<AdTreeFactory>(new AdTreeFactoryComparer());
            hashSet.Add(adTreeFactory1);
            hashSet.Add(adTreeFactory2);
            Assert.AreEqual(1, hashSet.Count);



            adTreeFactory1 = new AdTreeFactory(EnglishPattern.A_O)
            {
                Right = new AdTreeFactory(EnglishPattern.A_O)
            };

            adTreeFactory2 = new AdTreeFactory(EnglishPattern.A_O)
            {
                Right = new AdTreeFactory(EnglishPattern.A_O.SetRightFirst())
            };

            Assert.IsFalse(new AdTreeFactoryComparer().Equals(adTreeFactory1, adTreeFactory2));

            hashSet = new HashSet<AdTreeFactory>(new AdTreeFactoryComparer());
            hashSet.Add(adTreeFactory1);
            hashSet.Add(adTreeFactory2);
            Assert.AreEqual(2, hashSet.Count);
        }
    }
}

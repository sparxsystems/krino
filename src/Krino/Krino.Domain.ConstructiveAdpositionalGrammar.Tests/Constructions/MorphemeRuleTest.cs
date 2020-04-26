using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions
{
    [TestFixture]
    public class MorphemeRuleTest
    {
        [Test]
        public void IsMatch_DefaultConstructor()
        {
            // Default constructor created rule shall accept everything.
            MorphemeRule morphemeRule = new MorphemeRule();

            Assert.IsTrue(morphemeRule.IsMatch("", 0));
            Assert.IsTrue(morphemeRule.IsMatch(null, 0));
            Assert.IsTrue(morphemeRule.IsMatch("bla", ulong.MaxValue));
        }

        [Test]
        public void IsMatch()
        {
            MorphemeRule morphemeRule = new MorphemeRule("hello", "bla", 7, 32);
            Assert.IsTrue(morphemeRule.IsMatch("hello", 15));
            Assert.IsFalse(morphemeRule.IsMatch("bla", 15));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 39));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 0));

            morphemeRule = new MorphemeRule(null, "bla", 7, 32);
            Assert.IsTrue(morphemeRule.IsMatch("hi", 15));
            Assert.IsTrue(morphemeRule.IsMatch("", 15));
            Assert.IsTrue(morphemeRule.IsMatch(null, 15));
            Assert.IsFalse(morphemeRule.IsMatch("bla", 15));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 39));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 0));

            morphemeRule = new MorphemeRule("", "bla", 7, 32);
            Assert.IsFalse(morphemeRule.IsMatch("hi", 15));
            Assert.IsTrue(morphemeRule.IsMatch("", 15));
            Assert.IsFalse(morphemeRule.IsMatch(null, 15));
            Assert.IsFalse(morphemeRule.IsMatch("bla", 15));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 39));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 0));

            morphemeRule = new MorphemeRule("hello", "", 4, 32);
            Assert.IsTrue(morphemeRule.IsMatch("hello", 5));
            Assert.IsFalse(morphemeRule.IsMatch("", 5));

            morphemeRule = new MorphemeRule(null, null, 7, 32);
            Assert.IsTrue(morphemeRule.IsMatch("hi", 15));
            Assert.IsTrue(morphemeRule.IsMatch("", 15));
            Assert.IsTrue(morphemeRule.IsMatch(null, 15));
            Assert.IsTrue(morphemeRule.IsMatch("bla", 15));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 39));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 0));
        }
    }
}

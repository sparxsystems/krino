using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions
{
    [TestFixture]
    public class PatternRuleTest
    {
        [Test]
        public void IsMatch_DefaultConstructor()
        {
            // Default constructor created rule shall accept everything.
            PatternRule rule = new PatternRule();
            Assert.IsTrue(rule.IsMatch("", 0, 0));
            Assert.IsTrue(rule.IsMatch(null, 0, 0));
            Assert.IsTrue(rule.IsMatch("hello", 0, 0));
            Assert.IsTrue(rule.IsMatch("hello", 100, 50));
        }

        [Test]
        public void IsMatch()
        {
            // Default constructor created rule shall accept everything.
            PatternRule rule = new PatternRule(default, 15, 32);
            Assert.IsTrue(rule.IsMatch("", 0, 31));
            Assert.IsTrue(rule.IsMatch(null, 0, 31));
            Assert.IsTrue(rule.IsMatch("hello", 0, 31));
            Assert.IsTrue(rule.IsMatch("hello", 100, 31));
            Assert.IsFalse(rule.IsMatch(null, 0, 15 + 32));
            Assert.IsFalse(rule.IsMatch("hello", 100, 15 + 32));

            rule = new PatternRule(new MorphemeRule("", "", 4, 0), 8, 0);
            Assert.IsFalse(rule.IsMatch("hello", 5, 9));
            Assert.IsFalse(rule.IsMatch("", 5, 9));
        }
    }
}

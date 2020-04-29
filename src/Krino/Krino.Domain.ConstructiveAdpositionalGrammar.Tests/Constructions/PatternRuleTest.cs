using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Vertical.Utils.Rules;
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
        public void IsMatch_Anything()
        {
            PatternRule rule = PatternRule.Anything;
            Assert.IsTrue(rule.IsMatch("", 0, 0));
            Assert.IsTrue(rule.IsMatch(null, 0, 0));
            Assert.IsTrue(rule.IsMatch("hello", 0, 0));
            Assert.IsTrue(rule.IsMatch("hello", 100, 50));
        }

        [Test]
        public void IsMatch()
        {
            // Pattern rule which matches any morpheme and requires the mask where the 2nd bit is set and the fourth bit is not set.
            PatternRule rule = new PatternRule(MorphemeRule.Anything, MaskRule.Is(2ul).And(Rule.Is(4ul).Not()));
            Assert.IsTrue(rule.IsMatch("", 0, 3));
            Assert.IsTrue(rule.IsMatch(null, 0, 3));
            Assert.IsTrue(rule.IsMatch("hello", 0, 3));
            Assert.IsTrue(rule.IsMatch("hello", 100, 3));
            Assert.IsFalse(rule.IsMatch("", 0, 5));
            Assert.IsFalse(rule.IsMatch(null, 0, 5));
            Assert.IsFalse(rule.IsMatch("hello", 100, 5));
        }
    }
}

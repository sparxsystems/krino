using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using NUnit.Framework;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions
{
    [TestFixture]
    public class PatternRuleTest
    {
        [Test]
        public void IsMatch()
        {
            // Pattern rule which matches any stative morpheme and requires the mask where the 2nd bit is set and the fourth bit is not set.
            PatternRule rule = new PatternRule(MorphemeRule.Any(GrammarCharacter.O), MaskRule.Is(2ul).And(RuleMaker.Is((BigInteger)4).Not()));
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

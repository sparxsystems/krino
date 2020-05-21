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
            Assert.IsTrue(rule.IsMatch(new Morpheme(""), 3));
            Assert.IsTrue(rule.IsMatch(new Morpheme(null), 3));
            Assert.IsTrue(rule.IsMatch(new Morpheme("hello"), 3));
            Assert.IsTrue(rule.IsMatch(new Morpheme("hello") { Attributes = 100 }, 3));
            Assert.IsFalse(rule.IsMatch(new Morpheme(""), 5));
            Assert.IsFalse(rule.IsMatch(new Morpheme(null), 5));
            Assert.IsFalse(rule.IsMatch(new Morpheme("hello") { Attributes = 100 }, 5));
        }
    }
}

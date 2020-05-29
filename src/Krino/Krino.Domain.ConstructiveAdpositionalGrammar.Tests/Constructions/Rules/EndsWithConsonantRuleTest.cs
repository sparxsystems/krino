using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules.Strings;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions.Rules
{
    [TestFixture]
    public class EndsWithConsonantRuleTest
    {
        [Test]
        public void Evaluate()
        {
            EndsWithConsonantRule rule = new EndsWithConsonantRule();
            Assert.IsTrue(rule.Evaluate("car"));

            // Vowel is in front of w - it is a vowel.
            Assert.IsFalse(rule.Evaluate("cow"));

            // Vowel is not in front of w - it is consonant.
            Assert.IsTrue(rule.Evaluate("carw"));

            Assert.IsFalse(rule.Evaluate(null));
        }
    }
}

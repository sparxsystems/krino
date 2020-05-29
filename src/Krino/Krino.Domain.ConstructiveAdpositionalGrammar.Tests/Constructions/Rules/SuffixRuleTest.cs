using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules.Strings;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions.Rules
{
    [TestFixture]
    public class SuffixRuleTest
    {
        [Test]
        public void Evauate()
        {
            SuffixRule rule = new SuffixRule("s");
            Assert.IsTrue(rule.Evaluate("books"));
            Assert.IsFalse(rule.Evaluate("book"));
            Assert.IsFalse(rule.Evaluate(null));
        }
    }
}

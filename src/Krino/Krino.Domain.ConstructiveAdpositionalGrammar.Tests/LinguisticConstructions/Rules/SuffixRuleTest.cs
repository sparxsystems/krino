using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules.Strings;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions.Rules
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

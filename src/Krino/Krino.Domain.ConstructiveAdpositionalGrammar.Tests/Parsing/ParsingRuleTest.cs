using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class ParsingRuleTest
    {
        [Test]
        public void AuxiliaryWordIs()
        {
            var rule = ParsingRule.WordIsOneOf("have", "has");
            Assert.IsTrue(rule.Evaluate(new Word("have", 0)));

            Assert.IsFalse(rule.Evaluate(new Word("bla", 0)));
        }
    }
}

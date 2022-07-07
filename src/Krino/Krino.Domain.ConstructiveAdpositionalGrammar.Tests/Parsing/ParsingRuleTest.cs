using Krino.Domain.ConstructiveGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveGrammar.Parsing;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveGrammar.Tests.Parsing
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

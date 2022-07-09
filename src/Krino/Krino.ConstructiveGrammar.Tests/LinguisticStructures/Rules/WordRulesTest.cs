using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Rules;
using NUnit.Framework;

namespace Krino.ConstructiveGrammar.Tests.LinguisticStructures.Rules
{
    [TestFixture]
    public class WordRulesTest
    {
        [Test]
        public void AuxiliaryWordIs()
        {
            var rule = WordRules.WordIsOneOf("have", "has");
            Assert.IsTrue(rule.Evaluate(new Word("have", 0)));

            Assert.IsFalse(rule.Evaluate(new Word("bla", 0)));
        }
    }
}

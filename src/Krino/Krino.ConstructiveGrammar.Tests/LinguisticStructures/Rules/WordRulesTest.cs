using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Rules;
using Krino.EnglishGrammar.Morphology;
using NUnit.Framework;

namespace Krino.ConstructiveGrammar.Tests.LinguisticStructures.Rules
{
    [TestFixture]
    public class WordRulesTest
    {
        [Test]
        public void AuxiliaryWordIs()
        {
            var morphology = new EnglishMorphology();

            var rule = WordRules.WordIsOneOf("have", "has");
            Assert.IsTrue(rule.Evaluate(new Word(morphology, "have", 0)));

            Assert.IsFalse(rule.Evaluate(new Word(morphology, "bla", 0)));
        }
    }
}

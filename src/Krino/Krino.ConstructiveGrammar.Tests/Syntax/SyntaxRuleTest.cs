using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.Syntax;
using NUnit.Framework;

namespace Krino.ConstructiveGrammar.Tests.Syntax
{
    [TestFixture]
    public class SyntaxRuleTest
    {
        [Test]
        public void AuxiliaryWordIs()
        {
            var rule = SyntaxRule.WordIsOneOf("have", "has");
            Assert.IsTrue(rule.Evaluate(new Word("have", 0)));

            Assert.IsFalse(rule.Evaluate(new Word("bla", 0)));
        }
    }
}

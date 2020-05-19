using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Rules;
using NUnit.Framework;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions.Rules
{
    [TestFixture]
    public class MaskRuleTest
    {
        [Test]
        public void Something()
        {
            Assert.IsTrue(MaskRule.Something.Evaluate(1));
            Assert.IsFalse(MaskRule.Something.Evaluate(0));
        }

        [Test]
        public void CompositionOfRules_And_Neg()
        {
            IRule<BigInteger> rule = MaskRule.Is(Attributes.O) & !MaskRule.Is(Attributes.O.NonLexeme);
            Assert.IsTrue(rule.Evaluate(Attributes.O.Lexeme));
            Assert.IsTrue(rule.Evaluate(Attributes.O));
            Assert.IsFalse(rule.Evaluate(Attributes.O.NonLexeme));
        }
    }
}

using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using NUnit.Framework;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions.Rules
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
            IRule<BigInteger> rule = MaskRule.Is(EnglishAttributes.O) & !MaskRule.Is(EnglishAttributes.O.NonLexeme);
            Assert.IsTrue(rule.Evaluate(EnglishAttributes.O.Lexeme));
            Assert.IsTrue(rule.Evaluate(EnglishAttributes.O));
            Assert.IsFalse(rule.Evaluate(EnglishAttributes.O.NonLexeme));
        }

        [Test]
        public void IsSubruleOf()
        {
            IRule<BigInteger> rule1 = MaskRule.Is(EnglishAttributes.I.Lexeme.Verb);
            IRule<BigInteger> rule2 = MaskRule.Is(EnglishAttributes.I.Lexeme);

            Assert.IsTrue(rule1.IsSubruleOf(rule2));
            Assert.IsTrue(rule1.IsSubruleOf(rule1));
            Assert.IsTrue(rule2.IsSubruleOf(rule2));
            Assert.IsFalse(rule2.IsSubruleOf(rule1));
        }
    }
}

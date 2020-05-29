using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions
{
    [TestFixture]
    public class PatternTest
    {
        [Test]
        public void IsPrimitiveTransference()
        {
            Pattern pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.A.Lexeme),
                RightRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.Nothing,
            };
            Assert.IsTrue(pattern.IsPrimitiveTransference());

            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is("hello", Attributes.A.Lexeme),
                RightRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.Nothing,
            };
            Assert.IsTrue(pattern.IsPrimitiveTransference());

            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.A_Lexeme,
            };
            Assert.IsFalse(pattern.IsPrimitiveTransference());
        }

        [Test]
        public void IsModifier()
        {
            Pattern pattern = new Pattern("A-O")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.O,
                LeftRule = MorphemeRule.A.SetOrder(1),
            };
            Assert.IsTrue(pattern.IsModifier());


            pattern = new Pattern("A")
            {
                MorphemeRule = MorphemeRule.A_Lexeme,
                RightRule = MorphemeRule.Nothing,
                LeftRule = MorphemeRule.Nothing,
            };
            Assert.IsFalse(pattern.IsModifier());
        }

        [Test]
        public void IsAdPositionModifier()
        {
            Pattern pattern = new Pattern("A-U-A")
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U),
                RightRule = MorphemeRule.A.SetOrder(1),
                LeftRule = MorphemeRule.A,
            };
            Assert.IsTrue(pattern.IsAdPositionModifier());


            pattern = new Pattern("A-U-A")
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U),

                // Same order means it is not adposition modifier.
                RightRule = MorphemeRule.A,
                LeftRule = MorphemeRule.A,
            };
            Assert.IsFalse(pattern.IsAdPositionModifier());



            pattern = new Pattern("A-O")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.O,
                LeftRule = MorphemeRule.A.SetOrder(1),
            };
            Assert.IsFalse(pattern.IsAdPositionModifier());
        }
    }
}

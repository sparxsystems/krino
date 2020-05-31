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
        public void IsMorpheme()
        {
            Pattern pattern = Pattern.Morpheme(Attributes.A);
            Assert.IsTrue(pattern.IsMorpheme());

            // Empty string rule causes it is not the morpheme pattern.
            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is("", Attributes.A.Lexeme),
                RightRule = MorphemeRule.Nothing,
                LeftRule = MorphemeRule.Nothing,
            };
            Assert.IsFalse(pattern.IsMorpheme());


            // Right and left rule is not Nothing - it is not the morpheme rule.
            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.E.Lexeme),
                RightRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.O.Lexeme),
                LeftRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.I.Lexeme),
            };
            Assert.IsFalse(pattern.IsMorpheme());
        }

        [Test]
        public void IsPrimitiveTransference()
        {
            Pattern pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is("", Attributes.A.Lexeme),
                RightRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.Nothing,
            };
            Assert.IsTrue(pattern.IsPrimitiveTransference());

            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is("", Attributes.A.Lexeme),
                RightRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.Is("s", Attributes.A.NonLexeme.Prefix)
            };
            Assert.IsFalse(pattern.IsPrimitiveTransference());

            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.A_Lexeme,
            };
            Assert.IsFalse(pattern.IsPrimitiveTransference());
        }

        [Test]
        public void IsEpsilonAdPosition()
        {
            Pattern pattern = Pattern.EpsilonAdPosition("A-O", Attributes.A, Attributes.O);
            Assert.IsTrue(pattern.IsEpsilonAdPosition());


            pattern = new Pattern("A-U-A")
            {
                // Grammar character is not epsilon -> it is not epsilon adposition.
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U),
                RightRule = MorphemeRule.A_Lexeme,
                LeftRule = MorphemeRule.A_Lexeme,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());


            pattern = new Pattern("A-O")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.O_Lexeme,
                // It is not epsilon adposition.
                LeftRule = MorphemeRule.Nothing,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());
        }
    }
}

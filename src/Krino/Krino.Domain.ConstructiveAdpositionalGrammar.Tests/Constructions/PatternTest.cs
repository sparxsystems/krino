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
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Nothing,
            };
            Assert.IsFalse(pattern.IsMorpheme());


            // Right and left rule is not Nothing - it is not the morpheme rule.
            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.E.Lexeme),
                LeftRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.I.Lexeme),
                RightRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.O.Lexeme),
            };
            Assert.IsFalse(pattern.IsMorpheme());
        }

        [Test]
        public void IsPrimitiveTransference()
        {
            Pattern pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is("", Attributes.A.Lexeme),
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.O_Lexeme,
            };
            Assert.IsTrue(pattern.IsGrammarCharacterTransference());

            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Is("", Attributes.A.Lexeme),
                LeftRule = MorphemeRule.Is("s", Attributes.A.NonLexeme.Prefix),
                RightRule = MorphemeRule.O_Lexeme,
            };
            Assert.IsFalse(pattern.IsGrammarCharacterTransference());

            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Epsilon,
                LeftRule = MorphemeRule.A_Lexeme,
                RightRule = MorphemeRule.O_Lexeme,
            };
            Assert.IsFalse(pattern.IsGrammarCharacterTransference());
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
                LeftRule = MorphemeRule.A_Lexeme,
                RightRule = MorphemeRule.A_Lexeme,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());


            pattern = new Pattern("A-O")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                // It is not epsilon adposition.
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.O_Lexeme,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());
        }

        [Test]
        public void IsMorphematicAdPosition()
        {
            Pattern pattern = Pattern.MorphematicAdPosition("O-E-I", Attributes.E, Attributes.O, Attributes.I);
            Assert.IsTrue(pattern.IsMorphematicAdPosition());

            pattern = Pattern.EpsilonAdPosition("A-O", Attributes.A, Attributes.O);
            Assert.IsFalse(pattern.IsMorphematicAdPosition());
        }
    }
}

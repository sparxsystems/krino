using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class PatternTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void IsMorpheme()
        {
            Pattern pattern = EnglishPattern.Morpheme(EnglishAttributes.A);
            Assert.IsTrue(pattern.IsMorpheme);

            // Empty string rule causes it is not the morpheme pattern.
            pattern = new Pattern()
            {
                MorphemeRule = EnglishMorphemeRule.Is("", EnglishAttributes.A.Lexeme),
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Nothing,
            };
            Assert.IsFalse(pattern.IsMorpheme);


            // Right and left rule is not Nothing - it is not the morpheme rule.
            pattern = new Pattern()
            {
                MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.E.Lexeme),
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme),
            };
            Assert.IsFalse(pattern.IsMorpheme);
        }

        [Test]
        public void IsPrimitiveTransference()
        {
            Pattern pattern = new Pattern()
            {
                MorphemeRule = EnglishMorphemeRule.Is("", EnglishAttributes.A.Lexeme),
                LeftRule = MorphemeRule.Nothing,
                RightRule = EnglishMorphemeRule.O_Lexeme,
            };
            Assert.IsTrue(pattern.IsMonoTransference);

            pattern = new Pattern()
            {
                MorphemeRule = EnglishMorphemeRule.Is("", EnglishAttributes.A.Lexeme),
                LeftRule = EnglishMorphemeRule.Is("s", EnglishAttributes.A.NonLexeme.Prefix),
                RightRule = EnglishMorphemeRule.O_Lexeme,
            };
            Assert.IsFalse(pattern.IsMonoTransference);

            pattern = new Pattern()
            {
                MorphemeRule = MorphemeRule.Epsilon,
                LeftRule = EnglishMorphemeRule.A_Lexeme,
                RightRule = EnglishMorphemeRule.O_Lexeme,
            };
            Assert.IsFalse(pattern.IsMonoTransference);
        }

        [Test]
        public void IsEpsilonAdPosition()
        {
            Pattern pattern = EnglishPattern.EpsilonAdPosition("A-O", "", EnglishAttributes.A, EnglishAttributes.O);
            Assert.IsTrue(pattern.IsEpsilonAdPosition());


            pattern = new Pattern("A-U-A")
            {
                // Grammar character is not epsilon -> it is not epsilon adposition.
                MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.U),
                LeftRule = EnglishMorphemeRule.A_Lexeme,
                RightRule = EnglishMorphemeRule.A_Lexeme,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());


            pattern = new Pattern("A-O")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                // It is not epsilon adposition.
                LeftRule = MorphemeRule.Nothing,
                RightRule = EnglishMorphemeRule.O_Lexeme,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());
        }

        [Test]
        public void IsMorphematicAdPosition()
        {
            Pattern pattern = EnglishPattern.MorphematicAdPosition("O-E-I", "", EnglishAttributes.E, EnglishAttributes.O, EnglishAttributes.I);
            Assert.IsTrue(pattern.IsMorphematicAdPosition());

            pattern = EnglishPattern.EpsilonAdPosition("A-O", "", EnglishAttributes.A, EnglishAttributes.O);
            Assert.IsFalse(pattern.IsMorphematicAdPosition());
        }
    }
}

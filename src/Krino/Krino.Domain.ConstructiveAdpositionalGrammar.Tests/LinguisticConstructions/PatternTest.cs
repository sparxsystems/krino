using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using NUnit.Framework;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class PatternTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void CopyConstructor()
        {
            var pattern = new Pattern("A-U-A")
            {
                Description = "hello",
                IsLeftFirst = true,

                ValencyPosition = 1,

                UpRule = new MorphemeRule(GrammarCharacter.A, RuleMaker.Is("bla1"), RuleMaker.Is<BigInteger>(1)),
                UpAttributes = 100,

                LeftRule = new MorphemeRule(GrammarCharacter.O, RuleMaker.Is("bla2"), RuleMaker.Is<BigInteger>(2)),
                LeftAttributes = 200,

                RightRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Is("bla3"), RuleMaker.Is<BigInteger>(3)),
                RightAttributes = 300,
            };

            var copy = new Pattern(pattern);

            Assert.AreEqual(pattern.Name, copy.Name);
            Assert.AreEqual(pattern.Description, copy.Description);
            Assert.AreEqual(pattern.ValencyPosition, copy.ValencyPosition);
            Assert.AreEqual(pattern.IsLeftFirst, copy.IsLeftFirst);
            Assert.IsTrue(pattern.UpRule.Equals(copy.UpRule));
            Assert.AreEqual(pattern.UpAttributes, copy.UpAttributes);
            Assert.IsTrue(pattern.LeftRule.Equals(copy.LeftRule));
            Assert.AreEqual(pattern.LeftAttributes, copy.LeftAttributes);
            Assert.IsTrue(pattern.RightRule.Equals(copy.RightRule));
            Assert.AreEqual(pattern.RightAttributes, copy.RightAttributes);

            Assert.IsTrue(pattern.Equals(copy));
        }

        [Test]
        public void IsMorpheme()
        {
            Pattern pattern = EnglishPattern.Morpheme(EnglishAttributes.A);
            Assert.IsTrue(pattern.IsMorpheme);

            // Empty string rule causes it is not the morpheme pattern.
            pattern = new Pattern()
            {
                UpRule = EnglishMorphemeRule.Is("", EnglishAttributes.A.Lexeme),
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Nothing,
            };
            Assert.IsFalse(pattern.IsMorpheme);


            // Right and left rule is not Nothing - it is not the morpheme rule.
            pattern = new Pattern()
            {
                UpRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.E.Lexeme),
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
                UpRule = EnglishMorphemeRule.Is("", EnglishAttributes.A.Lexeme),
                LeftRule = MorphemeRule.Nothing,
                RightRule = EnglishMorphemeRule.O_Lexeme,
            };
            Assert.IsTrue(pattern.IsMonoTransference);

            pattern = new Pattern()
            {
                UpRule = EnglishMorphemeRule.Is("", EnglishAttributes.A.Lexeme),
                LeftRule = EnglishMorphemeRule.Is("s", EnglishAttributes.A.NonLexeme.Prefix),
                RightRule = EnglishMorphemeRule.O_Lexeme,
            };
            Assert.IsFalse(pattern.IsMonoTransference);

            pattern = new Pattern()
            {
                UpRule = MorphemeRule.Epsilon,
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
                UpRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.U),
                LeftRule = EnglishMorphemeRule.A_Lexeme,
                RightRule = EnglishMorphemeRule.A_Lexeme,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());


            pattern = new Pattern("A-O")
            {
                UpRule = MorphemeRule.Epsilon,
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

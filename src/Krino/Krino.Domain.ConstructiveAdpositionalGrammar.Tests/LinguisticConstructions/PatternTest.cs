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

                UpRule = new MorphemeRule(myAttributesModel, RuleMaker.Is("bla1"), RuleMaker.Is<BigInteger>(1)),
                LeftRule = new MorphemeRule(myAttributesModel, RuleMaker.Is("bla2"), RuleMaker.Is<BigInteger>(2)),
                RightRule = new MorphemeRule(myAttributesModel, RuleMaker.Is("bla3"), RuleMaker.Is<BigInteger>(3)),
            };

            var copy = new Pattern(pattern);

            Assert.AreEqual(pattern.Name, copy.Name);
            Assert.AreEqual(pattern.Description, copy.Description);
            Assert.AreEqual(pattern.ValencyPosition, copy.ValencyPosition);
            Assert.AreEqual(pattern.IsLeftFirst, copy.IsLeftFirst);
            Assert.IsTrue(pattern.UpRule.Equals(copy.UpRule));
            Assert.IsTrue(pattern.LeftRule.Equals(copy.LeftRule));
            Assert.IsTrue(pattern.RightRule.Equals(copy.RightRule));

            Assert.IsTrue(pattern.Equals(copy));
        }

        [Test]
        public void IsMorpheme()
        {
            Pattern pattern = EnglishPattern.A_Lexeme;
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
                RightRule = EnglishMorphemeRule.O_Lexeme_Something,
            };
            Assert.IsTrue(pattern.IsMonoTransference);

            pattern = new Pattern()
            {
                UpRule = EnglishMorphemeRule.Is("", EnglishAttributes.A.Lexeme),
                LeftRule = EnglishMorphemeRule.Is("s", EnglishAttributes.A.NonLexeme.Prefix),
                RightRule = EnglishMorphemeRule.O_Lexeme_Something,
            };
            Assert.IsFalse(pattern.IsMonoTransference);

            pattern = new Pattern()
            {
                UpRule = MorphemeRule.Epsilon,
                LeftRule = EnglishMorphemeRule.A_Lexeme_Something,
                RightRule = EnglishMorphemeRule.O_Lexeme_Something,
            };
            Assert.IsFalse(pattern.IsMonoTransference);
        }

        [Test]
        public void IsEpsilonAdPosition()
        {
            Pattern pattern = EnglishPattern.A_O;
            Assert.IsTrue(pattern.IsEpsilonAdPosition());


            pattern = new Pattern("A-U-A")
            {
                // Grammar character is not epsilon -> it is not epsilon adposition.
                UpRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.U),
                LeftRule = EnglishMorphemeRule.A_Lexeme_Something,
                RightRule = EnglishMorphemeRule.A_Lexeme_Something,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());


            pattern = new Pattern("A-O")
            {
                UpRule = MorphemeRule.Epsilon,
                // It is not epsilon adposition.
                LeftRule = MorphemeRule.Nothing,
                RightRule = EnglishMorphemeRule.O_Lexeme_Something,
            };
            Assert.IsFalse(pattern.IsEpsilonAdPosition());
        }

        [Test]
        public void IsMorphematicAdPosition()
        {
            Pattern pattern = EnglishPattern.O_E_I;
            Assert.IsTrue(pattern.IsMorphematicAdPosition());

            pattern = EnglishPattern.A_O;
            Assert.IsFalse(pattern.IsMorphematicAdPosition());
        }
    }
}

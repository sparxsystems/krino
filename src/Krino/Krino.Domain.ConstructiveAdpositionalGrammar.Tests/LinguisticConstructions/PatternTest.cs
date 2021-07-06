using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
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
            Assert.IsTrue(EnglishPattern.A_Lexeme_Adjective.IsMorpheme);
            Assert.IsTrue(EnglishPattern.O_Lexeme_Noun.IsMorpheme);

            Assert.IsFalse(EnglishPattern.A_O.IsMorpheme);
            Assert.IsFalse(EnglishPattern.Will_I.IsMorpheme);
            Assert.IsFalse(EnglishPattern.O_to_O_s.IsMorpheme);
            Assert.IsFalse(EnglishPattern.O_to_A.IsMorpheme);
        }

        [Test]
        public void IsUnipolarMorphemeTransference()
        {
            Assert.IsTrue(EnglishPattern.O_to_A.IsUnipolarMorphemeTransference);

            Assert.IsFalse(EnglishPattern.I_Lexeme_Verb.IsUnipolarMorphemeTransference);
            Assert.IsFalse(EnglishPattern.O_E_I.IsUnipolarMorphemeTransference);
            Assert.IsFalse(EnglishPattern.A_O.IsUnipolarMorphemeTransference);
            Assert.IsFalse(EnglishPattern.O_to_O_s.IsUnipolarMorphemeTransference);
        }

        [Test]
        public void IsBipolarMorphemeTransference()
        {
            Assert.IsTrue(EnglishPattern.O_to_O_s.IsBipolarMorphemeTransference);
            Assert.IsTrue(EnglishPattern.I_to_A_ed.IsBipolarMorphemeTransference);

            Assert.IsFalse(EnglishPattern.O_to_A.IsBipolarMorphemeTransference);
            Assert.IsFalse(EnglishPattern.I_Lexeme_Verb.IsUnipolarMorphemeTransference);
            Assert.IsFalse(EnglishPattern.O_E_I.IsUnipolarMorphemeTransference);

            Assert.IsFalse(EnglishPattern.Will_I.IsUnipolarMorphemeTransference);
        }

        [Test]
        public void IsEpsilonAdPosition()
        {
            Assert.IsTrue(EnglishPattern.A_O.IsEpsilonAdPosition);
            
            Assert.IsFalse(EnglishPattern.I_Lexeme_Verb.IsEpsilonAdPosition);
            Assert.IsFalse(EnglishPattern.O_E_I.IsEpsilonAdPosition);
            Assert.IsFalse(EnglishPattern.O_to_O_s.IsEpsilonAdPosition);
            Assert.IsFalse(EnglishPattern.O_to_A.IsEpsilonAdPosition);
        }

        [Test]
        public void IsMorphematicAdPosition()
        {
            Assert.IsTrue(EnglishPattern.O_E_I.IsMorphematicAdPosition);
            Assert.IsTrue(EnglishPattern.e_Period_I.IsMorphematicAdPosition);

            Assert.IsFalse(EnglishPattern.O_Lexeme_Noun.IsMorphematicAdPosition);
            Assert.IsFalse(EnglishPattern.O_Suffix_s.IsMorphematicAdPosition);
            Assert.IsFalse(EnglishPattern.O_to_O_s.IsMorphematicAdPosition);
            Assert.IsFalse(EnglishPattern.O_to_A.IsMorphematicAdPosition);
            Assert.IsFalse(EnglishPattern.A_O.IsMorphematicAdPosition);
        }



        [Test]
        public void Equals_Patterns()
        {
            var pattern1 = EnglishPattern.O_E_I;
            var pattern2 = new Pattern(EnglishPattern.O_E_I);

            Assert.IsTrue(pattern1 == pattern2);
        }
    }
}

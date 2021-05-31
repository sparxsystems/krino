using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class MorphemeRuleTest
    {
        [Test]
        public void CopyConstructor()
        {
            var morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(EnglishAttributes.I.Lexeme.Interjection));
            morphemeRule.SetSubstitution(SubstitutionRules.Epsilon_U_E);

            var copy = new MorphemeRule(morphemeRule);
            Assert.AreEqual(morphemeRule.GrammarCharacter, copy.GrammarCharacter);
            Assert.IsTrue(morphemeRule.MorphRule.Equals(copy.MorphRule));
            Assert.IsTrue(morphemeRule.AttributesRule.Equals(copy.AttributesRule));
            Assert.IsTrue(morphemeRule.SubstitutionRule.Equals(copy.SubstitutionRule));

            Assert.IsTrue(morphemeRule.Equals(copy));
        }

        [Test]
        public void Nothing()
        {
            IAttributesModel attributesModel = new EnglishAttributesModel();

            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme(attributesModel, "", 0)));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme(attributesModel, null, 0)));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme(attributesModel, "bla", ulong.MaxValue)));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme(attributesModel, "bla", 123)));
        }

        [Test]
        public void IsMatch()
        {
            IAttributesModel attributesModel = new EnglishAttributesModel();

            // Or
            MorphemeRule morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(EnglishAttributes.I.Lexeme.Interjection).Or(MaskRule.Is(EnglishAttributes.I.Lexeme.Verb)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme(attributesModel, "", EnglishAttributes.I.Lexeme.Verb)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme(attributesModel, "", EnglishAttributes.I.Lexeme.Interjection)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme(attributesModel, "", EnglishAttributes.I.Lexeme.Verb | EnglishAttributes.I.Lexeme.Interjection)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme(attributesModel, "", EnglishAttributes.O)));

            // And
            morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(EnglishAttributes.I.Lexeme.Interjection).And(MaskRule.Is(EnglishAttributes.I.Lexeme.Verb)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme(attributesModel, "", EnglishAttributes.I.Lexeme.Verb)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme(attributesModel, "", EnglishAttributes.I.Lexeme.Interjection)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme(attributesModel, "", EnglishAttributes.I.Lexeme.Verb | EnglishAttributes.I.Lexeme.Interjection)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme(attributesModel, "", EnglishAttributes.O)));
        }
    }
}

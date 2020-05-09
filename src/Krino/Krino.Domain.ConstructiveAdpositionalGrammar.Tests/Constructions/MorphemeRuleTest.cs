using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions
{
    [TestFixture]
    public class MorphemeRuleTest
    {
        [Test]
        public void Anything()
        {
            Assert.IsTrue(MorphemeRule.Anything.IsMatch("", 0));
            Assert.IsTrue(MorphemeRule.Anything.IsMatch(null, 0));
            Assert.IsTrue(MorphemeRule.Anything.IsMatch("bla", ulong.MaxValue));
        }

        [Test]
        public void Nothing()
        {
            Assert.IsFalse(MorphemeRule.Nothing.IsMatch("", 0));
            Assert.IsFalse(MorphemeRule.Nothing.IsMatch(null, 0));
            Assert.IsFalse(MorphemeRule.Nothing.IsMatch("bla", ulong.MaxValue));
            Assert.IsFalse(MorphemeRule.Nothing.IsMatch("bla", 123));
        }

        [Test]
        public void IsMatch()
        {
            MorphemeRule morphemeRule = new MorphemeRule(Rule.Anything<string>(), MaskRule.Is(Attributes.I).Or(MaskRule.Is(Attributes.NonLexeme.Affix.Suffix)));
            Assert.IsTrue(morphemeRule.IsMatch("", Attributes.I | Attributes.NonLexeme.Affix.Suffix));
            Assert.IsTrue(morphemeRule.IsMatch("", Attributes.I));
            Assert.IsTrue(morphemeRule.IsMatch("", Attributes.NonLexeme.Affix.Suffix));
            Assert.IsFalse(morphemeRule.IsMatch("", Attributes.O));

            morphemeRule = new MorphemeRule(Rule.Anything<string>(), MaskRule.Is(Attributes.I).And(MaskRule.Is(Attributes.NonLexeme.Affix.Suffix)));
            Assert.IsTrue(morphemeRule.IsMatch("", Attributes.I | Attributes.NonLexeme.Affix.Suffix));
            Assert.IsFalse(morphemeRule.IsMatch("", Attributes.I));
            Assert.IsFalse(morphemeRule.IsMatch("", Attributes.NonLexeme.Affix.Suffix));
            Assert.IsFalse(morphemeRule.IsMatch("", Attributes.O));
        }

        [Test]
        public void IsMatch_GrammarCharacter()
        {
            MorphemeRule morphemeRule = new MorphemeRule(Rule.Anything<string>(), MaskRule.Is(Attributes.I).Or(MaskRule.Is(Attributes.NonLexeme.Affix.Suffix)));
            Assert.IsTrue(morphemeRule.IsMatch(GrammarCharacter.I));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.O));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.E));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.A));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.U));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.Epsilon));

            morphemeRule = new MorphemeRule(Rule.Anything<string>(), MaskRule.Is(Attributes.I).And(MaskRule.Is(Attributes.NonLexeme.Affix.Suffix)));
            Assert.IsTrue(morphemeRule.IsMatch(GrammarCharacter.I));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.O));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.E));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.A));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.U));
            Assert.IsFalse(morphemeRule.IsMatch(GrammarCharacter.Epsilon));
        }
    }
}

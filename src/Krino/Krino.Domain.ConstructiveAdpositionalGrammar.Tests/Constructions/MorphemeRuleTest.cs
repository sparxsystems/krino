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
            // Or
            MorphemeRule morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.I.Lexeme.Interjection).Or(MaskRule.Is(Attributes.I.Lexeme.Verb)));
            Assert.IsTrue(morphemeRule.IsMatch("", Attributes.I.Lexeme.Verb));
            Assert.IsTrue(morphemeRule.IsMatch("", Attributes.I.Lexeme.Interjection));
            Assert.IsTrue(morphemeRule.IsMatch("", Attributes.I.Lexeme.Verb | Attributes.I.Lexeme.Interjection));
            Assert.IsFalse(morphemeRule.IsMatch("", Attributes.O));

            // And
            morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.I.Lexeme.Interjection).And(MaskRule.Is(Attributes.I.Lexeme.Verb)));
            Assert.IsFalse(morphemeRule.IsMatch("", Attributes.I.Lexeme.Verb));
            Assert.IsFalse(morphemeRule.IsMatch("", Attributes.I.Lexeme.Interjection));
            Assert.IsTrue(morphemeRule.IsMatch("", Attributes.I.Lexeme.Verb | Attributes.I.Lexeme.Interjection));
            Assert.IsFalse(morphemeRule.IsMatch("", Attributes.O));
        }
    }
}

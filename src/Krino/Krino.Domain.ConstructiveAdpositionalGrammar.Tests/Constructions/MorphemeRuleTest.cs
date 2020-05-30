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
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme("", 0)));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme(null, 0)));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme("bla", ulong.MaxValue)));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme("bla", 123)));
        }

        [Test]
        public void IsMatch()
        {
            // Or
            MorphemeRule morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.I.Lexeme.Interjection).Or(MaskRule.Is(Attributes.I.Lexeme.Verb)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme("", Attributes.I.Lexeme.Verb)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme("", Attributes.I.Lexeme.Interjection)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme("", Attributes.I.Lexeme.Verb | Attributes.I.Lexeme.Interjection)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme("", Attributes.O)));

            // And
            morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.I.Lexeme.Interjection).And(MaskRule.Is(Attributes.I.Lexeme.Verb)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme("", Attributes.I.Lexeme.Verb)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme("", Attributes.I.Lexeme.Interjection)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme("", Attributes.I.Lexeme.Verb | Attributes.I.Lexeme.Interjection)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme("", Attributes.O)));
        }
    }
}

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
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme("")));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme(null)));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme("bla") { Attributes = ulong.MaxValue }));
            Assert.IsFalse(MorphemeRule.Nothing.Evaluate(new Morpheme("bla") { Attributes = 123 }));
        }

        [Test]
        public void IsMatch()
        {
            // Or
            MorphemeRule morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.I.Lexeme.Interjection).Or(MaskRule.Is(Attributes.I.Lexeme.Verb)));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme("") { Attributes = Attributes.I.Lexeme.Verb }));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme("") { Attributes = Attributes.I.Lexeme.Interjection }));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme("") { Attributes = Attributes.I.Lexeme.Verb | Attributes.I.Lexeme.Interjection }));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme("") { Attributes = Attributes.O }));

            // And
            morphemeRule = new MorphemeRule(GrammarCharacter.I, RuleMaker.Anything<string>(), MaskRule.Is(Attributes.I.Lexeme.Interjection).And(MaskRule.Is(Attributes.I.Lexeme.Verb)));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme("") { Attributes = Attributes.I.Lexeme.Verb }));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme("") { Attributes = Attributes.I.Lexeme.Interjection }));
            Assert.IsTrue(morphemeRule.Evaluate(new Morpheme("") { Attributes = Attributes.I.Lexeme.Verb | Attributes.I.Lexeme.Interjection }));
            Assert.IsFalse(morphemeRule.Evaluate(new Morpheme("") { Attributes = Attributes.O }));
        }

        [Test]
        public void O_Not_NonLexeme()
        {
            Assert.IsTrue(MorphemeRule.O_Not_NonLexeme.Evaluate(new Morpheme("") { Attributes = Attributes.O.Lexeme }));
            Assert.IsTrue(MorphemeRule.O_Not_NonLexeme.Evaluate(new Morpheme("") { Attributes = Attributes.O}));
            Assert.IsFalse(MorphemeRule.O_Not_NonLexeme.Evaluate(new Morpheme("") { Attributes = Attributes.O.NonLexeme }));
        }
    }
}

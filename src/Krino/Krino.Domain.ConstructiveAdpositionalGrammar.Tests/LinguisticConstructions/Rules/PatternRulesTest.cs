using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions.Rules
{
    [TestFixture]
    public class PatternRulesTest
    {
        [Test]
        public void ByRightMorphemeRule()
        {
            var parent = EnglishPattern.O2_I;
            var child = EnglishPattern.O1_I;
            var rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O1_I;
            child = EnglishPattern.O2_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));

            parent = EnglishPattern.EpsilonAdPosition("E-I", "Rule to add a circumstantial adverb after verb valencies.", EnglishAttributes.E.Lexeme.Adverb, EnglishAttributes.I.Lexeme);
            child = EnglishPattern.O2_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.MorphematicAdPosition("O-E-I", "Circumstantial prepostion.", EnglishAttributes.E.Lexeme.Preposition, EnglishAttributes.O.Lexeme, EnglishAttributes.I.Lexeme);
            child = EnglishPattern.O2_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O1_I;
            child = EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme, "Rule accepting verbant lexemes.");
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O1_I;
            child = EnglishPattern.PairTransference("I>will_I", "Rule for simple future of a lexeme verbant.",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Future,
                    EnglishMorphemeRule.Is(MorphRules.Is("will"), EnglishAttributes.I.Lexeme.Verb.Modal).SetSubstitution(SubstitutionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.Infinitive));
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));
        }


        [Test]
        public void ByLeftMorphemeRule()
        {
            var parent = EnglishPattern.O2_I;
            var child = EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme, "Rule accepting stative lexemes.");
            var rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O2_I;
            child = EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme, "Rule accepting adjunctive lexemes.");
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));

            parent = EnglishPattern.O2_I;
            child = EnglishPattern.PairTransference("O>O_s", "Rule to make a noun plural.",
                EnglishAttributes.O.Lexeme.Noun | EnglishAttributes.O.Lexeme.Noun.Sememe.Number.Plural,
                EnglishMorphemeRule.Is("s", EnglishAttributes.O.NonLexeme.Suffix),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme.Noun));
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));


            parent = EnglishPattern.PairTransference("O>O_s", "Rule to make a noun plural.",
                EnglishAttributes.O.Lexeme.Noun | EnglishAttributes.O.Lexeme.Noun.Sememe.Number.Plural,
                EnglishMorphemeRule.Is("s", EnglishAttributes.O.NonLexeme.Suffix),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme.Noun));
            child = EnglishPattern.Morpheme("s", EnglishAttributes.O.NonLexeme.Suffix, "Os");
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.PairTransference("O>O_s", "Rule to make a noun plural.",
                EnglishAttributes.O.Lexeme.Noun | EnglishAttributes.O.Lexeme.Noun.Sememe.Number.Plural,
                EnglishMorphemeRule.Is("s", EnglishAttributes.O.NonLexeme.Suffix),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme.Noun));
            child = EnglishPattern.Morpheme(EnglishAttributes.O.NonLexeme.Suffix, "O+", "Rule accepting stative suffix.");
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));
        }
    }
}

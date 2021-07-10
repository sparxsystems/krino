using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticConstructions.Rules
{
    [TestFixture]
    public class PatternRulesTest
    {
        private static EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void UpPatternRule()
        {
            var parent = EnglishPattern.O_U_O;
            var child = EnglishPattern.U_Lexeme_Conjunction;
            var rule = PatternRules.ByUpMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O_U_O;
            child = EnglishPattern.U_NonLexeme_Punctuation_Period;
            rule = PatternRules.ByUpMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));

            parent = EnglishPattern.O_U_O;
            child = EnglishPattern.I_Lexeme_Verb;
            rule = PatternRules.ByUpMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));


            parent = EnglishPattern.I_as_O;
            child = EnglishPattern.U_Lexeme_Conjunction_As;
            rule = PatternRules.ByUpMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.I_as_O;
            child = EnglishPattern.U_Lexeme_Conjunction;
            rule = PatternRules.ByUpMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));
        }

        [Test]
        public void RightPatternRule()
        {
            var parent = EnglishPattern.O2_I;
            var child = EnglishPattern.O1_I;
            var rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O1_I;
            child = EnglishPattern.O2_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));

            parent = EnglishPattern.E_I;
            child = EnglishPattern.O2_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O_E_I;
            child = EnglishPattern.O2_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O1_I;
            child = EnglishPattern.I_Lexeme_Verb;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O1_I;
            child = EnglishPattern.Will_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));


            // Not allowed morphematic adposition.
            parent = EnglishPattern.E_I
                .SetRightSubstitutionRule(PatternSubstitutionRules.Nothing);
            child = EnglishPattern.I_U_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));

            // Morphematic adposition allowed.
            parent = EnglishPattern.E_I
                .SetRightSubstitutionRule(PatternSubstitutionRules.Epsilon_U_E);
            child = EnglishPattern.I_U_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));


            parent = EnglishPattern.I_to_A_ed;
            child = EnglishPattern.I_Lexeme_Verb;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            // This shall be false because we try to attach the non-lexeme suffix to the right, where a lexeme is expected.
            parent = EnglishPattern.I_to_A_ed;
            child = EnglishPattern.I_Suffix_ed;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));

            parent = EnglishPattern.O_to_O_s;
            child = EnglishPattern.O_Lexeme_Noun;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));


            parent = EnglishPattern.e_Period_I;
            child = EnglishPattern.I_U_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            // Grammar transference.
            parent = EnglishPattern.O1_I;
            child = EnglishPattern.Will_I;
            rule = PatternRules.ByRightMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));
        }


        [Test]
        public void LeftPatternRule()
        {
            var parent = EnglishPattern.O2_I;
            var child = EnglishPattern.O_Lexeme_Noun;
            var rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O2_I;
            child = EnglishPattern.A_Lexeme_Adjective;
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));

            parent = EnglishPattern.O2_I;
            child = EnglishPattern.O_to_O_s;
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));


            parent = EnglishPattern.O_to_O_s;
            child = Pattern.Morpheme(myAttributesModel, "s", EnglishAttributes.O.NonLexeme.Suffix, "Os");
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.O_to_O_s;
            child = EnglishPattern.O_to_O_s;
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));


            // Morphematic adposition NOT allowed.
            parent = EnglishPattern.E_I
                .SetLeftSubstitutionRule(PatternSubstitutionRules.Nothing);
            child = Pattern.MorphematicAdPosition(myAttributesModel, "E-U-E", "", EnglishAttributes.U.Lexeme, EnglishAttributes.E.Lexeme, EnglishAttributes.E.Lexeme);
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));

            // Morphematic adposition allowed.
            parent = EnglishPattern.E_I
                .SetLeftSubstitutionRule(PatternSubstitutionRules.Epsilon_U_E);
            child = Pattern.MorphematicAdPosition(myAttributesModel, "E-U-E", "", EnglishAttributes.U.Lexeme, EnglishAttributes.E.Lexeme, EnglishAttributes.E.Lexeme.Adverb);
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));


            parent = EnglishPattern.I_to_A_ed;
            child = EnglishPattern.I_Suffix_ed;
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));

            parent = EnglishPattern.I_to_A_ed;
            child = EnglishPattern.I_Suffix_ing;
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsFalse(rule.Evaluate(child));


            // Grammar transference.
            parent = EnglishPattern.I_U_I;
            child = EnglishPattern.Will_I;
            rule = PatternRules.ByLeftMorphemeRule(parent);
            Assert.IsTrue(rule.Evaluate(child));
        }

    }
}

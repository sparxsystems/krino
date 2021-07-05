using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public static class EnglishPattern
    {
        private static EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        public static Pattern O_Lexeme { get; } = Pattern.Morpheme(myAttributesModel, EnglishAttributes.O.Lexeme, "Rule accepting stative lexemes.");
        public static Pattern I_Lexeme { get; } = Pattern.Morpheme(myAttributesModel, EnglishAttributes.I.Lexeme, "Rule accepting verbant lexemes.");
        public static Pattern A_Lexeme { get; } = Pattern.Morpheme(myAttributesModel, EnglishAttributes.A.Lexeme, "Rule accepting adjunctive lexemes.");
        public static Pattern E_Lexeme_Adverb { get; } = Pattern.Morpheme(myAttributesModel, EnglishAttributes.E.Lexeme.Adverb, "Rule accepting circumstantial adverb lexemes.");

        public static Pattern E_Lexeme_Preposition { get; } = Pattern.Morpheme(myAttributesModel, EnglishAttributes.E.Lexeme.Preposition, "Rule accepting circumstantial preposition lexemes.");
        public static Pattern U_Lexeme_Conjunction { get; } = Pattern.Morpheme(myAttributesModel, EnglishAttributes.U.Lexeme.Conjunction, "Rule accepting conjunction lexemes.");
        public static Pattern U_NonLexeme_Punctuation { get; } = Pattern.Morpheme(myAttributesModel, EnglishAttributes.U.NonLexeme.PunctuationMark, "Rule accepting punctuation marks.");


        public static Pattern O_Suffix = Pattern.Morpheme(myAttributesModel, EnglishAttributes.O.NonLexeme.Suffix, "O+", "Rule accepting stative suffix.");
        public static Pattern I_Suffix = Pattern.Morpheme(myAttributesModel, EnglishAttributes.I.NonLexeme.Suffix, "I+", "Rule accepting verbant suffix.");


        public static Pattern O1_I { get; } = Pattern.O1_I(myAttributesModel);
        public static Pattern O2_I { get; } = Pattern.O2_I(myAttributesModel);
        public static Pattern O3_I { get; } = Pattern.O3_I(myAttributesModel);
        public static Pattern O4_I { get; } = Pattern.O4_I(myAttributesModel);
        public static Pattern O5_I { get; } = Pattern.O5_I(myAttributesModel);

        // E.g. Speaking is prohibited. 'prohibited' is on the 2nd valency position.
        public static Pattern A2_I { get; } = Pattern.A2_I(myAttributesModel);

        public static Pattern O_to_A { get; } = Pattern.MonoTransference(myAttributesModel, "O>A", EnglishAttributes.A.Lexeme.Adjective, EnglishAttributes.O.Lexeme.Noun);


        public static Pattern I_s { get; } = Pattern.PairTransference(myAttributesModel, "I>I_s", "Rule to transform verb to 3rd person.",
                EnglishAttributes.I.Lexeme.Verb | EnglishAttributes.I.Lexeme.Verb.Sememe.Person.Third,
                EnglishMorphemeRule.Is("s", EnglishAttributes.I.NonLexeme.Suffix).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb));

        // negation: TODO: negation should appear in sememe attributes ??
        public static Pattern Not_I { get; } = Pattern.PairTransference(myAttributesModel, "I>not_I", "Rule negating a lexeme verbant.",
            EnglishAttributes.I.Lexeme.Verb.Form.Infinitive,
            EnglishMorphemeRule.Is(MorphRules.Is("not"), EnglishAttributes.E.Lexeme.Adverb).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
            EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.Infinitive))
            .SetLeftFirst();

        public static Pattern Will_I { get; } = Pattern.PairTransference(myAttributesModel, "I>will_I", "Rule for simple future of a lexeme verbant.",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Future,
                    EnglishMorphemeRule.Is(MorphRules.Is("will"), EnglishAttributes.I.Lexeme.Verb.Modal).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.Infinitive))
                    .SetLeftFirst();

        public static Pattern Have_I { get; } = Pattern.PairTransference(myAttributesModel, "I>have_I", "Rule for present perfect.",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Present | EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                    EnglishMorphemeRule.Is(MorphRules.Is("have"), EnglishAttributes.I.Lexeme.Verb).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple))
                    .SetLeftFirst();

        public static Pattern Been_I_ing { get; } = Pattern.PairTransference(myAttributesModel, "been-I_ing", "Rule for continuous present perfect.",
                    EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple,
                    EnglishMorphemeRule.Is("been", EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Past | EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Continuous))
                    .SetLeftFirst();

        public static Pattern I_ing { get; } = Pattern.PairTransference(myAttributesModel, "I>I_ing", "Rule for continuous verb.",
                EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Continuous,
                EnglishMorphemeRule.Is("ing", EnglishAttributes.I.NonLexeme.Suffix).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal));

        public static Pattern I_to_A_ed { get; } = Pattern.PairTransference(myAttributesModel, "I>A_ed", "Rule to make adjective from verb.",
                EnglishAttributes.A.Lexeme.Adjective,
                EnglishMorphemeRule.Is("ed", EnglishAttributes.I.NonLexeme.Suffix).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal));

        public static Pattern I_to_O_ing { get; } = Pattern.PairTransference(myAttributesModel, "I>O_ing", "Rule to make noun from verb.",
                EnglishAttributes.O.Lexeme.Noun,
                EnglishMorphemeRule.Is("ing", EnglishAttributes.I.NonLexeme.Suffix).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal));

        public static Pattern I_to_O_er { get; } = Pattern.PairTransference(myAttributesModel, "I>O_er", "Rule to make noun from verb.",
                EnglishAttributes.O.Lexeme.Noun,
                EnglishMorphemeRule.Is("er", EnglishAttributes.I.NonLexeme.Suffix).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal));

        public static Pattern O_s { get; } = Pattern.PairTransference(myAttributesModel, "O>O_s", "Rule to make a noun plural.",
                EnglishAttributes.O.Lexeme.Noun | EnglishAttributes.O.Lexeme.Noun.Sememe.Number.Plural,
                EnglishMorphemeRule.Is("s", EnglishAttributes.O.NonLexeme.Suffix).SetMorphematicAdPositionRule(MorphematicAdPositionRules.Nothing),
                EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.O.Lexeme.Noun));


        public static Pattern A_O { get; } = Pattern.EpsilonAdPosition(myAttributesModel, "A-O", "Rule to add an adjunctive before a stative.", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                .SetLeftFirst()
                .SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Epsilon);

        public static Pattern E_I { get; } = Pattern.EpsilonAdPosition(myAttributesModel, "E-I", "Rule to add a circumstantial adverb after verb valencies.", EnglishAttributes.E.Lexeme.Adverb, EnglishAttributes.I.Lexeme)
                .SetMorphematicAdPositionRuleForLeft(MorphematicAdPositionRules.Nothing);


        public static Pattern O_E_I { get; } = Pattern.MorphematicAdPosition(myAttributesModel, "O-E-I", "Circumstantial prepostion.", EnglishAttributes.E.Lexeme.Preposition, EnglishAttributes.O.Lexeme, EnglishAttributes.I.Lexeme);

        // E.g. speed of light
        public static Pattern O_E_O { get; } = Pattern.MorphematicAdPosition(myAttributesModel, "O-E-O", "Circumstantial prepostion.", EnglishAttributes.E.Lexeme.Preposition, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme);

        public static Pattern O_U_O { get; } = Pattern.MorphematicAdPosition(myAttributesModel, "O-U-O", "Connecting statives with a conjunction.", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme);

        public static Pattern I_U_O { get; } = Pattern.MorphematicAdPosition(myAttributesModel, "I-U-O", "Connecting verbant and stative with a conjunction.", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.I.Lexeme, EnglishAttributes.O.Lexeme);

        public static Pattern I_U_I { get; } = Pattern.MorphematicAdPosition(myAttributesModel, "I-U-I", "Complex and compound sentences.", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.I.Lexeme, EnglishAttributes.I.Lexeme);

        public static Pattern e_Period_I { get; } = Pattern.MorphematicAdPosition(myAttributesModel, "•", "Rule for the period punctuation mark.", EnglishAttributes.U.NonLexeme.PunctuationMark.Period, MorphemeRule.Nothing, MorphemeRule.I_Lexeme_Something(myAttributesModel));

    }
}

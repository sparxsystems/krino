using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public static class EnglishPattern
    {
        private static EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        public static Pattern O_Lexeme_Noun => Pattern.Morpheme(myAttributesModel, EnglishAttributes.O.Lexeme.Noun, "O.Noun", "Rule accepting nouns.");
        public static Pattern O_Lexeme_Pronoun => Pattern.Morpheme(myAttributesModel, EnglishAttributes.O.Lexeme.Pronoun, "O.Pronoun", "Rule accepting pronouns.");

        public static Pattern I_Lexeme_Verb => Pattern.Morpheme(myAttributesModel, EnglishAttributes.I.Lexeme.Verb, "I.Verb", "Rule accepting verbant lexemes.");
        public static Pattern I_Lexeme_Verb_Been => Pattern.Morpheme(myAttributesModel, "been", EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple, "I.Verb.Been", "Rule accepting verb been.");
        public static Pattern I_Lexeme_Verb_Have => Pattern.Morpheme(myAttributesModel, "have", EnglishAttributes.I.Lexeme.Verb, "I.Verb.Have", "Rule accepting verb have.");
        public static Pattern I_Lexeme_Verb_Will => Pattern.Morpheme(myAttributesModel, "will", EnglishAttributes.I.Lexeme.Verb.Modal, "I.Verb.Will", "Rule accepting modal verb will.");

        public static Pattern A_Lexeme_Adjective => Pattern.Morpheme(myAttributesModel, EnglishAttributes.A.Lexeme.Adjective, "A.Adjective", "Rule accepting adjective lexemes.");
        public static Pattern A_Lexeme_Determiner => Pattern.Morpheme(myAttributesModel, EnglishAttributes.A.Lexeme.Determiner, "A.Determiner", "Rule accepting determiner lexemes.");

        public static Pattern E_Lexeme_Adverb => Pattern.Morpheme(myAttributesModel, EnglishAttributes.E.Lexeme.Adverb, "E.Adverb", "Rule accepting circumstantial adverb lexemes.");
        public static Pattern E_Lexeme_Adverb_Not => Pattern.Morpheme(myAttributesModel, "not", EnglishAttributes.E.Lexeme.Adverb, "E.Adverb.Not", "Rule accepting circumstantial adverb lexemes.");
        public static Pattern E_Lexeme_Preposition => Pattern.Morpheme(myAttributesModel, EnglishAttributes.E.Lexeme.Preposition, "E.Preposition", "Rule accepting circumstantial preposition lexemes.");

        public static Pattern U_Lexeme_Conjunction => Pattern.Morpheme(myAttributesModel, EnglishAttributes.U.Lexeme.Conjunction, "U.Connjunction", "Rule accepting conjunction lexemes.");
        public static Pattern U_NonLexeme_Punctuation => Pattern.Morpheme(myAttributesModel, EnglishAttributes.U.NonLexeme.PunctuationMark, "U.Punctuation", "Rule accepting punctuation marks.");


        public static Pattern O_Suffix_s => Pattern.Morpheme(myAttributesModel, "s", EnglishAttributes.O.NonLexeme.Suffix, "O-s", "Rule accepting stative suffix s.");

        public static Pattern I_Suffix_s => Pattern.Morpheme(myAttributesModel, "s", EnglishAttributes.I.NonLexeme.Suffix, "I-er", "Rule accepting verb suffix er.");
        public static Pattern I_Suffix_er => Pattern.Morpheme(myAttributesModel, "er", EnglishAttributes.I.NonLexeme.Suffix, "I-er", "Rule accepting verb suffix er.");
        public static Pattern I_Suffix_ing => Pattern.Morpheme(myAttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix, "I-ing", "Rule accepting verb suffix ing.");
        public static Pattern I_Suffix_ed => Pattern.Morpheme(myAttributesModel, "ed", EnglishAttributes.I.NonLexeme.Suffix, "I-ed", "Rule accepting verb suffix ed.");


        public static Pattern O1_I => Pattern.O1_I(myAttributesModel);
        public static Pattern O2_I => Pattern.O2_I(myAttributesModel);
        public static Pattern O3_I => Pattern.O3_I(myAttributesModel);
        public static Pattern O4_I => Pattern.O4_I(myAttributesModel);
        public static Pattern O5_I => Pattern.O5_I(myAttributesModel);

        // E.g. Speaking is prohibited. 'prohibited' is on the 2nd valency position.
        public static Pattern A2_I => Pattern.A2_I(myAttributesModel);

        public static Pattern O_to_A => Pattern.UnipolarMorphemeTransference(myAttributesModel, "O>A", "Rule to adjective from noun.", EnglishAttributes.A.Lexeme.Adjective, EnglishAttributes.O.Lexeme.Noun);


        public static Pattern I_to_I_s => Pattern.BipolarMorphemeTransference(myAttributesModel, "I>I_s", "Rule to transform verb to 3rd person.",
                EnglishAttributes.I.Lexeme.Verb | EnglishAttributes.I.Lexeme.Verb.Sememe.Person.Third,
                "s", EnglishAttributes.I.NonLexeme.Suffix,
                EnglishAttributes.I.Lexeme.Verb);

        public static Pattern I_to_I_ing => Pattern.BipolarMorphemeTransference(myAttributesModel, "I>I_ing", "Rule for continuous verb.",
                EnglishAttributes.I.Lexeme.Verb.Form.PresentParticiple,
                "ing", EnglishAttributes.I.NonLexeme.Suffix,
                EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal);

        public static Pattern I_to_A_ed => Pattern.BipolarMorphemeTransference(myAttributesModel, "I>A_ed", "Rule to make adjective from verb.",
                EnglishAttributes.A.Lexeme.Adjective,
                "ed", EnglishAttributes.I.NonLexeme.Suffix,
                EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal);

        public static Pattern I_to_O_ing => Pattern.BipolarMorphemeTransference(myAttributesModel, "I>O_ing", "Rule to make noun from verb.",
                EnglishAttributes.O.Lexeme.Noun,
                "ing", EnglishAttributes.I.NonLexeme.Suffix,
                EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal);

        public static Pattern I_to_O_er => Pattern.BipolarMorphemeTransference(myAttributesModel, "I>O_er", "Rule to make noun from verb.",
                EnglishAttributes.O.Lexeme.Noun,
                "er", EnglishAttributes.I.NonLexeme.Suffix,
                EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal);

        public static Pattern O_to_O_s => Pattern.BipolarMorphemeTransference(myAttributesModel, "O>O_s", "Rule to make a noun plural.",
                EnglishAttributes.O.Lexeme.Noun.Sememe.Number.Plural,
                "s", EnglishAttributes.O.NonLexeme.Suffix,
                EnglishAttributes.O.Lexeme.Noun);



        public static Pattern A_O => Pattern.EpsilonAdPosition(myAttributesModel, "A-O", "Rule to add an adjunctive before a stative.",
                EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                .SetLeftFirst();

        public static Pattern E_I => Pattern.EpsilonAdPosition(myAttributesModel, "E-I", "Rule to add a circumstantial adverb after verb valencies.",
                EnglishAttributes.E.Lexeme.Adverb, EnglishAttributes.I.Lexeme.Verb)
                .SetMorphematicAdPositionRuleForLeft(MorphematicAdPositionRules.Nothing);


        // negation: TODO: negation should appear in sememe attributes ??
        public static Pattern Not_I => Pattern.GrammarAdPosition(myAttributesModel, "not_I", "Rule negating a lexeme verbant.",
                EnglishAttributes.I.Lexeme.Verb.Form.Base,
                "not", EnglishAttributes.E.Lexeme.Adverb,
                EnglishAttributes.I.Lexeme.Verb)
                .SetLeftFirst()
                .SetLeftPatternRule(PatternRules.Is(E_Lexeme_Adverb_Not));

        public static Pattern Will_I => Pattern.GrammarAdPosition(myAttributesModel, "will_I", "Rule for simple future of a lexeme verbant.",
                EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Future,
                "will", EnglishAttributes.I.Lexeme.Verb.Modal,
                EnglishAttributes.I.Lexeme.Verb)
                .SetLeftFirst()
                .SetLeftPatternRule(PatternRules.Is(I_Lexeme_Verb_Will));

        public static Pattern Have_Been_I_ing => Pattern.GrammarAdPosition(myAttributesModel, "have_been_I_ing", "Rule for 'have been' followed by verb in present participle form.",
                EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Present | EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.ContinousPerfect,
                EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Present | EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                EnglishAttributes.I.Lexeme.Verb.Form.PresentParticiple)
                .SetLeftFirst()
                .SetLeftPatternRule(PatternRules.Is(Have_Been_Auxiliary))
                .SetRightPatternRule(PatternRules.Is(I_to_I_ing));

        public static Pattern Have_Been_Auxiliary => Pattern.GrammarAdPosition(myAttributesModel, "have_been", "Rule for auxiliary verbs in present perfect continuous.",
                EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Present | EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                "have", EnglishAttributes.I.Lexeme.Verb,
                "been", EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple)
                .SetLeftFirst()
                .SetLeftPatternRule(PatternRules.Is(I_Lexeme_Verb_Have))
                .SetRightPatternRule(PatternRules.Is(I_Lexeme_Verb_Been));

        public static Pattern Have_I => Pattern.GrammarAdPosition(myAttributesModel, "have_I", "Rule for present perfect.",
                EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Present | EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                "have", EnglishAttributes.I.Lexeme.Verb,
                EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple)
                .SetLeftFirst()
                .SetLeftPatternRule(PatternRules.Is(I_Lexeme_Verb_Have));



        public static Pattern O_E_I => Pattern.MorphematicAdPosition(myAttributesModel, "O-E-I", "Circumstantial prepostion.",
            EnglishAttributes.E.Lexeme.Preposition,
            EnglishAttributes.O.Lexeme, EnglishAttributes.I.Lexeme.Verb);

        // E.g. speed of light
        public static Pattern O_E_O => Pattern.MorphematicAdPosition(myAttributesModel, "O-E-O", "Circumstantial prepostion.",
            EnglishAttributes.E.Lexeme.Preposition,
            EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme);


        public static Pattern A_U_A => Pattern.MorphematicAdPosition(myAttributesModel, "A-U-A", "Connecting adjectives with a conjunction.",
            EnglishAttributes.U.Lexeme.Conjunction, 
            EnglishAttributes.A.Lexeme.Adjective, EnglishAttributes.A.Lexeme.Adjective);

        public static Pattern O_U_O => Pattern.MorphematicAdPosition(myAttributesModel, "O-U-O", "Connecting statives with a conjunction.",
            EnglishAttributes.U.Lexeme.Conjunction,
            EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme);

        public static Pattern I_U_O => Pattern.MorphematicAdPosition(myAttributesModel, "I-U-O", "Digretion after subject.",
            EnglishAttributes.U.Lexeme.Conjunction,
            EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.O.Lexeme);

        public static Pattern I_U_I => Pattern.MorphematicAdPosition(myAttributesModel, "I-U-I", "Complex and compound sentences.",
            EnglishAttributes.U.Lexeme.Conjunction,
            EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb);

        public static Pattern e_Period_I => Pattern.MorphematicAdPosition(myAttributesModel, "•", "Rule for the period punctuation mark.",
            EnglishAttributes.U.NonLexeme.PunctuationMark.Period,
            MorphemeRule.Nothing, MorphemeRule.I_Lexeme_Verb_Something(myAttributesModel));

    }
}

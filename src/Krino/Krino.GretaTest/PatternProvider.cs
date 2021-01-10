using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using System.Collections.Generic;

namespace Krino.GretaTest
{
    public static class PatternProvider
    {
        public static List<Pattern> Patterns = new List<Pattern>()
        {
            EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme),
            EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme),
            EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme),
            EnglishPattern.Morpheme(EnglishAttributes.E.Lexeme.Adverb),

            EnglishPattern.O1_I.SetLeftFirst(),
            EnglishPattern.O1_I,
            EnglishPattern.O2_I,
            EnglishPattern.O3_I,
            EnglishPattern.O4_I,
            EnglishPattern.O5_I,

            EnglishPattern.Morpheme("O+", EnglishAttributes.O.NonLexeme.Suffix),
            EnglishPattern.Morpheme("I+", EnglishAttributes.I.NonLexeme.Suffix),

            EnglishPattern.MonoTransference("O>A", EnglishAttributes.A.Lexeme.Adjective, EnglishAttributes.O.Lexeme.Noun),

            // negation: TODO: negation sjould appear in sememe attributes ??
            EnglishPattern.PairTransference("I>not_I",
                    EnglishAttributes.I.Lexeme.Verb.Form.Infinitive,
                    EnglishMorphemeRule.Is(MorphRules.Is("not"), EnglishAttributes.E.Lexeme.Adverb).SetSubstitution(SubstitutionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.Infinitive))
                    .SetLeftFirst(),

            // simple future
            EnglishPattern.PairTransference("I>will_I",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Future,
                    EnglishMorphemeRule.Is(MorphRules.Is("will"), EnglishAttributes.I.Lexeme.Verb.Modal).SetSubstitution(SubstitutionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.Infinitive))
                    .SetLeftFirst(),

            // present perfect
            EnglishPattern.PairTransference("I>have_I",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Present | EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                    EnglishMorphemeRule.Is(MorphRules.Is("have"), EnglishAttributes.I.Lexeme.Verb).SetSubstitution(SubstitutionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple))
                    .SetLeftFirst(),

            EnglishPattern.PairTransference("been-I_ing",
                    EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple,
                    EnglishMorphemeRule.Is("been", EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Past | EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple).SetSubstitution(SubstitutionRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Continuous))
                    .SetLeftFirst(),

            EnglishPattern.PairTransference("I>I_ing",
                EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Continuous,
                EnglishAttributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous, 0,
                EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal),


            EnglishPattern.PairTransference("O>O_s",
                EnglishAttributes.O.Lexeme.Noun | EnglishAttributes.O.Lexeme.Noun.Sememe.Number.Plural,
                EnglishAttributes.O.NonLexeme.Suffix,
                EnglishAttributes.O.Lexeme.Noun)
                .SetSubstitutionForLeft(SubstitutionRules.Nothing),




            EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                .SetLeftFirst()
                .SetSubstitutionForRight(SubstitutionRules.Epsilon),

            EnglishPattern.EpsilonAdPosition("E-I", EnglishAttributes.E.Lexeme.Adverb, EnglishAttributes.I.Lexeme)
                //.SetLeftFirst()
                .SetSubstitutionForLeft(SubstitutionRules.Nothing),



            // Verbant circumstantial with a preposition.
            EnglishPattern.MorphematicAdPosition("O-E-I", EnglishAttributes.E.Lexeme.Preposition, EnglishAttributes.O.Lexeme, EnglishAttributes.I.Lexeme),
            
            // E.g. speed of light
            EnglishPattern.MorphematicAdPosition("O-E-O", EnglishAttributes.E.Lexeme.Preposition, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme),


            // E.g. O and O
            EnglishPattern.MorphematicAdPosition("O-U-O", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme),

            // E.g. I as O ('the world' as 'people were saying')
            EnglishPattern.MorphematicAdPosition("I-U-O", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.I.Lexeme, EnglishAttributes.O.Lexeme),


            new Pattern("•")
            {
                MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.U.NonLexeme.PunctuationMark.Period),
                LeftRule = MorphemeRule.Anything,
                RightRule = new MorphemeRule(GrammarCharacter.e, MorphRules.Anything, MaskRule.Something),
            },

        };
    }
}

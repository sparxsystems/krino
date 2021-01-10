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


            EnglishPattern.PairTransference("I>SimpleFuture",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Future,
                    EnglishMorphemeRule.Is(MorphRules.Is("will"), EnglishAttributes.I.Lexeme.Verb.Modal).SetSubstitution(InheritanceRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.Infinitive))
                    .SetLeftFirst(),

            EnglishPattern.PairTransference("I>PresentPerfect",
                    EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Present | EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                    EnglishMorphemeRule.Is(MorphRules.Is("have"), EnglishAttributes.I.Lexeme.Verb).SetSubstitution(InheritanceRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple))
                    .SetLeftFirst(),

            EnglishPattern.PairTransference("been-I_ing",
                    EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple,
                    EnglishMorphemeRule.Is("been", EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Past | EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple).SetSubstitution(InheritanceRules.Nothing),
                    EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Continuous))
                    .SetLeftFirst(),

            EnglishPattern.PairTransference("I>I_ing",
                EnglishAttributes.I.Lexeme.Verb.Sememe.Aspect.Continuous,
                EnglishAttributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous, 0,
                EnglishAttributes.I.Lexeme.Verb, EnglishAttributes.I.Lexeme.Verb.Modal),

            


            EnglishPattern.EpsilonAdPosition("A-O", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForRight(InheritanceRules.Epsilon),

            EnglishPattern.EpsilonAdPosition("E-I", EnglishAttributes.E.Lexeme.Adverb, EnglishAttributes.I.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForLeft(InheritanceRules.Nothing),

            EnglishPattern.EpsilonAdPosition("O+-O", EnglishAttributes.O.NonLexeme.Suffix, EnglishAttributes.O.Lexeme)
                .SetInheritanceForLeft(InheritanceRules.Nothing)
                .SetInheritanceForRight(InheritanceRules.Epsilon),

            //PatternExt.EpsilonAdPosition("I-I", Attributes.I.Lexeme.Verb, Attributes.I.Lexeme.Verb)
            //    .SetLeftFirst()
            //    .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),

            // Verbant circumstantial with a preposition.
            EnglishPattern.MorphematicAdPosition("O-E-I", EnglishAttributes.E.Lexeme.Preposition, EnglishAttributes.O.Lexeme, EnglishAttributes.I.Lexeme),
            
            // E.g. speed of light
            EnglishPattern.MorphematicAdPosition("O-E-O", EnglishAttributes.E.Lexeme.Preposition, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme),


            // E.g. O and O
            EnglishPattern.MorphematicAdPosition("O-U-O", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme),

            //PatternExt.MorphematicAdPosition("I-U-O", Attributes.U.Lexeme.Conjunction, Attributes.I.Lexeme, Attributes.O.Lexeme)
            //    .SetRightFirst(),


            new Pattern("•")
            {
                MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, EnglishAttributes.U.NonLexeme.PunctuationMark.Period),
                LeftRule = MorphemeRule.Anything,
                RightRule = new MorphemeRule(GrammarCharacter.e, MorphRules.Anything, MaskRule.Something),
            },

        };
    }
}

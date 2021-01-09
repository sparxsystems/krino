using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Attributing;
using System.Collections.Generic;

namespace Krino.GretaTest
{
    public static class PatternProvider
    {
        public static List<Pattern> Patterns = new List<Pattern>()
        {
            Pattern.Morpheme(Attributes.O.Lexeme),
            Pattern.Morpheme(Attributes.I.Lexeme),
            Pattern.Morpheme(Attributes.A.Lexeme),
            Pattern.Morpheme(Attributes.E.Lexeme.Adverb),

            Pattern.O1_I.SetLeftFirst(),
            Pattern.O1_I.SetRightFirst(),
            Pattern.O2_I,
            Pattern.O3_I,
            Pattern.O4_I,
            Pattern.O5_I,

            Pattern.Morpheme("O+", Attributes.O.NonLexeme.Suffix),
            Pattern.Morpheme("I+", Attributes.I.NonLexeme.Suffix),

            Pattern.GrammerCharacterTransference("O>A", Attributes.A.Lexeme.Adjective, Attributes.O.Lexeme.Noun),


            Pattern.Transference("I>SimpleFuture",
                    Attributes.I.Lexeme.Verb.Sememe.Tense.Future,
                    MorphemeRule.Is(MorphRuleMaker.Is("will"), Attributes.I.Lexeme.Verb.Modal).SetOrder(1).SetInheritance(InheritanceRuleMaker.Nothing),
                    MorphemeRule.Is(MorphRuleMaker.Something, Attributes.I.Lexeme.Verb.Form.Infinitive)),

            Pattern.Transference("I>PresentPerfect",
                    Attributes.I.Lexeme.Verb.Sememe.Tense.Present | Attributes.I.Lexeme.Verb.Sememe.Aspect.Perfect,
                    MorphemeRule.Is(MorphRuleMaker.Is("have"), Attributes.I.Lexeme.Verb).SetOrder(1).SetInheritance(InheritanceRuleMaker.Nothing),
                    MorphemeRule.Is(MorphRuleMaker.Something, Attributes.I.Lexeme.Verb.Form.PastParticiple)),

            Pattern.Transference("been-I_ing",
                    Attributes.I.Lexeme.Verb.Form.PastParticiple,
                    MorphemeRule.Is("been", Attributes.I.Lexeme.Verb.Sememe.Tense.Past | Attributes.I.Lexeme.Verb.Form.PastParticiple).SetOrder(1).SetInheritance(InheritanceRuleMaker.Nothing),
                    MorphemeRule.Is(MorphRuleMaker.Something, Attributes.I.Lexeme.Verb.Sememe.Aspect.Continuous)),

            Pattern.Transference("I>I_ing",
                Attributes.I.Lexeme.Verb.Sememe.Aspect.Continuous,
                Attributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous, 0,
                Attributes.I.Lexeme.Verb, Attributes.I.Lexeme.Verb.Modal),

            


            Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForRight(InheritanceRuleMaker.Epsilon),

            Pattern.EpsilonAdPosition("E-I", Attributes.E.Lexeme.Adverb, Attributes.I.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),

            Pattern.EpsilonAdPosition("O+-O", Attributes.O.NonLexeme.Suffix, Attributes.O.Lexeme)
                .SetRightFirst()
                .SetInheritanceForLeft(InheritanceRuleMaker.Nothing)
                .SetInheritanceForRight(InheritanceRuleMaker.Epsilon),

            //Pattern.EpsilonAdPosition("I-I", Attributes.I.Lexeme.Verb, Attributes.I.Lexeme.Verb)
            //    .SetLeftFirst()
            //    .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),

            // Verbant circumstantial with a preposition.
            Pattern.MorphematicAdPosition("O-E-I", Attributes.E.Lexeme.Preposition, Attributes.O.Lexeme, Attributes.I.Lexeme)
                .SetRightFirst(),
            
            // E.g. speed of light
            Pattern.MorphematicAdPosition("O-E-O", Attributes.E.Lexeme.Preposition, Attributes.O.Lexeme, Attributes.O.Lexeme)
                .SetRightFirst(),


            // E.g. O and O
            Pattern.MorphematicAdPosition("O-U-O", Attributes.U.Lexeme.Conjunction, Attributes.O.Lexeme, Attributes.O.Lexeme)
                .SetRightFirst(),

            //Pattern.MorphematicAdPosition("I-U-O", Attributes.U.Lexeme.Conjunction, Attributes.I.Lexeme, Attributes.O.Lexeme)
            //    .SetRightFirst(),


            new Pattern("•")
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U.NonLexeme.PunctuationMark.Period),
                LeftRule = MorphemeRule.Anything,
                RightRule = new MorphemeRule(GrammarCharacter.e, MorphRuleMaker.Anything, MaskRule.Something),
            },

        };
    }
}

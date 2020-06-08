using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
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

            Pattern.Morpheme("O+", Attributes.O.NonLexeme.Suffix),
            Pattern.Morpheme("I+", Attributes.I.NonLexeme.Suffix),

            Pattern.PrimitiveTransference("O>A", Attributes.A.Lexeme, Attributes.O.Lexeme.Noun),

            Pattern.Transference("I>I_ing",
                Attributes.I.Lexeme.Verb.Sememe.Aspect.Continuous,
                Attributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous, 0,
                Attributes.I.Lexeme.Verb, Attributes.I.Lexeme.Verb.Modal),

            Pattern.O1_I.SetLeftFirst(),
            Pattern.O1_I.SetRightFirst(),
            Pattern.O2_I,
            Pattern.O3_I,
            Pattern.O4_I,
            Pattern.O5_I,


            Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForRight(InheritanceRuleMaker.Epsilon),

            Pattern.EpsilonAdPosition("E-I", Attributes.E.Lexeme.Adverb, Attributes.I.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),

            Pattern.EpsilonAdPosition("O+-O", Attributes.O.NonLexeme.Suffix, Attributes.O.Lexeme)
                .SetRightFirst(),

            Pattern.EpsilonAdPosition("I-I", Attributes.I.Lexeme.Verb, Attributes.I.Lexeme.Verb)
                .SetLeftFirst()
                .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),

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
                RightRule = new MorphemeRule(GrammarCharacter.e, MorphRuleMaker.Anything, MaskRule.Something),
                LeftRule = MorphemeRule.Anything,
            },

        };
    }
}

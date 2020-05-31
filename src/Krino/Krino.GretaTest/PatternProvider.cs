using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
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
            Pattern.Morpheme(Attributes.E.Lexeme),

            Pattern.Morpheme("O+", Attributes.O.NonLexeme.Suffix),

            Pattern.PrimitiveTransference("O>A", Attributes.A.Lexeme, Attributes.O.Lexeme),

            Pattern.O1_I,
            Pattern.O2_I,
            Pattern.O3_I,
            Pattern.O4_I,
            Pattern.O5_I,


            Pattern.EpsilonAdPosition("A-O", Attributes.A.Lexeme, Attributes.O.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForRight(InheritanceRuleMaker.Epsilon),

            Pattern.EpsilonAdPosition("E-I", Attributes.E.Lexeme, Attributes.I.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),

            Pattern.EpsilonAdPosition("O+-O", Attributes.O.Lexeme, Attributes.O.NonLexeme.Suffix)
                .SetRightFirst(),

            Pattern.EpsilonAdPosition("I-I", Attributes.I.Lexeme.Verb.Modal, Attributes.I.Lexeme)
                .SetLeftFirst()
                .SetInheritanceForLeft(InheritanceRuleMaker.Nothing),

            Pattern.Transference("O-E-I", Attributes.E.Lexeme, Attributes.O.Lexeme, Attributes.I.Lexeme)
                .SetRightFirst(),

            Pattern.Transference("O-U-O", Attributes.U.Lexeme.Conjunction, Attributes.O.Lexeme, Attributes.O.Lexeme)
                .SetRightFirst(),

            Pattern.Transference("I-U-O", Attributes.U.Lexeme.Conjunction, Attributes.I.Lexeme, Attributes.O.Lexeme)
                .SetRightFirst(),


            new Pattern("EOS")
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U.NonLexeme.PunctuationMark.Period),
                RightRule = MorphemeRule.Anything.SetOrder(1),
                LeftRule = MorphemeRule.Anything,
            },

        };
    }
}

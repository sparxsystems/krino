using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
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
            new Pattern("O")
            {
                MorphemeRule = MorphemeRule.O_Lexeme,
                RightRule = MorphemeRule.Nothing,
                LeftRule = MorphemeRule.Nothing,
            },

            new Pattern("I")
            {
                MorphemeRule = MorphemeRule.I_Lexeme,
                RightRule = MorphemeRule.Nothing,
                LeftRule = MorphemeRule.Nothing,
            },

            new Pattern("A")
            {
                MorphemeRule = MorphemeRule.A_Lexeme,
                RightRule = MorphemeRule.Nothing,
                LeftRule = MorphemeRule.Nothing,
            },

            new Pattern("E")
                {
                    MorphemeRule = MorphemeRule.E_Not_NonLexeme,
                    RightRule = MorphemeRule.I_Not_NonLexeme,
                    LeftRule = MorphemeRule.O_Not_NonLexeme
                },

            new Pattern("O1-I")
            {
                MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                RightRule = MorphemeRule.I_Not_NonLexeme,
                LeftRule = MorphemeRule.O_Not_NonLexeme,
            },
            new Pattern("O2-I")
            {
                MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(2),
                RightRule = MorphemeRule.I_Not_NonLexeme,
                LeftRule = MorphemeRule.O_Not_NonLexeme,
            },

            new Pattern("A-O")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.O_Not_NonLexeme,
                LeftRule = MorphemeRule.A_Not_NonLexeme.SetOrder(1),
            },

            new Pattern("O-and-O")
            {
                MorphemeRule = MorphemeRule.Is(GrammarCharacter.U, "and", Attributes.U.Lexeme),
                RightRule = MorphemeRule.O_Not_NonLexeme,
                LeftRule = MorphemeRule.O_Not_NonLexeme
            },


            new Pattern("O>A")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.EmptyString, MaskRule.Is(Attributes.A) & !MaskRule.Is(Attributes.A.NonLexeme)),
                LeftRule = MorphemeRule.O_Not_NonLexeme,
            },


            new Pattern(".")
            {
                MorphemeRule = MorphemeRule.Is(GrammarCharacter.U, ".", Attributes.U.NonLexeme),
                RightRule = MorphemeRule.Something(GrammarCharacter.I),
                LeftRule = MorphemeRule.Anything,
            },
        };
    }
}

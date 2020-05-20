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
                RightRule = PatternRule.Nothing,
                LeftRule = PatternRule.Nothing,
            },

            new Pattern("I")
            {
                MorphemeRule = MorphemeRule.I_Lexeme,
                RightRule = PatternRule.Nothing,
                LeftRule = PatternRule.Nothing,
            },

            new Pattern("A")
            {
                MorphemeRule = MorphemeRule.A_Lexeme,
                RightRule = PatternRule.Nothing,
                LeftRule = PatternRule.Nothing,
            },

            new Pattern("E")
                {
                    MorphemeRule = MorphemeRule.E_Not_NonLexeme,
                    RightRule = new PatternRule(MorphemeRule.I_Not_NonLexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme)
                },

            new Pattern("O1-I")
            {
                PatternAttributes = PatternAttributes.ValencyPosition.First,
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = new PatternRule(MorphemeRule.I_Not_NonLexeme),
                LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
            },
            new Pattern("O2-I")
            {
                PatternAttributes = PatternAttributes.ValencyPosition.Second,
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = new PatternRule(MorphemeRule.I_Not_NonLexeme),
                LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
            },

            new Pattern("A-O")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
                LeftRule = new PatternRule(MorphemeRule.A_Not_NonLexeme)
            },

            new Pattern("O-and-O")
            {
                MorphemeRule = MorphemeRule.Is(GrammarCharacter.U, "and", Attributes.U.Lexeme),
                RightRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
                LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme)
            },


            new Pattern("O>A")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = new PatternRule(new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.Nothing, MaskRule.Is(Attributes.A) & !MaskRule.Is(Attributes.A.NonLexeme))),
                LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
            },


            new Pattern(".")
            {
                MorphemeRule = MorphemeRule.Is(GrammarCharacter.U, ".", Attributes.U.NonLexeme),
                RightRule = new PatternRule(MorphemeRule.Something(GrammarCharacter.I)),
                LeftRule = new PatternRule(MorphemeRule.Anything),
            },
        };
    }
}

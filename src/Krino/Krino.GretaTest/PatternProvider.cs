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
                MorphemeRule = MorphemeRule.E_Lexeme,
                RightRule = MorphemeRule.Nothing,
                LeftRule = MorphemeRule.Nothing,
            },

            new Pattern("O-E-I")
            {
                MorphemeRule = MorphemeRule.E_Lexeme,
                RightRule = MorphemeRule.I.SetOrder(1),
                LeftRule = MorphemeRule.O
            },

            new Pattern("O-suffix")
            {
                MorphemeRule = new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Something, MaskRule.Is(Attributes.O.NonLexeme.NounSuffix)),
                RightRule = MorphemeRule.Nothing,
                LeftRule = MorphemeRule.Nothing,
            },

            new Pattern("O1-I")
            {
                MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                RightRule = MorphemeRule.I,
                LeftRule = MorphemeRule.O.SetOrder(1),
            },
            new Pattern("O2-I")
            {
                MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(2),
                RightRule = MorphemeRule.I.SetOrder(1),
                LeftRule = MorphemeRule.O,
            },

            new Pattern("A-O")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.O.SetInheritance(InheritanceRuleMaker.Epsilon),
                LeftRule = MorphemeRule.A.SetOrder(1),
            },

            new Pattern("E-I")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.I,
                LeftRule = MorphemeRule.E_Lexeme.SetOrder(1),
            },

            new Pattern("O-U-O")
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U.Lexeme.Conjunction),
                RightRule = MorphemeRule.O.SetOrder(1),
                LeftRule = MorphemeRule.O
            },

            // Primitive transference.
            new Pattern("O>A")
            {
                MorphemeRule = MorphemeRule.Is("", Attributes.A.Lexeme),
                RightRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.Nothing,
            },

            new Pattern("O>O-suffix")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.O_Lexeme.SetOrder(1),
                LeftRule = new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Something, MaskRule.Is(Attributes.O.NonLexeme.NounSuffix)),
            },

            new Pattern("I>I")
            {
                MorphemeRule = MorphemeRule.Epsilon,
                RightRule = MorphemeRule.I_Lexeme,
                LeftRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.I.Lexeme.Verb.Modal).SetOrder(1),
            },


            new Pattern(".")
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, Attributes.U.NonLexeme.PunctuationMark),
                RightRule = MorphemeRule.Anything.SetOrder(1),
                LeftRule = MorphemeRule.Anything,
            },
        };
    }
}

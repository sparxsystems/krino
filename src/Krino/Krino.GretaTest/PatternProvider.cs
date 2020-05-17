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

            new Pattern("O1-I")
            {
                PatternAttributes = PatternAttributes.ValencyPosition.First,
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
                MorphemeRule = MorphemeRule.Is(GrammarCharacter.U, "and", Attributes.U.Lexeme.Conjunction),
                RightRule = new PatternRule(MorphemeRule.O_Not_NonLexeme),
                LeftRule = new PatternRule(MorphemeRule.O_Not_NonLexeme)
            },

            new Pattern(".")
            {
                MorphemeRule = MorphemeRule.Is(GrammarCharacter.U, ".", Attributes.U.NonLexeme.PunctuationMark.Period),
                RightRule = new PatternRule(MorphemeRule.Something(GrammarCharacter.I)),
                LeftRule = new PatternRule(MorphemeRule.Anything),
            },
        };
    }
}

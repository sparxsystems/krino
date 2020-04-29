using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    public class ConstructiveDictionary : IConstructiveDictionary
    {
        public MultiDictionary<string, IMorpheme> Lexemes { get; } = new MultiDictionary<string, IMorpheme>()
        {
            {"I", new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun.Subjective } },
            {"book", new Morpheme("book") { Attributes = StructuralAttributes.O.Noun.Common.Concrete | StructuralAttributes.O.Noun.Countable } },
            {"read", new Morpheme("read") { Attributes = StructuralAttributes.I.Verb.Bivalent | StructuralAttributes.I.Verb.Unergative } },
            {"the", new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner.DefiniteArticle } },
        };


        public List<IPattern> Patterns { get; } = new List<IPattern>()
        {
            new Pattern()
            {
                MorphemeRule = MorphemeRule.O,
                AdPositionRule = PatternRule.Epsilon,
                RightRule = PatternRule.Nothing,
                LeftRule = PatternRule.Nothing,
            },
            new Pattern()
            {
                MorphemeRule = MorphemeRule.O,
                AdPositionRule = PatternRule.EpsilonValency1,
                RightRule = PatternRule.Nothing,
                LeftRule = PatternRule.Nothing,
            },
            new Pattern()
            {
                MorphemeRule = MorphemeRule.I2,
                AdPositionRule = PatternRule.Epsilon,
                RightRule = PatternRule.Nothing,
                LeftRule = PatternRule.Nothing,
            },
            new Pattern()
            {
                MorphemeRule = MorphemeRule.A,
                AdPositionRule = PatternRule.Epsilon,
                RightRule = PatternRule.Nothing,
                LeftRule = PatternRule.Nothing,
            },

            new Pattern()
            {
                MorphemeRule = MorphemeRule.Epsilon,
                AdPositionRule = PatternRule.Anything,
                RightRule = PatternRule.Anything,
                LeftRule = PatternRule.Anything,
            },
            new Pattern()
            {
                PatternAttributes = PatternAttributes.ValencyPosition.First,

                MorphemeRule = MorphemeRule.Epsilon,
                AdPositionRule = PatternRule.Anything,
                RightRule = PatternRule.Anything,
                LeftRule = PatternRule.Anything,
            },
        };

        
    }
}

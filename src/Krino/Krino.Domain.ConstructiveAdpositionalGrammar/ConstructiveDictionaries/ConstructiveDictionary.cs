using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
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
                MorphemeMatchingRule = MorphemeRule.O,
                AdPositionMatchingRule = PatternRule.Epsilon,
                RightMatchingRule = PatternRule.Nothing,
                LeftMatchingRule = PatternRule.Nothing,
            },
            new Pattern()
            {
                MorphemeMatchingRule = MorphemeRule.O,
                AdPositionMatchingRule = PatternRule.EpsilonValency1,
                RightMatchingRule = PatternRule.Nothing,
                LeftMatchingRule = PatternRule.Nothing,
            },
            new Pattern()
            {
                MorphemeMatchingRule = MorphemeRule.I2,
                AdPositionMatchingRule = PatternRule.Epsilon,
                RightMatchingRule = PatternRule.Nothing,
                LeftMatchingRule = PatternRule.Nothing,
            },
            new Pattern()
            {
                MorphemeMatchingRule = MorphemeRule.A,
                AdPositionMatchingRule = PatternRule.Epsilon,
                RightMatchingRule = PatternRule.Nothing,
                LeftMatchingRule = PatternRule.Nothing,
            },

            new Pattern()
            {
                MorphemeMatchingRule = MorphemeRule.Epsilon,
                AdPositionMatchingRule = PatternRule.Anything,
                RightMatchingRule = PatternRule.Anything,
                LeftMatchingRule = PatternRule.Anything,
            },
            new Pattern()
            {
                PatternAttributes = PatternAttributes.ValencyPosition.First,

                MorphemeMatchingRule = MorphemeRule.Epsilon,
                AdPositionMatchingRule = PatternRule.Anything,
                RightMatchingRule = PatternRule.Anything,
                LeftMatchingRule = PatternRule.Anything,
            },
        };

        
    }
}

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
        public MultiDictionaryUniqueValue<string, IMorpheme> Lexemes { get; } = new MultiDictionaryUniqueValue<string, IMorpheme>()
        {
            {"I", new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun.Subjective } },
            {"book", new Morpheme("book") { Attributes = StructuralAttributes.O.Noun.Common.Concrete | StructuralAttributes.O.Noun.Countable } },
            {"read", new Morpheme("read") { Attributes = StructuralAttributes.I.Verb.Bivalent | StructuralAttributes.I.Verb.Unergative } },
            {"the", new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner.DefiniteArticle } },
        };


        public List<IPattern> Patterns { get; } = new List<IPattern>()
        {
        };

        
    }
}

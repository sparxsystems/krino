using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Declares the constructive dictionary as defined by Constructive Adpositional Grammar.
    /// </summary>
    public interface IConstructiveDictionary
    {
        /// <summary>
        /// List of known patterns.
        /// </summary>
        List<IPattern> Patterns { get; }

 //       bool AddPattern()

        /// <summary>
        /// List of known lexemes.
        /// </summary>
        MultiDictionaryUniqueValue<string, IMorpheme> Lexemes { get; }
    }
}

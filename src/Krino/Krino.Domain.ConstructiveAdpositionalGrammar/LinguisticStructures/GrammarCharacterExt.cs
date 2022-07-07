using System;
using System.Linq;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    /// <summary>
    /// Utility and extenstion functionality for the GrammarCharacter enum.
    /// </summary>
    public static class GrammarCharacterExt
    {
        /// <summary>
        /// Returns all enums of GrammarCharacter.
        /// </summary>
        /// <returns></returns>
        public static GrammarCharacter[] GetValues()
        {
            GrammarCharacter[] grammarCharacters = Enum.GetValues(typeof(GrammarCharacter)).Cast<GrammarCharacter>().ToArray();
            return grammarCharacters;
        }
    }
}

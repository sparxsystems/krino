using Krino.ConstructiveGrammar.LinguisticStructures;
using System.Collections.Generic;

namespace Krino.ConstructiveGrammar.Dictionary
{
    public interface IConstructiveDictionary
    {
        /// <summary>
        /// Parses the text and creates the grammatical structure.
        /// </summary>
        /// <param name="text"></param>
        /// <returns></returns>
        IReadOnlyList<IText> AnalyzeText(string text);
    }
}

using Krino.ConstructiveGrammar.LinguisticStructures;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.ConstructiveGrammar.ConstructiveDictionaries
{
    public interface IConstructiveDictionary2
    {
        IEnumerable<IMorpheme> FreeMorphemes { get; }

        IEnumerable<IMorpheme> BoundMorphemes { get; }

        /// <summary>
        /// Finds all word decompositions which can represent the given word.
        /// </summary>
        /// <param name="word"></param>
        /// <param name="maxDistance">How much the value can differ from the word in the dictionary.</param>
        /// <returns></returns>
        IEnumerable<IWord> FindWords(string word, int maxDistance);


        /// <summary>
        /// Finds all free morphemes matching the value and maximal distnce from the word.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="maxDistance">How much the value can differ from the word in the dictionary.</param>
        /// <returns></returns>
        IEnumerable<IMorpheme> FindFreeMorphemes(string value, int maxDistance);

        /// <summary>
        /// Finds all bound morphemes matching the value.
        /// </summary>
        /// <remarks>
        /// Bound morphemes are morphemes which cannot stand alone e.g. prefixes, suffixes, ... .)
        /// </remarks>
        /// <param name="value"></param>
        /// <returns></returns>
        IEnumerable<IMorpheme> FindBoundMorphemes(string value);


        
    }
}

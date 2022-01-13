using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IWord : ILinguisticStructure
    {
        /// <summary>
        /// One lexeme and non-lexemes the word consists of.
        /// </summary>
        IEnumerable<Morpheme> Morphemes { get; }
    }
}

using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Represents a possibe morpheme compositions of a word.
    /// </summary>
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class WordDecomposition
    {
        public WordDecomposition(string word, IReadOnlyList<WordConstruct> compositions)
        {
            Word = word;
            Compositions = compositions ?? new List<WordConstruct>(0);
        }

        public string Word { get; private set; }

        public IReadOnlyList<WordConstruct> Compositions { get; private set; }

        private string DebuggerDisplay => $"{Word} : {Compositions.Count}";
    }
}

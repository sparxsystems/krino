using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Represents a word consisting of morphemes.
    /// </summary>
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class WordConstruct
    {
        public WordConstruct(IReadOnlyList<Morpheme> wordMorphemes)
        {
            Morphemes = wordMorphemes;
        }

        /// <summary>
        /// Sequence of morpehemes composing the word.
        /// </summary>
        public IReadOnlyList<Morpheme> Morphemes { get; private set; }

        private string DebuggerDisplay => Morphemes != null ? string.Join("-", Morphemes.Select(x => x.Morph)) : "";
    }
}

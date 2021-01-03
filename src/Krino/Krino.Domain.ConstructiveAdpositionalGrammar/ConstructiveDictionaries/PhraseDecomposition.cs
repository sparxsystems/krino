using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Represents a phrase consisting of words and their possible compositions.
    /// </summary>
    public class PhraseDecomposition
    {
        public PhraseDecomposition(IReadOnlyList<WordDecomposition> words)
        {
            Words = words;
        }

        public string Phrase => Words != null ? string.Join(" ", Words.Select(x => x.Word)) : "";
        public IReadOnlyList<WordDecomposition> Words { get; private set; }
    }
}
